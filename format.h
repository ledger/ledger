#ifndef _FORMAT_H
#define _FORMAT_H

#include "journal.h"
#include "valexpr.h"
#include "walk.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width);

std::string partial_account_name(const account_t&   account,
				 const unsigned int start_depth);

struct element_t
{
  enum kind_t {
    STRING,
    VALUE_EXPR,
    DATE_STRING,
    CLEARED,
    CODE,
    PAYEE,
    ACCOUNT_NAME,
    ACCOUNT_FULLNAME,
    OPT_AMOUNT,
    AMOUNT,
    TOTAL,
    SPACER,
    DEPTH_SPACER
  };

  bool		 align_left;
  unsigned int	 min_width;
  unsigned int	 max_width;

  kind_t	 type;
  std::string	 chars;
  value_expr_t * val_expr;

  struct element_t * next;

  element_t() : align_left(false), min_width(0), max_width(0),
		type(STRING), val_expr(NULL), next(NULL) {}

  ~element_t() {
    if (val_expr) delete val_expr;
    if (next) delete next;	// recursive, but not too deep
  }
};

struct format_t
{
  std::string format_string;
  element_t * elements;

  static std::string	date_format;
  static value_expr_t * amount_expr;
  static value_expr_t * total_expr;

  format_t() : elements(NULL) {}
  format_t(const std::string& _format) : elements(NULL) {
    reset(_format);
  }
  ~format_t() {
    if (elements) delete elements;
  }

  void reset(const std::string& _format) {
    if (elements)
      delete elements;
    elements = parse_elements(_format);
    format_string = _format;
  }

  static element_t * parse_elements(const std::string& fmt);

  void format(std::ostream& out, const details_t& details) const;

  static void compute_amount(value_t& result, const details_t& details) {
    if (amount_expr)
      amount_expr->compute(result, details);
  }

  static void compute_total(value_t& result, const details_t& details) {
    if (total_expr)
      total_expr->compute(result, details);
  }
};

class format_transactions : public item_handler<transaction_t>
{
 protected:
  std::ostream& output_stream;
  format_t	first_line_format;
  format_t	next_lines_format;
  entry_t *     last_entry;

 public:
  format_transactions(std::ostream&      _output_stream,
		      const std::string& format);

  virtual void flush() {
    output_stream.flush();
  }
  virtual void operator()(transaction_t& xact);
};

class format_entries : public format_transactions
{
 public:
  format_entries(std::ostream& output_stream, const std::string& format)
    : format_transactions(output_stream, format) {}

  void format_last_entry();

  virtual void flush() {
    if (last_entry) {
      format_last_entry();
      last_entry = NULL;
    }
    format_transactions::flush();
  }
  virtual void operator()(transaction_t& xact);
};

bool disp_subaccounts_p(const account_t& account,
			const item_predicate<account_t>& disp_pred,
			const account_t *& to_show);

inline bool disp_subaccounts_p(const account_t& account) {
  const account_t * temp;
  return disp_subaccounts_p(account, item_predicate<account_t>(NULL), temp);
}

bool display_account(const account_t& account,
		     const item_predicate<account_t>& disp_pred);

class format_account : public item_handler<account_t>
{
  std::ostream& output_stream;

  item_predicate<account_t> disp_pred;

 public:
  format_t format;

  format_account(std::ostream&      _output_stream,
		 const std::string& _format,
		 const std::string& display_predicate = NULL)
    : output_stream(_output_stream), format(_format),
      disp_pred(display_predicate) {}

  virtual void flush() {
    output_stream.flush();
  }

  virtual void operator()(account_t& account);
};

class format_equity : public item_handler<account_t>
{
  std::ostream& output_stream;
  format_t	first_line_format;
  format_t	next_lines_format;

  item_predicate<account_t> disp_pred;

  mutable value_t total;

 public:
  format_equity(std::ostream&      _output_stream,
		const std::string& _format,
		const std::string& display_predicate);

  virtual void flush();
  virtual void operator()(account_t& account);
};

} // namespace ledger

#endif // _REPORT_H
