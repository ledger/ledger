#ifndef _FORMAT_H
#define _FORMAT_H

#include "ledger.h"
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
    VALUE,
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
  static value_expr_t * value_expr;
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

  static void compute_value(value_t& result, const details_t& details) {
    if (value_expr)
      value_expr->compute(result, details);
  }

  static void compute_total(value_t& result, const details_t& details) {
    if (total_expr)
      total_expr->compute(result, details);
  }
};

class format_transactions : public item_handler<transaction_t>
{
  std::ostream&   output_stream;
  const format_t& first_line_format;
  const format_t& next_lines_format;
  entry_t *       last_entry;

 public:
  format_transactions(std::ostream&   _output_stream,
		      const format_t& _first_line_format,
		      const format_t& _next_lines_format)
    : output_stream(_output_stream),
      first_line_format(_first_line_format),
      next_lines_format(_next_lines_format), last_entry(NULL) {}

  virtual void flush() {
    output_stream.flush();
  }

  virtual void operator()(transaction_t& xact) {
    if (! transaction_has_xdata(xact) ||
	! (transaction_xdata(xact).dflags & TRANSACTION_DISPLAYED)) {
      if (last_entry != xact.entry) {
	first_line_format.format(output_stream, details_t(xact));
	last_entry = xact.entry;
      } else {
	next_lines_format.format(output_stream, details_t(xact));
      }
      transaction_xdata(xact).dflags |= TRANSACTION_DISPLAYED;
    }
  }
};

class format_account : public item_handler<account_t>
{
  std::ostream&   output_stream;
  const format_t& format;

  item_predicate<account_t> disp_pred;

 public:
  format_account(std::ostream&      _output_stream,
		 const format_t&    _format,
		 const std::string& display_predicate = NULL)
    : output_stream(_output_stream), format(_format),
      disp_pred(display_predicate) {}

  static bool disp_subaccounts_p(const account_t& account,
				 const item_predicate<account_t>& disp_pred,
				 const account_t *& to_show);
  static bool disp_subaccounts_p(const account_t& account) {
    const account_t * temp;
    return disp_subaccounts_p(account, item_predicate<account_t>(NULL), temp);
  }

  static bool display_account(const account_t& account,
			      const item_predicate<account_t>& disp_pred);

  virtual void flush() {
    output_stream.flush();
  }

  virtual void operator()(account_t& account) {
    if (display_account(account, disp_pred)) {
      if (! account.parent) {
	account_xdata(account).dflags |= ACCOUNT_TO_DISPLAY;
      } else {
	format.format(output_stream, details_t(account));
	account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
      }
    }
  }
};

class format_equity : public item_handler<account_t>
{
  std::ostream&   output_stream;
  const format_t& first_line_format;
  const format_t& next_lines_format;

  item_predicate<account_t> disp_pred;

  mutable value_t total;

 public:
  format_equity(std::ostream&      _output_stream,
		const format_t&    _first_line_format,
		const format_t&    _next_lines_format,
		const std::string& display_predicate = NULL)
    : output_stream(_output_stream),
      first_line_format(_first_line_format),
      next_lines_format(_next_lines_format),
      disp_pred(display_predicate) {
    entry_t header_entry;
    header_entry.payee = "Opening Balances";
    header_entry.date  = std::time(NULL);
    first_line_format.format(output_stream, details_t(header_entry));
  }

  virtual void flush() {
    account_xdata_t xdata;
    xdata.value = total;
    xdata.value.negate();
    account_t summary(NULL, "Equity:Opening Balances");
    summary.data = &xdata;
    next_lines_format.format(output_stream, details_t(summary));
    output_stream.flush();
  }

  virtual void operator()(account_t& account) {
    if (format_account::display_account(account, disp_pred)) {
      next_lines_format.format(output_stream, details_t(account));
      if (account_has_xdata(account))
	total += account_xdata(account).value;
      account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
    }
  }
};

} // namespace ledger

#endif // _REPORT_H
