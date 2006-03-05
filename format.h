#ifndef _FORMAT_H
#define _FORMAT_H

#include "journal.h"
#include "valexpr.h"
#include "walk.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width,
		      const int style = 2);

std::string partial_account_name(const account_t&   account,
				 const unsigned int start_depth);

struct element_t
{
  enum kind_t {
    STRING,
    VALUE_EXPR,
    SOURCE,
    ENTRY_BEG_POS,
    ENTRY_BEG_LINE,
    ENTRY_END_POS,
    ENTRY_END_LINE,
    XACT_BEG_POS,
    XACT_BEG_LINE,
    XACT_END_POS,
    XACT_END_LINE,
    DATE_STRING,
    COMPLETE_DATE_STRING,
    CLEARED,
    ENTRY_CLEARED,
    CODE,
    PAYEE,
    OPT_ACCOUNT,
    ACCOUNT_NAME,
    ACCOUNT_FULLNAME,
    AMOUNT,
    OPT_AMOUNT,
    TOTAL,
    NOTE,
    OPT_NOTE,
    SPACER,
    DEPTH_SPACER
  };

  bool	       align_left;
  unsigned int min_width;
  unsigned int max_width;

  kind_t       type;
  std::string  chars;
  value_expr * val_expr;

  struct element_t * next;

  element_t() : align_left(false), min_width(0), max_width(0),
		type(STRING), val_expr(NULL), next(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor element_t");
  }

  ~element_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor element_t");
    if (val_expr) delete val_expr;
    if (next) delete next;	// recursive, but not too deep
  }
};

struct format_t
{
  std::string format_string;
  element_t * elements;

  format_t() : elements(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor format_t");
  }
  format_t(const std::string& _format) : elements(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor format_t");
    reset(_format);
  }
  ~format_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor format_t");
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
};

class format_transactions : public item_handler<transaction_t>
{
 protected:
  std::ostream&   output_stream;
  format_t	  first_line_format;
  format_t	  next_lines_format;
  entry_t *       last_entry;
  transaction_t * last_xact;

 public:
  format_transactions(std::ostream& _output_stream,
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

  virtual void format_last_entry();

  virtual void flush() {
    if (last_entry) {
      format_last_entry();
      last_entry = NULL;
    }
    format_transactions::flush();
  }
  virtual void operator()(transaction_t& xact);
};

void print_entry(std::ostream& out, const entry_t& entry);

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
    : output_stream(_output_stream), disp_pred(display_predicate),
      format(_format) {}

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

#endif // _FORMAT_H
