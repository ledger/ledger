#ifndef _FORMAT_H
#define _FORMAT_H

#include "ledger.h"
#include "valexpr.h"

namespace ledger {

std::string truncated(const std::string& str, unsigned int width);

std::string partial_account_name(const account_t *  account,
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
    SPACER
  };

  bool         align_left;
  unsigned int min_width;
  unsigned int max_width;

  kind_t       type;
  std::string  chars;
  node_t *     val_expr;

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
  element_t * elements;

  static std::auto_ptr<node_t> value_expr;
  static std::auto_ptr<node_t> total_expr;

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
  }

  static element_t * parse_elements(const std::string& fmt);

  void format_elements(std::ostream& out, const details_t& details) const;

  static void compute_value(balance_t& result, const details_t& details) {
    if (value_expr.get())
      value_expr->compute(result, details);
  }

  static void compute_total(balance_t& result, const details_t& details) {
    if (total_expr.get())
      total_expr->compute(result, details);
  }
};

class format_transaction
{
  std::ostream&   output_stream;
  const format_t& first_line_format;
  const format_t& next_lines_format;
  const bool      collapsed;
  const bool      inverted;

  item_predicate<transaction_t> disp_pred_functor;

  typedef bool (*intercept_t)(transaction_t * xact);

  intercept_t intercept;

  mutable balance_pair_t  subtotal;
  mutable unsigned int    count;
  mutable entry_t *	  last_entry;
  mutable transaction_t * last_xact;

 public:
  format_transaction(std::ostream&   _output_stream,
		     const format_t& _first_line_format,
		     const format_t& _next_lines_format,
		     const node_t *  display_predicate,
		     const bool      _collapsed = false,
		     const bool      _inverted  = false,
		     intercept_t     _intercept = NULL)
    : output_stream(_output_stream),
      first_line_format(_first_line_format),
      next_lines_format(_next_lines_format),
      collapsed(_collapsed), inverted(_inverted),
      disp_pred_functor(display_predicate),
      intercept(_intercept), count(0),
      last_entry(NULL), last_xact(NULL) {}

  void start() const {}
  void finish() const {
    if (subtotal)
      report_cumulative_subtotal();
  }

  void report_cumulative_subtotal() const;
  void operator()(transaction_t * xact) const;
};

// An intercept that can be used to report changes in commodity value
bool report_changed_values(transaction_t * xact);


class format_account
{
  std::ostream&   output_stream;
  const format_t& format;

  item_predicate<account_t> disp_pred_functor;

  mutable const account_t * last_account;

 public:
  format_account(std::ostream&   _output_stream,
		 const format_t& _format,
		 const node_t *  display_predicate = NULL)
    : output_stream(_output_stream), format(_format),
      disp_pred_functor(display_predicate), last_account(NULL) {}

  void start() const {}
  void finish() const {}

  void operator()(const account_t *  account,
		  const unsigned int max_depth  = 1,
		  const bool         report_top = false) const;
};

} // namespace ledger

#endif // _REPORT_H
