#ifndef _FORMAT_H
#define _FORMAT_H

#include "journal.h"
#include "expr.h"
#include "walk.h"

namespace ledger {

string truncated(const string& str, unsigned int width,
		      const int style = 2);

string partial_account_name(const account_t&   account,
				 const unsigned int start_depth);

#define ELEMENT_ALIGN_LEFT 0x01
#define ELEMENT_HIGHLIGHT  0x02

struct element_t : public noncopyable
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

  kind_t	type;
  unsigned char flags;
  string	chars;
  unsigned char min_width;
  unsigned char max_width;
  expr_t	val_expr;

  struct element_t * next;

  element_t() : type(STRING), flags(false),
		min_width(0), max_width(0), next(NULL) {
    TRACE_CTOR(element_t, "");
  }

  ~element_t() {
    TRACE_DTOR(element_t);
    if (next) checked_delete(next); // recursive, but not too deep
  }
};

struct format_t : public noncopyable
{
  string format_string;
  element_t * elements;

  enum elision_style_t {
    TRUNCATE_TRAILING,
    TRUNCATE_MIDDLE,
    TRUNCATE_LEADING,
    ABBREVIATE
  };

  static elision_style_t elision_style;
  static int abbrev_length;

  static bool ansi_codes;
  static bool ansi_invert;

  format_t() : elements(NULL) {
    TRACE_CTOR(format_t, "");
  }
  format_t(const string& _format) : elements(NULL) {
    TRACE_CTOR(format_t, "const string&");
    reset(_format);
  }
  ~format_t() {
    TRACE_DTOR(format_t);
    if (elements) checked_delete(elements);
  }

  void reset(const string& _format) {
    if (elements)
      checked_delete(elements);
    elements = parse_elements(_format);
    format_string = _format;
  }

  static element_t * parse_elements(const string& fmt);

  static string truncate(const string& str, unsigned int width,
			      const bool is_account = false);

  void format(std::ostream& out, const scope_t& scope) const;
};

class format_xacts : public item_handler<xact_t>
{
protected:
  std::ostream&   output_stream;
  format_t	  first_line_format;
  format_t	  next_lines_format;
  entry_t *       last_entry;
  xact_t * last_xact;

public:
  format_xacts(std::ostream& _output_stream,
		      const string& format);
  ~format_xacts() throw() {
    TRACE_DTOR(format_xacts);
  }

  virtual void flush() {
    output_stream.flush();
  }
  virtual void operator()(xact_t& xact);
};

class format_entries : public format_xacts
{
 public:
  format_entries(std::ostream& output_stream, const string& format)
    : format_xacts(output_stream, format) {
    TRACE_CTOR(format_entries, "std::ostream&, const string&");
  }
  ~format_entries() throw() {
    TRACE_DTOR(format_entries);
  }

  virtual void format_last_entry();

  virtual void flush() {
    if (last_entry) {
      format_last_entry();
      last_entry = NULL;
    }
    format_xacts::flush();
  }
  virtual void operator()(xact_t& xact);
};

void print_entry(std::ostream& out, const entry_base_t& entry,
		 const string& prefix = "");

bool disp_subaccounts_p(const account_t& account,
			const optional<item_predicate<account_t> >& disp_pred,
			const account_t *& to_show);

inline bool disp_subaccounts_p(const account_t& account) {
  const account_t * temp;
  return disp_subaccounts_p(account, none, temp);
}

bool display_account(const account_t& account,
		     const optional<item_predicate<account_t> >& disp_pred);

class format_accounts : public item_handler<account_t>
{
  std::ostream& output_stream;

  item_predicate<account_t> disp_pred;

 public:
  format_t format;

  format_accounts(std::ostream& _output_stream,
		  const string& _format,
		  const string& display_predicate = NULL)
    : output_stream(_output_stream), disp_pred(display_predicate),
      format(_format) {
    TRACE_CTOR(format_accounts, "std::ostream&, const string&, const string&");
  }
  ~format_accounts() throw() {
    TRACE_DTOR(format_accounts);
  }

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
  format_equity(std::ostream& _output_stream,
		const string& _format,
		const string& display_predicate);

  virtual void flush();
  virtual void operator()(account_t& account);
};

class format_error : public error
{
 public:
  format_error(const string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~format_error() throw() {}
};

} // namespace ledger

#endif // _FORMAT_H
