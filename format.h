#ifndef _FORMAT_H
#define _FORMAT_H

#include "valexpr.h"
#include "error.h"
#include "debug.h"

#include <list>

namespace ledger {

#if 0
std::string abbrev(const std::string& str, unsigned int width,
		   const int style = 2);

std::string partial_account_name(const account_t&   account,
				 const unsigned int start_depth);
#endif

struct element_t
{
  bool	      align_left;
  short	      min_width;
  short	      max_width;
  bool        column;
  std::string chars;
  valexpr_t   valexpr;

  element_t()
  : align_left(false), min_width(0), max_width(0), column(false) {
    TRACE_CTOR("element_t()");
  }
  ~element_t() {
    TRACE_DTOR("element_t");
  }
};

struct format_t
{
  std::string format_string;
  std::list<element_t> elements;

#if 0
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
#endif

  format_t() {
    TRACE_CTOR("format_t()");
  }
  format_t(const std::string& fmt) {
    TRACE_CTOR("format_t(const std::string&)");
    parse(fmt);
  }
  ~format_t() {
    TRACE_DTOR("format_t");
  }

  void parse(const std::string& fmt);

#if 0
  static std::string truncate(const std::string& str, unsigned int width,
			      const bool is_account = false);
#endif

  void format(std::ostream& out, valexpr_t::scope_t * details = NULL,
	      int column = 0) const;
};

class format_error : public error {
 public:
  format_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~format_error() throw() {}
};

} // namespace ledger

#endif // _FORMAT_H
