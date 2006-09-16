#ifndef _FORMAT_H
#define _FORMAT_H

#include "valexpr.h"
#include "error.h"
#include "debug.h"

#include <list>

namespace ledger {

struct element_t
{
  bool	      align_left;
  short	      min_width;
  short	      max_width;
  bool        column;
  std::string chars;
  valexpr_t   valexpr;

  element_t()
  : align_left(false), min_width(-1), max_width(-1), column(false) {
    TRACE_CTOR("element_t()");
  }

#if DEBUG_LEVEL >= BETA
  element_t(const element_t& other)
    : align_left(other.align_left),
      min_width(other.min_width),
      max_width(other.max_width),
      column(other.column),
      chars(other.chars),
      valexpr(other.valexpr) {
    TRACE_CTOR("element_t(copy)");
  }
#endif

  ~element_t() {
    TRACE_DTOR("element_t");
  }
};

struct format_t
{
  std::string format_string;
  std::list<element_t> elements;

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
  void compile(const std::string& fmt, valexpr_t::scope_t * scope = NULL) {
    parse(fmt);
    compile(scope);
  }

  void compile(valexpr_t::scope_t * scope = NULL);

  int format(std::ostream& out, valexpr_t::scope_t * details = NULL,
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
