#ifndef _FORMAT_H
#define _FORMAT_H

#include "xpath.h"
#include "error.h"
#include "debug.h"

#include <list>

namespace ledger {

class format_t
{
 public:
  struct element_t
  {
    bool	align_left;
    short	min_width;
    short	max_width;

    enum kind_t { UNKNOWN, TEXT, COLUMN, XPATH, GROUP } kind;
    union {
      std::string *  chars;
      xml::xpath_t * xpath;
      format_t *     format;
    };

    element_t()
      : align_left(false), min_width(-1), max_width(-1),
	kind(UNKNOWN), chars(NULL) {
      TRACE_CTOR("element_t()");
    }

    ~element_t() {
      TRACE_DTOR("element_t");

      switch (kind) {
      case TEXT:
	delete chars;
	break;
      case XPATH:
	delete xpath;
	break;
      case GROUP:
	delete format;
	break;
      default:
	assert(! chars);
	break;
      }
    }

   private:
    element_t(const element_t& other);
  };

  struct element_formatter_t {
    virtual ~element_formatter_t() {}
    virtual int operator()(std::ostream& out, element_t * element,
			   xml::node_t * context, int column) const;
  };

  std::string		 format_string;
  std::list<element_t *> elements;

 private:
  format_t(const format_t&);

 public:
  format_t() {
    TRACE_CTOR("format_t()");
  }
  format_t(const std::string& fmt) {
    TRACE_CTOR("format_t(const std::string&)");
    parse(fmt);
  }

  void clear_elements() {
    for (std::list<element_t *>::iterator i = elements.begin();
	 i != elements.end();
	 i++)
      delete *i;
    elements.clear();
  }

  virtual ~format_t() {
    TRACE_DTOR("format_t");
    clear_elements();
  }

  void parse(const std::string& fmt);

  void compile(const std::string& fmt, xml::node_t * context = NULL) {
    parse(fmt);
    compile(context);
  }
  void compile(xml::node_t * context = NULL);

  int format(std::ostream& out, xml::node_t * context = NULL,
	     int column = 0, const element_formatter_t& formatter =
	     element_formatter_t()) const;

  operator bool() const {
    return ! format_string.empty();
  }
};

class format_error : public error {
 public:
  format_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~format_error() throw() {}
};

} // namespace ledger

#endif // _FORMAT_H
