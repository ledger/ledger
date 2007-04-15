#include "parser.h"

#if 0
#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <Python.h>

using namespace boost::python;
using namespace ledger;

struct py_parser_t : public parser_t
{
  PyObject * self;
  py_parser_t(PyObject * self_) : self(self_) {}

  virtual bool test(std::istream& in) const {
    return call_method<bool>(self, "test", in);
  }

  virtual repitem_t * parse(std::istream&       in,
			    journal_t *	        journal,
			    account_t *	        master	      = NULL,
			    const std::string * original_file = NULL) {
    return call_method<unsigned int>(self, "parse", in, journal, master,
				     original_file);
  }
};

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(parser_parse_overloads,
				       py_parser_t::parse, 2, 4)

BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_overloads, parse_journal, 2, 4)
BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_file_overloads,
				parse_journal_file, 2, 4)

void export_parser() {
  class_< parser_t, py_parser_t, boost::noncopyable > ("Parser")
    .def("test", &py_parser_t::test)
    .def("parse", &py_parser_t::parse, parser_parse_overloads())
    ;

  def("register_parser", register_parser);
  def("unregister_parser", unregister_parser);

  def("parse_journal", parse_journal, parse_journal_overloads());
  def("parse_journal_file", parse_journal_file, parse_journal_file_overloads());
}

#endif // USE_BOOST_PYTHON
#endif
