#include "parser.h"
#include "journal.h"

#include <fstream>
#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

namespace ledger {

typedef std::list<parser_t *> parsers_list;

static parsers_list * parsers = NULL;

void initialize_parser_support()
{
  parsers = new parsers_list;
}

void shutdown_parser_support()
{
  if (parsers) {
    delete parsers;
    parsers = NULL;
  }
}

bool register_parser(parser_t * parser)
{
  parsers_list::iterator i;
  for (i = parsers->begin(); i != parsers->end(); i++)
    if (*i == parser)
      break;
  if (i != parsers->end())
    return false;

  parsers->push_back(parser);

  return true;
}

bool unregister_parser(parser_t * parser)
{
  parsers_list::iterator i;
  for (i = parsers->begin(); i != parsers->end(); i++)
    if (*i == parser)
      break;
  if (i == parsers->end())
    return false;

  parsers->erase(i);

  return true;
}

unsigned int parse_journal(std::istream&       in,
			   journal_t *	       journal,
			   account_t *	       master,
			   const std::string * original_file)
{
  if (! master && journal)
    master = journal->master;

  for (parsers_list::iterator i = parsers->begin();
       i != parsers->end();
       i++)
    if ((*i)->test(in))
      return (*i)->parse(in, journal, master, original_file);

  return 0;
}

unsigned int parse_journal_file(const std::string&  path,
				journal_t *	    journal,
				account_t *	    master,
				const std::string * original_file)
{
  if (journal)
    journal->sources.push_back(path);

  if (access(path.c_str(), R_OK) == -1)
    throw new error(std::string("Cannot read file '") + path + "'");

  if (! original_file)
    original_file = &path;

  std::ifstream stream(path.c_str());
  return parse_journal(stream, journal, master, original_file);
}

} // namespace ledger

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

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
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
