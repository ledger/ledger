#include "parser.h"
#include "ledger.h"

#include <fstream>
#include <deque>

namespace ledger {

typedef std::deque<parser_t *> parsers_list;

static parsers_list parsers;

bool register_parser(parser_t * parser)
{
  parsers_list::iterator i;
  for (i = parsers.begin(); i != parsers.end(); i++)
    if (*i == parser)
      break;
  if (i != parsers.end())
    return false;

  parsers.push_back(parser);

  return true;
}

bool unregister_parser(parser_t * parser)
{
  parsers_list::iterator i;
  for (i = parsers.begin(); i != parsers.end(); i++)
    if (*i == parser)
      break;
  if (i == parsers.end())
    return false;

  parsers.erase(i);

  return true;
}

unsigned int parse_journal(std::istream&       in,
			   journal_t *	       journal,
			   account_t *	       master,
			   const std::string * original_file)
{
  if (! master)
    master = journal->master;

  for (parsers_list::iterator i = parsers.begin();
       i != parsers.end();
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
  journal->sources.push_back(path);

  if (access(path.c_str(), R_OK) == -1)
    return 0;

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

struct parser_wrap : public parser_t
{
  PyObject * self;
  parser_wrap(PyObject * self_) : self(self_) {}

  virtual bool test(std::istream& in) const {
    return call_method<bool>(self, "test", in);
  }

  virtual unsigned int parse(std::istream&	 in,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL) {
    return call_method<unsigned int>(self, "__call__", in, journal, master,
				     original_file);
  }
};

BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_overloads, parse_journal, 2, 4)
BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_file_overloads,
				parse_journal_file, 2, 4)

void export_parser() {
  class_< parser_t, parser_wrap, boost::noncopyable > ("Parser")
    ;

  def("register_parser", register_parser);
  def("unregister_parser", unregister_parser);
  def("parse_journal", parse_journal, parse_journal_overloads());
  def("parse_journal_file", parse_journal_file, parse_journal_file_overloads());
}

#endif // USE_BOOST_PYTHON
