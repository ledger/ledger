#include "parser.h"
#include "journal.h"
#include "config.h"

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

bool add_parser(parser_t * parser)
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

bool remove_parser(parser_t * parser)
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
			   config_t&           config,
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
      return (*i)->parse(in, config, journal, master, original_file);

  return 0;
}

unsigned int parse_journal_file(const std::string&  path,
				config_t&           config,
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
  return parse_journal(stream, config, journal, master, original_file);
}

extern parser_t * binary_parser_ptr;
extern parser_t * xml_parser_ptr;
extern parser_t * textual_parser_ptr;

unsigned int parse_ledger_data(config_t&   config,
			       journal_t * journal,
			       parser_t *  cache_parser,
			       parser_t *  xml_parser,
			       parser_t *  stdin_parser)
{
  unsigned int entry_count = 0;

  if (! cache_parser)
    cache_parser = binary_parser_ptr;
  if (! xml_parser)
    xml_parser = xml_parser_ptr;
  if (! stdin_parser)
    stdin_parser = textual_parser_ptr;

  DEBUG_PRINT("ledger.config.cache",
	      "3. use_cache = " << config.use_cache);

  if (config.use_cache && ! config.cache_file.empty() &&
      ! config.data_file.empty()) {
    DEBUG_PRINT("ledger.config.cache",
		"using_cache " << config.cache_file);
    config.cache_dirty = true;
    if (access(config.cache_file.c_str(), R_OK) != -1) {
      std::ifstream stream(config.cache_file.c_str());
      if (cache_parser && cache_parser->test(stream)) {
	std::string price_db_orig = journal->price_db;
	journal->price_db = config.price_db;
	entry_count += cache_parser->parse(stream, config, journal,
					   NULL, &config.data_file);
	if (entry_count > 0)
	  config.cache_dirty = false;
	else
	  journal->price_db = price_db_orig;
      }
    }
  }

  if (entry_count == 0 && ! config.data_file.empty()) {
    account_t * acct = NULL;
    if (! config.account.empty())
      acct = journal->find_account(config.account);

    journal->price_db = config.price_db;
    if (! journal->price_db.empty() &&
	access(journal->price_db.c_str(), R_OK) != -1) {
      if (parse_journal_file(journal->price_db, config, journal)) {
	throw new error("Entries not allowed in price history file");
      } else {
	DEBUG_PRINT("ledger.config.cache",
		    "read price database " << journal->price_db);
	journal->sources.pop_back();
      }
    }

    DEBUG_PRINT("ledger.config.cache",
		"rejected cache, parsing " << config.data_file);
    if (config.data_file == "-") {
      config.use_cache = false;
      journal->sources.push_back("<stdin>");
#if 0
      // jww (2006-03-23): Why doesn't XML work on stdin?
      if (xml_parser && std::cin.peek() == '<')
	entry_count += xml_parser->parse(std::cin, config, journal,
					 acct);
      else if (stdin_parser)
#endif
	entry_count += stdin_parser->parse(std::cin, config,
					   journal, acct);
    }
    else if (access(config.data_file.c_str(), R_OK) != -1) {
      entry_count += parse_journal_file(config.data_file, config,
					journal, acct);
      if (! journal->price_db.empty())
	journal->sources.push_back(journal->price_db);
    }
  }

  VALIDATE(journal->valid());

  return entry_count;
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
			     config_t&		 config,
			     journal_t *	 journal,
			     account_t *	 master        = NULL,
			     const std::string * original_file = NULL) {
    return call_method<unsigned int>(self, "parse", in, config, journal,
				     master, original_file);
  }
};

BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_overloads, parse_journal, 3, 5)
BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_file_overloads,
				parse_journal_file, 3, 5)

void export_parser() {
  class_< parser_t, py_parser_t, boost::noncopyable > ("Parser")
    .def("test", &parser_t::test)
    .def("parse", &parser_t::parse)
    ;

  def("add_parser", add_parser);
  def("remove_parser", remove_parser);
  def("parse_journal", parse_journal, parse_journal_overloads());
  def("parse_journal_file", parse_journal_file, parse_journal_file_overloads());
}

#endif // USE_BOOST_PYTHON
