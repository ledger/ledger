#ifdef USE_PCH
#include "pch.h"
#else
#include "session.h"
#include "repitem.h"

#include <fstream>
#endif

namespace ledger {

unsigned int session_t::read_journal(std::istream&       in,
				     journal_t *         journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  if (! master)
    master = journal->master;

  journal->data = repitem_t::wrap(journal);

  for (std::list<parser_t *>::iterator i = parsers.begin();
       i != parsers.end();
       i++)
    if ((*i)->test(in))
      return (*i)->parse(in, journal, master, original_file);

  return 0;
}

unsigned int session_t::read_journal(const std::string&  path,
				     journal_t *         journal,
				     account_t *	 master,
				     const std::string * original_file)
{
  journal->sources.push_back(path);

  if (access(path.c_str(), R_OK) == -1)
    throw new error(std::string("Cannot read file '") + path + "'");

  if (! original_file)
    original_file = &path;

  std::ifstream stream(path.c_str());
  return read_journal(stream, journal, master, original_file);
}

void session_t::read_init()
{
  if (init_file.empty())
    return;

  if (access(init_file.c_str(), R_OK) == -1)
    throw new error(std::string("Cannot read init file '") + init_file + "'");

  std::ifstream init(init_file.c_str());

  // jww (2006-09-15): Read initialization options here!
}

journal_t * session_t::read_data(const std::string& master_account)
{
  TRACE_PUSH(parser, "Parsing journal file");

  journal_t * journal = new_journal();

  unsigned int entry_count = 0;

  DEBUG_PRINT("ledger.cache",
	      "3. use_cache = " << use_cache);

  if (use_cache && ! cache_file.empty() &&
      ! data_file.empty()) {
    DEBUG_PRINT("ledger.cache",
		"using_cache " << cache_file);
    cache_dirty = true;
    if (access(cache_file.c_str(), R_OK) != -1) {
      std::ifstream stream(cache_file.c_str());

      std::string price_db_orig = journal->price_db;
      journal->price_db = price_db;
      entry_count += read_journal(stream, journal, NULL,
					  &data_file);
      if (entry_count > 0)
	cache_dirty = false;
      else
	journal->price_db = price_db_orig;
    }
  }

  if (entry_count == 0 && ! data_file.empty()) {
    account_t * acct = NULL;
    if (! master_account.empty())
      acct = journal->find_account(master_account);

    journal->price_db = price_db;
    if (! journal->price_db.empty() &&
	access(journal->price_db.c_str(), R_OK) != -1) {
      if (read_journal(journal->price_db, journal)) {
	throw new error("Entries not allowed in price history file");
      } else {
	DEBUG_PRINT("ledger.cache",
		    "read price database " << journal->price_db);
	journal->sources.pop_back();
      }
    }

    DEBUG_PRINT("ledger.cache",
		"rejected cache, parsing " << data_file);
    if (data_file == "-") {
      use_cache = false;
      journal->sources.push_back("<stdin>");
      entry_count += read_journal(std::cin, journal, acct);
    }
    else if (access(data_file.c_str(), R_OK) != -1) {
      entry_count += read_journal(data_file, journal, acct);
      if (! journal->price_db.empty())
	journal->sources.push_back(journal->price_db);
    }
  }

  VALIDATE(journal->valid());

  if (entry_count == 0)
    throw new error("Failed to locate any journal entries; "
		    "did you specify a valid file with -f?");

  TRACE_POP(parser, "Finished parsing");

  return journal;
}

bool session_t::resolve(const std::string& name, value_t& result,
			valexpr_t::scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'd':
    if (name == "date_format") {
      result = datetime_t::output_format;
      return true;
    }
    break;

  case 'n':
    switch (*++p) {
    case 'o':
      if (name == "now") {
	result = now;
	return true;
      }
      break;
    }
    break;

  case 'r':
    if (name == "register_format") {
      result = register_format;
      return true;
    }
    break;
  }

  return valexpr_t::scope_t::resolve(name, result, locals);
}

valexpr_t::node_t * session_t::lookup(const std::string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (std::strncmp(p, "option_", 7) == 0) {
      p = p + 7;
      switch (*p) {
      case 'f':
	if (! *(p + 1) || std::strcmp(p, "file") == 0)
	  return MAKE_FUNCTOR(session_t, option_file);
	break;

      case 'v':
	if (std::strcmp(p, "verbose") == 0)
	  return MAKE_FUNCTOR(session_t, option_verbose);
	break;
      }
    }
    break;
  }

  return valexpr_t::scope_t::lookup(name);
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

using namespace boost::python;
using namespace ledger;

void export_session()
{
  class_< session_t > ("Session")
    .def_readwrite("init_file", &session_t::init_file)
    .def_readwrite("data_file", &session_t::data_file)
    .def_readwrite("cache_file", &session_t::cache_file)
    .def_readwrite("price_db", &session_t::price_db)

    .def_readwrite("balance_format", &session_t::balance_format)
    .def_readwrite("register_format", &session_t::register_format)
    .def_readwrite("wide_register_format", &session_t::wide_register_format)
    .def_readwrite("plot_amount_format", &session_t::plot_amount_format)
    .def_readwrite("plot_total_format", &session_t::plot_total_format)
    .def_readwrite("print_format", &session_t::print_format)
    .def_readwrite("write_hdr_format", &session_t::write_hdr_format)
    .def_readwrite("write_xact_format", &session_t::write_xact_format)
    .def_readwrite("equity_format", &session_t::equity_format)
    .def_readwrite("prices_format", &session_t::prices_format)
    .def_readwrite("pricesdb_format", &session_t::pricesdb_format)

    .def_readwrite("pricing_leeway", &session_t::pricing_leeway)

    .def_readwrite("download_quotes", &session_t::download_quotes)
    .def_readwrite("use_cache", &session_t::use_cache)
    .def_readwrite("cache_dirty", &session_t::cache_dirty)
    .def_readwrite("debug_mode", &session_t::debug_mode)
    .def_readwrite("verbose_mode", &session_t::verbose_mode)
    .def_readwrite("trace_mode", &session_t::trace_mode)

    .def_readwrite("journals", &session_t::journals)
    ;
}

#endif // USE_BOOST_PYTHON
