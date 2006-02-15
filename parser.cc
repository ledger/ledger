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
  if (! master)
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
  journal->sources.push_back(path);

  if (access(path.c_str(), R_OK) == -1)
    throw error(std::string("Cannot read file '") + path + "'");

  if (! original_file)
    original_file = &path;

  std::ifstream stream(path.c_str());
  return parse_journal(stream, journal, master, original_file);
}

unsigned int parse_ledger_data(journal_t *	  journal,
			       const std::string& data_file,
			       const std::string& init_file,
			       const std::string& price_db,
			       bool		  use_cache,
			       const std::string& cache_file,
			       bool *		  cache_dirty,
			       parser_t *         cache_parser,
			       parser_t *         xml_parser,
			       parser_t *         stdin_parser,
			       const std::string& default_account)
{
  unsigned int entry_count = 0;

  DEBUG_PRINT("ledger.config.cache", "3. use_cache = " << use_cache);

  if (! init_file.empty() && access(init_file.c_str(), R_OK) != -1) {
    if (parse_journal_file(init_file, journal) ||
	journal->auto_entries.size() > 0 ||
	journal->period_entries.size() > 0)
      throw error(std::string("Entries found in initialization file '") +
		  init_file + "'");

    journal->sources.pop_front(); // remove init file
  }

  if (use_cache && ! cache_file.empty() && ! data_file.empty()) {
    DEBUG_PRINT("ledger.config.cache", "using_cache " << cache_file);
    if (cache_dirty)
      *cache_dirty = true;
    if (access(cache_file.c_str(), R_OK) != -1) {
      std::ifstream stream(cache_file.c_str());
      if (cache_parser && cache_parser->test(stream)) {
	std::string price_db_orig = journal->price_db;
	journal->price_db = price_db;
	entry_count += cache_parser->parse(stream, journal, NULL, &data_file);
	if (entry_count > 0) {
	  if (cache_dirty)
	    *cache_dirty = false;
	} else {
	  journal->price_db = price_db_orig;
	}
      }
    }
  }

  if (entry_count == 0 && ! data_file.empty()) {
    account_t * acct = NULL;
    if (! default_account.empty())
      acct = journal->find_account(default_account);

    journal->price_db = price_db;
    if (! journal->price_db.empty() &&
	access(journal->price_db.c_str(), R_OK) != -1) {
      if (parse_journal_file(journal->price_db, journal)) {
	throw error("Entries not allowed in price history file");
      } else {
	DEBUG_PRINT("ledger.config.cache",
		    "read price database " << journal->price_db);
	journal->sources.pop_back();
      }
    }

    DEBUG_PRINT("ledger.config.cache",
		"rejected cache, parsing " << data_file);
    if (data_file == "-") {
      use_cache = false;
      journal->sources.push_back("<stdin>");
#if 0
      if (xml_parser && std::cin.peek() == '<')
	entry_count += xml_parser->parse(std::cin, journal, acct);
      else if (stdin_parser)
#endif
	entry_count += stdin_parser->parse(std::cin, journal, acct);
    }
    else if (access(data_file.c_str(), R_OK) != -1) {
      entry_count += parse_journal_file(data_file, journal, acct);
      if (! journal->price_db.empty())
	journal->sources.push_back(journal->price_db);
    }
  }

  VALIDATE(journal->valid());

  return entry_count;
}

extern parser_t * binary_parser_ptr;
extern parser_t * xml_parser_ptr;
extern parser_t * textual_parser_ptr;

unsigned int parse_ledger_data(journal_t * journal, config_t& config)
{
  return parse_ledger_data(journal, config.data_file, config.init_file,
			   config.price_db, config.use_cache,
			   config.cache_file, &config.cache_dirty,
			   binary_parser_ptr, xml_parser_ptr,
			   textual_parser_ptr, config.account);
}

} // namespace ledger
