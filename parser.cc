#include "parser.h"
#include "journal.h"
#include "config.h"

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

unsigned int parse_journal(std::istream& in,
			   config_t&     config,
			   journal_t *	 journal,
			   account_t *	 master,
			   const path *	 original_file)
{
  if (! master)
    master = journal->master;

  for (parsers_list::iterator i = parsers->begin();
       i != parsers->end();
       i++)
    if ((*i)->test(in))
      return (*i)->parse(in, config, journal, master, original_file);

  return 0;
}

unsigned int parse_journal_file(const path&  pathname,
				config_t&    config,
				journal_t *  journal,
				account_t *  master,
				const path * original_file)
{
  journal->sources.push_back(pathname);

  if (! boost::filesystem::exists(pathname))
    throw new error(string("Cannot read file '") +
		    string(pathname.string()) + "'");

  if (! original_file)
    original_file = &pathname;

  boost::filesystem::ifstream stream(pathname);
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
    xml_parser	 = xml_parser_ptr;
  if (! stdin_parser)
    stdin_parser = textual_parser_ptr;

  DEBUG("ledger.config.cache",
	      "3. use_cache = " << config.use_cache);

  if (! config.init_file.empty() &&
      boost::filesystem::exists(config.init_file)) {
    if (parse_journal_file(config.init_file.string(), config, journal) ||
	journal->auto_entries.size() > 0 ||
	journal->period_entries.size() > 0)
      throw new error(string("Entries found in initialization file '") +
		      string(config.init_file.string()) + "'");

    journal->sources.pop_front(); // remove init file
  }

  if (config.use_cache && ! config.cache_file.empty() &&
      ! config.data_file.empty()) {
    DEBUG("ledger.config.cache",
		"using_cache " << config.cache_file);
    config.cache_dirty = true;
    if (boost::filesystem::exists(config.cache_file)) {
      boost::filesystem::ifstream stream(config.cache_file);
      if (cache_parser && cache_parser->test(stream)) {
	optional<path> price_db_orig = journal->price_db;
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
    if (journal->price_db &&
	boost::filesystem::exists(*journal->price_db)) {
      if (parse_journal_file(*journal->price_db, config, journal)) {
	throw new error("Entries not allowed in price history file");
      } else {
	DEBUG("ledger.config.cache",
	      "read price database " << *journal->price_db);
	journal->sources.pop_back();
      }
    }

    DEBUG("ledger.config.cache",
		"rejected cache, parsing " << config.data_file);
    if (config.data_file == "-") {
      config.use_cache = false;
      journal->sources.push_back("<stdin>");
#if 0
      // jww (2006-03-23): Why doesn't XML work on stdin?
      if (xml_parser && std::cin.peek() == '<')
	entry_count += xml_parser->parse(std::cin, config, journal, acct);
      else if (stdin_parser)
#endif
	entry_count += stdin_parser->parse(std::cin, config, journal, acct);
    }
    else if (boost::filesystem::exists(config.data_file)) {
      entry_count += parse_journal_file(config.data_file, config, journal,
					acct);
      if (journal->price_db)
	journal->sources.push_back(*journal->price_db);
    }
  }

  VERIFY(journal->valid());

  return entry_count;
}

} // namespace ledger
