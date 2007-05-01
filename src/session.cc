#include "session.h"

namespace ledger {

unsigned int session_t::read_journal(std::istream&	   in,
				     journal_t *	   journal,
				     account_t *	   master,
				     const optional<path>& original)
{
  if (! master)
    master = journal->master;

  for (ptr_list<parser_t>::iterator i = parsers.begin();
       i != parsers.end();
       i++)
    if (i->test(in))
      return i->parse(in, journal, master, original);

  return 0;
}

unsigned int session_t::read_journal(const path&	   pathname,
				     journal_t *	   journal,
				     account_t *	   master,
				     const optional<path>& original)
{
  journal->sources.push_back(pathname);

  if (! exists(pathname))
    throw filesystem_error(BOOST_CURRENT_FUNCTION, pathname,
			   "Cannot read file");

  ifstream stream(pathname);
  return read_journal(stream, journal, master,
		      original ? original : pathname);
}

void session_t::read_init()
{
  if (! init_file)
    return;

  if (! exists(*init_file))
    throw filesystem_error(BOOST_CURRENT_FUNCTION, *init_file,
			   "Cannot read init file");

  ifstream init(*init_file);

  // jww (2006-09-15): Read initialization options here!
}

journal_t * session_t::read_data(const string& master_account)
{
  if (data_file.empty())
    throw_(parse_error, "No journal file was specified (please use -f)");

  TRACE_START(parser, 1, "Parsing journal file");

  journal_t * journal = new_journal();
  journal->document = new xml::document_t;
  journal->document->set_top(xml::wrap_node(journal->document, journal));

  unsigned int entry_count = 0;

  DEBUG("ledger.cache", "3. use_cache = " << use_cache);

  if (use_cache && cache_file) {
    DEBUG("ledger.cache", "using_cache " << cache_file->string());
    cache_dirty = true;
    if (exists(*cache_file)) {
      ifstream stream(*cache_file);

      optional<path> price_db_orig = journal->price_db;
      try {
	journal->price_db = price_db;

	entry_count += read_journal(stream, journal, NULL, data_file);
	if (entry_count > 0)
	  cache_dirty = false;

	journal->price_db = price_db_orig;
      }
      catch (...) {
	journal->price_db = price_db_orig;
	throw;
      }
    }
  }

  if (entry_count == 0) {
    account_t * acct = NULL;
    if (! master_account.empty())
      acct = journal->find_account(master_account);

    journal->price_db = price_db;
    if (journal->price_db && exists(*journal->price_db)) {
      if (read_journal(*journal->price_db, journal)) {
	throw_(parse_error, "Entries not allowed in price history file");
      } else {
	DEBUG("ledger.cache",
	      "read price database " << journal->price_db->string());
	journal->sources.pop_back();
      }
    }

    DEBUG("ledger.cache", "rejected cache, parsing " << data_file.string());
    if (data_file == "-") {
      use_cache = false;
      journal->sources.push_back("<stdin>");
      entry_count += read_journal(std::cin, journal, acct);
    }
    else if (exists(data_file)) {
      entry_count += read_journal(data_file, journal, acct);
      if (journal->price_db)
	journal->sources.push_back(*journal->price_db);
    }
  }

  VERIFY(journal->valid());

  if (entry_count == 0)
    throw_(parse_error, "Failed to locate any journal entries; "
	   "did you specify a valid file with -f?");

  TRACE_STOP(parser, 1);

  return journal;
}

bool session_t::resolve(const string& name, value_t& result,
			xml::xpath_t::scope_t * locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'd':
    if (name == "date_format") {
      // jww (2007-04-18): What to do here?
#if 0
      result.set_string(moment_t::output_format);
#endif
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

  return xml::xpath_t::scope_t::resolve(name, result, locals);
}

xml::xpath_t::op_t * session_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (std::strncmp(p, "option_", 7) == 0) {
      p = p + 7;
      switch (*p) {
      case 'd':
	if (std::strcmp(p, "debug") == 0)
	  return MAKE_FUNCTOR(session_t, option_debug);
	break;

      case 'f':
	if (! *(p + 1) || std::strcmp(p, "file") == 0)
	  return MAKE_FUNCTOR(session_t, option_file);
	break;

      case 't':
	if (std::strcmp(p, "trace") == 0)
	  return MAKE_FUNCTOR(session_t, option_trace);
	break;

      case 'v':
	if (! *(p + 1) || std::strcmp(p, "verbose") == 0)
	  return MAKE_FUNCTOR(session_t, option_verbose);
	else if (std::strcmp(p, "verify") == 0)
	  return MAKE_FUNCTOR(session_t, option_verify);
	break;
      }
    }
    break;
  }

  return xml::xpath_t::scope_t::lookup(name);
}

// jww (2007-04-26): All of Ledger should be accessed through a
// session_t object
void initialize()
{
  IF_VERIFY()
    initialize_memory_tracing();

  amount_t::initialize();
  xml::xpath_t::initialize();
}

void shutdown()
{
  xml::xpath_t::shutdown();
  amount_t::shutdown();

  IF_VERIFY() {
    INFO("Ledger shutdown (Boost/libstdc++ may still hold memory)");
    shutdown_memory_tracing();
  } else {
    INFO("Ledger shutdown");
  }
}

} // namespace ledger
