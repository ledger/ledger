/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "session.h"
#include "report.h"
#include "walk.h"

namespace ledger {

session_t * session_t::current = NULL;

#if 0
boost::mutex session_t::session_mutex;
#endif

void set_session_context(session_t * session)
{
#if 0
  session_t::session_mutex.lock();
#endif

  if (session && ! session_t::current) {
    session_t::initialize();
  }
  else if (! session && session_t::current) {
    session_t::shutdown();
#if 0
    session_t::session_mutex.unlock();
#endif
  }

  session_t::current = session;
}

void release_session_context()
{
#if 0
  session_t::session_mutex.unlock();
#endif
}

session_t::session_t()
  : register_format
    ("%-.10D %-.20P %-.22A %12.67t %!12.80T\n%/"
     "%32|%-.22A %12.67t %!12.80T\n"),
    wide_register_format
    ("%-.10D  %-.35P %-.38A %22.108t %!22.132T\n%/"
     "%48|%-.38A %22.108t %!22.132T\n"),
    print_format
    ("\n%d %Y%C%P\n    %-34W  %12o%n\n%/    %-34W  %12o%n\n"),
    balance_format
    ("%20T  %2_%-a\n"),
    equity_format
    ("\n%D %Y%C%P\n%/    %-34W  %12t\n"),
    plot_amount_format
    ("%D %(S(t))\n"),
    plot_total_format
    ("%D %(S(T))\n"),
    write_hdr_format
    ("%d %Y%C%P\n"),
    write_xact_format
    ("    %-34W  %12o%n\n"),
    prices_format
    ("%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n"),
    pricesdb_format
    ("P %[%Y/%m/%d %H:%M:%S] %A %t\n"),

    pricing_leeway(24 * 3600),

    download_quotes(false),
    use_cache(false),
    cache_dirty(false),

    now(now),

#if 0
    elision_style(ABBREVIATE),
#endif
    abbrev_length(2),

    ansi_codes(false),
    ansi_invert(false),

    master(new account_t(NULL, ""))
{
  TRACE_CTOR(session_t, "");
}

session_t::~session_t()
{
  TRACE_DTOR(session_t);
}

std::size_t session_t::read_journal(journal_t&	  journal,
				    std::istream& in,
				    const path&	  pathname,
				    account_t *   master)
{
  if (! master)
    master = journal.master;

  foreach (journal_t::parser_t& parser, parsers)
    if (parser.test(in))
      return parser.parse(in, *this, journal, master, &pathname);

  return 0;
}

std::size_t session_t::read_journal(journal_t&	journal,
				    const path& pathname,
				    account_t * master)
{
  journal.sources.push_back(pathname);

  if (! exists(pathname))
    throw_(std::logic_error, "Cannot read file" << pathname);

  ifstream stream(pathname);
  return read_journal(journal, stream, pathname, master);
}

void session_t::read_init()
{
  if (! init_file)
    return;

  if (! exists(*init_file))
    throw_(std::logic_error, "Cannot read init file" << *init_file);

  ifstream init(*init_file);

  // jww (2006-09-15): Read initialization options here!
}

std::size_t session_t::read_data(journal_t&    journal,
				 const string& master_account)
{
  if (data_file.empty())
    throw_(parse_error, "No journal file was specified (please use -f)");

  TRACE_START(parser, 1, "Parsing journal file");

  std::size_t entry_count = 0;

  DEBUG("ledger.cache", "3. use_cache = " << use_cache);

  if (use_cache && cache_file) {
    DEBUG("ledger.cache", "using_cache " << cache_file->string());
    cache_dirty = true;
    if (exists(*cache_file)) {
      push_variable<optional<path> >
	save_price_db(journal.price_db, price_db);

      entry_count += read_journal(journal, *cache_file);
      if (entry_count > 0)
	cache_dirty = false;
    }
  }

  if (entry_count == 0) {
    account_t * acct = NULL;
    if (! master_account.empty())
      acct = journal.find_account(master_account);

    journal.price_db = price_db;
    if (journal.price_db && exists(*journal.price_db)) {
      if (read_journal(journal, *journal.price_db)) {
	throw_(parse_error, "Entries not allowed in price history file");
      } else {
	DEBUG("ledger.cache",
	      "read price database " << journal.price_db->string());
	journal.sources.pop_back();
      }
    }

    DEBUG("ledger.cache", "rejected cache, parsing " << data_file.string());
    if (data_file == "-") {
      use_cache = false;
      journal.sources.push_back("/dev/stdin");
      entry_count += read_journal(journal, std::cin, "/dev/stdin", acct);
    }
    else if (exists(data_file)) {
      entry_count += read_journal(journal, data_file, acct);
      if (journal.price_db)
	journal.sources.push_back(*journal.price_db);
      clean_accounts();
    }
  }

  VERIFY(journal.valid());

  TRACE_STOP(parser, 1);

  return entry_count;
}

namespace {
  account_t * find_account_re_(account_t * account, const mask_t& regexp)
  {
    if (regexp.match(account->fullname()))
      return account;

    foreach (accounts_map::value_type& pair, account->accounts)
      if (account_t * a = find_account_re_(pair.second, regexp))
	return a;

    return NULL;
  }
}

account_t * session_t::find_account_re(const string& regexp)
{
  return find_account_re_(master.get(), mask_t(regexp));
}

void session_t::clean_xacts()
{
  session_xacts_iterator walker(*this);
  pass_down_xacts
    (xact_handler_ptr(new clear_xact_xdata), walker);
}

void session_t::clean_xacts(entry_t& entry)
{
  entry_xacts_iterator walker(entry);
  pass_down_xacts(xact_handler_ptr(new clear_xact_xdata), walker);
}

void session_t::clean_accounts()
{
  accounts_iterator acct_walker(*master);
  pass_down_accounts<accounts_iterator>
    (acct_handler_ptr(new clear_account_xdata), acct_walker);
}

#if 0
value_t session_t::resolve(const string& name, expr_t::scope_t& locals)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'd':
#if 0
    if (name == "date_format") {
      // jww (2007-04-18): What to do here?
      return string_value(moment_t::output_format);
    }
#endif
    break;

  case 'n':
    switch (*++p) {
    case 'o':
      if (name == "now")
	return value_t(now);
      break;
    }
    break;

  case 'r':
    if (name == "register_format")
      return string_value(register_format);
    break;
  }
  return expr_t::scope_t::resolve(name, locals);
}
#endif

expr_t::ptr_op_t session_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (std::strncmp(p, "opt_", 4) == 0) {
      p = p + 4;
      switch (*p) {
      case 'd':
	if (std::strcmp(p, "debug_") == 0)
	  return MAKE_FUNCTOR(session_t::option_debug_);
	break;

      case 'f':
	if ((*(p + 1) == '_' && ! *(p + 2)) ||
	    std::strcmp(p, "file_") == 0)
	  return MAKE_FUNCTOR(session_t::option_file_);
	break;

      case 't':
	if (std::strcmp(p, "trace_") == 0)
	  return MAKE_FUNCTOR(session_t::option_trace_);
	break;

      case 'v':
	if (! *(p + 1) || std::strcmp(p, "verbose") == 0)
	  return MAKE_FUNCTOR(session_t::option_verbose);
	else if (std::strcmp(p, "version") == 0)
	  return MAKE_FUNCTOR(session_t::option_version);
	else if (std::strcmp(p, "verify") == 0)
	  return MAKE_FUNCTOR(session_t::option_verify);
	break;
      }
    }
    break;
  }

  return expr_t::ptr_op_t();
}

// jww (2007-04-26): All of Ledger should be accessed through a
// session_t object
void session_t::initialize()
{
  amount_t::initialize();
  value_t::initialize();
  expr_t::initialize();
}

void session_t::shutdown()
{
  expr_t::shutdown();
  value_t::shutdown();
  amount_t::shutdown();
}

} // namespace ledger
