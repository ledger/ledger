/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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
#include "iterators.h"
#include "filters.h"

namespace ledger {

void set_session_context(session_t * session)
{
  if (session) {
    amount_t::initialize(session->commodity_pool);

    // jww (2009-02-04): Is amount_t the right place for parse_conversion to
    // happen?
    amount_t::parse_conversion("1.0m", "60s");
    amount_t::parse_conversion("1.0h", "60m");

    value_t::initialize();
  }
  else if (! session) {
    value_t::shutdown();
    amount_t::shutdown();
  }
}

session_t::session_t()
  : flush_on_next_data_file(false),

    current_year(CURRENT_DATE().year()),

    commodity_pool(new commodity_pool_t),
    master(new account_t),
    journal(new journal_t(master.get()))
{
  TRACE_CTOR(session_t, "");

  if (const char * home_var = std::getenv("HOME"))
    HANDLER(price_db_).on((path(home_var) / ".pricedb").string());
  else
    HANDLER(price_db_).on(path("./.pricedb").string());

  // Add time commodity conversions, so that timelog's may be parsed
  // in terms of seconds, but reported as minutes or hours.
  if (commodity_t * commodity = commodity_pool->create("s"))
    commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
  else
    assert(false);
}

std::size_t session_t::read_journal(std::istream& in,
				    const path&	  pathname,
				    account_t *   master)
{
  if (! master)
    master = journal->master;
  std::size_t count = journal->parse(in, *this, master, &pathname,
				     HANDLED(strict));
  clean_accounts();		// remove calculated totals
  return count;
}

std::size_t session_t::read_journal(const path& pathname,
				    account_t * master)
{
  if (! exists(pathname))
    throw_(std::logic_error, "Cannot read file" << pathname);

  ifstream stream(pathname);
  return read_journal(stream, pathname, master);
}

std::size_t session_t::read_data(const string& master_account)
{
  if (HANDLER(file_).data_files.empty())
    throw_(parse_error, "No journal file was specified (please use -f)");

  std::size_t entry_count = 0;

  account_t * acct = journal->master;
  if (! master_account.empty())
    acct = journal->find_account(master_account);

  if (HANDLED(price_db_)) {
    path price_db_path = resolve_path(HANDLER(price_db_).str());
    if (exists(price_db_path) && read_journal(price_db_path) > 0)
	throw_(parse_error, "Entries not allowed in price history file");
  }

  foreach (const path& pathname, HANDLER(file_).data_files) {
    path filename = resolve_path(pathname);
    if (filename == "-") {
      // To avoid problems with stdin and pipes, etc., we read the entire
      // file in beforehand into a memory buffer, and then parcel it out
      // from there.
      std::ostringstream buffer;

      while (std::cin.good() && ! std::cin.eof()) {
	char line[8192];
	std::cin.read(line, 8192);
	std::streamsize count = std::cin.gcount();
	buffer.write(line, count);
      }
      buffer.flush();

      std::istringstream buf_in(buffer.str());

      entry_count += read_journal(buf_in, "/dev/stdin", acct);
    }
    else if (exists(filename)) {
      entry_count += read_journal(filename, acct);
    }
    else {
      throw_(parse_error, "Could not read journal file '" << filename << "'");
    }
  }

  VERIFY(journal->valid());

  return entry_count;
}

void session_t::read_journal_files()
{
  INFO_START(journal, "Read journal file");

  string master_account;
  if (HANDLED(account_))
    master_account = HANDLER(account_).str();

  std::size_t count = read_data(master_account);
  if (count == 0)
    throw_(parse_error, "Failed to locate any journal entries; "
	   "did you specify a valid file with -f?");

  INFO_FINISH(journal);

  INFO("Found " << count << " entries");
}

void session_t::close_journal_files()
{
  journal.reset();
  master.reset();
  commodity_pool.reset();
  amount_t::shutdown();
  
  commodity_pool.reset(new commodity_pool_t);
  amount_t::initialize(commodity_pool);
  master.reset(new account_t);
  journal.reset(new journal_t(master.get()));
}

void session_t::clean_xacts()
{
  journal_xacts_iterator walker(*journal.get());
  pass_down_xacts(xact_handler_ptr(new clear_xact_xdata), walker);
}

void session_t::clean_xacts(entry_t& entry)
{
  entry_xacts_iterator walker(entry);
  pass_down_xacts(xact_handler_ptr(new clear_xact_xdata), walker);
}

void session_t::clean_accounts()
{
  basic_accounts_iterator acct_walker(*master);
  pass_down_accounts(acct_handler_ptr(new clear_account_xdata), acct_walker);
  master->clear_xdata();
}

option_t<session_t> * session_t::lookup_option(const char * p)
{
  switch (*p) {
  case 'a':
    OPT_(account_); // -a
    break;
  case 'd':
    OPT(download); // -Q
    break;
  case 'f':
    OPT_(file_); // -f
    break;
  case 'i':
    OPT(input_date_format_);
    break;
  case 'l':
    OPT(leeway_);
    break;
  case 'p':
    OPT(price_db_);
    break;
  case 's':
    OPT(strict);
    break;
  case 'Q':
    OPT_CH(download); // -Q
    break;
  }
  return NULL;
}

expr_t::ptr_op_t session_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'n':
    if (is_eq(p, "now"))
      return MAKE_FUNCTOR(session_t::fn_now);
    break;

  case 'o':
    if (WANT_OPT()) { p += OPT_PREFIX_LEN;
      if (option_t<session_t> * handler = lookup_option(p))
	return MAKE_OPT_HANDLER(session_t, handler);
    }
    break;

  case 't':
    if (is_eq(p, "today"))
      return MAKE_FUNCTOR(session_t::fn_today);
    break;
  }

  return symbol_scope_t::lookup(name);
}

} // namespace ledger
