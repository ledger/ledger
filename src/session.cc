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

#include <system.hh>

#include "session.h"
#include "xact.h"
#include "account.h"
#include "journal.h"
#include "iterators.h"
#include "filters.h"
#include "archive.h"

namespace ledger {

void set_session_context(session_t * session)
{
  if (session) {
    times_initialize();
    amount_t::initialize(session->journal->commodity_pool);

    amount_t::parse_conversion("1.0m", "60s");
    amount_t::parse_conversion("1.0h", "60m");

    value_t::initialize();
  }
  else if (! session) {
    value_t::shutdown();
    amount_t::shutdown();
    times_shutdown();
  }
}

session_t::session_t()
  : flush_on_next_data_file(false),
    current_year(CURRENT_DATE().year()),
    journal(new journal_t)
{
  TRACE_CTOR(session_t, "");

  if (const char * home_var = std::getenv("HOME"))
    HANDLER(price_db_).on(none, (path(home_var) / ".pricedb").string());
  else
    HANDLER(price_db_).on(none, path("./.pricedb").string());
}

std::size_t session_t::read_data(const string& master_account)
{
  bool populated_data_files = false;

  if (HANDLER(file_).data_files.empty()) {
    path file;
    if (const char * home_var = std::getenv("HOME"))
      file = path(home_var) / ".ledger";

    if (! file.empty() && exists(file))
      HANDLER(file_).data_files.push_back(file);
    else
      throw_(parse_error, "No journal file was specified (please use -f)");

    populated_data_files = true;
  }

  std::size_t xact_count = 0;

  account_t * acct = journal->master;
  if (! master_account.empty())
    acct = journal->find_account(master_account);

  optional<path> price_db_path;
  if (HANDLED(price_db_))
    price_db_path = resolve_path(HANDLER(price_db_).str());

  optional<archive_t> cache;
  if (HANDLED(cache_) && master_account.empty())
    cache = archive_t(HANDLED(cache_).str());

  if (! (cache &&
	 cache->should_load(HANDLER(file_).data_files) &&
	 cache->load(journal))) {
    if (price_db_path) {
      if (exists(*price_db_path)) {
	if (journal->read(*price_db_path) > 0)
	  throw_(parse_error, _("Transactions not allowed in price history file"));
      }
    }

    foreach (const path& pathname, HANDLER(file_).data_files) {
      if (pathname == "-") {
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

	xact_count += journal->read(buf_in, "/dev/stdin", acct);
	journal->sources.push_back(journal_t::fileinfo_t());
      } else {
	xact_count += journal->read(pathname, acct);
      }
    }

    assert(xact_count == journal->xacts.size());

    if (cache && cache->should_save(journal))
      cache->save(journal);
  }

  if (populated_data_files)
    HANDLER(file_).data_files.clear();

  VERIFY(journal->valid());

  return journal->xacts.size();
}

void session_t::read_journal_files()
{
  INFO_START(journal, "Read journal file");

  string master_account;
  if (HANDLED(account_))
    master_account = HANDLER(account_).str();

  std::size_t count = read_data(master_account);
  if (count == 0)
    throw_(parse_error,
	   _("Failed to locate any transactions; did you specify a valid file with -f?"));

  INFO_FINISH(journal);

  INFO("Found " << count << " transactions");
}

void session_t::close_journal_files()
{
  journal.reset();
  amount_t::shutdown();
  
  journal.reset(new journal_t);
  amount_t::initialize(journal->commodity_pool);
}

option_t<session_t> * session_t::lookup_option(const char * p)
{
  switch (*p) {
  case 'Q':
    OPT_CH(download); // -Q
    break;
  case 'Z':
    OPT_CH(price_exp_);
    break;
  case 'a':
    OPT_(account_); // -a
    break;
  case 'c':
    OPT(cache_);
    break;
  case 'd':
    OPT(download); // -Q
    break;
  case 'e':
    OPT(european);
    break;
  case 'f':
    OPT_(file_); // -f
    break;
  case 'i':
    OPT(input_date_format_);
    break;
  case 'l':
    OPT_ALT(price_exp_, leeway_);
    break;
  case 'p':
    OPT(price_db_);
    else OPT(price_exp_);
    break;
  case 's':
    OPT(strict);
    break;
  }
  return NULL;
}

expr_t::ptr_op_t session_t::lookup(const symbol_t::kind_t kind,
				   const string& name)
{
  switch (kind) {
  case symbol_t::FUNCTION:
    // Check if they are trying to access an option's setting or value.
    if (option_t<session_t> * handler = lookup_option(name.c_str()))
      return MAKE_OPT_FUNCTOR(session_t, handler);
    break;

  case symbol_t::OPTION:
    if (option_t<session_t> * handler = lookup_option(name.c_str()))
      return MAKE_OPT_HANDLER(session_t, handler);
    break;

  default:
    break;
  }

  return symbol_scope_t::lookup(kind, name);
}

} // namespace ledger
