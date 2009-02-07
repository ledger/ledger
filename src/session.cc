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
#include "textual.h"

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
    master(new account_t(NULL, ""))
{
  TRACE_CTOR(session_t, "");

  if (const char * home_var = std::getenv("HOME"))
    HANDLER(price_db_).on((path(home_var) / ".pricedb").string());
  else
    HANDLER(price_db_).on(path("./.pricedb").string());

  register_parser(new textual_parser_t);

  // Add time commodity conversions, so that timelog's may be parsed
  // in terms of seconds, but reported as minutes or hours.
  if (commodity_t * commodity = commodity_pool->create("s"))
    commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
  else
    assert(false);
}

journal_t * session_t::create_journal()
{
  journal_t * journal = new journal_t(master.get());
  journals.push_back(journal);
  return journal;
}

void session_t::close_journal(journal_t * journal)
{
  for (ptr_list<journal_t>::iterator i = journals.begin();
       i != journals.end();
       i++)
    if (&*i == journal) {
      journals.erase(i);
      return;
    }
  assert(false);
  checked_delete(journal);
}

std::size_t session_t::read_journal(journal_t&	  journal,
				    std::istream& in,
				    const path&	  pathname,
				    account_t *   master)
{
  if (! master)
    master = journal.master;

  foreach (journal_t::parser_t& parser, parsers)
#if defined(TEST_FOR_PARSER)
    if (parser.test(in))
#endif
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

std::size_t session_t::read_data(journal_t&    journal,
				 const string& master_account)
{
  if (HANDLER(file_).data_files.empty())
    throw_(parse_error, "No journal file was specified (please use -f)");

  std::size_t entry_count = 0;

  if (entry_count == 0) {
    account_t * acct = journal.master;
    if (! master_account.empty())
      acct = journal.find_account(master_account);

    journal.price_db = HANDLER(price_db_).str();
    if (journal.price_db && exists(*journal.price_db)) {
      if (read_journal(journal, *journal.price_db)) {
	throw_(parse_error, "Entries not allowed in price history file");
      } else {
	journal.sources.pop_back();
      }
    }


    foreach (const path& pathname, HANDLER(file_).data_files) {
      if (pathname == "-") {
	journal.sources.push_back("/dev/stdin");

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

	entry_count += read_journal(journal, buf_in, "/dev/stdin", acct);
      }
      else if (exists(pathname)) {
	entry_count += read_journal(journal, pathname, acct);
	if (journal.price_db)
	  journal.sources.push_back(*journal.price_db);
      }
      else {
	throw_(parse_error, "Could not open journal file '" << pathname << "'");
      }
    }
  }

  VERIFY(journal.valid());

  return entry_count;
}

void session_t::unregister_parser(journal_t::parser_t * parser)
{
  for (ptr_list<journal_t::parser_t>::iterator i = parsers.begin();
       i != parsers.end();
       i++) {
    if (&*i == parser) {
      parsers.erase(i);
      return;
    }
  }
  assert(false);
  checked_delete(parser);
}

void session_t::clean_xacts()
{
  session_xacts_iterator walker(*this);
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
    if (WANT_OPT()) { p += OPT_PREFIX_LEN;
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
      case 'p':
	OPT(price_db_);
	break;
      case 'Q':
	OPT_CH(download); // -Q
	break;
      }
    }
    break;
  }
  return NULL;
}

} // namespace ledger
