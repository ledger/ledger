/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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
    amount_t::initialize();

    amount_t::parse_conversion("1.0m", "60s");
    amount_t::parse_conversion("1.00h", "60m");

    value_t::initialize();
  }
  else if (! session) {
    value_t::shutdown();
    amount_t::shutdown();
    times_shutdown();
  }
}

session_t::session_t()
  : flush_on_next_data_file(false), journal(new journal_t)
{
 parsing_context.push();

  TRACE_CTOR(session_t, "");
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

  account_t * acct;
  if (master_account.empty())
    acct = journal->master;
  else
    acct = journal->find_account(master_account);

  optional<path> price_db_path;
  if (HANDLED(price_db_)) {
    price_db_path = resolve_path(HANDLER(price_db_).str());
    if (!exists(price_db_path.get())) {
      throw_(parse_error, _f("Could not find specified price-db file %1%") % price_db_path);
    }
  } else {
    if (const char * home_var = std::getenv("HOME")) {
      price_db_path = (path(home_var) / ".pricedb");
    } else {
      price_db_path = ("./.ledgerrc");
    }
  }

  if (HANDLED(day_break))
    journal->day_break = true;

  if (HANDLED(recursive_aliases))
    journal->recursive_aliases = true;
  if (HANDLED(no_aliases))
    journal->no_aliases = true;

  if (HANDLED(explicit))
    journal->force_checking = true;
  if (HANDLED(check_payees))
    journal->check_payees = true;

  if (HANDLED(permissive))
    journal->checking_style = journal_t::CHECK_PERMISSIVE;
  else if (HANDLED(pedantic))
    journal->checking_style = journal_t::CHECK_ERROR;
  else if (HANDLED(strict))
    journal->checking_style = journal_t::CHECK_WARNING;

  if (HANDLED(value_expr_))
    journal->value_expr = HANDLER(value_expr_).str();

#if HAVE_BOOST_SERIALIZATION
  optional<archive_t> cache;
  if (HANDLED(cache_) && master_account.empty())
    cache = archive_t(HANDLED(cache_).str());

  if (! (cache &&
         cache->should_load(HANDLER(file_).data_files) &&
         cache->load(*journal.get()))) {
#endif // HAVE_BOOST_SERIALIZATION
    if (price_db_path) {
      if (exists(*price_db_path)) {
        parsing_context.push(*price_db_path);
        parsing_context.get_current().journal = journal.get();
        try {
          if (journal->read(parsing_context) > 0)
            throw_(parse_error, _("Transactions not allowed in price history file"));
        }
        catch (...) {
          parsing_context.pop();
          throw;
        }
        parsing_context.pop();
      }
    }

    foreach (const path& pathname, HANDLER(file_).data_files) {
      if (pathname == "-" || pathname == "/dev/stdin") {
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

        shared_ptr<std::istream> stream(new std::istringstream(buffer.str()));
        parsing_context.push(stream);
      } else {
        parsing_context.push(pathname);
      }

      parsing_context.get_current().journal = journal.get();
      parsing_context.get_current().master  = acct;
      try {
        xact_count += journal->read(parsing_context);
      }
      catch (...) {
        parsing_context.pop();
        throw;
      }
      parsing_context.pop();
    }

    DEBUG("ledger.read", "xact_count [" << xact_count
          << "] == journal->xacts.size() [" << journal->xacts.size() << "]");
    assert(xact_count == journal->xacts.size());

#if HAVE_BOOST_SERIALIZATION
    if (cache && cache->should_save(*journal.get()))
      cache->save(*journal.get());
  }
#endif // HAVE_BOOST_SERIALIZATION

  if (populated_data_files)
    HANDLER(file_).data_files.clear();

  VERIFY(journal->valid());

  return journal->xacts.size();
}

journal_t * session_t::read_journal_files()
{
  INFO_START(journal, "Read journal file");

  string master_account;
  if (HANDLED(master_account_))
    master_account = HANDLER(master_account_).str();

#if DEBUG_ON
  std::size_t count =
#endif
    read_data(master_account);

  INFO_FINISH(journal);

#if DEBUG_ON
  INFO("Found " << count << " transactions");
#endif

  return journal.get();
}

journal_t * session_t::read_journal(const path& pathname)
{
  HANDLER(file_).data_files.clear();
  HANDLER(file_).data_files.push_back(pathname);

  return read_journal_files();
}

journal_t * session_t::read_journal_from_string(const string& data)
{
  HANDLER(file_).data_files.clear();

  shared_ptr<std::istream> stream(new std::istringstream(data));
  parsing_context.push(stream);

  parsing_context.get_current().journal = journal.get();
  parsing_context.get_current().master  = journal->master;
  try {
    journal->read(parsing_context);
  }
  catch (...) {
    parsing_context.pop();
    throw;
  }
  parsing_context.pop();

  return journal.get();
}

void session_t::close_journal_files()
{
  journal.reset();
  amount_t::shutdown();

  journal.reset(new journal_t);
  amount_t::initialize();
}

value_t session_t::fn_account(call_scope_t& args)
{
  if (args[0].is_string())
    return scope_value(journal->find_account(args.get<string>(0), false));
  else if (args[0].is_mask())
    return scope_value(journal->find_account_re(args.get<mask_t>(0).str()));
  else
    return NULL_VALUE;
}

value_t session_t::fn_min(call_scope_t& args)
{
  return args[1] < args[0] ? args[1] : args[0];
}
value_t session_t::fn_max(call_scope_t& args)
{
  return args[1] > args[0] ? args[1] : args[0];
}

value_t session_t::fn_int(call_scope_t& args)
{
  return args[0].to_long();
}
value_t session_t::fn_str(call_scope_t& args)
{
  return string_value(args[0].to_string());
}

value_t session_t::fn_lot_price(call_scope_t& args)
{
  amount_t amt(args.get<amount_t>(1, false));
  if (amt.has_annotation() && amt.annotation().price)
    return *amt.annotation().price;
  else
    return NULL_VALUE;
}
value_t session_t::fn_lot_date(call_scope_t& args)
{
  amount_t amt(args.get<amount_t>(1, false));
  if (amt.has_annotation() && amt.annotation().date)
    return *amt.annotation().date;
  else
    return NULL_VALUE;
}
value_t session_t::fn_lot_tag(call_scope_t& args)
{
  amount_t amt(args.get<amount_t>(1, false));
  if (amt.has_annotation() && amt.annotation().tag)
    return string_value(*amt.annotation().tag);
  else
    return NULL_VALUE;
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
  case 'c':
    OPT(cache_);
    else OPT(check_payees);
    break;
  case 'd':
    OPT(download); // -Q
    else OPT(decimal_comma);
    else OPT(day_break);
    break;
  case 'e':
    OPT(explicit);
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
  case 'm':
    OPT(master_account_);
    break;
  case 'n':
    OPT(no_aliases);
    break;
  case 'p':
    OPT(price_db_);
    else OPT(price_exp_);
    else OPT(pedantic);
    else OPT(permissive);
    break;
  case 'r':
    OPT(recursive_aliases);
    break;
  case 's':
    OPT(strict);
    break;
  case 't':
    OPT(time_colon);
    break;
  case 'v':
    OPT(value_expr_);
  }
  return NULL;
}

expr_t::ptr_op_t session_t::lookup(const symbol_t::kind_t kind,
                                   const string& name)
{
  const char * p = name.c_str();

  switch (kind) {
  case symbol_t::FUNCTION:
    switch (*p) {
    case 'a':
      if (is_eq(p, "account"))
        return MAKE_FUNCTOR(session_t::fn_account);
      break;

    case 'l':
      if (is_eq(p, "lot_price"))
        return MAKE_FUNCTOR(session_t::fn_lot_price);
      else if (is_eq(p, "lot_date"))
        return MAKE_FUNCTOR(session_t::fn_lot_date);
      else if (is_eq(p, "lot_tag"))
        return MAKE_FUNCTOR(session_t::fn_lot_tag);
      break;

    case 'i':
      if (is_eq(p, "int"))
        return MAKE_FUNCTOR(session_t::fn_int);
      break;

    case 'm':
      if (is_eq(p, "min"))
        return MAKE_FUNCTOR(session_t::fn_min);
      else if (is_eq(p, "max"))
        return MAKE_FUNCTOR(session_t::fn_max);
      break;

    case 's':
      if (is_eq(p, "str"))
        return MAKE_FUNCTOR(session_t::fn_str);
      break;

    default:
      break;
    }
    // Check if they are trying to access an option's setting or value.
    if (option_t<session_t> * handler = lookup_option(p))
      return MAKE_OPT_FUNCTOR(session_t, handler);
    break;

  case symbol_t::OPTION:
    if (option_t<session_t> * handler = lookup_option(p))
      return MAKE_OPT_HANDLER(session_t, handler);
    break;

  default:
    break;
  }

  return symbol_scope_t::lookup(kind, name);
}

} // namespace ledger
