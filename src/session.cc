/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

/**
 * @file   session.cc
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Session management: journal loading, commodity initialization,
 *        and session-level function/option lookup.
 *
 * The session is Ledger's "document model."  It owns the journal and is
 * responsible for reading all data files, the price database, and stdin
 * when "-" is given as a file.  The reading order matters because the
 * price database is loaded first (so that commodity prices are available
 * during journal parsing), and session-level options like --strict and
 * --pedantic must be applied to the journal before the first line is
 * read.
 */

#include <system.hh>

#include "session.h"
#include "xact.h"
#include "account.h"
#include "journal.h"
#include "iterators.h"
#include "filters.h"

namespace ledger {

/*--- Session Context (Global State) ---*/

/**
 * @brief Initialize or shut down global subsystems tied to the session.
 *
 * When @p session is non-null, this initializes the time, amount, and
 * value subsystems and registers the built-in time unit conversions
 * (minutes to seconds, hours to minutes).  When null, it shuts them
 * down in reverse order.
 *
 * This function exists because several subsystems use file-scope static
 * variables.  Switching the "current session" re-points those globals so
 * that a different session's data is in scope.
 */
void set_session_context(session_t* session) {
  if (session) {
    times_initialize();
    amount_t::initialize();

    amount_t::parse_conversion("1.0m", "60s");
    amount_t::parse_conversion("1.00h", "60m");

    value_t::initialize();
  } else if (!session) {
    value_t::shutdown();
    amount_t::shutdown();
    times_shutdown();
  }
}

/*--- Construction ---*/

session_t::session_t() : flush_on_next_data_file(false), journal(new journal_t) {
  // Push an initial (empty) parsing context so that the stack is never
  // empty during the session's lifetime.
  parsing_context.push();

  TRACE_CTOR(session_t, "");
}

/*--- Journal Reading ---*/

/**
 * @brief Core data-loading method that reads the price database and all
 *        journal files.
 *
 * The loading sequence is:
 *   1. Determine the data files (from --file or the default ~/.ledger).
 *   2. Apply session-level journal flags (day_break, checking_style, etc.).
 *   3. Read the price database if one exists.
 *   4. Read each data file in order, accumulating transactions.
 *   5. Verify the journal's internal consistency.
 *
 * When reading from stdin ("-" or "/dev/stdin"), the entire input is
 * buffered into memory first to avoid problems with pipes and TTYs.
 */
std::size_t session_t::read_data(const string& master_account) {
  bool populated_data_files = false;

  // If no --file was specified, try ~/.ledger as a default
  if (HANDLER(file_).data_files.empty()) {
    path file;
    if (const char* home_var = std::getenv("HOME"))
      file = path(home_var) / ".ledger";

    if (!file.empty() && exists(file))
      HANDLER(file_).data_files.push_back(file);
    else
      throw_(parse_error, "No journal file was specified (please use -f)");

    populated_data_files = true;
  }

  // Capture any transactions that already exist in the journal (e.g., added
  // programmatically via journal.add_xact() from Python) before we start
  // reading files, so the consistency check below only validates transactions
  // that were read from files in this call.
  std::size_t initial_xact_count = journal->xacts.size();
  std::size_t xact_count = 0;

  // Determine the master account under which all postings will be placed
  account_t* acct;
  if (master_account.empty())
    acct = journal->master;
  else
    acct = journal->find_account(master_account);

  // Resolve the price database path
  optional<path> price_db_path;
  if (HANDLED(price_db_)) {
    price_db_path = resolve_path(HANDLER(price_db_).str());
    if (!exists(price_db_path.get())) {
      throw_(parse_error, _f("Could not find specified price-db file %1%") % price_db_path);
    }
  } else {
    if (const char* home_var = std::getenv("HOME")) {
      price_db_path = (path(home_var) / ".pricedb");
    } else {
      price_db_path = ("./.ledgerrc");
    }
  }

  // Transfer session-level option flags to the journal object before
  // any parsing begins, because these flags affect how the parser
  // validates input.
  if (HANDLED(day_break))
    journal->day_break = true;

  if (HANDLED(time_round_)) {
    int minutes = std::stoi(HANDLER(time_round_).str());
    if (minutes > 0 && minutes <= INT_MAX / 60) {
      journal->time_round = minutes * 60;
    } else {
      throw std::out_of_range("time_round value too large");
    }
  }

  if (HANDLED(recursive_aliases))
    journal->recursive_aliases = true;
  if (HANDLED(no_aliases))
    journal->no_aliases = true;

  if (HANDLED(explicit)) {
    // No-op
  }
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

  if (HANDLED(lot_matching_))
    journal->lot_matching_policy = HANDLER(lot_matching_).policy;

  // Read the price database first so that commodity prices are available
  // during journal parsing (e.g., for balance assertions involving
  // market values).
  if (price_db_path) {
    if (exists(*price_db_path)) {
      parsing_context.push(*price_db_path);
      parsing_context.get_current().journal = journal.get();
      try {
        if (journal->read(parsing_context, HANDLER(hashes_).hash_type) > 0)
          throw_(parse_error, _("Transactions not allowed in price history file"));
      } catch (...) {
        parsing_context.pop();
        throw;
      }
      parsing_context.pop();
    }
  }

  // Read each journal data file in order
  for (const path& pathname : HANDLER(file_).data_files) {
    if (pathname == "-" || pathname == "/dev/stdin") {
      // To avoid problems with stdin and pipes, etc., we read the entire
      // file in beforehand into a memory buffer, and then parcel it out
      // from there.
      std::ostringstream buffer;

      while (std::cin.good() && !std::cin.eof()) {
        char line[8192];
        std::cin.read(line, 8192);
        std::streamsize count = std::cin.gcount();
        buffer.write(line, count);
      }
      buffer.flush();

      std::shared_ptr<string> content = std::make_shared<string>(buffer.str());
      std::shared_ptr<std::istream> stream(new std::istringstream(*content));
      parsing_context.push(stream);
      parsing_context.get_current().source_content = content;
    } else {
      parsing_context.push(pathname);
    }

    parsing_context.get_current().journal = journal.get();
    parsing_context.get_current().master = acct;
    try {
      xact_count += journal->read(parsing_context, HANDLER(hashes_).hash_type);
    } catch (...) {
      parsing_context.pop();
      throw;
    }
    parsing_context.pop();
  }

  DEBUG("ledger.read", "xact_count [" << xact_count << "] == journal->xacts.size() ["
                                      << journal->xacts.size() << "] - initial_xact_count ["
                                      << initial_xact_count << "]");
  assert(xact_count == journal->xacts.size() - initial_xact_count);

  if (populated_data_files)
    HANDLER(file_).data_files.clear();

  VERIFY(journal->valid());

  return journal->xacts.size();
}

journal_t* session_t::read_journal_files() {
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

/*--- Programmatic API ---*/

journal_t* session_t::read_journal(const path& pathname) {
  journal.reset(new journal_t);

  HANDLER(file_).data_files.clear();
  HANDLER(file_).data_files.push_back(pathname);

  return read_journal_files();
}

journal_t* session_t::read_journal_from_string(const string& data) {
  journal.reset(new journal_t);

  HANDLER(file_).data_files.clear();

  std::shared_ptr<std::istream> stream(new std::istringstream(data));
  parsing_context.push(stream);

  parsing_context.get_current().journal = journal.get();
  parsing_context.get_current().master = journal->master;
  try {
    journal->read(parsing_context, HANDLER(hashes_).hash_type);
  } catch (...) {
    parsing_context.pop();
    throw;
  }
  parsing_context.pop();

  return journal.get();
}

void session_t::close_journal_files() {
  journal.reset(new journal_t);
}

journal_t* session_t::get_journal() {
  return journal.get();
}

/*--- Built-in Functions ---*/

value_t session_t::fn_account(call_scope_t& args) {
  // NOLINTBEGIN(bugprone-branch-clone)
  if (args[0].is_string())
    return scope_value(journal->find_account(args.get<string>(0), false));
  else if (args[0].is_mask())
    return scope_value(journal->find_account_re(args.get<mask_t>(0).str()));
  else
    return NULL_VALUE;
  // NOLINTEND(bugprone-branch-clone)
}

value_t session_t::fn_min(call_scope_t& args) {
  if (args[0].is_null())
    return args[1];
  if (args[1].is_null())
    return args[0];
  return args[1] < args[0] ? args[1] : args[0];
}
value_t session_t::fn_max(call_scope_t& args) {
  if (args[0].is_null())
    return args[1];
  if (args[1].is_null())
    return args[0];
  return args[1] < args[0] ? args[0] : args[1];
}

value_t session_t::fn_int(call_scope_t& args) {
  return args[0].to_long();
}
value_t session_t::fn_str(call_scope_t& args) {
  return string_value(args[0].to_string());
}

namespace {
/// @brief Resolve the amount to inspect for lot annotation functions.
///
/// If an explicit argument is provided, use it.  Otherwise, look up
/// "amount" from the scope chain (e.g., the current posting's amount),
/// so that lot_price, lot_date, and lot_tag can be called with zero
/// arguments.
amount_t resolve_lot_amount(call_scope_t& args) {
  if (!args.empty())
    return args.get<amount_t>(0, false);

  if (expr_t::ptr_op_t amount_op = args.lookup(symbol_t::FUNCTION, "amount")) {
    value_t val = amount_op->calc(args);
    if (!val.is_null())
      return val.to_amount();
  }
  return amount_t();
}
} // namespace

value_t session_t::fn_lot_price(call_scope_t& args) {
  amount_t amt(resolve_lot_amount(args));
  if (amt.has_annotation() && amt.annotation().price)
    return *amt.annotation().price;
  else
    return NULL_VALUE;
}
value_t session_t::fn_lot_date(call_scope_t& args) {
  amount_t amt(resolve_lot_amount(args));
  if (amt.has_annotation() && amt.annotation().date)
    return *amt.annotation().date;
  else
    return NULL_VALUE;
}
value_t session_t::fn_lot_tag(call_scope_t& args) {
  amount_t amt(resolve_lot_amount(args));
  if (amt.has_annotation() && amt.annotation().tag)
    return string_value(*amt.annotation().tag);
  else
    return NULL_VALUE;
}

/*--- Option Lookup ---*/

option_t<session_t>* session_t::lookup_option(const char* p) {
  // NOLINTBEGIN(bugprone-branch-clone)
  switch (*p) {
  case 'Q':
    OPT_CH(download); // -Q
    break;
  case 'Z':
    OPT_CH(price_exp_);
    break;
  case 'c':
    OPT(check_payees);
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
  case 'g':
    OPT_(getquote_);
    break;
  case 'h':
    OPT(hashes_);
    break;
  case 'i':
    OPT(input_date_format_);
    break;
  case 'l':
    OPT_ALT(price_exp_, leeway_);
    else OPT(lot_matching_);
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
    else OPT(time_round_);
    break;
  case 'v':
    OPT(value_expr_);
    break;
  default:
    break;
  }
  // NOLINTEND(bugprone-branch-clone)
  return nullptr;
}

/*--- Symbol Lookup ---*/

expr_t::ptr_op_t session_t::lookup(const symbol_t::kind_t kind, const string& name) {
  const char* p = name.c_str();

  switch (kind) {
  case symbol_t::FUNCTION:
    // NOLINTBEGIN(bugprone-branch-clone)
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
    // NOLINTEND(bugprone-branch-clone)
    // Check if they are trying to access an option's setting or value.
    if (option_t<session_t>* handler = lookup_option(p))
      return MAKE_OPT_FUNCTOR(session_t, handler);
    break;

  case symbol_t::OPTION:
    if (option_t<session_t>* handler = lookup_option(p))
      return MAKE_OPT_HANDLER(session_t, handler);
    break;

  default:
    break;
  }

  return symbol_scope_t::lookup(kind, name);
}

} // namespace ledger
