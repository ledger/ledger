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
 * @file   csv.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Implementation of the CSV file parser.
 *
 * This file converts CSV bank statements into Ledger transactions.  The
 * overall flow is:
 *
 *   1. **Header parsing** (read_index): The first non-comment line is split
 *      into column names, each matched against regex patterns to build a
 *      column-to-field-type index (e.g., column 0 = date, column 3 = payee).
 *
 *   2. **Row parsing** (read_xact): Each subsequent line is split into
 *      fields and mapped to transaction/posting attributes according to the
 *      index.  Two postings are created per row: one carrying the CSV
 *      amount (with the account resolved via payee-to-account mappings)
 *      and a balancing posting to the master account.
 *
 *   3. **Field reading** (read_field): Handles CSV quoting conventions
 *      including double-quote delimiters, pipe delimiters, backslash
 *      escapes, and doubled-quote escaping.
 *
 * Payee alias mappings (from `payee` directives in the journal) and
 * account mappings (from `account` directives) are consulted during
 * transaction construction to normalize payee names and resolve accounts.
 */

#include <system.hh>

#include "csv.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "pool.h"

namespace ledger {

/*--- Field and Line Reading ---*/

string csv_reader::read_field(std::istream& in) {
  string field;

  char c;
  // Quoted field: delimited by " or |.  Supports backslash escaping and
  // doubled-quote escaping (e.g., "" within a double-quoted field).
  if (in.peek() == '"' || in.peek() == '|') {
    in.get(c);
    char x;
    while (in.good() && !in.eof()) {
      in.get(x);
      if (x == '\\') { // NOLINT(bugprone-branch-clone)
        in.get(x);
      } else if (x == '"' && in.peek() == '"') {
        in.get(x);
      } else if (x == c) {
        if (x == '|')
          in.unget();
        else if (in.peek() == ',')
          in.get(c);
        break;
      }
      if (x != '\0')
        field += x;
    }
  } else {
    // Unquoted field: read until comma or end of line
    while (in.good() && !in.eof()) {
      in.get(c);
      if (in.good()) {
        if (c == ',')
          break;
        if (c != '\0')
          field += c;
      }
    }
  }
  trim(field);
  return field;
}

char* csv_reader::next_line(std::istream& in) {
  while (in.good() && !in.eof() && in.peek() == '#')
    std::getline(in, context.linebuf);

  if (!in.good() || in.eof() || in.peek() == -1)
    return nullptr;

  std::getline(in, context.linebuf);

  return context.linebuf.data();
}

/*--- Header Index Construction ---*/

void csv_reader::read_index(std::istream& in) {
  char* line = next_line(in);
  if (!line)
    return;

  std::istringstream instr(line);

  while (instr.good() && !instr.eof()) {
    string field = read_field(instr); // NOLINT(bugprone-unused-local-non-trivial-variable)
    names.push_back(field);

    DEBUG("csv.parse", "Header field: " << field);
    for (auto& mask : masks) {
      if (mask.first.match(field)) {
        index.push_back(mask.second);
        break;
      }
    }
  }
}

/*--- Transaction Construction ---*/

xact_t* csv_reader::read_xact(bool rich_data) {
  char* line = next_line(*context.stream.get());
  if (!line || index.empty())
    return nullptr;
  context.linenum++;

  std::istringstream instr(line);

  // Phase 1: Create the transaction and its primary posting.
  // CSV transactions are marked CLEARED by default since they come
  // from bank statements (the bank has already settled them).
  unique_ptr<xact_t> xact(new xact_t);
  unique_ptr<post_t> post(new post_t);

  xact->set_state(item_t::CLEARED);

  xact->pos = position_t();
  xact->pos->pathname = context.pathname;
  xact->pos->source_content = context.source_content;
  xact->pos->beg_pos = context.stream->tellg();
  xact->pos->beg_line = context.linenum;
  xact->pos->sequence = context.sequence++;

  post->xact = xact.get();

  post->pos = position_t();
  post->pos->pathname = context.pathname;
  post->pos->source_content = context.source_content;
  post->pos->beg_pos = context.stream->tellg();
  post->pos->beg_line = context.linenum;
  post->pos->sequence = context.sequence++;

  post->set_state(item_t::CLEARED);
  post->account = nullptr;

  // Phase 2: Walk the CSV columns and populate transaction fields
  // according to the header index built during construction.
  std::vector<int>::size_type n = 0;
  amount_t amt;
  string total;
  string field;

  while (instr.good() && !instr.eof() && n < index.size()) {
    field = read_field(instr);

    switch (index[n]) {
    case FIELD_DATE:
      if (!field.empty())
        xact->_date = parse_date(field);
      break;

    case FIELD_DATE_AUX:
      if (!field.empty())
        xact->_date_aux = parse_date(field);
      break;

    case FIELD_CODE:
      if (!field.empty())
        xact->code = field;
      break;

    case FIELD_PAYEE: {
      bool found = false;
      for (payee_alias_mapping_t& value : context.journal->payee_alias_mappings) {
        DEBUG("csv.mappings", "Looking for payee mapping: " << value.first);
        if (value.first.match(field)) {
          xact->payee = value.second;
          found = true;
          break;
        }
      }
      if (!found)
        xact->payee = field;
      break;
    }

    case FIELD_DEBIT:
    case FIELD_CREDIT: {
      if (field.length() == 0)
        break;
      std::istringstream amount_str(field); // NOLINT(bugprone-unused-local-non-trivial-variable)
      (void)amt.parse(amount_str, PARSE_NO_REDUCE);
      if (!amt.has_commodity() && commodity_pool_t::current_pool->default_commodity)
        amt.set_commodity(*commodity_pool_t::current_pool->default_commodity);
      // For a debit column: negate only if the value is positive (unsigned
      // convention where the bank gives the debit amount as a positive number).
      // If the value is already negative (signed convention, e.g. some credit
      // unions write -70.55 in the "Amount Debit" column), it already encodes
      // the outflow direction and must not be negated again.
      if (index[n] == FIELD_DEBIT && amt.sign() > 0)
        amt = -amt;
      if (!post->amount.is_null())
        throw_(csv_error, _("Only one of credit, debit, or amount may have a "
                            "value per transaction"));
      post->amount = amt;
      break;
    }

    case FIELD_COST: {
      if (field.length() == 0)
        break;
      std::istringstream amount_str(field); // NOLINT(bugprone-unused-local-non-trivial-variable)
      (void)amt.parse(amount_str, PARSE_NO_REDUCE);
      if (!amt.has_commodity() && commodity_pool_t::current_pool->default_commodity)
        amt.set_commodity(*commodity_pool_t::current_pool->default_commodity);
      post->cost = amt;
      break;
    }

    case FIELD_TOTAL:
      total = field;
      break;

    case FIELD_NOTE:
      if (!field.empty())
        xact->note = field;
      break;

    case FIELD_UNKNOWN:
      if (!names[n].empty() && !field.empty())
        xact->set_tag(names[n], string_value(field));
      break;
    }
    n++;
  }

  // Validate that required fields were present in this CSV row.
  if (!xact->_date)
    throw_(csv_error, _("CSV line is missing a date field"));

  // If cost was provided but shares the same commodity as the amount, it is
  // redundant (a 1:1 no-op conversion) and would cause a validation error.
  // Clear it so the posting is treated as a simple same-currency entry.
  if (post->cost && !post->amount.is_null() &&
      post->amount.commodity() == post->cost->commodity()) {
    post->cost = std::nullopt;
    amt = post->amount;
  }

  // Phase 3: Attach rich metadata if requested (import timestamp and
  // the original CSV line for auditability).
  if (rich_data) {
    xact->set_tag(_("Imported"), string_value(format_date(CURRENT_DATE(), FMT_WRITTEN)));
    xact->set_tag(_("CSV"), string_value(line));
  }

  // Phase 4: Resolve the primary posting's account using the journal's
  // payee-to-account mappings (from `account` directives with payee conditions).

  for (account_mapping_t& value : context.journal->payees_for_unknown_accounts) {
    if (value.first.match(xact->payee)) {
      post->account = value.second;
      break;
    }
  }

  xact->add_post(post.release());

  // Phase 5: Create the balancing posting.  This posting goes to the master
  // account (typically something like "Assets:Checking") and carries the
  // negated amount so the transaction balances to zero.

  post.reset(new post_t);

  post->xact = xact.get();

  post->pos = position_t();
  post->pos->pathname = context.pathname;
  post->pos->source_content = context.source_content;
  post->pos->beg_pos = context.stream->tellg();
  post->pos->beg_line = context.linenum;
  post->pos->sequence = context.sequence++;

  post->set_state(item_t::CLEARED);
  post->account = context.master;

  if (!amt.is_null())
    post->amount = -amt;

  if (!total.empty()) {
    std::istringstream assigned_amount_str(total);
    (void)amt.parse(assigned_amount_str, PARSE_NO_REDUCE);
    if (!amt.has_commodity() && commodity_pool_t::current_pool->default_commodity)
      amt.set_commodity(*commodity_pool_t::current_pool->default_commodity);
    post->assigned_amount = amt;
  }

  xact->add_post(post.release());

  return xact.release();
}

} // namespace ledger
