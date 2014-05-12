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

#include "csv.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "pool.h"

namespace ledger {

string csv_reader::read_field(std::istream& in)
{
  string field;

  char c;
  if (in.peek() == '"' || in.peek() == '|') {
    in.get(c);
    char x;
    while (in.good() && ! in.eof()) {
      in.get(x);
      if (x == '\\') {
        in.get(x);
      }
      else if (x == '"' && in.peek() == '"') {
        in.get(x);
      }
      else if (x == c) {
        if (x == '|')
          in.unget();
        else if (in.peek() == ',')
          in.get(c);
        break;
      }
      if (x != '\0')
        field += x;
    }
  }
  else {
    while (in.good() && ! in.eof()) {
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

char * csv_reader::next_line(std::istream& in)
{
  while (in.good() && ! in.eof() && in.peek() == '#')
    in.getline(context.linebuf, parse_context_t::MAX_LINE);

  if (! in.good() || in.eof() || in.peek() == -1)
    return NULL;

  in.getline(context.linebuf, parse_context_t::MAX_LINE);

  return context.linebuf;
}

void csv_reader::read_index(std::istream& in)
{
  char * line = next_line(in);
  if (! line)
    return;

  std::istringstream instr(line);

  while (instr.good() && ! instr.eof()) {
    string field = read_field(instr);
    names.push_back(field);

    if (date_mask.match(field))
      index.push_back(FIELD_DATE);
    else if (date_aux_mask.match(field))
      index.push_back(FIELD_DATE_AUX);
    else if (code_mask.match(field))
      index.push_back(FIELD_CODE);
    else if (payee_mask.match(field))
      index.push_back(FIELD_PAYEE);
    else if (amount_mask.match(field))
      index.push_back(FIELD_AMOUNT);
    else if (cost_mask.match(field))
      index.push_back(FIELD_COST);
    else if (total_mask.match(field))
      index.push_back(FIELD_TOTAL);
    else if (note_mask.match(field))
      index.push_back(FIELD_NOTE);
    else
      index.push_back(FIELD_UNKNOWN);

    DEBUG("csv.parse", "Header field: " << field);
  }
}

xact_t * csv_reader::read_xact(bool rich_data)
{
  char * line = next_line(*context.stream.get());
  if (! line || index.empty())
    return NULL;
  context.linenum++;

  std::istringstream instr(line);

  unique_ptr<xact_t> xact(new xact_t);
  unique_ptr<post_t> post(new post_t);

  xact->set_state(item_t::CLEARED);

  xact->pos           = position_t();
  xact->pos->pathname = context.pathname;
  xact->pos->beg_pos  = context.stream->tellg();
  xact->pos->beg_line = context.linenum;
  xact->pos->sequence = context.sequence++;

  post->xact = xact.get();

  post->pos           = position_t();
  post->pos->pathname = context.pathname;
  post->pos->beg_pos  = context.stream->tellg();
  post->pos->beg_line = context.linenum;
  post->pos->sequence = context.sequence++;

  post->set_state(item_t::CLEARED);
  post->account = NULL;

  std::vector<int>::size_type n = 0;
  amount_t amt;
  string total;
  string field;

  while (instr.good() && ! instr.eof() && n < index.size()) {
    field = read_field(instr);

    switch (index[n]) {
    case FIELD_DATE:
      xact->_date = parse_date(field);
      break;

    case FIELD_DATE_AUX:
      if (! field.empty())
        xact->_date_aux = parse_date(field);
      break;

    case FIELD_CODE:
      if (! field.empty())
        xact->code = field;
      break;

    case FIELD_PAYEE: {
      bool found = false;
      foreach (payee_alias_mapping_t& value, context.journal->payee_alias_mappings) {
        DEBUG("csv.mappings", "Looking for payee mapping: " << value.first);
        if (value.first.match(field)) {
          xact->payee = value.second;
          found = true;
          break;
        }
      }
      if (! found)
        xact->payee = field;
      break;
    }

    case FIELD_AMOUNT: {
      std::istringstream amount_str(field);
      amt.parse(amount_str, PARSE_NO_REDUCE);
      if (! amt.has_commodity() &&
          commodity_pool_t::current_pool->default_commodity)
        amt.set_commodity(*commodity_pool_t::current_pool->default_commodity);
      post->amount = amt;
      break;
    }

    case FIELD_COST: {
      std::istringstream amount_str(field);
      amt.parse(amount_str, PARSE_NO_REDUCE);
      if (! amt.has_commodity() &&
          commodity_pool_t::current_pool->default_commodity)
        amt.set_commodity
          (*commodity_pool_t::current_pool->default_commodity);
      post->cost = amt;
      break;
    }

    case FIELD_TOTAL:
      total = field;
      break;

    case FIELD_NOTE:
      if (! field.empty())
        xact->note = field;
      break;

    case FIELD_UNKNOWN:
      if (! names[n].empty() && ! field.empty())
        xact->set_tag(names[n], string_value(field));
      break;
    }
    n++;
  }

  if (rich_data) {
    xact->set_tag(_("Imported"),
                  string_value(format_date(CURRENT_DATE(), FMT_WRITTEN)));
    xact->set_tag(_("CSV"), string_value(line));
  }

  // Translate the account name, if we have enough information to do so

  foreach (account_mapping_t& value, context.journal->payees_for_unknown_accounts) {
    if (value.first.match(xact->payee)) {
      post->account = value.second;
      break;
    }
  }

  xact->add_post(post.release());

  // Create the "balancing post", which refers to the account for this data

  post.reset(new post_t);

  post->xact = xact.get();

  post->pos           = position_t();
  post->pos->pathname = context.pathname;
  post->pos->beg_pos  = context.stream->tellg();
  post->pos->beg_line = context.linenum;
  post->pos->sequence = context.sequence++;

  post->set_state(item_t::CLEARED);
  post->account = context.master;

  if (! amt.is_null())
    post->amount = - amt;

  if (! total.empty()) {
    std::istringstream assigned_amount_str(total);
    amt.parse(assigned_amount_str, PARSE_NO_REDUCE);
    if (! amt.has_commodity() &&
        commodity_pool_t::current_pool->default_commodity)
      amt.set_commodity(*commodity_pool_t::current_pool->default_commodity);
    post->assigned_amount = amt;
  }

  xact->add_post(post.release());

  return xact.release();
}

} // namespace ledger
