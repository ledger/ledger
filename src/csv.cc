/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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

string csv_reader::read_field()
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
      else if (x == c) {
	if (x == '|')
	  in.unget();
	else if (in.peek() == ',')
	  in.get(c);
	break;
      }
      field += x;
    }
  }
  else {
    
  }
  return field;
}

xact_t * csv_reader::read_xact(journal_t& journal, account_t * bucket)
{
  static char linebuf[MAX_LINE + 1];

  if (! in.good() || in.eof())
    return NULL;

  std::auto_ptr<xact_t> xact;

  while (in.good() && ! in.eof() && in.peek() == '#')
    in.getline(linebuf, MAX_LINE);

  xact.reset(new xact_t);

  xact->pos	      = position_t();
  xact->pos->pathname = "jww (2010-03-05): unknown";
  xact->pos->beg_pos  = in.tellg();
  xact->pos->beg_line = 0;
  xact->pos->sequence = 0;

  string date	= read_field(); trim(date);
  string code	= read_field(); trim(code);
  string payee	= read_field(); trim(payee);

  if (date.empty())
    return NULL;

  xact->set_state(item_t::CLEARED);
  xact->_date = parse_date(date);
  if (! code.empty())
    xact->code  = code;

  bool found = false;
  foreach (payee_mapping_t& value, journal.payee_mappings) {
    DEBUG("csv.mappings", "Looking for payee mapping: " << value.first);
    if (value.first.match(payee)) {
      xact->payee = value.second;
      found = true;
      break;
    }
  }
  if (! found)
    xact->payee = payee;

  string amount = read_field(); trim(amount);
  string total	= read_field(); trim(total);
  in.getline(linebuf, MAX_LINE); // skip to the next line

  std::auto_ptr<post_t> post(new post_t);

  post->xact = xact.get();

#if 0
  post->pos	      = position_t();
  post->pos->pathname = pathname;
  post->pos->beg_pos  = line_beg_pos;
  post->pos->beg_line = linenum;
  post->pos->sequence = context.sequence++;
#endif

  post->set_state(item_t::CLEARED);
  post->account = journal.master->find_account(_("Expenses:Unknown"));

  foreach (account_mapping_t& value, journal.account_mappings) {
    if (value.first.match(xact->payee)) {
      post->account = value.second;
      break;
    }
  }

  std::istringstream amount_str(amount);
  amount_t amt;
  amt.parse(amount_str, PARSE_NO_REDUCE);
  if (! amt.has_commodity() &&
      commodity_pool_t::current_pool->default_commodity)
    amt.set_commodity
      (*commodity_pool_t::current_pool->default_commodity);
  post->amount = amt;

  xact->add_post(post.release());

  post.reset(new post_t);

  post->xact = xact.get();

#if 0
  post->pos	      = position_t();
  post->pos->pathname = pathname;
  post->pos->beg_pos  = line_beg_pos;
  post->pos->beg_line = linenum;
  post->pos->sequence = context.sequence++;
#endif

  post->set_state(item_t::CLEARED);
  post->account = bucket;
  post->amount	= - amt;

  if (! total.empty()) {
    std::istringstream assigned_amount_str(total);
    amount_t assigned_amount;
    assigned_amount.parse(assigned_amount_str, PARSE_NO_REDUCE);
    post->assigned_amount = assigned_amount;
  }
  
  xact->add_post(post.release());

  return xact.release();
}

} // namespace ledger
