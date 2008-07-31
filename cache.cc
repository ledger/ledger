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

#include "cache.h"
#include "binary.h"

namespace ledger {

using namespace binary;

#if 0
void read_xact(const char *& data, xact_t * xact)
{
  read_number(data, xact->_date);
  read_number(data, xact->_date_eff);
  xact->account = accounts[read_long<account_t::ident_t>(data) - 1];

  unsigned char flag = read_number<unsigned char>(data);
  if (flag == 0) {
    xact->amount.read(data);
  }
  else if (flag == 1) {
    xact->amount.read(data);
    xact->amount_expr = expr_t();
    xact->amount_expr->set_text(read_string(data));
  }
  else {
    xact->amount_expr = expr_t();
    xact->amount_expr->read(data);
  }

  if (read_bool(data)) {
    xact->cost = amount_t();
    xact->cost->read(data);

    xact->cost_expr = expr_t();
    xact->cost_expr->read(data);
  } else {
    xact->cost = none;
  }

  read_number(data, xact->state);
  xact->set_flags(read_number<xact_t::flags_t>(data));
  xact->add_flags(XACT_BULK_ALLOC);
  read_string(data, xact->note);

  xact->beg_pos = read_long<unsigned long>(data);
  read_long(data, xact->beg_line);
  xact->end_pos = read_long<unsigned long>(data);
  read_long(data, xact->end_line);

  xact->data = NULL;

  if (xact->amount_expr)
    expr_t::compute_amount(xact->amount_expr.get(), xact->amount, xact);
}

void write_xact(std::ostream& out, xact_t * xact,
		bool ignore_calculated)
{
  write_number(out, xact->_date);
  write_number(out, xact->_date_eff);
  write_long(out, xact->account->ident);

  if (ignore_calculated && xact->has_flags(XACT_CALCULATED)) {
    write_number<unsigned char>(out, 0);
    amount_t().write(out);
  }
  else if (xact->amount_expr) {
    write_number<unsigned char>(out, 2);
    // jww (2008-07-30): Um, is this right?
    xact->amount_expr->write(out);
  }
  else if (! xact->amount_expr->text().empty()) {
    write_number<unsigned char>(out, 1);
    xact->amount.write(out);
    write_string(out, xact->amount_expr->text());
  }
  else {
    write_number<unsigned char>(out, 0);
    xact->amount.write(out);
  }

  if (xact->cost &&
      (! (ignore_calculated && xact->has_flags(XACT_CALCULATED)))) {
    write_bool(out, true);
    xact->cost->write(out);
    // jww (2008-07-30): What if there is no cost expression?
    xact->cost_expr->write(out);
  } else {
    write_bool(out, false);
  }

  write_number(out, xact->state);
  write_number(out, xact->flags());
  write_string(out, xact->note);

  write_long(out, xact->beg_pos);
  write_long(out, xact->beg_line);
  write_long(out, xact->end_pos);
  write_long(out, xact->end_line);
}

void read_entry_base(const char *& data, entry_base_t * entry,
		     xact_t *& xact_pool, bool& finalize)
{
  read_long(data, entry->src_idx);
  entry->beg_pos = read_long<unsigned long>(data);
  read_long(data, entry->beg_line);
  entry->end_pos = read_long<unsigned long>(data);
  read_long(data, entry->end_line);

  bool ignore_calculated = read_bool(data);

  for (unsigned long i = 0, count = read_long<unsigned long>(data);
       i < count;
       i++) {
    new(xact_pool) xact_t;
    read_xact(data, xact_pool);
    if (ignore_calculated && xact_pool->has_flags(XACT_CALCULATED))
      finalize = true;
    entry->add_xact(xact_pool++);
  }
}

void write_entry_base(std::ostream& out, entry_base_t * entry)
{
  write_long(out, entry->src_idx);
  write_long(out, entry->beg_pos);
  write_long(out, entry->beg_line);
  write_long(out, entry->end_pos);
  write_long(out, entry->end_line);

  bool ignore_calculated = false;
  for (xacts_list::const_iterator i = entry->xacts.begin();
       i != entry->xacts.end();
       i++)
    if ((*i)->amount_expr) {
      ignore_calculated = true;
      break;
    }

  write_bool(out, ignore_calculated);

  write_long(out, entry->xacts.size());
  for (xacts_list::const_iterator i = entry->xacts.begin();
       i != entry->xacts.end();
       i++)
    write_xact(out, *i, ignore_calculated);
}

void read_entry(const char *& data, entry_t * entry,
		xact_t *& xact_pool, bool& finalize)
{
  read_entry_base(data, entry, xact_pool, finalize);
  read_number(data, entry->_date);
  read_number(data, entry->_date_eff);
  read_string(data, entry->code);
  read_string(data, entry->payee);
}

void write_entry(std::ostream& out, entry_t * entry)
{
  write_entry_base(out, entry);
  write_number(out, entry->_date);
  write_number(out, entry->_date_eff);
  write_string(out, entry->code);
  write_string(out, entry->payee);
}

void read_auto_entry(const char *& data, auto_entry_t * entry,
			    xact_t *& xact_pool)
{
  bool ignore;
  read_entry_base(data, entry, xact_pool, ignore);

  expr_t expr;
  expr.read(data);
  entry->predicate = item_predicate<xact_t>(expr);
}

void write_auto_entry(std::ostream& out, auto_entry_t * entry)
{
  write_entry_base(out, entry);
  entry->predicate.predicate.write(out);
}

void read_period_entry(const char *& data, period_entry_t * entry,
			      xact_t *& xact_pool, bool& finalize)
{
  read_entry_base(data, entry, xact_pool, finalize);
  read_string(data, &entry->period_string);
  std::istringstream stream(entry->period_string);
  entry->period.parse(stream);
}

void write_period_entry(std::ostream& out, period_entry_t * entry)
{
  write_entry_base(out, entry);
  write_string(out, entry->period_string);
}

commodity_t::base_t * read_commodity_base(const char *& data)
{
  string str;
  
  read_string(data, str);

  std::auto_ptr<commodity_t::base_t> commodity(new commodity_t::base_t(str));

  read_string(data, str);
  if (! str.empty())
    commodity->name = str;

  read_string(data, str);
  if (! str.empty())
    commodity->note = str;

  read_number(data, commodity->precision);
  unsigned long flags;
  read_number(data, flags);
  commodity->set_flags(flags);

  return commodity.release();
}

void write_commodity_base(std::ostream& out, commodity_t::base_t * commodity)
{
  // jww (2008-04-22): Not using this anymore?
  //commodity->ident = ++base_commodity_index;

  write_string(out, commodity->symbol);
  // jww (2008-04-22): What to do with optional members?
  write_string(out, *commodity->name);
  write_string(out, *commodity->note);
  write_number(out, commodity->precision);
  write_number(out, commodity->flags());
}

void read_commodity_base_extra(const char *& data,
				      commodity_t::ident_t ident)
{
  commodity_t::base_t * commodity = base_commodities[ident];

  bool read_history = false;
  for (unsigned long i = 0, count = read_long<unsigned long>(data);
       i < count;
       i++) {
    datetime_t when;
    read_number(data, when);
    amount_t amt;
    amt.read(data);

    // Upon insertion, amt will be copied, which will cause the amount to be
    // duplicated (and thus not lost when the journal's item_pool is deleted).
    if (! commodity->history)
      commodity->history = commodity_t::history_t();
    commodity->history->prices.insert(commodity_t::base_t::history_pair(when, amt));

    read_history = true;
  }
  if (read_history)
    read_number(data, commodity->history->last_lookup);

  if (read_bool(data)) {
    amount_t amt;
    amt.read(data);
    commodity->smaller = amount_t(amt);
  }

  if (read_bool(data)) {
    amount_t amt;
    amt.read(data);
    commodity->larger = amount_t(amt);
  }
}

void write_commodity_base_extra(std::ostream& out,
				commodity_t::base_t * commodity)
{
#if 0
  // jww (2008-04-22): What did bogus_time used to do?
  if (commodity->history && commodity->history->bogus_time)
    commodity->remove_price(commodity->history->bogus_time);
#endif

  if (! commodity->history) {
    write_long<unsigned long>(out, 0);
  } else {
    write_long<unsigned long>(out, commodity->history->prices.size());
    for (commodity_t::history_map::const_iterator
	   i = commodity->history->prices.begin();
	 i != commodity->history->prices.end();
	 i++) {
      write_number(out, (*i).first);
      (*i).second.write(out);
    }
    write_number(out, commodity->history->last_lookup);
  }

  if (commodity->smaller) {
    write_bool(out, true);
    commodity->smaller->write(out);
  } else {
    write_bool(out, false);
  }

  if (commodity->larger) {
    write_bool(out, true);
    commodity->larger->write(out);
  } else {
    write_bool(out, false);
  }
}

commodity_t * read_commodity(const char *& data)
{
  commodity_t::base_t * base =
    base_commodities[read_long<commodity_t::ident_t>(data) - 1];

  commodity_t * commodity =
    new commodity_t(amount_t::current_pool,
		    shared_ptr<commodity_t::base_t>(base));

  *commodities_next++ = commodity;

  string str;
  read_string(data, str);
  if (! str.empty())
    commodity->qualified_symbol = str;
  commodity->annotated = false;

  return commodity;
}

void write_commodity(std::ostream& out, commodity_t * commodity)
{
  commodity->ident = ++commodity_index;

  // jww (2008-04-22): Is this used anymore?
  //write_long(out, commodity->base->ident);
  // jww (2008-04-22): Optional!
  write_string(out, *commodity->qualified_symbol);
}

commodity_t * read_commodity_annotated(const char *& data)
{
  commodity_t * commodity = 
    commodities[read_long<commodity_t::ident_t>(data) - 1];

  annotation_t details;

  string str;
  read_string(data, str);

  // This read-and-then-assign causes a new amount to be allocated which does
  // not live within the bulk allocation pool, since that pool will be deleted
  // *before* the commodities are destroyed.
  amount_t amt;
  amt.read(data);
  details.price = amt;

#if 0
  // jww (2008-04-22): These are optional members!
  read_number(data, details.date);
  read_string(data, details.tag);
#endif

  annotated_commodity_t * ann_comm =
    new annotated_commodity_t(commodity, details);
  *commodities_next++ = ann_comm;

  if (! str.empty())
    ann_comm->qualified_symbol = str;

  return ann_comm;
}

void write_commodity_annotated(std::ostream& out,
				      commodity_t * commodity)
{
  commodity->ident = ++commodity_index;

  // jww (2008-04-22): No longer needed?
  //write_long(out, commodity->base->ident);
  // jww (2008-04-22): Optional!
  write_string(out, *commodity->qualified_symbol);

  annotated_commodity_t * ann_comm =
    static_cast<annotated_commodity_t *>(commodity);

  // jww (2008-04-22): No longer needed?
  //write_long(out, ann_comm->base->ident);
  // jww (2008-04-22): Make a write_annotation_details function; and optional!
  ann_comm->details.price->write(out);
  ann_comm->details.date->write(out);
  ann_comm->details.tag->write(out);
}

inline
account_t * read_account(const char *& data, account_t * master = NULL)
{
  account_t * acct = new account_t(NULL);

  accounts[account_ident++] = acct;

  account_t::ident_t id;
  read_long(data, id);	// parent id
  if (id == 0xffffffff)
    acct->parent = NULL;
  else
    acct->parent = accounts[id - 1];

  read_string(data, acct->name);
  read_string(data, acct->note);
  read_number(data, acct->depth);

  // If all of the subaccounts will be added to a different master
  // account, throw away what we've learned about the recorded
  // journal's own master account.

  if (master && acct != master) {
    checked_delete(acct);
    acct = master;
  }

  for (account_t::ident_t i = 0,
	 count = read_long<account_t::ident_t>(data);
       i < count;
       i++) {
    account_t * child = read_account(data);
    child->parent = acct;
    assert(acct != child);
    acct->add_account(child);
  }

  return acct;
}

namespace {
  inline account_t::ident_t count_accounts(account_t * account)
  {
    account_t::ident_t count = 1;

    for (accounts_map::iterator i = account->accounts.begin();
	 i != account->accounts.end();
	 i++)
      count += count_accounts((*i).second);

    return count;
  }
}

void write_account(std::ostream& out, account_t * account)
{
  account->ident = ++account_ident;

  if (account->parent)
    write_long(out, account->parent->ident);
  else
    write_long<account_t::ident_t>(out, 0xffffffff);

  write_string(out, account->name);
  write_string(out, account->note);
  write_number(out, account->depth);

  write_number<std::size_t>(out, account->accounts.size());

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    write_account(out, (*i).second);
}

unsigned int read_journal(std::istream& in,
			  const path&   file,
			  journal_t&    journal,
			  account_t *   master)
{
  using namespace binary;

  // Read in the files that participated in this journal, so that they
  // can be checked for changes on reading.

  if (! file.empty()) {
    for (unsigned short i = 0,
	   count = read_number<unsigned short>(in);
	 i < count;
	 i++) {
      path pathname = read_string(in);
      std::time_t old_mtime;
      read_number(in, old_mtime);
      struct stat info;
      // jww (2008-04-22): can this be done differently now?
      stat(pathname.string().c_str(), &info);
      if (std::difftime(info.st_mtime, old_mtime) > 0)
	return 0;

      sources.push_back(pathname);
    }

    // Make sure that the cache uses the same price database,
    // otherwise it means that LEDGER_PRICE_DB has been changed, and
    // we should ignore this cache file.
    if (read_bool(in)) {
      string pathname;
      read_string(in, pathname);
      if (! price_db ||
	  price_db->string() != std::string(pathname))
	return 0;
    }
  }

  // jww (2008-07-31): bind master to session.master

  if (read_bool(data))
    basket = accounts[read_long<account_t::ident_t>(data) - 1];

  // Read in the entries and xacts

  for (std::size_t i = 0; i < count; i++) {
    new(entry_pool) entry_t;
    bool finalize = false;
    read_entry(data, entry_pool, xact_pool, finalize);
    entry_pool->journal = &journal;
    if (finalize && ! entry_pool->finalize())
      continue;
    entries.push_back(entry_pool++);
  }

  for (std::size_t i = 0; i < auto_count; i++) {
    auto_entry_t * auto_entry = new auto_entry_t;
    read_auto_entry(data, auto_entry, xact_pool);
    auto_entry->journal = &journal;
    auto_entries.push_back(auto_entry);
  }

  for (std::size_t i = 0; i < period_count; i++) {
    period_entry_t * period_entry = new period_entry_t;
    bool finalize = false;
    read_period_entry(data, period_entry, xact_pool, finalize);
    period_entry->journal = &journal;
    if (finalize && ! period_entry->finalize())
      continue;
    period_entries.push_back(period_entry);
  }

  VERIFY(journal.valid());

  return count;
}

std::pair<std::size_t, std::size_t>
write_journal(std::ostream& out, const journal_t& journal)
{
  using namespace binary;

  // Write out the files that participated in this journal, so that
  // they can be checked for changes on reading.

  if (sources.empty()) {
    write_number<unsigned short>(out, 0);
  } else {
    write_number<unsigned short>(out, sources.size());
    for (paths_list::const_iterator i = sources.begin();
	 i != sources.end();
	 i++) {
      write_string(out, (*i).string());
      struct stat info;
      stat((*i).string().c_str(), &info);
      write_number(out, std::time_t(info.st_mtime));
    }

    // Write out the price database that relates to this data file, so
    // that if it ever changes the cache can be invalidated.
    if (price_db) {
      write_bool(out, true);
      write_string(out, price_db->string());
    } else {
      write_bool(out, false);
    }
  }

  // Write out the basket accounts

  if (basket) {
    write_bool(out, true);
    write_long(out, basket->ident);
  } else {
    write_bool(out, false);
  }

  // Write out the entries and xacts

  std::size_t this_entry_count = 0;
  std::size_t this_xact_count  = 0;

  for (entries_list::const_iterator i = entries.begin();
       i != entries.end();
       i++) {
    write_entry(out, *i);

    this_entry_count++;
    this_xact_count += (*i)->xacts.size();
  }

  for (auto_entries_list::const_iterator i = auto_entries.begin();
       i != auto_entries.end();
       i++) {
    write_auto_entry(out, *i);

    this_entry_count++;
    this_xact_count += (*i)->xacts.size();
  }

  for (period_entries_list::const_iterator i = period_entries.begin();
       i != period_entries.end();
       i++) {
    write_period_entry(out, *i);

    this_entry_count++;
    this_xact_count += (*i)->xacts.size();
  }

  return std::pair<std::size_t, std::size_t>(this_entry_count,
					     this_xact_count);
}

std::size_t read_session(std::istream& in,
			 const path&   file,
			 session_t&    session)
{
  using namespace binary;

  // Read all of the data in at once, so that we're just dealing with
  // a big data buffer.

  std::size_t data_size = read_number<std::size_t>(in);

  scoped_array<char> data_pool(new char[data_size]);

  in.read(data_pool, data_size);

  const char * data = data_pool.get();

  // Read in the accounts

  accounts.resize(read_number<std::size_t>(data));
  account_ident = 0;

  if (session.master)
    checked_delete(session.master);
  session.master = read_account(data);

  // Allocate the memory needed for the entries, xacts and bigints in one
  // large block, which is then chopped up and custom constructed as
  // necessary.

  entry_count        = read_number<std::size_t>(data);
  auto_entry_count   = read_number<std::size_t>(data);
  period_entry_count = read_number<std::size_t>(data);
  xact_count	     = read_number<std::size_t>(data);
  bigints_count	     = read_number<std::size_t>(data);

#define ENTRIES_SIZE   (sizeof(entry_t) * entry_count)
#define XACTS_SIZE     (sizeof(xact_t) * xact_count)
#define BIGINTS_SIZE   (amount_t::sizeof_bigint_t() * bigints_count)

#define ENTRIES_OFFSET 0
#define XACTS_OFFSET   ENTRIES_SIZE
#define BIGINTS_OFFSET (ENTRIES_SIZE + XACTS_SIZE)

  item_pool.reset(new char[ENTRIES_SIZE + XACTS_SIZE + BIGINTS_SIZE]);

  entry_pool   = reinterpret_cast<entry_t *>(item_pool.get() + ENTRIES_OFFSET);
  xact_pool    = reinterpret_cast<xact_t *>(item_pool.get() + XACTS_OFFSET);
  bigints      = item_pool.get() + BIGINTS_OFFSET;
  bigints_next = bigints;
  bigint_ident = 0;

#if 0
  // Read in the base commodities and the derived commodities

  base_commodity_count = read_number<std::size_t>(data);
  base_commodities.resize(base_commodity_count);

  for (std::size_t i = 0; i < base_commodity_count; i++) {
    commodity_t::base_t * base = read_commodity_base(data);
    session.commodity_pool->commodities.push_back(base);

    std::pair<base_commodities_map::iterator, bool> result =
      commodity_base_t::commodities.insert
      (base_commodities_pair(commodity->symbol, commodity));
    if (! result.second) {
      base_commodities_map::iterator c =
	commodity_t::base_t::commodities.find(commodity->symbol);

      // It's possible the user might have used a commodity in a value
      // expression passed to an option, we'll just override the flags, but
      // keep the commodity pointer intact.
      if (c == commodity_t::base_t::commodities.end())
	throw_(cache_error, "Failed to read base commodity from cache: "
	       << commodity->symbol);

      (*c).second->name	     = commodity->name;
      (*c).second->note	     = commodity->note;
      (*c).second->precision = commodity->precision;
      (*c).second->flags     = commodity->flags;

      if ((*c).second->smaller)
	checked_delete((*c).second->smaller);
      (*c).second->smaller   = commodity->smaller;
      if ((*c).second->larger)
	checked_delete((*c).second->larger);
      (*c).second->larger    = commodity->larger;

      *(base_commodities_next - 1) = (*c).second;

      checked_delete(commodity);
    }
  }

  commodity_count = read_number<std::size_t>(data);
  commodities.resize(commodity_count);

  for (std::size_t i = 0; i < commodity_count; i++) {
    commodity_t * commodity;
    string	  mapping_key;

    if (! read_bool(data)) {
      commodity	  = read_commodity(data);
      mapping_key = commodity->base->symbol;
    } else {
      read_string(data, mapping_key);
      commodity = read_commodity_annotated(data);
    }

    session.commodity_pool->commodities.push_back(commodity);

    if (! result.second) {
      commodities_map::iterator c =
	commodity_t::commodities.find(mapping_key);
      if (c == commodity_t::commodities.end())
	throw_(cache_error, "Failed to read commodity from cache: "
	       << commodity->symbol());

      *(commodities_next - 1) = (*c).second;
      checked_delete(commodity);
    }
  }

  for (std::size_t i = 0; i < base_commodity_count; i++)
    read_commodity_base_extra(data, i);

  commodity_t::ident_t ident = read_number<commodity_t::ident_t>(data);
  if (ident == 0xffffffff || ident == 0)
    session.commodity_pool->default_commodity = NULL;
  else
    session.commodity_pool->default_commodity = commodities[ident - 1];
#endif

  // Clean up and return the number of entries read

  accounts.clear();
  commodities.clear();

  VERIFY(session.valid());

  return count;
}

void write_session(std::ostream& out, session_t& session)
{
  using namespace binary;

  write_number_nocheck(out, binary_magic_number);
  write_number_nocheck(out, format_version);

  // This number gets patched at the end of the function
  ostream_pos_type data_val = out.tellp();
  write_number<std::size_t>(out, 0);

  // Write out the accounts

  write_number<std::size_t>(out, count_accounts(session.master));
  write_account(out, session.master);

  // Write out the number of entries, xacts, and amounts

  write_number<std::size_t>(out, entries.size());
  write_number<std::size_t>(out, auto_entries.size());
  write_number<std::size_t>(out, period_entries.size());

  // These two numbers get patched at the end of the function
  ostream_pos_type xacts_val = out.tellp();
  write_number<std::size_t>(out, 0);
  ostream_pos_type bigints_val = out.tellp();
  write_number<std::size_t>(out, 0);

  bigint_ident = 0;

#if 0
  // Write out the commodities
  // jww (2008-04-22): This whole section needs to be reworked

  write_number<std::size_t>(out, session.commodity_pool->commodities.size());
  write_number<std::size_t>(out, session.commodity_pool->commodities.size());

  for (base_commodities_map::const_iterator i =
	 commodity_t::base_t::commodities.begin();
       i != commodity_t::base_t::commodities.end();
       i++)
    write_commodity_base(out, (*i).second);

  write_number<commodity_t::ident_t>
    (out, commodity_t::commodities.size());

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++) {
    if (! (*i).second->annotated) {
      write_bool(out, false);
      write_commodity(out, (*i).second);
    }
  }

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++) {
    if ((*i).second->annotated) {
      write_bool(out, true);
      write_string(out, (*i).first); // the mapping key
      write_commodity_annotated(out, (*i).second);
    }
  }

  // Write out the history and smaller/larger convertible links after
  // both the base and the main commodities have been written, since
  // the amounts in both will refer to the mains.

  for (base_commodities_map::const_iterator i =
	 commodity_t::base_t::commodities.begin();
       i != commodity_t::base_t::commodities.end();
       i++)
    write_commodity_base_extra(out, (*i).second);

  if (commodity_t::default_commodity)
    write_number(out, commodity_t::default_commodity->ident);
  else
    write_number<commodity_t::ident_t>(out, 0xffffffff);
#endif

  // Back-patch several counts which were not known beforehand

  out.seekp(data_val);
  write_number<std::size_t>(out, (static_cast<std::size_t>(out.tellp()) -
				  static_cast<std::size_t>(data_val) -
				  sizeof(std::size_t)));
  out.seekp(xacts_val);
  write_number<std::size_t>(out, xact_count);
  out.seekp(bigints_val);
  write_number<std::size_t>(out, bigints_count);
}
#endif

} // namespace ledger
