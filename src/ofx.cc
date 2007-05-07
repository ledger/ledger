/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#include "ofx.h"

namespace ledger {

typedef std::map<const string, account_t *>   accounts_map;
typedef std::map<const string, commodity_t *> commodities_map;

journal_t *	curr_journal;
accounts_map	ofx_accounts;
commodities_map ofx_account_currencies;
commodities_map ofx_securities;
account_t *	master_account;

int ofx_proc_statement_cb(struct OfxStatementData data, void * statement_data)
{
}

int ofx_proc_account_cb(struct OfxAccountData data, void * account_data)
{
  if (! data.account_id_valid)
    return -1;

  DEBUG("ledger.ofx.parse", "account " << data.account_name);
  account_t * account = new account_t(master_account, data.account_name);
  curr_journal->add_account(account);
  ofx_accounts.insert(accounts_pair(data.account_id, account));

  if (data.currency_valid) {
    commodity_t * commodity = commodity_t::find_or_create(data.currency);
    commodity->add_flags(COMMODITY_STYLE_SUFFIXED | COMMODITY_STYLE_SEPARATED);

    commodities_map::iterator i = ofx_account_currencies.find(data.account_id);
    if (i == ofx_account_currencies.end())
      ofx_account_currencies.insert(commodities_pair(data.account_id,
						     commodity));
  }

  return 0;
}

int ofx_proc_transaction_cb(struct OfxTransactionData data,
			    void * transaction_data)
{
  if (! data.account_id_valid || ! data.units_valid)
    return -1;

  accounts_map::iterator i = ofx_accounts.find(data.account_id);
  assert(i != ofx_accounts.end());
  account_t * account = (*i).second;

  entry_t * entry = new entry_t;

  entry->add_transaction(new transaction_t(account));
  transaction_t * xact = entry->transactions.back();

  // get the account's default currency
  commodities_map::iterator ac = ofx_account_currencies.find(data.account_id);
  assert(ac != ofx_account_currencies.end());
  commodity_t * default_commodity = (*ac).second;

  std::ostringstream stream;
  stream << - data.units;

  // jww (2005-02-09): what if the amount contains fees?

  if (data.unique_id_valid) {
    commodities_map::iterator s = ofx_securities.find(data.unique_id);
    assert(s != ofx_securities.end());
    xact->amount = stream.str() + " " + (*s).second->base_symbol();
  } else {
    xact->amount = stream.str() + " " + default_commodity->base_symbol();
  }

  if (data.unitprice_valid && data.unitprice != 1.0) {
    std::ostringstream cstream;
    stream << - data.unitprice << " " << default_commodity->base_symbol();
    xact->cost = new amount_t(stream.str());
  }

  DEBUG("ofx.parse", "xact " << xact->amount << " from " << *xact->account);

  if (data.date_initiated_valid)
    entry->_date = data.date_initiated;
  else if (data.date_posted_valid)
    entry->_date = data.date_posted;

  if (data.check_number_valid)
    entry->code = data.check_number;
  else if (data.reference_number_valid)
    entry->code = data.reference_number;

  if (data.name_valid)
    entry->payee = data.name;

  if (data.memo_valid)
    xact->note = data.memo;

  // jww (2005-02-09): check for fi_id_corrected?  or is this handled
  // by the library?

  // Balance all entries into <Unknown>, since it is not specified.
  account = curr_journal->find_account("<Unknown>");
  entry->add_transaction(new transaction_t(account));

  if (! curr_journal->add_entry(entry)) {
    print_entry(std::cerr, *entry);
#if 0
    // jww (2005-02-09): uncomment
    have_error = "The above entry does not balance";
#endif
    checked_delete(entry);
    return -1;
  }
  return 0;
}

int ofx_proc_security_cb(struct OfxSecurityData data, void * security_data)
{
  if (! data.unique_id_valid)
    return -1;

  string symbol;
  if (data.ticker_valid)
    symbol = data.ticker;
  else if (data.currency_valid)
    symbol = data.currency;
  else
    return -1;

  commodity_t * commodity = commodity_t::find_or_create(symbol);
  commodity->add_flags(COMMODITY_STYLE_SUFFIXED | COMMODITY_STYLE_SEPARATED);

  if (data.secname_valid)
    commodity->set_name(data.secname);

  if (data.memo_valid)
    commodity->set_note(data.memo);

  commodities_map::iterator i = ofx_securities.find(data.unique_id);
  if (i == ofx_securities.end()) {
    DEBUG("ledger.ofx.parse", "security " << symbol);
    ofx_securities.insert(commodities_pair(data.unique_id, commodity));
  }

  // jww (2005-02-09): What is the commodity for data.unitprice?
  if (data.date_unitprice_valid && data.unitprice_valid) {
    DEBUG("ledger.ofx.parse", "  price " << data.unitprice);
    commodity->add_price(data.date_unitprice, amount_t(data.unitprice));
  }

  return 0;
}

int ofx_proc_status_cb(struct OfxStatusData data, void * status_data)
{
}

bool ofx_parser_t::test(std::istream& in) const
{
  char buf[80];

  in.getline(buf, 79);
  if (std::strncmp(buf, "OFXHEADER", 9) == 0) {
    in.clear();
    in.seekg(0, std::ios::beg);
    return true;
  }
  else if (std::strncmp(buf, "<?xml", 5) != 0) {
    in.clear();
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.getline(buf, 79);
  if (std::strncmp(buf, "<?OFX", 5) != 0 &&
      std::strncmp(buf, "<?ofx", 5) != 0) {
    in.clear();
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.clear();
  in.seekg(0, std::ios::beg);
  return true;
}

unsigned int ofx_parser_t::parse(std::istream&	     in,
				 journal_t *	     journal,
				 account_t *	     master,
				 const string * original_file)
{
  if (! original_file)
    return 0;

  curr_journal   = journal;
  master_account = master ? master : journal->master;

  LibofxContextPtr libofx_context = libofx_get_new_context();

  ofx_set_statement_cb  (libofx_context, ofx_proc_statement_cb, 0);
  ofx_set_account_cb    (libofx_context, ofx_proc_account_cb, 0);
  ofx_set_transaction_cb(libofx_context, ofx_proc_transaction_cb, 0);
  ofx_set_security_cb   (libofx_context, ofx_proc_security_cb, 0);
  ofx_set_status_cb     (libofx_context, ofx_proc_status_cb, 0);

  // The processing is done by way of callbacks, which are all defined
  // above.
  libofx_proc_file(libofx_context, original_file->c_str(), AUTODETECT);

  libofx_free_context(libofx_context);

  return 1; // jww (2005-02-09): count;
}

} // namespace ledger
