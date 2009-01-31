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

#include "gnucash.h"
#include "session.h"
#include "account.h"

namespace ledger {

typedef std::map<const string, account_t *>  accounts_map;
typedef std::pair<const string, account_t *> accounts_pair;

typedef std::map<account_t *, commodity_t *>  account_comm_map;
typedef std::pair<account_t *, commodity_t *> account_comm_pair;

#if 0

static journal_t *	curr_journal;
static account_t *	master_account;
static account_t *	curr_account;
static string		curr_account_id;
static entry_t *	curr_entry;
static commodity_t *	entry_comm;
static commodity_t *	curr_comm;
static amount_t		curr_value;
static amount_t		curr_quant;
static XML_Parser	current_parser;
static accounts_map	accounts_by_id;
static account_comm_map	account_comms;
static std::size_t	count;
static string		have_error;

static std::istream *   instreamp;
static std::size_t	offset;
static XML_Parser       parser;
static path      	pathname;
static std::size_t	src_idx;
static istream_pos_type beg_pos;
static unsigned long    beg_line;

static xact_t::state_t curr_state;

static enum action_t {
  NO_ACTION,
  ACCOUNT_NAME,
  ACCOUNT_ID,
  ACCOUNT_PARENT,
  COMM_SYM,
  COMM_NAME,
  COMM_PREC,
  ENTRY_NUM,
  ALMOST_ENTRY_DATE,
  ENTRY_DATE,
  ENTRY_DESC,
  XACT_STATE,
  XACT_AMOUNT,
  XACT_VALUE,
  XACT_QUANTITY,
  XACT_ACCOUNT,
  XACT_NOTE
} action;

static void startElement(void *, const char *name, const char **)
{
  if (std::strcmp(name, "gnc:account") == 0) {
    curr_account = new account_t(master_account);
  }
  else if (std::strcmp(name, "act:name") == 0)
    action = ACCOUNT_NAME;
  else if (std::strcmp(name, "act:id") == 0)
    action = ACCOUNT_ID;
  else if (std::strcmp(name, "act:parent") == 0)
    action = ACCOUNT_PARENT;
  else if (std::strcmp(name, "gnc:commodity") == 0)
    curr_comm = NULL;
  else if (std::strcmp(name, "cmdty:id") == 0)
    action = COMM_SYM;
  else if (std::strcmp(name, "cmdty:name") == 0)
    action = COMM_NAME;
  else if (std::strcmp(name, "cmdty:fraction") == 0)
    action = COMM_PREC;
  else if (std::strcmp(name, "gnc:xact") == 0) {
    assert(! curr_entry);
    curr_entry = new entry_t;
  }
  else if (std::strcmp(name, "trn:num") == 0)
    action = ENTRY_NUM;
  else if (std::strcmp(name, "trn:date-posted") == 0)
    action = ALMOST_ENTRY_DATE;
  else if (action == ALMOST_ENTRY_DATE && std::strcmp(name, "ts:date") == 0)
    action = ENTRY_DATE;
  else if (std::strcmp(name, "trn:description") == 0)
    action = ENTRY_DESC;
  else if (std::strcmp(name, "trn:split") == 0) {
    assert(curr_entry);
    curr_entry->add_xact(new xact_t(curr_account));
  }
  else if (std::strcmp(name, "split:reconciled-state") == 0)
    action = XACT_STATE;
  else if (std::strcmp(name, "split:amount") == 0)
    action = XACT_AMOUNT;
  else if (std::strcmp(name, "split:value") == 0)
    action = XACT_VALUE;
  else if (std::strcmp(name, "split:quantity") == 0)
    action = XACT_QUANTITY;
  else if (std::strcmp(name, "split:account") == 0)
    action = XACT_ACCOUNT;
  else if (std::strcmp(name, "split:memo") == 0)
    action = XACT_NOTE;
}

static void endElement(void *, const char *name)
{
  if (std::strcmp(name, "gnc:account") == 0) {
    assert(curr_account);
    if (curr_account->parent == master_account)
      curr_journal->add_account(curr_account);
    accounts_by_id.insert(accounts_pair(curr_account_id, curr_account));
    curr_account = NULL;
  }
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    curr_comm = NULL;
  }
  else if (std::strcmp(name, "gnc:xact") == 0) {
    assert(curr_entry);

    // Add the new entry (what gnucash calls a 'xact') to the
    // journal
    if (! curr_journal->add_entry(curr_entry)) {
#if 0
      print_entry(std::cerr, *curr_entry);
#endif
      have_error = "The above entry does not balance";
      checked_delete(curr_entry);
    } else {
      curr_entry->src_idx  = src_idx;
      curr_entry->beg_pos  = beg_pos;
      curr_entry->beg_line = beg_line;
      curr_entry->end_pos  = instreamp->tellg();
      curr_entry->end_line = XML_GetCurrentLineNumber(parser) - offset;
      count++;
    }

    // Clear the relevant variables for the next run
    curr_entry = NULL;
    entry_comm = NULL;
  }
  else if (std::strcmp(name, "trn:split") == 0) {
    xact_t * xact = curr_entry->xacts.back();

    // Identify the commodity to use for the value of this
    // xact.  The quantity indicates how many times that value
    // the xact is worth.
    amount_t value;
    commodity_t * default_commodity = NULL;
    account_comm_map::iterator ac = account_comms.find(xact->account);
    if (ac != account_comms.end())
      default_commodity = (*ac).second;

    if (default_commodity) {
      curr_quant.set_commodity(*default_commodity);
      value = curr_quant.rounded();

      if (curr_value.commodity() == *default_commodity)
	curr_value = value;
    } else {
      value = curr_quant;
    }

    xact->set_state(curr_state);
    xact->amount = value;
    if (value != curr_value)
      xact->cost = curr_value;

    xact->beg_pos  = beg_pos;
    xact->beg_line = beg_line;
    xact->end_pos  = instreamp->tellg();
    xact->end_line = XML_GetCurrentLineNumber(parser) - offset;

    // Clear the relevant variables for the next run
    curr_state = xact_t::UNCLEARED;
    curr_value = amount_t();
    curr_quant = amount_t();
  }

  action = NO_ACTION;
}


static amount_t convert_number(const string& number,
			       int * precision = NULL)
{
  const char * num = number.c_str();

  if (char * p = std::strchr(num, '/')) {
    string numer_str(num, p - num);
    string denom_str(p + 1);

    amount_t amt(numer_str);
    amount_t den(denom_str);

    if (precision)
      *precision = denom_str.length() - 1;

    if (! den) {
      have_error = "Denominator in entry is zero!";
      return amt;
    } else {
      return amt / den;
    }
  } else {
    return amount_t(number);
  }
}

static void dataHandler(void *, const char *s, int len)
{
  switch (action) {
  case ACCOUNT_NAME:
    curr_account->name = string(s, len);
    break;

  case ACCOUNT_ID:
    curr_account_id = string(s, len);
    break;

  case ACCOUNT_PARENT: {
    accounts_map::iterator i = accounts_by_id.find(string(s, len));
    assert(i != accounts_by_id.end());
    curr_account->parent = (*i).second;
    curr_account->depth  = curr_account->parent->depth + 1;
    (*i).second->add_account(curr_account);
    break;
  }

  case COMM_SYM: {
      string symbol(s, len);
    if (symbol == "USD") symbol = "$";

    curr_comm = amount_t::current_pool->find_or_create(symbol);
    assert(curr_comm);

    if (symbol != "$")
      curr_comm->add_flags(COMMODITY_STYLE_SEPARATED);

    if (curr_account)
      account_comms.insert(account_comm_pair(curr_account, curr_comm));
    else if (curr_entry)
      entry_comm = curr_comm;
    break;
  }

  case COMM_NAME:
    curr_comm->set_name(string(s, len));
    break;

  case COMM_PREC:
    curr_comm->set_precision(len - 1);
    break;

  case ENTRY_NUM:
    curr_entry->code = string(s, len);
    break;

  case ENTRY_DATE:
    curr_entry->_date = parse_date(string(s, len));
    break;

  case ENTRY_DESC:
    curr_entry->payee = string(s, len);
    break;

  case XACT_STATE:
    if (*s == 'y')
      curr_state = xact_t::CLEARED;
    else if (*s == 'n')
      curr_state = xact_t::UNCLEARED;
    else
      curr_state = xact_t::PENDING;
    break;

  case XACT_VALUE: {
    int precision;
    assert(entry_comm);
    curr_value = convert_number(string(s, len), &precision);
    curr_value.set_commodity(*entry_comm);

    if (precision > entry_comm->precision())
      entry_comm->set_precision(precision);
    break;
  }

  case XACT_QUANTITY:
    curr_quant = convert_number(string(s, len));
    break;

  case XACT_ACCOUNT: {
    xact_t * xact = curr_entry->xacts.back();

    accounts_map::iterator i = accounts_by_id.find(string(s, len));
    if (i != accounts_by_id.end()) {
      xact->account = (*i).second;
    } else {
      xact->account = curr_journal->find_account("<Unknown>");

      have_error = (string("Could not find account ") +
		    string(s, len));
    }
    break;
  }

  case XACT_NOTE:
    curr_entry->xacts.back()->note = string(s, len);
    break;

  case NO_ACTION:
  case ALMOST_ENTRY_DATE:
  case XACT_AMOUNT:
    break;

  default:
    assert(false);
    break;
  }
}

#endif

bool gnucash_parser_t::test(std::istream& in) const
{
  char	 buf[80];
  char * p;

  in.read(buf, 11);
  if (utf8::is_bom(buf))
    p = &buf[3];
  else
    p = buf;

  if (std::strncmp(p, "<?xml", 5) != 0) {
    in.clear();
    in.seekg(0, std::ios::beg);
    return false;
  }

  in.clear();
  in.seekg(0, std::ios::beg);
  return true;
}

std::size_t gnucash_parser_t::parse(std::istream& in,
				    session_t&    session,
				    journal_t&	  journal,
				    account_t *   master,
				    const path *  original_file)
{
#if 0
  char buf[BUFSIZ];

#if 0
  // jww (2008-05-08): Replace this
  // This is the date format used by Gnucash, so override whatever the
  // user specified.
  date_t::input_format = "%Y-%m-%d %H:%M:%S %z";
#endif

  count		 = 0;
  action	 = NO_ACTION;
  curr_journal	 = &journal;
  master_account = master ? master : session.master.get();
  curr_account	 = NULL;
  curr_entry	 = NULL;
  curr_comm	 = NULL;
  entry_comm	 = NULL;
  curr_state	 = xact_t::UNCLEARED;

  instreamp = &in;
  pathname  = original_file ? *original_file : "<gnucash>";
  src_idx   = journal.sources.size() - 1;

  // GnuCash uses the USD commodity without defining it, which really
  // means $.
  commodity_t * usd = amount_t::current_pool->find_or_create("$");
  usd->set_precision(2);
  usd->add_flags(COMMODITY_STYLE_THOUSANDS);

  offset = 2;
  parser = current_parser = XML_ParserCreate(NULL);

  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  while (in.good() && ! in.eof()) {
    beg_pos  = in.tellg();
    beg_line = (XML_GetCurrentLineNumber(parser) - offset) + 1;

    in.getline(buf, BUFSIZ - 1);
    std::strcat(buf, "\n");
    if (! XML_Parse(parser, buf, std::strlen(buf), in.eof())) {
      //unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      const char *  msg  = XML_ErrorString(XML_GetErrorCode(parser));
      XML_ParserFree(parser);
      throw parse_error(msg);
    }

    if (! have_error.empty()) {
      //unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      parse_error err(have_error);
      std::cerr << "Error: " << err.what() << std::endl;
      have_error = "";
    }
  }

  XML_ParserFree(parser);

  accounts_by_id.clear();
  curr_account_id.clear();

  return count;
#else
  return 0;
#endif
}

} // namespace ledger
