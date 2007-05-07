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

#include "gnucash.h"

namespace ledger {

void startElement(void *userData, const char *name, const char ** /* attrs */)
{
  gnucash_parser_t * parser = static_cast<gnucash_parser_t *>(userData);

  if (std::strcmp(name, "gnc:account") == 0) {
    parser->curr_account = new account_t(parser->master_account);
  }
  else if (std::strcmp(name, "act:name") == 0)
    parser->action = gnucash_parser_t::ACCOUNT_NAME;
  else if (std::strcmp(name, "act:id") == 0)
    parser->action = gnucash_parser_t::ACCOUNT_ID;
  else if (std::strcmp(name, "act:parent") == 0)
    parser->action = gnucash_parser_t::ACCOUNT_PARENT;
  else if (std::strcmp(name, "gnc:commodity") == 0)
    parser->curr_comm = NULL;
  else if (std::strcmp(name, "cmdty:id") == 0)
    parser->action = gnucash_parser_t::COMM_SYM;
  else if (std::strcmp(name, "cmdty:name") == 0)
    parser->action = gnucash_parser_t::COMM_NAME;
  else if (std::strcmp(name, "cmdty:fraction") == 0)
    parser->action = gnucash_parser_t::COMM_PREC;
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(! parser->curr_entry);
    parser->curr_entry = new entry_t;
  }
  else if (std::strcmp(name, "trn:num") == 0)
    parser->action = gnucash_parser_t::ENTRY_NUM;
  else if (std::strcmp(name, "trn:date-posted") == 0)
    parser->action = gnucash_parser_t::ALMOST_ENTRY_DATE;
  else if (parser->action == gnucash_parser_t::ALMOST_ENTRY_DATE &&
	   std::strcmp(name, "ts:date") == 0)
    parser->action = gnucash_parser_t::ENTRY_DATE;
  else if (std::strcmp(name, "trn:description") == 0)
    parser->action = gnucash_parser_t::ENTRY_DESC;
  else if (std::strcmp(name, "trn:split") == 0) {
    assert(parser->curr_entry);
    parser->curr_entry->add_transaction(new transaction_t(parser->curr_account));
  }
  else if (std::strcmp(name, "split:reconciled-state") == 0)
    parser->action = gnucash_parser_t::XACT_STATE;
  else if (std::strcmp(name, "split:amount") == 0)
    parser->action = gnucash_parser_t::XACT_AMOUNT;
  else if (std::strcmp(name, "split:value") == 0)
    parser->action = gnucash_parser_t::XACT_VALUE;
  else if (std::strcmp(name, "split:quantity") == 0)
    parser->action = gnucash_parser_t::XACT_QUANTITY;
  else if (std::strcmp(name, "split:account") == 0)
    parser->action = gnucash_parser_t::XACT_ACCOUNT;
  else if (std::strcmp(name, "split:memo") == 0)
    parser->action = gnucash_parser_t::XACT_NOTE;
}

void endElement(void *userData, const char *name)
{
  gnucash_parser_t * parser = static_cast<gnucash_parser_t *>(userData);

  if (std::strcmp(name, "gnc:account") == 0) {
    assert(parser->curr_account);
    if (parser->curr_account->parent == parser->master_account)
      parser->curr_journal->add_account(parser->curr_account);
    parser->accounts_by_id.insert
      (accounts_map::value_type(parser->curr_account_id, parser->curr_account));
    parser->curr_account = NULL;
  }
  else if (std::strcmp(name, "gnc:commodity") == 0) {
    parser->curr_comm = NULL;
  }
  else if (std::strcmp(name, "gnc:transaction") == 0) {
    assert(parser->curr_entry);

    // Add the new entry (what gnucash calls a 'transaction') to the
    // journal
    if (! parser->curr_journal->add_entry(parser->curr_entry)) {
      print_entry(std::cerr, *parser->curr_entry);
      parser->have_error = "The above entry does not balance";
      checked_delete(parser->curr_entry);
    } else {
      parser->curr_entry->src_idx  = parser->src_idx;
      parser->curr_entry->beg_pos  = parser->beg_pos;
      parser->curr_entry->beg_line = parser->beg_line;
      parser->curr_entry->end_pos  = parser->instreamp->tellg();
      parser->curr_entry->end_line =
	XML_GetCurrentLineNumber(parser->expat_parser) - parser->offset;
      parser->count++;
    }

    // Clear the relevant variables for the next run
    parser->curr_entry = NULL;
    parser->entry_comm = NULL;
  }
  else if (std::strcmp(name, "trn:split") == 0) {
    transaction_t * xact = parser->curr_entry->transactions.back();

    // Identify the commodity to use for the value of this
    // transaction.  The quantity indicates how many times that value
    // the transaction is worth.
    amount_t value;
    commodity_t * default_commodity = NULL;
    if (parser->entry_comm) {
      default_commodity = parser->entry_comm;
    } else {
      gnucash_parser_t::account_comm_map::iterator ac =
	parser->account_comms.find(xact->account);
      if (ac != parser->account_comms.end())
	default_commodity = (*ac).second;
    }

    if (default_commodity) {
      parser->curr_quant.set_commodity(*default_commodity);
      value = parser->curr_quant.round();

      if (parser->curr_value.commodity() == *default_commodity)
	parser->curr_value = value;
    } else {
      value = parser->curr_quant;
    }

    xact->state  = parser->curr_state;
    xact->amount = value;
    if (value != parser->curr_value)
      xact->cost = amount_t(parser->curr_value);

    xact->beg_pos  = parser->beg_pos;
    xact->beg_line = parser->beg_line;
    xact->end_pos  = parser->instreamp->tellg();
    xact->end_line =
      XML_GetCurrentLineNumber(parser->expat_parser) - parser->offset;

    // Clear the relevant variables for the next run
    parser->curr_state = transaction_t::UNCLEARED;
    parser->curr_value = amount_t();
    parser->curr_quant = amount_t();
  }

  parser->action = gnucash_parser_t::NO_ACTION;
}

amount_t gnucash_parser_t::convert_number(const string& number,
					  int * precision)
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

void dataHandler(void *userData, const char *s, int len)
{
  gnucash_parser_t * parser = static_cast<gnucash_parser_t *>(userData);

  switch (parser->action) {
  case gnucash_parser_t::ACCOUNT_NAME:
    parser->curr_account->name = string(s, len);
    break;

  case gnucash_parser_t::ACCOUNT_ID:
    parser->curr_account_id = string(s, len);
    break;

  case gnucash_parser_t::ACCOUNT_PARENT: {
    accounts_map::iterator i = parser->accounts_by_id.find(string(s, len));
    assert(i != parser->accounts_by_id.end());
    parser->curr_account->parent = (*i).second;
    parser->curr_account->depth  = parser->curr_account->parent->depth + 1;
    (*i).second->add_account(parser->curr_account);
    break;
  }

  case gnucash_parser_t::COMM_SYM: {
    string symbol(s, len);
    if (symbol == "USD") symbol = "$";

    parser->curr_comm = amount_t::current_pool->find_or_create(symbol);
    assert(parser->curr_comm);

    if (symbol != "$")
      parser->curr_comm->add_flags(COMMODITY_STYLE_SEPARATED);

    if (parser->curr_account)
      parser->account_comms.insert
	(gnucash_parser_t::account_comm_map::value_type
	 (parser->curr_account, parser->curr_comm));
    else if (parser->curr_entry)
      parser->entry_comm = parser->curr_comm;
    break;
  }

  case gnucash_parser_t::COMM_NAME:
    parser->curr_comm->set_name(string(s, len));
    break;

  case gnucash_parser_t::COMM_PREC:
    parser->curr_comm->set_precision(len - 1);
    break;

  case gnucash_parser_t::ENTRY_NUM:
    parser->curr_entry->code = string(s, len);
    break;

  case gnucash_parser_t::ENTRY_DATE:
    parser->curr_entry->_date = parse_datetime(string(s, len));
    break;

  case gnucash_parser_t::ENTRY_DESC:
    parser->curr_entry->payee = string(s, len);
    break;

  case gnucash_parser_t::XACT_STATE:
    if (*s == 'y')
      parser->curr_state = transaction_t::CLEARED;
    else if (*s == 'n')
      parser->curr_state = transaction_t::UNCLEARED;
    else
      parser->curr_state = transaction_t::PENDING;
    break;

  case gnucash_parser_t::XACT_VALUE: {
    int precision;
    assert(parser->entry_comm);
    parser->curr_value = parser->convert_number(string(s, len), &precision);
    parser->curr_value.set_commodity(*parser->entry_comm);

    if (precision > parser->entry_comm->precision())
      parser->entry_comm->set_precision(precision);
    break;
  }

  case gnucash_parser_t::XACT_QUANTITY:
    parser->curr_quant = parser->convert_number(string(s, len));
    break;

  case gnucash_parser_t::XACT_ACCOUNT: {
    transaction_t * xact = parser->curr_entry->transactions.back();

    accounts_map::iterator i =
      parser->accounts_by_id.find(string(s, len));
    if (i != parser->accounts_by_id.end()) {
      xact->account = (*i).second;
    } else {
      xact->account = parser->curr_journal->find_account("<Unknown>");

      parser->have_error = (string("Could not find account ") +
			    string(s, len));
    }
    break;
  }

  case gnucash_parser_t::XACT_NOTE:
    parser->curr_entry->transactions.back()->note = string(s, len);
    break;

  case gnucash_parser_t::NO_ACTION:
  case gnucash_parser_t::ALMOST_ENTRY_DATE:
  case gnucash_parser_t::XACT_AMOUNT:
    break;

  default:
    assert(false);
    break;
  }
}

bool gnucash_parser_t::test(std::istream& in) const
{
  char buf[5];
  in.read(buf, 5);
  in.clear();
  in.seekg(0, std::ios::beg);

  return std::strncmp(buf, "<?xml", 5) == 0;
}

unsigned int gnucash_parser_t::parse(std::istream&	   in,
				     journal_t *	   journal,
				     account_t *	   master,
				     const optional<path>& original_file)
{
  char buf[BUFSIZ];

  // This is the date format used by Gnucash, so override whatever the
  // user specified.
  //
  // jww (2006-09-13): Make this parser local somehow.
  //date_t::input_format = "%Y-%m-%d %H:%M:%S %z";

  count		 = 0;
  action	 = NO_ACTION;
  curr_journal	 = journal;
  master_account = master ? master : journal->master;
  curr_account	 = NULL;
  curr_entry	 = NULL;
  curr_comm	 = NULL;
  entry_comm	 = NULL;
  curr_state	 = transaction_t::UNCLEARED;

  instreamp = &in;
  pathname  = original_file ? *original_file : "<gnucash>";
  src_idx   = journal->sources.size() - 1;

  // GnuCash uses the USD commodity without defining it, which really
  // means $.
  commodity_t * usd = amount_t::current_pool->find_or_create("$");
  usd->set_precision(2);
  usd->add_flags(COMMODITY_STYLE_THOUSANDS);

  offset = 2;
  expat_parser = XML_ParserCreate(NULL);

  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);
  XML_SetUserData(parser, this);

  while (in.good() && ! in.eof()) {
    beg_pos  = in.tellg();
    beg_line = (XML_GetCurrentLineNumber(parser) - offset) + 1;

    in.getline(buf, BUFSIZ - 1);
    std::strcat(buf, "\n");
    if (! XML_Parse(parser, buf, std::strlen(buf), in.eof())) {
      //unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
      const char *  msg  = XML_ErrorString(XML_GetErrorCode(parser));
      XML_ParserFree(parser);
      throw_(parse_error, msg);
    }

    if (! have_error.empty()) {
      //unsigned long line = XML_GetCurrentLineNumber(parser) - offset++;
#if 0
      // jww (2007-04-26): What is this doing?
      parse_error err(have_error);
      std::cerr << "Error: " << err.what() << std::endl;
#endif
      have_error = "";
    }
  }

  XML_ParserFree(parser);

  accounts_by_id.clear();
  curr_account_id.clear();

  return count;
}

} // namespace ledger
