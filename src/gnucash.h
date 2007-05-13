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

#ifndef _GNUCASH_H
#define _GNUCASH_H

#include "parser.h"
#include "journal.h"

namespace ledger {

struct gnucash_parser_t : public parser_t
{
  typedef std::map<const string, account_t *>  accounts_map;
  typedef std::map<account_t *, commodity_t *> account_comm_map;

  journal_t *	   curr_journal;
  account_t *	   master_account;
  account_t *	   curr_account;
  string	   curr_account_id;
  entry_t *	   curr_entry;
  commodity_t *	   entry_comm;
  commodity_t *	   curr_comm;
  amount_t	   curr_value;
  amount_t	   curr_quant;
  XML_Parser	   expat_parser;
  accounts_map	   accounts_by_id;
  account_comm_map account_comms;
  unsigned int	   count;
  string	   have_error;

  std::istream *   instreamp;
  unsigned int     offset;
  XML_Parser       parser;
  path		   pathname;
  unsigned int     src_idx;
  unsigned long	   beg_pos;
  unsigned long    beg_line;

  transaction_t::state_t curr_state;

  enum action_t {
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

 public:
  virtual bool test(std::istream& in) const;

  virtual unsigned int parse(std::istream&	   in,
			     journal_t *	   journal,
			     account_t *	   master   = NULL,
			     const optional<path>& original = none);

  amount_t convert_number(const string& number, int * precision = NULL);
};

} // namespace ledger

#endif // _GNUCASH_H
