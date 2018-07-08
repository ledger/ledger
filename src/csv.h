/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
 * @addtogroup data
 */

/**
 * @file   csv.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _CSV_H
#define _CSV_H

#include "value.h"
#include "context.h"

namespace ledger {

class xact_t;
class journal_t;
class account_t;

class csv_reader
{
  parse_context_t context;

  enum headers_t {
    FIELD_DATE = 0,
    FIELD_DATE_AUX,
    FIELD_CODE,
    FIELD_PAYEE,
    FIELD_AMOUNT,
    FIELD_COST,
    FIELD_TOTAL,
    FIELD_NOTE,

    FIELD_UNKNOWN
  };

  mask_t date_mask;
  mask_t date_aux_mask;
  mask_t code_mask;
  mask_t payee_mask;
  mask_t amount_mask;
  mask_t cost_mask;
  mask_t total_mask;
  mask_t note_mask;

  std::vector<int>    index;
  std::vector<string> names;

public:
  csv_reader(parse_context_t& _context)
    : context(_context),
      date_mask("date"),
      date_aux_mask("posted( ?date)?"),
      code_mask("code"),
      payee_mask("(payee|desc(ription)?|title)"),
      amount_mask("amount"),
      cost_mask("cost"),
      total_mask("total"),
      note_mask("note") {
    read_index(*context.stream.get());
    TRACE_CTOR(csv_reader, "parse_context_t&");
  }
  ~csv_reader() {
    TRACE_DTOR(csv_reader);
  }

  void   read_index(std::istream& in);
  string read_field(std::istream& in);
  char * next_line(std::istream& in);

  xact_t * read_xact(bool rich_data);

  const char * get_last_line() const {
    return context.linebuf;
  }
  path get_pathname() const {
    return context.pathname;
  }
  std::size_t get_linenum() const {
    return context.linenum;
  }
};

} // namespace ledger

#endif // _CSV_H
