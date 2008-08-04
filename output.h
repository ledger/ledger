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

#ifndef _OUTPUT_H
#define _OUTPUT_H

#include "report.h"
#include "handler.h"
#include "format.h"

namespace ledger {

class format_xacts : public item_handler<xact_t>
{
protected:
  report_t& report;
  format_t  first_line_format;
  format_t  next_lines_format;
  entry_t * last_entry;
  xact_t *  last_xact;

public:
  format_xacts(report_t& _report, const string& format);
  virtual ~format_xacts() {
    TRACE_DTOR(format_xacts);
  }

  virtual void flush() {
    report.output_stream->flush();
  }
  virtual void operator()(xact_t& xact);
};

class format_entries : public format_xacts
{
 public:
  format_entries(report_t& _report, const string& format)
    : format_xacts(_report, format) {
    TRACE_CTOR(format_entries, "report_t&, const string&");
  }
  virtual ~format_entries() {
    TRACE_DTOR(format_entries);
  }

  virtual void format_last_entry();

  virtual void flush() {
    if (last_entry) {
      format_last_entry();
      last_entry = NULL;
    }
    format_xacts::flush();
  }
  virtual void operator()(xact_t& xact);

private:
  void print_entry(std::ostream&       out,
		   const entry_base_t& entry,
		   const string&       prefix = "");
};

class format_accounts : public item_handler<account_t>
{
protected:
  report_t& report;

  item_predicate<account_t> disp_pred;

  bool disp_subaccounts_p(account_t& account, account_t *& to_show);
  bool display_account(account_t& account);

public:
  format_t format;

  format_accounts(report_t&	_report,
		  const string& _format,
		  const string& display_predicate = "" /*,
		  const bool    print_final_total = true */)
    : report(_report), disp_pred(display_predicate), format(_format) {
    TRACE_CTOR(format_accounts, "report&, const string&, const string&");
  }
  virtual ~format_accounts() {
    TRACE_DTOR(format_accounts);
  }

  virtual void flush();

  virtual void operator()(account_t& account);
};

class format_equity : public format_accounts
{
  format_t first_line_format;
  format_t next_lines_format;

  mutable value_t total;

 public:
  format_equity(report_t&     _report,
		const string& _format,
		const string& display_predicate = "");
  virtual ~format_equity() {
    TRACE_DTOR(format_equity);
  }

  virtual void flush();
  virtual void operator()(account_t& account);
};

} // namespace ledger

#endif // _OUTPUT_H
