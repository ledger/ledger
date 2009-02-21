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

/**
 * @addtogroup report
 */

/**
 * @file   output.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _OUTPUT_H
#define _OUTPUT_H

#include "report.h"
#include "format.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class format_xacts : public item_handler<xact_t>
{
protected:
  report_t& report;
  format_t  first_line_format;
  format_t  next_lines_format;
  format_t  between_format;
  entry_t * last_entry;
  xact_t *  last_xact;
  bool      print_raw;

public:
  format_xacts(report_t& _report, const string& format,
	       bool _print_raw = false);
  virtual ~format_xacts() {
    TRACE_DTOR(format_xacts);
  }

  virtual void flush() {
    report.output_stream.flush();
  }
  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class gather_statistics : public item_handler<xact_t>
{
protected:
  report_t& report;
  entry_t * last_entry;
  xact_t *  last_xact;

  struct statistics_t {
    std::set<path> filenames;

    std::size_t total_entries;
    std::size_t total_xacts;
    std::size_t total_uncleared_xacts;
    std::size_t total_last_7_days;
    std::size_t total_last_30_days;
    std::size_t total_this_month;

    date_t earliest_xact;
    date_t latest_xact;

    std::set<string> accounts_referenced;
    std::set<string> payees_referenced;

    statistics_t()
      : total_entries(0), total_xacts(0), total_uncleared_xacts(0),
	total_last_7_days(0), total_last_30_days(0), total_this_month(0) {}
  } statistics;

public:
  gather_statistics(report_t& _report)
    : report(_report), last_entry(NULL), last_xact(NULL) {
    TRACE_CTOR(gather_statistics, "report&");
  }
  virtual ~gather_statistics() {
    TRACE_DTOR(gather_statistics);
  }

  virtual void flush();
  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class format_accounts : public item_handler<account_t>
{
protected:
  report_t&	 report;
  format_t	 format;
  item_predicate disp_pred;

  std::list<account_t *> posted_accounts;

public:
  format_accounts(report_t&	_report,
		  const string& _format = "")
    : report(_report), format(_format), disp_pred()
  {
    TRACE_CTOR(format_accounts, "report&, const string&, bool");

    if (report.HANDLED(display_)) {
      DEBUG("account.display",
	    "Account display predicate: " << report.HANDLER(display_).str());
      disp_pred.predicate.parse(report.HANDLER(display_).str());
    }
  }
  virtual ~format_accounts() {
    TRACE_DTOR(format_accounts);
  }

  virtual void post_account(account_t& account);
  virtual void flush();

  virtual void operator()(account_t& account);
};

} // namespace ledger

#endif // _OUTPUT_H
