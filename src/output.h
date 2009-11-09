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
 */
#ifndef _OUTPUT_H
#define _OUTPUT_H

#include "chain.h"
#include "predicate.h"
#include "format.h"
#include "account.h"

namespace ledger {

class xact_t;
class post_t;
class report_t;

class format_posts : public item_handler<post_t>
{
protected:
  report_t& report;
  format_t  first_line_format;
  format_t  next_lines_format;
  format_t  between_format;
  xact_t *  last_xact;
  post_t *  last_post;
  bool      print_raw;

public:
  format_posts(report_t& _report, const string& format,
	       bool _print_raw = false);
  virtual ~format_posts() {
    TRACE_DTOR(format_posts);
  }

  virtual void flush();
  virtual void operator()(post_t& post);
};

class format_accounts : public item_handler<account_t>
{
protected:
  report_t&   report;
  format_t    account_line_format;
  format_t    total_line_format;
  format_t    separator_format;
  predicate_t disp_pred;

  std::list<account_t *> posted_accounts;

public:
  format_accounts(report_t& _report, const string& _format);
  virtual ~format_accounts() {
    TRACE_DTOR(format_accounts);
  }

  std::pair<std::size_t, std::size_t>
  mark_accounts(account_t& account, const bool flat);

  virtual std::size_t post_account(account_t& account, const bool flat);
  virtual void	      flush();

  virtual void operator()(account_t& account);
};

} // namespace ledger

#endif // _OUTPUT_H
