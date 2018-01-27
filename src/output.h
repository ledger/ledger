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
class item_t;
class report_t;

class format_posts : public item_handler<post_t>
{
protected:
  report_t&   report;
  format_t    first_line_format;
  format_t    next_lines_format;
  format_t    between_format;
  format_t    prepend_format;
  std::size_t prepend_width;
  xact_t *    last_xact;
  post_t *    last_post;
  bool        first_report_title;
  string      report_title;

public:
  format_posts(report_t& _report, const string& format,
               const optional<string>& _prepend_format = none,
               std::size_t _prepend_width = 0);
  virtual ~format_posts() {
    TRACE_DTOR(format_posts);
  }

  virtual void title(const string& str) {
    report_title = str;
  }

  virtual void flush();
  virtual void operator()(post_t& post);

  virtual void clear() {
    last_xact    = NULL;
    last_post    = NULL;

    report_title = "";

    item_handler<post_t>::clear();
  }
};

class format_accounts : public item_handler<account_t>
{
protected:
  report_t&   report;
  format_t    account_line_format;
  format_t    total_line_format;
  format_t    separator_format;
  format_t    prepend_format;
  std::size_t prepend_width;
  predicate_t disp_pred;
  bool        first_report_title;
  string      report_title;

  std::list<account_t *> posted_accounts;

public:
  format_accounts(report_t& _report, const string& _format,
                  const optional<string>& _prepend_format = none,
                  std::size_t _prepend_width = 0);
  virtual ~format_accounts() {
    TRACE_DTOR(format_accounts);
  }

  std::pair<std::size_t, std::size_t>
  mark_accounts(account_t& account, const bool flat);

  virtual void title(const string& str) {
    report_title = str;
  }

  virtual std::size_t post_account(account_t& account, const bool flat);
  virtual void        flush();

  virtual void operator()(account_t& account);

  virtual void clear() {
    disp_pred.mark_uncompiled();
    posted_accounts.clear();

    report_title = "";

    item_handler<account_t>::clear();
  }
};

class report_accounts : public item_handler<post_t>
{
protected:
  report_t& report;

  typedef std::map<account_t *, std::size_t>::value_type accounts_pair;
  typedef std::map<account_t *, std::size_t, account_compare> accounts_report_map;

  accounts_report_map accounts;

public:
  report_accounts(report_t& _report) : report(_report) {
    TRACE_CTOR(report_accounts, "report&");
  }
  virtual ~report_accounts() {
    TRACE_DTOR(report_accounts);
  }

  virtual void flush();
  virtual void operator()(post_t& post);

  virtual void clear() {
    accounts.clear();
    item_handler<post_t>::clear();
  }
};

class report_payees : public item_handler<post_t>
{
protected:
  report_t& report;

  std::map<string, std::size_t> payees;

  typedef std::map<string, std::size_t>::value_type payees_pair;

public:
  report_payees(report_t& _report) : report(_report) {
    TRACE_CTOR(report_payees, "report&");
  }
  virtual ~report_payees() {
    TRACE_DTOR(report_payees);
  }

  virtual void flush();
  virtual void operator()(post_t& post);

  virtual void clear() {
    payees.clear();
    item_handler<post_t>::clear();
  }
};

class report_tags : public item_handler<post_t>
{
protected:
  report_t& report;

  std::map<string, std::size_t> tags;

  typedef std::map<string, std::size_t>::value_type tags_pair;

public:
  report_tags(report_t& _report) : report(_report) {
    TRACE_CTOR(report_tags, "report&");
  }
  virtual ~report_tags() {
    TRACE_DTOR(report_tags);
  }

  virtual void flush();
  virtual void gather_metadata(item_t& item);
  virtual void operator()(post_t& post);

  virtual void clear() {
    tags.clear();
    item_handler<post_t>::clear();
  }
};


class report_commodities : public item_handler<post_t>
{
protected:
  report_t& report;

  typedef std::map<commodity_t *, std::size_t>::value_type commodities_pair;
  typedef std::map<commodity_t *, std::size_t, commodity_compare> commodities_report_map;

  commodities_report_map commodities;

public:
  report_commodities(report_t& _report) : report(_report) {
    TRACE_CTOR(report_commodities, "report&");
  }
  virtual ~report_commodities() {
    TRACE_DTOR(report_commodities);
  }

  virtual void flush();
  virtual void operator()(post_t& post);

  virtual void clear() {
    commodities.clear();
    item_handler<post_t>::clear();
  }
};

} // namespace ledger

#endif // _OUTPUT_H
