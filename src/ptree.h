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
 * @file   ptree.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _PTREE_H
#define _PTREE_H

#include "chain.h"

namespace ledger {

class xact_t;
class account_t;
class commodity_t;
class post_t;
class report_t;

/**
 * @brief Brief
 *
 * Long.
 */
class format_ptree : public item_handler<post_t>
{
protected:
  report_t& report;

  typedef std::map<string, commodity_t *>  commodities_map;
  typedef std::pair<string, commodity_t *> commodities_pair;

  commodities_map      commodities;
  std::set<xact_t *>   transactions_set;
  std::deque<xact_t *> transactions;

public:
  enum format_t {
    FORMAT_XML
  } format;

  format_ptree(report_t& _report, format_t _format = FORMAT_XML)
    : report(_report), format(_format) {
    TRACE_CTOR(format_ptree, "report&, format_t");
  }
  virtual ~format_ptree() {
    TRACE_DTOR(format_ptree);
  }

  virtual void flush();
  virtual void operator()(post_t& post);

  virtual void clear() {
    commodities.clear();
    transactions_set.clear();
    transactions.clear();

    item_handler<post_t>::clear();
  }
};

} // namespace ledger

#endif // _PTREE_H
