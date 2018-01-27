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
 * @file   org.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _ORG_H
#define _ORG_H

#include "chain.h"
#include "format.h"

namespace ledger {

class xact_t;
class post_t;
class report_t;

class posts_to_org_table : public item_handler<post_t>
{
protected:
  typedef std::list<xact_t *>      xacts_list;
  typedef std::map<xact_t *, bool> xacts_present_map;

  report_t& report;
  format_t  first_line_format;
  format_t  next_lines_format;
  format_t  amount_lines_format;
  format_t  prepend_format;
  xact_t *  last_xact;
  post_t *  last_post;
  bool      header_printed;
  bool      first_report_title;
  string    report_title;

public:
  posts_to_org_table(report_t&               _report,
                     const optional<string>& _prepend_format = none);
  virtual ~posts_to_org_table() {
    TRACE_DTOR(posts_to_org_table);
  }

  virtual void flush();
  virtual void operator()(post_t& post);

  virtual void clear() {
    last_xact          = NULL;
    last_post          = NULL;
    header_printed     = false;
    first_report_title = true;
    report_title       = "";

    item_handler<post_t>::clear();
  }
};

} // namespace ledger

#endif // _ORG_H
