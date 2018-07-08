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
 * @addtogroup expr
 */

/**
 * @file   draft.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#ifndef _DRAFT_H
#define _DRAFT_H

#include "exprbase.h"
#include "value.h"

namespace ledger {

class journal_t;
class xact_t;

class draft_t : public expr_base_t<value_t>
{
  typedef expr_base_t<value_t> base_type;

  struct xact_template_t
  {
    optional<date_t> date;
    optional<string> code;
    optional<string> note;
    mask_t           payee_mask;

    struct post_template_t {
      bool               from;
      optional<mask_t>   account_mask;
      optional<amount_t> amount;
      optional<string>   cost_operator;
      optional<amount_t> cost;

      post_template_t() : from(false) {
        TRACE_CTOR(post_template_t, "");
      }
      ~post_template_t() throw() {
        TRACE_DTOR(post_template_t);
      }
    };

    std::list<post_template_t> posts;

    xact_template_t() {
      TRACE_CTOR(xact_template_t, "");
    }
    xact_template_t(const xact_template_t& other)
      : date(other.date),
        code(other.code),
        note(other.note),
        payee_mask(other.payee_mask),
        posts(other.posts)
    {
      TRACE_CTOR(xact_template_t, "copy");
    }
    ~xact_template_t() throw() {
      TRACE_DTOR(xact_template_t);
    }

    void dump(std::ostream& out) const;
  };

  optional<xact_template_t> tmpl;

public:
  draft_t(const value_t& args) : base_type() {
    if (! args.empty())
      parse_args(args);
    TRACE_CTOR(draft_t, "value_t");
  }
  virtual ~draft_t() throw() {
    TRACE_DTOR(draft_t);
  }

  void parse_args(const value_t& args);

  virtual result_type real_calc(scope_t&) {
    assert(false);
    return true;
  }

  xact_t * insert(journal_t& journal);

  virtual void dump(std::ostream& out) const {
    if (tmpl)
      tmpl->dump(out);
  }
};

value_t xact_command(call_scope_t& args);
value_t template_command(call_scope_t& args);

} // namespace ledger

#endif // _DRAFT_H
