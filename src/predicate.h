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
 * @addtogroup expr
 */

/**
 * @file   predicate.h
 * @author John Wiegley
 *
 * @ingroup expr
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _PREDICATE_H
#define _PREDICATE_H

#include "expr.h"
#include "scope.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class item_predicate
{
public:
  expr_t	 predicate;
  keep_details_t what_to_keep;

  item_predicate() {
    TRACE_CTOR(item_predicate, "");
  }
  item_predicate(const item_predicate& other)
    : predicate(other.predicate), what_to_keep(other.what_to_keep) {
    TRACE_CTOR(item_predicate, "copy");
  }
  item_predicate(const expr_t&	       _predicate,
		 const keep_details_t& _what_to_keep)
    : predicate(_predicate), what_to_keep(_what_to_keep) {
    TRACE_CTOR(item_predicate, "const expr_t&, const keep_details_t&");
  }
  item_predicate(const string&	       _predicate,
		 const keep_details_t& _what_to_keep)
    : predicate(expr_t(_predicate)), what_to_keep(_what_to_keep) {
    TRACE_CTOR(item_predicate, "const string&, const keep_details_t&");
  }
  ~item_predicate() throw() {
    TRACE_DTOR(item_predicate);
  }

  bool operator()(scope_t& item) {
    try {
      return ! predicate || predicate.calc(item).strip_annotations(what_to_keep);
    }
    catch (const std::exception& err) {
      add_error_context(_("While determining truth of predicate expression:"));
      add_error_context(expr_context(predicate));
      throw;
    }
  }
};

string args_to_predicate_expr(value_t::sequence_t::const_iterator begin,
			      value_t::sequence_t::const_iterator end);

} // namespace ledger

#endif // _PREDICATE_H
