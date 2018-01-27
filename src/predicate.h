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
 * @file   predicate.h
 * @author John Wiegley
 *
 * @ingroup expr
 */
#ifndef _PREDICATE_H
#define _PREDICATE_H

#include "expr.h"
#include "commodity.h"
#include "annotate.h"

namespace ledger {

class predicate_t : public expr_t
{
public:
  keep_details_t what_to_keep;

  predicate_t(const keep_details_t& _what_to_keep = keep_details_t())
    : what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "");
  }
  predicate_t(const predicate_t& other)
    : expr_t(other), what_to_keep(other.what_to_keep) {
    TRACE_CTOR(predicate_t, "copy");
  }
  predicate_t(ptr_op_t              _ptr,
              const keep_details_t& _what_to_keep,
              scope_t *             _context = NULL)
    : expr_t(_ptr, _context), what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "ptr_op_t, keep_details_t, scope_t *");
  }
  predicate_t(const string&         str,
              const keep_details_t& _what_to_keep,
              const parse_flags_t&  flags = PARSE_DEFAULT)
    : expr_t(str, flags), what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "string, keep_details_t, parse_flags_t");
  }
  predicate_t(std::istream&         in,
              const keep_details_t& _what_to_keep,
              const parse_flags_t&  flags = PARSE_DEFAULT)
    : expr_t(in, flags), what_to_keep(_what_to_keep) {
    TRACE_CTOR(predicate_t, "std::istream&, keep_details_t, parse_flags_t");
  }
  virtual ~predicate_t() {
    TRACE_DTOR(predicate_t);
  }

  virtual value_t real_calc(scope_t& scope) {
    return (*this ?
            expr_t::real_calc(scope)
              .strip_annotations(what_to_keep)
              .to_boolean() :
            true);
  }
};

} // namespace ledger

#endif // _PREDICATE_H
