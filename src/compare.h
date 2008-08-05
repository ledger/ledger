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

#ifndef _COMPARE_H
#define _COMPARE_H

#include "expr.h"
#include "xact.h"
#include "account.h"

namespace ledger {

template <typename T>
class compare_items
{
  expr_t sort_order;

  compare_items();
  
public:
  compare_items(const compare_items& other) : sort_order(other.sort_order) {
    TRACE_CTOR(compare_items, "copy");
  }
  compare_items(const expr_t& _sort_order) : sort_order(_sort_order) {
    TRACE_CTOR(compare_items, "const value_expr&");
  }
  ~compare_items() throw() {
    TRACE_DTOR(compare_items);
  }
  bool operator()(T * left, T * right);
};

template <typename T>
bool compare_items<T>::operator()(T * left, T * right)
{
  assert(left);
  assert(right);
  return sort_order.calc(*left) < sort_order.calc(*right);
}

template <>
bool compare_items<xact_t>::operator()(xact_t * left, xact_t * right);
template <>
bool compare_items<account_t>::operator()(account_t * left,
					  account_t * right);

} // namespace ledger

#endif // _COMPARE_H
