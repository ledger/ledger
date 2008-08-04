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

#include "compare.h"

namespace ledger {

template <>
bool compare_items<xact_t>::operator()(xact_t * left, xact_t * right)
{
  assert(left);
  assert(right);

  xact_t::xdata_t& lxdata(left->xdata());
  if (! lxdata.has_flags(XACT_EXT_SORT_CALC)) {
    lxdata.sort_value = sort_order.calc(*left);
    lxdata.sort_value.reduce();
    lxdata.add_flags(XACT_EXT_SORT_CALC);
  }

  xact_t::xdata_t& rxdata(right->xdata());
  if (! rxdata.has_flags(XACT_EXT_SORT_CALC)) {
    rxdata.sort_value = sort_order.calc(*right);
    rxdata.sort_value.reduce();
    rxdata.add_flags(XACT_EXT_SORT_CALC);
  }

  DEBUG("ledger.walk.compare_items_xact",
	"lxdata.sort_value = " << lxdata.sort_value);
  DEBUG("ledger.walk.compare_items_xact",
	"rxdata.sort_value = " << rxdata.sort_value);

  return lxdata.sort_value < rxdata.sort_value;
}

template <>
bool compare_items<account_t>::operator()(account_t * left, account_t * right)
{
  assert(left);
  assert(right);

  account_t::xdata_t& lxdata(left->xdata());
  if (! lxdata.has_flags(ACCOUNT_EXT_SORT_CALC)) {
    lxdata.sort_value = sort_order.calc(*left);
    lxdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  }

  account_t::xdata_t& rxdata(right->xdata());
  if (! rxdata.has_flags(ACCOUNT_EXT_SORT_CALC)) {
    rxdata.sort_value = sort_order.calc(*right);
    rxdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  }

  return lxdata.sort_value < rxdata.sort_value;
}

} // namespace ledger
