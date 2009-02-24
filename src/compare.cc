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

#include "compare.h"
#include "op.h"

namespace ledger {

namespace {
  template <typename T>
  void push_sort_value(std::list<sort_value_t>& sort_values,
		       expr_t::ptr_op_t node, T * scope)
  {
    if (node->kind == expr_t::op_t::O_CONS) {
      push_sort_value(sort_values, node->left(), scope);
      push_sort_value(sort_values, node->right(), scope);
    }
    else {
      bool inverted = false;

      if (node->kind == expr_t::op_t::O_NEG) {
	inverted = true;
	node = node->left();
      }

      sort_values.push_back(sort_value_t());
      sort_values.back().inverted = inverted;
      sort_values.back().value	  = expr_t(node).calc(*scope).simplified();

      if (sort_values.back().value.is_null())
	throw_(calc_error,
	       "Could not determine sorting value based an expression");
    }
  }
}

template <typename T>
void compare_items<T>::find_sort_values(std::list<sort_value_t>& sort_values,
					T * scope)
{
  push_sort_value(sort_values, sort_order.get_op(), scope);
}

template <>
bool compare_items<post_t>::operator()(post_t * left, post_t * right)
{
  assert(left);
  assert(right);

  post_t::xdata_t& lxdata(left->xdata());
  if (! lxdata.has_flags(POST_EXT_SORT_CALC)) {
    find_sort_values(lxdata.sort_values, left);
    lxdata.add_flags(POST_EXT_SORT_CALC);
  }

  post_t::xdata_t& rxdata(right->xdata());
  if (! rxdata.has_flags(POST_EXT_SORT_CALC)) {
    find_sort_values(rxdata.sort_values, right);
    rxdata.add_flags(POST_EXT_SORT_CALC);
  }

  return sort_value_is_less_than(lxdata.sort_values, rxdata.sort_values);
}

template <>
bool compare_items<account_t>::operator()(account_t * left, account_t * right)
{
  assert(left);
  assert(right);

  account_t::xdata_t& lxdata(left->xdata());
  if (! lxdata.has_flags(ACCOUNT_EXT_SORT_CALC)) {
    find_sort_values(lxdata.sort_values, left);
    lxdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  }

  account_t::xdata_t& rxdata(right->xdata());
  if (! rxdata.has_flags(ACCOUNT_EXT_SORT_CALC)) {
    find_sort_values(rxdata.sort_values, right);
    rxdata.add_flags(ACCOUNT_EXT_SORT_CALC);
  }

  DEBUG("value.sort", "Comparing accounts " << left->fullname()
	<< " <> " << right->fullname());

  return sort_value_is_less_than(lxdata.sort_values, rxdata.sort_values);
}

} // namespace ledger
