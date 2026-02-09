/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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
 * @file   xdata_context.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Thread-local external xdata storage for report isolation.
 *
 * When a report is active, xdata for posts and accounts is stored
 * externally in an xdata_context_t rather than inside the post_t/account_t
 * objects themselves.  This allows report-time xdata to be automatically
 * cleaned up when the context goes out of scope, without requiring an
 * expensive walk over all journal entries via clear_xdata().
 */
#pragma once

#include "post.h"
#include "account.h"

namespace ledger {

class xdata_context_t {
  // Use unique_ptr for reference stability: pointers/references to xdata_t
  // objects remain valid even when the unordered_map rehashes.
  std::unordered_map<const post_t*,
                     std::unique_ptr<post_t::xdata_t>>    post_xdata_;
  std::unordered_map<const account_t*,
                     std::unique_ptr<account_t::xdata_t>> acct_xdata_;

  static thread_local xdata_context_t* current_;

public:
  xdata_context_t() {}
  ~xdata_context_t() {}

  // Non-copyable, non-movable
  xdata_context_t(const xdata_context_t&) = delete;
  xdata_context_t& operator=(const xdata_context_t&) = delete;

  static xdata_context_t* current() { return current_; }

  // RAII guard that sets/restores the current context pointer.
  class scope_guard {
    xdata_context_t* previous_;
  public:
    explicit scope_guard(xdata_context_t& ctx)
        : previous_(current_) {
      current_ = &ctx;
    }
    ~scope_guard() {
      current_ = previous_;
    }
    scope_guard(const scope_guard&) = delete;
    scope_guard& operator=(const scope_guard&) = delete;
  };

  // Post xdata access
  post_t::xdata_t& post_xdata(const post_t* post) {
    auto& ptr = post_xdata_[post];
    if (!ptr)
      ptr.reset(new post_t::xdata_t());
    return *ptr;
  }

  bool has_post_xdata(const post_t* post) const {
    return post_xdata_.find(post) != post_xdata_.end();
  }

  void clear_post_xdata(const post_t* post) {
    post_xdata_.erase(post);
  }

  // Account xdata access
  account_t::xdata_t& acct_xdata(const account_t* acct) {
    auto& ptr = acct_xdata_[acct];
    if (!ptr)
      ptr.reset(new account_t::xdata_t());
    return *ptr;
  }

  bool has_acct_xdata(const account_t* acct) const {
    return acct_xdata_.find(acct) != acct_xdata_.end();
  }

  void clear_acct_xdata(const account_t* acct) {
    acct_xdata_.erase(acct);
  }
};

} // namespace ledger
