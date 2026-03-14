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
 * @file   iterators.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Boost.Iterator-based traversal of journal entries and account trees.
 *
 * Ledger's reporting pipeline needs to walk two fundamental structures:
 *
 *   1. **Postings within transactions** -- a transaction (xact_t) owns a
 *      list of postings; reports iterate over them one by one.
 *   2. **Account hierarchies** -- the chart of accounts is a tree; balance
 *      reports traverse it depth-first, optionally sorting children.
 *
 * Every iterator in this file is a forward-only Boost iterator_facade
 * whose value type is a raw pointer (post_t* or account_t*).  Dereferencing
 * yields the pointer itself (not a reference to the pointee), which lets
 * callers write `while (post_t* p = *iter++) { ... }` -- a null pointer
 * signals exhaustion.
 *
 * The iterators compose naturally: journal_posts_iterator nests an
 * xacts_iterator and an xact_posts_iterator, advancing the outer iterator
 * whenever the inner one is exhausted.  sorted_accounts_iterator wraps a
 * basic_accounts_iterator with an expression-driven comparison functor
 * (compare_items) to implement the `--sort` option.
 */
#pragma once

#include "xact.h"
#include "post.h"
#include "account.h"
#include "temps.h"

namespace ledger {

class journal_t;
class report_t;

/**
 * @brief CRTP base class providing Boost.Iterator facade plumbing.
 *
 * All Ledger iterators derive from this template.  It stores a single
 * pointer (m_node) that doubles as both the current element and the
 * end-of-sequence sentinel (nullptr).  Subclasses implement `increment()`
 * to advance m_node.
 *
 * @tparam Derived              The concrete iterator type (CRTP).
 * @tparam Value                The pointer type yielded (e.g. post_t*).
 * @tparam CategoryOrTraversal  Boost traversal tag (always forward).
 */
template <typename Derived, typename Value, typename CategoryOrTraversal>
class iterator_facade_base : public boost::iterator_facade<Derived, Value, CategoryOrTraversal> {
  using node_base = Value;

public:
  iterator_facade_base() : m_node(nullptr) { TRACE_CTOR(iterator_facade_base, ""); }
  iterator_facade_base(const iterator_facade_base& i) : m_node(i.m_node) {
    TRACE_CTOR(iterator_facade_base, "copy");
  }
  iterator_facade_base& operator=(const iterator_facade_base&) = default;
  ~iterator_facade_base() noexcept { TRACE_DTOR(iterator_facade_base); }

  explicit iterator_facade_base(node_base p) : m_node(p) {}

  /// Advance to the next element; implemented by each concrete iterator.
  void increment();

private:
  friend class boost::iterator_core_access;

  bool equal(iterator_facade_base const& other) const { return this->m_node == other.m_node; }

  node_base& dereference() const { return const_cast<node_base&>(m_node); }

protected:
  node_base m_node; ///< Current element pointer; nullptr means past-the-end.
};

/**
 * @brief Iterates over the postings (line items) within a single transaction.
 *
 * Given one xact_t, this iterator yields each post_t* in order.  It is
 * the innermost building block used by journal_posts_iterator and
 * posts_commodities_iterator.
 */
class xact_posts_iterator
    : public iterator_facade_base<xact_posts_iterator, post_t*, boost::forward_traversal_tag> {
  posts_list::iterator posts_i;   ///< Current position in the posting list.
  posts_list::iterator posts_end; ///< Past-the-end sentinel for the posting list.

  bool posts_uninitialized; ///< True until reset() has been called.

public:
  xact_posts_iterator() : posts_uninitialized(true) { TRACE_CTOR(xact_posts_iterator, ""); }
  xact_posts_iterator(xact_t& xact) : posts_uninitialized(true) {
    reset(xact);
    TRACE_CTOR(xact_posts_iterator, "xact_t&");
  }
  xact_posts_iterator(const xact_posts_iterator& i)
      : iterator_facade_base<xact_posts_iterator, post_t*, boost::forward_traversal_tag>(i),
        posts_i(i.posts_i), posts_end(i.posts_end), posts_uninitialized(i.posts_uninitialized) {
    TRACE_CTOR(xact_posts_iterator, "copy");
  }
  xact_posts_iterator& operator=(const xact_posts_iterator&) = default;
  ~xact_posts_iterator() noexcept { TRACE_DTOR(xact_posts_iterator); }

  /// Point the iterator at a new transaction and advance to its first posting.
  void reset(xact_t& xact) {
    posts_i = xact.posts.begin();
    posts_end = xact.posts.end();

    posts_uninitialized = false;

    increment();
  }

  void increment() {
    if (posts_uninitialized || posts_i == posts_end)
      m_node = nullptr;
    else
      m_node = *posts_i++;
  }
};

/**
 * @brief Iterates over the transactions in a journal, in parse order.
 *
 * Walks the flat list of xact_t pointers stored in journal_t::xacts.
 * Used standalone or as the outer loop inside journal_posts_iterator.
 */
class xacts_iterator
    : public iterator_facade_base<xacts_iterator, xact_t*, boost::forward_traversal_tag> {
public:
  xacts_list::iterator xacts_i;   ///< Current position in the transaction list.
  xacts_list::iterator xacts_end; ///< Past-the-end sentinel.

  bool xacts_uninitialized; ///< True until reset() has been called.

  xacts_iterator() : xacts_uninitialized(true) { TRACE_CTOR(xacts_iterator, ""); }
  xacts_iterator(journal_t& journal) : xacts_uninitialized(false) {
    reset(journal);
    TRACE_CTOR(xacts_iterator, "journal_t&");
  }
  xacts_iterator(xacts_list::iterator beg, xacts_list::iterator end) : xacts_uninitialized(false) {
    reset(beg, end);
    TRACE_CTOR(xacts_iterator, "xacts_list::iterator, xacts_list::iterator");
  }
  xacts_iterator(const xacts_iterator& i)
      : iterator_facade_base<xacts_iterator, xact_t*, boost::forward_traversal_tag>(i),
        xacts_i(i.xacts_i), xacts_end(i.xacts_end), xacts_uninitialized(i.xacts_uninitialized) {
    TRACE_CTOR(xacts_iterator, "copy");
  }
  xacts_iterator& operator=(const xacts_iterator&) = default;
  ~xacts_iterator() noexcept { TRACE_DTOR(xacts_iterator); }

  /// Point the iterator at all transactions in a journal.
  void reset(journal_t& journal);

  /// Point the iterator at an explicit sub-range of a transaction list.
  void reset(xacts_list::iterator beg, xacts_list::iterator end) {
    xacts_i = beg;
    xacts_end = end;
    increment();
  }

  void increment();
};

/**
 * @brief Iterates over every posting in a journal, across all transactions.
 *
 * Composes xacts_iterator (outer) with xact_posts_iterator (inner).
 * When the inner iterator exhausts one transaction's postings, the outer
 * iterator advances to the next transaction and a fresh inner iterator
 * is created.  This is the primary iterator used by most report commands
 * (register, balance, etc.) to feed postings into the filter chain.
 */
class journal_posts_iterator
    : public iterator_facade_base<journal_posts_iterator, post_t*, boost::forward_traversal_tag> {
  xacts_iterator xacts;      ///< Outer: walks transactions in the journal.
  xact_posts_iterator posts; ///< Inner: walks postings in the current transaction.

public:
  journal_posts_iterator() { TRACE_CTOR(journal_posts_iterator, ""); }
  journal_posts_iterator(journal_t& journal) {
    reset(journal);
    TRACE_CTOR(journal_posts_iterator, "journal_t&");
  }
  journal_posts_iterator(const journal_posts_iterator& i)
      : iterator_facade_base<journal_posts_iterator, post_t*, boost::forward_traversal_tag>(i),
        xacts(i.xacts), posts(i.posts) {
    TRACE_CTOR(journal_posts_iterator, "copy");
  }
  journal_posts_iterator& operator=(const journal_posts_iterator&) = default;
  ~journal_posts_iterator() noexcept { TRACE_DTOR(journal_posts_iterator); }

  void reset(journal_t& journal);

  void increment();
};

/**
 * @brief Synthesizes postings from commodity price history.
 *
 * This iterator supports the `commodities` report.  During reset() it
 * scans every posting in the journal to collect referenced commodities,
 * then queries each commodity's price history via commodity_t::map_prices.
 * For each historical price point it creates a temporary transaction with
 * a posting whose amount is the price.  The resulting synthetic postings
 * can then flow through the normal reporting pipeline just like real ones.
 *
 * Temporary transactions are owned by the xact_temps list and the
 * temporaries_t allocator, both of which live for the lifetime of this
 * iterator.
 */
class posts_commodities_iterator : public iterator_facade_base<posts_commodities_iterator, post_t*,
                                                               boost::forward_traversal_tag> {
protected:
  journal_posts_iterator journal_posts; ///< Used during reset() to scan all real postings.
  xacts_iterator xacts;                 ///< Outer: walks synthetic transactions.
  xact_posts_iterator posts;            ///< Inner: walks postings in the current synthetic xact.
  xacts_list xact_temps;                ///< Owns the synthetic transaction objects.
  temporaries_t temps;                  ///< Arena allocator for temporary xacts and posts.
  bool latest_only;                     ///< If true, emit only the most recent price per commodity.

public:
  posts_commodities_iterator() : latest_only(false) { TRACE_CTOR(posts_commodities_iterator, ""); }
  posts_commodities_iterator(journal_t& journal, bool _latest_only = false)
      : latest_only(_latest_only) {
    reset(journal);
    TRACE_CTOR(posts_commodities_iterator, "journal_t&");
  }
  posts_commodities_iterator(const posts_commodities_iterator& i)
      : iterator_facade_base<posts_commodities_iterator, post_t*, boost::forward_traversal_tag>(i),
        journal_posts(i.journal_posts), xacts(i.xacts), posts(i.posts), xact_temps(i.xact_temps),
        temps(i.temps), latest_only(i.latest_only) {
    TRACE_CTOR(posts_commodities_iterator, "copy");
  }
  posts_commodities_iterator& operator=(const posts_commodities_iterator&) = default;
  ~posts_commodities_iterator() noexcept { TRACE_DTOR(posts_commodities_iterator); }

  void reset(journal_t& journal);

  void increment();
};

/**
 * @brief Depth-first traversal of the account hierarchy, in map order.
 *
 * Walks the tree of accounts rooted at a given account_t.  Children are
 * visited in the order they appear in the accounts_map (lexicographic by
 * account name).  The traversal uses an explicit stack of iterator pairs
 * (accounts_i / accounts_end) rather than recursion, so that it can be
 * driven incrementally from the Boost iterator facade.
 *
 * This iterator does NOT sort children; for sorted output, use
 * sorted_accounts_iterator instead.
 */
class basic_accounts_iterator : public iterator_facade_base<basic_accounts_iterator, account_t*,
                                                            boost::forward_traversal_tag> {
  std::list<accounts_map::const_iterator> accounts_i;   ///< Stack of current-child iterators.
  std::list<accounts_map::const_iterator> accounts_end; ///< Stack of end-of-children sentinels.

public:
  basic_accounts_iterator() { TRACE_CTOR(basic_accounts_iterator, ""); }
  basic_accounts_iterator(account_t& account) {
    push_back(account);
    increment();
    TRACE_CTOR(basic_accounts_iterator, "account_t&");
  }
  basic_accounts_iterator(const basic_accounts_iterator& i)
      : iterator_facade_base<basic_accounts_iterator, account_t*, boost::forward_traversal_tag>(i),
        accounts_i(i.accounts_i), accounts_end(i.accounts_end) {
    TRACE_CTOR(basic_accounts_iterator, "copy");
  }
  basic_accounts_iterator& operator=(const basic_accounts_iterator&) = default;
  ~basic_accounts_iterator() noexcept { TRACE_DTOR(basic_accounts_iterator); }

  void increment();

private:
  /// Push a new level onto the traversal stack for the children of @p account.
  void push_back(account_t& account) {
    accounts_i.push_back(account.accounts.begin());
    accounts_end.push_back(account.accounts.end());
  }
};

/**
 * @brief Depth-first traversal of accounts with expression-based sorting.
 *
 * Implements the `--sort EXPR` option for account-oriented reports (e.g.
 * `ledger balance --sort total`).  At each level of the account tree the
 * children are collected into a deque, sorted with compare_items using the
 * user-supplied sort expression, and then iterated in that sorted order.
 *
 * When flatten_all is true, the entire sub-tree is flattened into one
 * sorted list -- this supports the `--flat` display mode.
 *
 * Like basic_accounts_iterator, the traversal state is kept on an
 * explicit stack so that it can be driven incrementally.
 */
class sorted_accounts_iterator : public iterator_facade_base<sorted_accounts_iterator, account_t*,
                                                             boost::forward_traversal_tag> {
  expr_t sort_cmp;  ///< The user-supplied sort expression (e.g. "total", "-date").
  report_t& report; ///< Report context needed to evaluate the sort expression.
  bool flatten_all; ///< If true, flatten the entire tree before sorting (--flat).

  using accounts_deque_t = std::deque<account_t*>;

  std::list<accounts_deque_t> accounts_list; ///< Sorted children at each tree level.
  std::list<accounts_deque_t::const_iterator>
      sorted_accounts_i; ///< Stack of current-position iterators.
  std::list<accounts_deque_t::const_iterator> sorted_accounts_end; ///< Stack of end sentinels.

public:
  sorted_accounts_iterator(account_t& account, const expr_t& _sort_cmp, report_t& _report,
                           bool _flatten_all)
      : sort_cmp(_sort_cmp), report(_report), flatten_all(_flatten_all) {
    push_back(account);
    increment();
    TRACE_CTOR(sorted_accounts_iterator, "account_t&, expr_t, report_t&, bool");
  }
  sorted_accounts_iterator(const sorted_accounts_iterator& i)
      : iterator_facade_base<sorted_accounts_iterator, account_t*, boost::forward_traversal_tag>(i),
        sort_cmp(i.sort_cmp), report(i.report), flatten_all(i.flatten_all),
        accounts_list(i.accounts_list), sorted_accounts_i(i.sorted_accounts_i),
        sorted_accounts_end(i.sorted_accounts_end) {
    TRACE_CTOR(sorted_accounts_iterator, "copy");
  }
  sorted_accounts_iterator& operator=(const sorted_accounts_iterator&) = delete;
  ~sorted_accounts_iterator() noexcept { TRACE_DTOR(sorted_accounts_iterator); }

  void increment();

private:
  /// Collect, sort, and push the children of @p account onto the stack.
  void push_back(account_t& account);
  /// Recursively collect all descendants of @p account into @p deque (flat mode).
  void push_all(account_t& account, accounts_deque_t& deque);
  /// Collect immediate children of @p account into @p deque and sort them.
  void sort_accounts(account_t& account, accounts_deque_t& deque);
};

} // namespace ledger
