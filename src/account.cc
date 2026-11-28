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
 * @file   account.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Implementation of the hierarchical account tree
 *
 * This file implements account tree navigation (find_account,
 * find_account_re), posting management, name formatting (fullname,
 * partial_name), the expression-binding dispatch table, incremental
 * balance accumulation (amount, total), statistical gathering
 * (self_details, family_details), and cache management (clear_xdata,
 * clear_display_state).
 */
#include <system.hh>

#include "account.h"
#include "post.h"
#include "xact.h"
#include "annotate.h"

namespace ledger {

/*--- Account Lifecycle ---*/

account_t::~account_t() {
  TRACE_DTOR(account_t);

  for (accounts_map::value_type& pair : accounts) {
    if (!pair.second->has_flags(ACCOUNT_TEMP) || has_flags(ACCOUNT_TEMP)) {
      checked_delete(pair.second);
    }
  }
}

/*--- Account Tree Navigation ---*/

/**
 * @brief Split a colon-separated name and walk/create the account tree.
 *
 * Given a name like "Assets:Bank:Checking", this function splits on the
 * first ':' to get "Assets" and "Bank:Checking", finds or creates the
 * "Assets" child, then recurses with the remainder.  Newly created
 * accounts inherit ACCOUNT_TEMP and ACCOUNT_GENERATED flags from their
 * parent so that the entire subtree shares the same temporary/generated
 * status.
 */
account_t* account_t::find_account(string_view acct_name, const bool auto_create) {
  accounts_map::const_iterator i = accounts.find(acct_name);
  if (i != accounts.end())
    return (*i).second;

  string_view::size_type sep = acct_name.find(':');
  assert(sep < 256 || sep == string_view::npos);

  string_view first;
  string_view rest;
  if (sep == string_view::npos) {
    first = acct_name;
  } else {
    first = acct_name.substr(0, sep);
    rest = acct_name.substr(sep + 1);
  }

  if (first.empty()) {
    throw parse_error(_("Account name contains an empty sub-account name"));
  }

  account_t* account;

  i = accounts.find(first);
  if (i == accounts.end()) {
    if (!auto_create)
      return nullptr;

    account = new account_t(this, string(first));

    // An account created within a temporary or generated account is itself
    // temporary or generated, so that the whole tree has the same status.
    if (has_flags(ACCOUNT_TEMP))
      account->add_flags(ACCOUNT_TEMP);
    if (has_flags(ACCOUNT_GENERATED))
      account->add_flags(ACCOUNT_GENERATED);

#if DEBUG_ON
    std::pair<accounts_map::iterator, bool> result =
#endif
        accounts.insert(accounts_map::value_type(string(first), account));
#if DEBUG_ON
    assert(result.second);
#endif
  } else {
    account = (*i).second;
  }

  if (!rest.empty())
    account = account->find_account(rest, auto_create);

  return account;
}

namespace {
/** @brief Depth-first search helper for find_account_re. */
account_t* find_account_re_(account_t* account, const mask_t& regexp) {
  if (regexp.match(account->fullname()))
    return account;

  for (accounts_map::value_type& pair : account->accounts)
    if (account_t* a = find_account_re_(pair.second, regexp))
      return a;

  return nullptr;
}
} // namespace

account_t* account_t::find_account_re(const string& regexp) {
  return find_account_re_(this, mask_t(regexp));
}

namespace {
/** @brief Depth-first search helper that collects all matching accounts. */
void find_accounts_re_(account_t* account, const mask_t& regexp, std::vector<account_t*>& results) {
  if (regexp.match(account->fullname()))
    results.push_back(account);

  for (accounts_map::value_type& pair : account->accounts)
    find_accounts_re_(pair.second, regexp, results);
}
} // namespace

std::vector<account_t*> account_t::find_accounts_re(const string& regexp) {
  std::vector<account_t*> results;
  find_accounts_re_(this, mask_t(regexp), results);
  return results;
}

/*--- Post Management ---*/

/**
 * @brief Invalidate cached totals for this account and all ancestors.
 *
 * When a posting is added or removed, any previously computed totals
 * (self_details and family_details) become stale.  This function marks
 * the account's own details as uncalculated and then walks up to the
 * root, clearing each ancestor's family_details so that total() will
 * recompute on the next call.
 */
void invalidate_xdata_cache(account_t* account) {
  if (account->has_xdata()) {
    account_t::xdata_t& xdata_ = account->xdata();
    xdata_.self_details.gathered = false;
    xdata_.self_details.calculated = false;
    xdata_.family_details.gathered = false;
    xdata_.family_details.calculated = false;
    if (!xdata_.family_details.total.is_null()) {
      xdata_.family_details.total = ledger::value_t();
    }
    account_t* ancestor = account;
    while (ancestor->parent) {
      ancestor = ancestor->parent;
      if (ancestor->has_xdata()) {
        account_t::xdata_t& xdata = ancestor->xdata();
        xdata.family_details.gathered = false;
        xdata.family_details.calculated = false;
        xdata.family_details.total = ledger::value_t();
      }
    }
  }
}

void account_t::add_post(post_t* post) {
  posts.push_back(post);

  // Adding a new post changes the possible totals that may have been
  // computed before.
  invalidate_xdata_cache(this);
}

void account_t::add_deferred_post(const string& uuid, post_t* post) {
  if (!deferred_posts)
    deferred_posts = deferred_posts_map_t();

  deferred_posts_map_t::iterator i = deferred_posts->find(uuid);
  if (i == deferred_posts->end()) {
    posts_list lst;
    lst.push_back(post);
    deferred_posts->insert(deferred_posts_map_t::value_type(uuid, lst));
  } else {
    (*i).second.push_back(post);
  }
}

void account_t::apply_deferred_posts() {
  if (deferred_posts) {
    for (deferred_posts_map_t::value_type& pair : *deferred_posts) {
      for (post_t* post : pair.second)
        post->account->add_post(post);
    }
    deferred_posts = std::nullopt;
  }

  // Also apply in child accounts
  for (const accounts_map::value_type& pair : accounts)
    pair.second->apply_deferred_posts();
}

bool account_t::remove_post(post_t* post) {
  running_total_.reset();

  // It's possible that 'post' wasn't yet in this account, but try to
  // remove it anyway.  This can happen if there is an error during
  // parsing, when the posting knows what its account is, but
  // xact_t::finalize has not yet added that posting to the account.
  auto found = std::find(posts.rbegin(), posts.rend(), post);
  if (found != posts.rend())
    posts.erase(std::next(found).base());
  post->account = nullptr;

  if (deferred_posts) {
    for (auto& pair : *deferred_posts) {
      pair.second.remove(post);
    }
  }

  // Invalidate cached iterators
  if (xdata_) {
    xdata_->self_details.last_post = std::nullopt;
    xdata_->self_details.last_reported_post = std::nullopt;
    invalidate_xdata_cache(this);
  }

  lot_inventory.reset();
  return true;
}

void account_t::remove_posts(const std::set<post_t*>& doomed) {
  running_total_.reset();

  auto is_doomed = [&doomed](post_t* post) { return doomed.count(post) != 0; };

  posts.remove_if(is_doomed);
  for (post_t* post : doomed)
    post->account = nullptr;

  if (deferred_posts) {
    for (auto& pair : *deferred_posts) {
      pair.second.remove_if(is_doomed);
    }
  }

  // Invalidate cached iterators
  if (xdata_) {
    xdata_->self_details.last_post = std::nullopt;
    xdata_->self_details.last_reported_post = std::nullopt;
    invalidate_xdata_cache(this);
  }
  lot_inventory.reset();
}

static void record_lot(account_t::lot_inventory_t& inventory, const post_t& post) {
  const amount_t& amt(post.amount);
  if (!amt.has_commodity() || !amt.has_annotation())
    return;
  commodity_t* lot = &amt.commodity();
  const std::size_t next = inventory.seen.size();
  auto& seen =
      inventory.seen.try_emplace(lot, account_t::lot_inventory_t::seen_t{std::nullopt, next})
          .first->second;
  if (post.has_date() || (post.xact && post.xact->has_date())) {
    const date_t when = post.primary_date();
    if (!seen.first_date || when < *seen.first_date)
      seen.first_date = when;
  }
  auto& held = inventory.open[&lot->referent()];
  auto [it, fresh] = held.try_emplace(lot, amt);
  if (!fresh)
    it->second += amt;
  if (it->second.is_realzero())
    held.erase(it);
}

std::vector<account_lot_t> account_t::held_lots(const commodity_t& referent,
                                                const std::vector<const post_t*>& pending) {
  if (!lot_inventory)
    lot_inventory.emplace();
  lot_inventory_t& inventory(*lot_inventory);
  // add_post() only appends, and remove_post()/remove_posts() drop the
  // inventory, so the posts not yet folded in are the last ones.
  assert(inventory.counted <= posts.size());
  auto unseen = posts.end();
  std::advance(unseen, -static_cast<std::ptrdiff_t>(posts.size() - inventory.counted));
  for (; unseen != posts.end(); ++unseen)
    record_lot(inventory, **unseen);
  inventory.counted = posts.size();

  std::unordered_map<commodity_t*, amount_t> holdings;
  if (auto it = inventory.open.find(&referent); it != inventory.open.end())
    holdings = it->second;
  std::unordered_map<commodity_t*, std::size_t> pending_sequence;
  for (std::size_t i = 0; i < pending.size(); ++i) {
    const amount_t& amt(pending[i]->amount);
    if (!amt.has_commodity() || !amt.has_annotation() || &amt.commodity().referent() != &referent)
      continue;
    auto [it, fresh] = holdings.try_emplace(&amt.commodity(), amt);
    if (!fresh)
      it->second += amt;
    pending_sequence.try_emplace(&amt.commodity(), inventory.seen.size() + i);
  }
  std::vector<account_lot_t> lots;
  for (auto& [lot, quantity] : holdings) {
    if (quantity.sign() <= 0)
      continue;
    auto seen = inventory.seen.find(lot);
    std::optional<date_t> date = as_annotated_commodity(*lot).details.date;
    if (!date && seen != inventory.seen.end())
      date = seen->second.first_date;
    if (!date)
      continue;
    lots.push_back(
        {lot, quantity.strip_annotations(keep_details_t()), *date,
         seen != inventory.seen.end() ? seen->second.sequence : pending_sequence.at(lot)});
  }
  std::sort(lots.begin(), lots.end(), [](const account_lot_t& a, const account_lot_t& b) {
    return a.date != b.date ? a.date < b.date : a.sequence < b.sequence;
  });
  return lots;
}

/*--- Name Formatting ---*/

string account_t::fullname() const {
  if (!_fullname.empty()) {
    return _fullname;
  } else {
    const account_t* first = this;
    string fullname = name;

    while (first->parent) {
      first = first->parent;
      if (!first->name.empty())
        fullname = // NOLINT(performance-inefficient-string-concatenation)
            first->name + ":" + fullname;
    }

    _fullname = fullname;

    return fullname;
  }
}

namespace {
/// An ancestor stays visible in an indented account report when it is
/// displayed itself or when more than one child leads to a displayed
/// account; otherwise its name is folded into its one displayed
/// descendant's.
bool is_display_branch(const account_t& acct) {
  return acct.has_xflags(ACCOUNT_EXT_TO_DISPLAY) ||
         acct.children_with_flags(ACCOUNT_EXT_TO_DISPLAY, 2) > 1;
}
} // namespace

string account_t::partial_name(bool flat) const {
  string pname = name;

  for (const account_t* acct = parent; acct && acct->parent; acct = acct->parent) {
    if (!flat) {
      assert(acct->children_with_flags(ACCOUNT_EXT_TO_DISPLAY, 1) > 0);
      if (is_display_branch(*acct))
        break;
    }
    pname = acct->name + ":" + pname; // NOLINT(performance-inefficient-string-concatenation)
  }
  return pname;
}

std::ostream& operator<<(std::ostream& out, const account_t& account) {
  out << account.fullname();
  return out;
}

/*--- Metadata Operations (parent-walking inheritance) ---*/

bool account_t::has_tag(const string& tag, bool inherit) const {
  if (metadata_t::has_tag(tag))
    return true;
  if (inherit && parent && parent->parent)
    return parent->has_tag(tag, inherit);
  return false;
}

bool account_t::has_tag(const mask_t& tag_mask, const std::optional<mask_t>& value_mask,
                        bool inherit) const {
  if (metadata_t::has_tag(tag_mask, value_mask))
    return true;
  if (inherit && parent && parent->parent)
    return parent->has_tag(tag_mask, value_mask, inherit);
  return false;
}

std::optional<value_t> account_t::get_tag(const string& tag, bool inherit) const {
  if (std::optional<value_t> v = metadata_t::get_tag(tag))
    return v;
  if (inherit && parent && parent->parent)
    return parent->get_tag(tag, inherit);
  return std::nullopt;
}

std::optional<value_t> account_t::get_tag(const mask_t& tag_mask,
                                          const std::optional<mask_t>& value_mask,
                                          bool inherit) const {
  if (std::optional<value_t> v = metadata_t::get_tag(tag_mask, value_mask))
    return v;
  if (inherit && parent && parent->parent)
    return parent->get_tag(tag_mask, value_mask, inherit);
  return std::nullopt;
}

/*--- Expression Bindings ---*/

namespace {
value_t get_partial_name(call_scope_t& args) {
  return string_value(
      args.context<account_t>().partial_name(args.has<bool>(0) && args.get<bool>(0)));
}

value_t get_account(call_scope_t& args) { // this gets the name
  account_t& account(args.context<account_t>());
  if (args.has<string>(0)) {
    account_t* acct = account.parent;
    for (; acct && acct->parent; acct = acct->parent)
      ;
    if (args[0].is_string())
      return scope_value(acct->find_account(args.get<string>(0), false));
    else if (args[0].is_mask())
      return scope_value(acct->find_account_re(args.get<mask_t>(0).str()));
    else
      return NULL_VALUE;
  } else if (args.type_context() == value_t::SCOPE) {
    return scope_value(&account);
  } else {
    return string_value(account.fullname());
  }
}

value_t get_account_base(account_t& account) {
  return string_value(account.name);
}

value_t get_total(account_t& account) {
  return SIMPLIFIED_VALUE_OR_ZERO(account.total());
}

value_t get_amount(account_t& account) {
  // While compare_items<account_t> evaluates a sort key for an account whose
  // row shows its family total, 'amount' -- and so a, t and display_amount,
  // which reach it through amount_expr -- is that total.
  if (account.has_xdata() && account.xdata().sort_amount_as_total)
    return get_total(account);
  return SIMPLIFIED_VALUE_OR_ZERO(account.amount());
}

value_t get_subcount(account_t& account) {
  std::size_t count = 0;
  for (const post_t* post : account.posts) {
    if (post->has_xdata() && post->xdata().has_flags(POST_EXT_VISITED))
      count++;
  }
  return long(count);
}

value_t get_count(account_t& account) {
  long count = get_subcount(account).to_long();
  for (const accounts_map::value_type& pair : account.accounts)
    count += get_count(*pair.second).to_long();
  return count;
}
value_t get_cost(account_t&) {
  throw_(calc_error, _("An account does not have a 'cost' value"));
  return false;
}

value_t get_depth(account_t& account) {
  return long(account.depth);
}

value_t get_note(account_t& account) {
  return account.note ? string_value(*account.note) : NULL_VALUE;
}

value_t account_has_tag(call_scope_t& args) {
  return metadata_has_tag(args.context<account_t>(), args);
}

value_t account_get_tag(call_scope_t& args) {
  return metadata_get_tag(args.context<account_t>(), args);
}

value_t ignore(account_t&) {
  return false;
}

value_t get_true(account_t&) {
  return true;
}

value_t get_addr(account_t& account) {
  return boost::numeric_cast<long>(reinterpret_cast<uintptr_t>(&account));
}

value_t get_depth_parent(account_t& account) {
  std::size_t depth = 0;
  for (const account_t* acct = account.parent; acct && acct->parent; acct = acct->parent) {
    assert(acct->children_with_flags(ACCOUNT_EXT_TO_DISPLAY, 1) > 0);
    if (is_display_branch(*acct))
      depth++;
  }
  return long(depth);
}

value_t get_depth_spacer(account_t& account) {
  return string_value(
      string(2 * static_cast<std::size_t>(get_depth_parent(account).to_long()), ' '));
}

value_t get_has_children_to_display(account_t& account) {
  return account.children_with_flags(ACCOUNT_EXT_TO_DISPLAY, 1) > 0;
}

value_t get_latest_cleared(account_t& account) {
  return account.self_details().latest_cleared_post;
}

value_t get_earliest(account_t& account) {
  return account.self_details().earliest_post;
}
value_t get_earliest_checkin(account_t& account) {
  return (!account.self_details().earliest_checkin.is_not_a_date_time()
              ? value_t(account.self_details().earliest_checkin)
              : NULL_VALUE);
}

value_t get_latest(account_t& account) {
  return account.self_details().latest_post;
}
value_t get_account_date(account_t& account) {
  date_t date = account.family_details().latest_post;
  return is_valid(date) ? value_t(date) : NULL_VALUE;
}
value_t get_latest_checkout(account_t& account) {
  return (!account.self_details().latest_checkout.is_not_a_date_time()
              ? value_t(account.self_details().latest_checkout)
              : NULL_VALUE);
}
value_t get_latest_checkout_cleared(account_t& account) {
  return account.self_details().latest_checkout_cleared;
}

template <value_t (*Func)(account_t&)>
value_t get_wrapper(call_scope_t& args) {
  return (*Func)(args.context<account_t>());
}

value_t get_parent_account(account_t& account) {
  return scope_value(account.parent);
}

value_t get_display_parent_account(account_t& account) {
  // Returns the nearest non-elided ancestor for percentage calculations.
  // An ancestor is "elided" when it has exactly 1 child to display AND
  // does not itself have ACCOUNT_EXT_TO_DISPLAY set.
  // If all ancestors are elided (e.g., account is at display depth 0),
  // returns the direct structural parent.
  for (account_t* acct = account.parent; acct && acct->parent; acct = acct->parent) {
    if (is_display_branch(*acct))
      return scope_value(acct);
  }
  // All ancestors were elided; fall back to direct parent
  return scope_value(account.parent);
}

value_t fn_any(call_scope_t& args) {
  account_t& account(args.context<account_t>());
  expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

  for (post_t* p : account.posts) {
    bind_scope_t bound_scope(args, *p);
    if (expr->calc(bound_scope, args.locus, args.depth).to_boolean())
      return true;
  }
  return false;
}

value_t fn_all(call_scope_t& args) {
  account_t& account(args.context<account_t>());
  expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

  for (post_t* p : account.posts) {
    bind_scope_t bound_scope(args, *p);
    if (!expr->calc(bound_scope, args.locus, args.depth).to_boolean())
      return false;
  }
  return true;
}

/// The functions account_t binds by name.
constexpr symbol_entry_t<value_t (*)(call_scope_t&)> account_functions[] = {
    {"N", get_wrapper<&get_count>},
    {"O", get_wrapper<&get_total>},
    {"a", get_wrapper<&get_amount>},
    {"account", &get_account},
    {"account_base", get_wrapper<&get_account_base>},
    {"addr", get_wrapper<&get_addr>},
    {"all", &fn_all},
    {"amount", get_wrapper<&get_amount>},
    {"any", &fn_any},
    {"cost", get_wrapper<&get_cost>},
    {"count", get_wrapper<&get_count>},
    {"d", get_wrapper<&get_account_date>},
    {"date", get_wrapper<&get_account_date>},
    {"depth", get_wrapper<&get_depth>},
    {"depth_parent", get_wrapper<&get_depth_parent>},
    {"depth_spacer", get_wrapper<&get_depth_spacer>},
    {"display_parent", get_wrapper<&get_display_parent_account>},
    {"earliest", get_wrapper<&get_earliest>},
    {"earliest_checkin", get_wrapper<&get_earliest_checkin>},
    {"has_children_to_display", get_wrapper<&get_has_children_to_display>},
    {"has_meta", account_has_tag},
    {"has_tag", account_has_tag},
    {"is_account", get_wrapper<&get_true>},
    {"is_index", get_wrapper<&get_subcount>},
    {"l", get_wrapper<&get_depth>},
    {"latest", get_wrapper<&get_latest>},
    {"latest_checkout", get_wrapper<&get_latest_checkout>},
    {"latest_checkout_cleared", get_wrapper<&get_latest_checkout_cleared>},
    {"latest_cleared", get_wrapper<&get_latest_cleared>},
    {"meta", account_get_tag},
    {"n", get_wrapper<&get_subcount>},
    {"note", get_wrapper<&get_note>},
    {"parent", get_wrapper<&get_parent_account>},
    {"partial_account", get_partial_name},
    {"subcount", get_wrapper<&get_subcount>},
    {"tag", account_get_tag},
    {"total", get_wrapper<&get_total>},
    {"use_direct_amount", get_wrapper<&ignore>},
};
static_assert(symbol_table_sorted(account_functions), "account_functions must stay sorted");
} // namespace

/**
 * @brief Dispatch table mapping expression function names to account getters.
 *
 * This is the binding layer between value expressions and account data.
 * When a format string contains @c %(total) or a query uses @c account,
 * the expression engine calls lookup() on the account scope to resolve
 * the name to a callable functor.
 */
expr_t::ptr_op_t account_t::lookup(const symbol_t::kind_t kind, const string& fn_name) {
  if (kind == symbol_t::FUNCTION)
    if (const auto* fn = find_symbol(account_functions, fn_name))
      return WRAP_FUNCTOR(*fn);
  return nullptr;
}

bool account_t::valid() const {
  if (depth > 256) {
    DEBUG("ledger.validate", "account_t: depth > 256");
    return false;
  }

  for (const accounts_map::value_type& pair : accounts) {
    if (this == pair.second) {
      DEBUG("ledger.validate", "account_t: parent refers to itself!");
      return false;
    }

    if (!pair.second->valid()) {
      DEBUG("ledger.validate", "account_t: child not valid");
      return false;
    }
  }

  return true;
}

bool account_t::children_with_xdata() const {
  for (const accounts_map::value_type& pair : accounts)
    if (pair.second->has_xdata() || pair.second->children_with_xdata())
      return true;

  return false;
}

std::size_t account_t::children_with_flags(xdata_t::flags_t flags, std::size_t limit) const {
  std::size_t count = 0;

  for (auto it = accounts.begin(); it != accounts.end() && count < limit; ++it)
    if (it->second->has_xflags(flags) || it->second->children_with_flags(flags, 1) > 0)
      count++;

  return count;
}

/*--- Statistics Gathering ---*/

account_t::xdata_t::details_t& account_t::xdata_t::details_t::operator+=(const details_t& other) {
  posts_count += other.posts_count;
  posts_virtuals_count += other.posts_virtuals_count;
  posts_cleared_count += other.posts_cleared_count;
  posts_last_7_count += other.posts_last_7_count;
  posts_last_30_count += other.posts_last_30_count;
  posts_this_month_count += other.posts_this_month_count;

  if (!is_valid(earliest_post) ||
      (is_valid(other.earliest_post) && other.earliest_post < earliest_post))
    earliest_post = other.earliest_post;
  if (!is_valid(earliest_cleared_post) || (is_valid(other.earliest_cleared_post) &&
                                           other.earliest_cleared_post < earliest_cleared_post))
    earliest_cleared_post = other.earliest_cleared_post;

  if (!is_valid(latest_post) || (is_valid(other.latest_post) && other.latest_post > latest_post))
    latest_post = other.latest_post;
  if (!is_valid(latest_cleared_post) ||
      (is_valid(other.latest_cleared_post) && other.latest_cleared_post > latest_cleared_post))
    latest_cleared_post = other.latest_cleared_post;
  if (!is_valid(latest_past_post) ||
      (is_valid(other.latest_past_post) && other.latest_past_post > latest_past_post))
    latest_past_post = other.latest_past_post;

  filenames.insert(other.filenames.begin(), other.filenames.end());
  accounts_referenced.insert(other.accounts_referenced.begin(), other.accounts_referenced.end());
  payees_referenced.insert(other.payees_referenced.begin(), other.payees_referenced.end());
  return *this;
}

/*--- Cache Management ---*/

void account_t::clear_xdata() {
  xdata_ = std::nullopt;

  // Recurse into all children, including ACCOUNT_TEMP ones.  When --pivot is
  // active, transfer_details creates a temporary parent account ("Invoice")
  // and the original account hierarchy ("101", "Assets", "Receivables") is
  // grafted underneath it.  Children of a temp account inherit ACCOUNT_TEMP
  // (see find_account()), so skipping temp children here would leave their
  // xdata (notably family_details.calculated) stale across --group-by
  // groups, causing accounts_flusher in subsequent groups to return cached
  // zero totals and silently drop the group from the output (#1034).
  for (accounts_map::value_type& pair : accounts)
    pair.second->clear_xdata();
}

void account_t::clear_display_state() {
  if (xdata_) {
    // Reset display-related flags but preserve ACCOUNT_EXT_VISITED so
    // that accumulated self_details totals remain valid.
    xdata_->clear_flags();
    xdata_->add_flags(ACCOUNT_EXT_VISITED);

    // Reset family_details so total() recalculates the account tree,
    // but keep self_details intact so amounts accumulate across groups.
    xdata_->family_details.calculated = false;
    xdata_->family_details.total = value_t();

    // Reset the iteration position so amount() re-scans from the
    // beginning.  Posts already marked POST_EXT_CONSIDERED will be
    // skipped, but newly visited posts anywhere in the list will be
    // picked up.
    xdata_->self_details.last_post = std::nullopt;
    xdata_->self_details.last_reported_post = std::nullopt;

    xdata_->sort_values.clear();
  }

  // Recurse into all children, including ACCOUNT_TEMP ones (see the
  // companion comment in clear_xdata above for why this matters).
  for (accounts_map::value_type& pair : accounts)
    pair.second->clear_display_state();
}

/*--- Account Totals ---*/

/**
 * @brief Incrementally accumulate this account's balance from visited postings.
 *
 * This is the core balance-computation method.  It iterates only postings
 * that have been marked POST_EXT_VISITED by the report walker, which means
 * only transactions matching the current query contribute to the balance.
 *
 * To avoid re-scanning the entire posting list on every call, it uses
 * last_post and last_reported_post iterators to resume from where the
 * previous invocation stopped.  Each posting is marked POST_EXT_CONSIDERED
 * after being counted so it will not be double-counted if amount() is
 * called again (e.g., during a multi-group report).
 *
 * After exhausting the account's own posts list, it continues with
 * reported_posts -- postings that display filters have redirected to
 * this account.
 *
 * When an expression is supplied, totals are computed in temporary
 * variables rather than cached in xdata, because the expression changes
 * the semantics of the accumulation.
 */
value_t account_t::amount(bool real_only, expr_t* expr /*= nullptr*/) const {
  DEBUG("account.amount", "real only: " << real_only);

  if (!xdata_ || !xdata_->has_flags(ACCOUNT_EXT_VISITED))
    return NULL_VALUE;

  value_t total_, real_total_;
  value_t& total = expr ? total_ : xdata_->self_details.total;
  value_t& real_total = expr ? real_total_ : xdata_->self_details.real_total;
  posts_list::const_iterator i =
      !expr && xdata_->self_details.last_post ? *xdata_->self_details.last_post : posts.begin();

  bool reported = false;
  for (;; i++) {
    if (i == posts.end()) {
      // when we reach the end of normal posts, then iterate reported_posts
      i = !expr && xdata_->self_details.last_reported_post
              ? *xdata_->self_details.last_reported_post
              : xdata_->reported_posts.begin();
      reported = true;
    }
    if (i == xdata_->reported_posts.end())
      break;

    if (!expr) {
      if (reported)
        xdata_->self_details.last_reported_post = i;
      else
        xdata_->self_details.last_post = i;
    }

    if (!(*i)->xdata().has_flags(POST_EXT_VISITED))
      continue;
    if (!expr && (*i)->xdata().has_flags(POST_EXT_CONSIDERED))
      continue;

    (*i)->add_to_value(total, expr);
    if (!(*i)->has_flags(POST_VIRTUAL))
      (*i)->add_to_value(real_total, expr);

    if (!expr)
      (*i)->xdata().add_flags(POST_EXT_CONSIDERED);
  }

  return real_only ? real_total : total;
}

static value_t sum_posts(const posts_list& posts, bool real_only, const optional<date_t>& cutoff) {
  value_t result;
  for (const post_t* post : posts) {
    if (real_only && post->has_flags(POST_VIRTUAL))
      continue;
    if (!post->amount.is_null() && (!cutoff || post->date() <= *cutoff))
      add_or_set_value(result, post->amount);
  }
  return result;
}

value_t account_t::self_total(bool real_only, const optional<date_t>& cutoff) const {
  running_total_t& rt(running_total_);
  if (rt.aux_dates != item_t::use_aux_date) {
    rt.reset();
    rt.aux_dates = item_t::use_aux_date;
  }
  for (auto i = rt.last ? std::next(*rt.last) : posts.begin(); i != posts.end(); ++i) {
    const post_t* post = *i;
    if (!post->amount.is_null()) {
      add_or_set_value(rt.total, post->amount);
      if (!post->has_flags(POST_VIRTUAL))
        add_or_set_value(rt.real_total, post->amount);
    }
    date_t when = post->date();
    if (!rt.latest || when > *rt.latest)
      rt.latest = when;
    rt.last = i;
  }

  // A post dated after the cutoff exists only when posts arrived out of
  // date order (#847, #1092, #2071); only then rescan the account.
  if (cutoff && rt.latest && *cutoff < *rt.latest)
    return sum_posts(posts, real_only, cutoff);

  value_t result = real_only ? rt.real_total : rt.total;
  VERIFY(result == sum_posts(posts, real_only, cutoff));
  return result;
}

/**
 * @brief Recursive family total: own amount() plus children's total().
 *
 * Walks the account tree depth-first, summing each child's total() into
 * family_details.total, then adds this account's own amount().  The
 * result is cached via family_details.calculated so subsequent calls
 * return immediately.
 */
value_t account_t::total(expr_t* expr) const {
  if (!(xdata_ && xdata_->family_details.calculated)) {
    const_cast<account_t&>(*this).xdata().family_details.calculated = true;

    value_t temp;
    for (const accounts_map::value_type& pair : accounts) {
      temp = pair.second->total(expr);
      if (!temp.is_null())
        add_or_set_value(xdata_->family_details.total, temp);
    }

    temp = amount(false, expr);
    if (!temp.is_null())
      add_or_set_value(xdata_->family_details.total, temp);
  }
  return xdata_->family_details.total;
}

const account_t::xdata_t::details_t& account_t::self_details(bool gather_all) const {
  if (!(xdata_ && xdata_->self_details.gathered)) {
    const_cast<account_t&>(*this).xdata().self_details.gathered = true;

    const date_t today = CURRENT_DATE();
    for (const post_t* post : posts)
      xdata_->self_details.update(const_cast<post_t&>(*post), today, gather_all);
  }
  return xdata_->self_details;
}

const account_t::xdata_t::details_t& account_t::family_details(bool gather_all) const {
  if (!(xdata_ && xdata_->family_details.gathered)) {
    const_cast<account_t&>(*this).xdata().family_details.gathered = true;

    for (const accounts_map::value_type& pair : accounts)
      xdata_->family_details += pair.second->family_details(gather_all);

    xdata_->family_details += self_details(gather_all);
  }
  return xdata_->family_details;
}

void account_t::xdata_t::details_t::update(post_t& post, const date_t& today, bool gather_all) {
  posts_count++;

  if (post.has_flags(POST_VIRTUAL))
    posts_virtuals_count++;

  if (gather_all && post.pos)
    filenames.insert(*post.pos->pathname);

  const date_t date = post.date();

  // Recency counters should only include postings that have actually
  // occurred on or before today; future-dated postings must not inflate
  // the "in last N days" or "this month" tallies (issue #741).
  if (date <= today) {
    if (date.year() == today.year() && date.month() == today.month())
      posts_this_month_count++;

    if ((today - date).days() <= 30)
      posts_last_30_count++;
    if ((today - date).days() <= 7)
      posts_last_7_count++;

    if (!is_valid(latest_past_post) || date > latest_past_post)
      latest_past_post = date;
  }

  if (!is_valid(earliest_post) || date < earliest_post)
    earliest_post = date;
  if (!is_valid(latest_post) || date > latest_post)
    latest_post = date;

  if (post.checkin && (earliest_checkin.is_not_a_date_time() || *post.checkin < earliest_checkin))
    earliest_checkin = *post.checkin;

  if (post.checkout && (latest_checkout.is_not_a_date_time() || *post.checkout > latest_checkout)) {
    latest_checkout = *post.checkout;
    latest_checkout_cleared = post.state() == item_t::CLEARED;
  }

  if (post.state() == item_t::CLEARED) {
    posts_cleared_count++;

    if (!is_valid(earliest_cleared_post) || date < earliest_cleared_post)
      earliest_cleared_post = date;
    if (!is_valid(latest_cleared_post) || date > latest_cleared_post)
      latest_cleared_post = date;
  }

  if (gather_all) {
    accounts_referenced.insert(post.account->fullname());
    payees_referenced.insert(post.payee());
  }
}

/*--- XML Serialization ---*/

/**
 * @brief Serialize an account and its children into a Boost property tree.
 *
 * Recurses into child accounts, outputting name, fullname, account-amount,
 * and account-total for each account that satisfies @p pred.  Used by the
 * XML output handler.
 */
void put_account(property_tree::ptree& st, const account_t& acct,
                 const function<bool(const account_t&)>& pred) {
  if (pred(acct)) {
    std::ostringstream buf;
    buf.width(sizeof(intptr_t) * 2);
    buf.fill('0');
    buf << std::hex << reinterpret_cast<intptr_t>(&acct);

    st.put("<xmlattr>.id", buf.str());

    st.put("name", acct.name);
    st.put("fullname", acct.fullname());

    value_t total = acct.amount();
    if (!total.is_null())
      put_value(st.put("account-amount", ""), total);

    total = acct.total();
    if (!total.is_null())
      put_value(st.put("account-total", ""), total);

    for (const accounts_map::value_type& pair : acct.accounts)
      put_account(st.add("account", ""), *pair.second, pred);
  }
}

} // namespace ledger
