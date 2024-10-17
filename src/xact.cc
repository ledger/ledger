/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "context.h"
#include "format.h"
#include "pool.h"

namespace ledger {

xact_base_t::xact_base_t(const xact_base_t& xact_base)
  : item_t(xact_base), journal(xact_base.journal)
{
  TRACE_CTOR(xact_base_t, "copy");
}

xact_base_t::~xact_base_t()
{
  TRACE_DTOR(xact_base_t);

  if (! has_flags(ITEM_TEMP)) {
    foreach (post_t * post, posts) {
      // If the posting is a temporary, it will be destructed when the
      // temporary is.
      assert(! post->has_flags(ITEM_TEMP));

      if (post->account)
        post->account->remove_post(post);
      checked_delete(post);
    }
  }
}

void xact_base_t::add_post(post_t * post)
{
  // You can add temporary postings to transactions, but not real postings to
  // temporary transactions.
  if (! post->has_flags(ITEM_TEMP))
    assert(! has_flags(ITEM_TEMP));

  posts.push_back(post);
}

bool xact_base_t::remove_post(post_t * post)
{
  posts.remove(post);
  post->xact = NULL;
  return true;
}

bool xact_base_t::has_xdata()
{
  foreach (post_t * post, posts)
    if (post->has_xdata())
      return true;

  return false;
}

void xact_base_t::clear_xdata()
{
  foreach (post_t * post, posts)
    if (! post->has_flags(ITEM_TEMP))
      post->clear_xdata();
}

value_t xact_base_t::magnitude() const
{
  value_t halfbal = 0L;
  foreach (const post_t * post, posts) {
    if (post->amount.sign() > 0) {
      if (post->cost)
        halfbal += *post->cost;
      else
        halfbal += post->amount;
    }
  }
  return halfbal;
}

namespace {
  inline bool account_ends_with_special_char(const string& name) {
    string::size_type len(name.length());
    return (std::isdigit(static_cast<unsigned char>(name[len - 1])) ||
            name[len - 1] == ')' ||
            name[len - 1] == '}' ||
            name[len - 1] == ']');
  }

  struct add_balancing_post
  {
    bool         first;
    xact_base_t& xact;
    post_t *     null_post;

    explicit add_balancing_post(xact_base_t& _xact, post_t * _null_post)
      : first(true), xact(_xact), null_post(_null_post) {
      TRACE_CTOR(add_balancing_post, "xact_base_t&, post_t *");
    }
    add_balancing_post(const add_balancing_post& other)
      : first(other.first), xact(other.xact), null_post(other.null_post) {
      TRACE_CTOR(add_balancing_post, "copy");
    }
    ~add_balancing_post() throw() {
      TRACE_DTOR(add_balancing_post);
    }

    void operator()(const amount_t& amount) {
      if (first) {
        null_post->amount = amount.negated();
        null_post->add_flags(POST_CALCULATED);
        first = false;
      } else {
        unique_ptr<post_t> p(new post_t(null_post->account, amount.negated(),
                                        null_post->flags() | ITEM_GENERATED | POST_CALCULATED));
        p->set_state(null_post->state());
        xact.add_post(p.release());
      }
    }
  };
}

bool xact_base_t::finalize()
{
  // Scan through and compute the total balance for the xact.  This is used
  // for auto-calculating the value of xacts with no cost, and the per-unit
  // price of unpriced commodities.

  value_t  balance;
  post_t * null_post = NULL;

  foreach (post_t * post, posts) {
    if (! post->must_balance())
      continue;

    amount_t& p(post->cost ? *post->cost : post->amount);
    if (! p.is_null()) {
      DEBUG("xact.finalize", "post must balance = " << p.reduced());
      // If the amount was a cost, it very likely has the
      // "keep_precision" flag set, meaning commodity display precision
      // is ignored when displaying the amount.  We never want this set
      // for the balance, so we must clear the flag in a temporary to
      // avoid it propagating into the balance.
      add_or_set_value(balance, p.keep_precision() ?
                       p.rounded().reduced() : p.reduced());
    }
    else if (null_post) {
      bool post_account_bad =
        account_ends_with_special_char(post->account->fullname());
      bool null_post_account_bad =
        account_ends_with_special_char(null_post->account->fullname());

      if (post_account_bad || null_post_account_bad)
        throw_(std::logic_error,
               _f("Posting with null amount's account may be misspelled:\n  \"%1%\"")
               % (post_account_bad ? post->account->fullname() :
                   null_post->account->fullname()));
      else
        throw_(std::logic_error,
               _("Only one posting with null amount allowed per transaction"));
    }
    else {
      null_post = post;
    }
  }
  VERIFY(balance.valid());

#if DEBUG_ON
  DEBUG("xact.finalize", "initial balance = " << balance);
  DEBUG("xact.finalize", "balance is " << balance.label());
  if (balance.is_balance())
    DEBUG("xact.finalize", "balance commodity count = "
          << balance.as_balance().amounts.size());
#endif

  // If there is only one post, balance against the default account if one has
  // been set.

  if (journal && journal->bucket && posts.size() == 1 && ! balance.is_null()) {
    null_post = new post_t(journal->bucket, ITEM_INFERRED);
    null_post->_state = (*posts.begin())->_state;
    add_post(null_post);
  }

  if (! null_post && balance.is_balance() &&
      balance.as_balance().amounts.size() == 2) {
    // When an xact involves two different commodities (regardless of how
    // many posts there are) determine the conversion ratio by dividing the
    // total value of one commodity by the total value of the other.  This
    // establishes the per-unit cost for this post for both commodities.

    DEBUG("xact.finalize",
          "there were exactly two commodities, and no null post");

    bool     saw_cost = false;
    post_t * top_post = NULL;

    foreach (post_t * post, posts) {
      if (! post->amount.is_null() && post->must_balance()) {
        if (post->amount.has_annotation())
          top_post = post;
        else if (! top_post)
          top_post = post;
      }

      if (post->cost && ! post->has_flags(POST_COST_CALCULATED)) {
        saw_cost = true;
        break;
      }
    }

    if (! saw_cost && top_post) {
      const balance_t& bal(balance.as_balance());

      DEBUG("xact.finalize", "there were no costs, and a valid top_post");

      balance_t::amounts_map::const_iterator a = bal.amounts.begin();

      const amount_t * x = &(*a++).second;
      const amount_t * y = &(*a++).second;

      if (*x && *y) {
        if (x->commodity() != top_post->amount.commodity())
          std::swap(x, y);

        DEBUG("xact.finalize", "primary   amount = " << *x);
        DEBUG("xact.finalize", "secondary amount = " << *y);

        commodity_t& comm(x->commodity());
        amount_t     per_unit_cost = (*y / *x).abs().unrounded();

        DEBUG("xact.finalize", "per_unit_cost = " << per_unit_cost);

        foreach (post_t * post, posts) {
          const amount_t& amt(post->amount.reduced());

          if (post->must_balance() && amt.commodity() == comm) {
            balance -= amt;
            post->cost = per_unit_cost * amt;
            post->add_flags(POST_COST_CALCULATED);
            balance += *post->cost;

            DEBUG("xact.finalize", "set post->cost to = " << *post->cost);
          }
        }
      }
    }
  }

  posts_list copy(posts);

  if (has_date()) {
    foreach (post_t * post, copy) {
      if (! post->cost)
        continue;

      if (post->amount.commodity() == post->cost->commodity())
        throw_(balance_error,
               _("A posting's cost must be of a different commodity than its amount"));

      cost_breakdown_t breakdown =
        commodity_pool_t::current_pool->exchange(
          post->amount, *post->cost, false, ! post->has_flags(POST_COST_VIRTUAL),
          datetime_t(date(), time_duration(0, 0, 0, 0)));

      if (post->amount.has_annotation() && post->amount.annotation().price) {
        if (breakdown.basis_cost.commodity() == breakdown.final_cost.commodity()) {
          DEBUG("xact.finalize", "breakdown.basis_cost = " << breakdown.basis_cost);
          DEBUG("xact.finalize", "breakdown.final_cost = " << breakdown.final_cost);
          if (amount_t gain_loss = breakdown.basis_cost - breakdown.final_cost) {
            DEBUG("xact.finalize", "gain_loss = " << gain_loss);
            gain_loss.in_place_round();
            DEBUG("xact.finalize", "gain_loss rounds to = " << gain_loss);
            if (post->must_balance())
              add_or_set_value(balance, gain_loss.reduced());
#if 0
            account_t * account;
            if (gain_loss.sign() > 0)
              account = journal->find_account(_("Equity:Capital Gains"));
            else
              account = journal->find_account(_("Equity:Capital Losses"));

            post_t * p = new post_t(account, gain_loss, ITEM_GENERATED);
            p->set_state(post->state());
            if (post->has_flags(POST_VIRTUAL)) {
              DEBUG("xact.finalize", "gain_loss came from a virtual post");
              p->add_flags(post->flags() & (POST_VIRTUAL | POST_MUST_BALANCE));
            }
            add_post(p);
#else
            *post->cost += gain_loss;
#endif
            DEBUG("xact.finalize", "added gain_loss, balance = " << balance);
          } else {
            DEBUG("xact.finalize", "gain_loss would have displayed as zero");
          }
        }
      } else {
        post->amount =
          breakdown.amount.has_annotation() ?
          amount_t(breakdown.amount,
                   annotation_t(breakdown.amount.annotation().price,
                                breakdown.amount.annotation().date,
                                post->amount.has_annotation() ?
                                post->amount.annotation().tag :
                                breakdown.amount.annotation().tag,
                                breakdown.amount.annotation().value_expr)) :
          breakdown.amount;
        DEBUG("xact.finalize", "added breakdown, balance = " << balance);
      }

      if (post->has_flags(POST_COST_FIXATED) &&
          post->amount.has_annotation() && post->amount.annotation().price) {
        DEBUG("xact.finalize", "fixating annotation price");
        post->amount.annotation().add_flags(ANNOTATION_PRICE_FIXATED);
      }
    }
  }

  if (null_post != NULL) {
    // If one post has no value at all, its value will become the inverse of
    // the rest.  If multiple commodities are involved, multiple posts are
    // generated to balance them all.

    DEBUG("xact.finalize", "there was a null posting");
    add_balancing_post post_adder(*this, null_post);

    if (balance.is_balance())
      balance.as_balance_lval().map_sorted_amounts(post_adder);
    else if (balance.is_amount())
      post_adder(balance.as_amount_lval());
    else if (balance.is_long())
      post_adder(balance.to_amount());
    else if (! balance.is_null() && ! balance.is_realzero())
      throw_(balance_error, _("Transaction does not balance"));

    balance = NULL_VALUE;

  }
  DEBUG("xact.finalize", "resolved balance = " << balance);

  if (! balance.is_null() && ! balance.is_zero()) {
    add_error_context(item_context(*this, _("While balancing transaction")));
    add_error_context(_("Unbalanced remainder is:"));
    add_error_context(value_context(balance));
    add_error_context(_("Amount to balance against:"));
    add_error_context(value_context(magnitude()));
    throw_(balance_error, _("Transaction does not balance"));
  }

  // Add a pointer to each posting to their related accounts

  if (dynamic_cast<xact_t *>(this)) {
    bool all_null  = true;
    bool some_null = false;

    foreach (post_t * post, posts) {
      assert(post->account);

      if (! post->amount.is_null()) {
        all_null = false;
        post->amount.in_place_reduce();
      } else {
        some_null = true;
      }

      if (post->has_flags(POST_DEFERRED)) {
        if (!post->amount.is_null())
          post->account->add_deferred_post(id(), post);
      } else {
        post->account->add_post(post);
      }

      post->xdata().add_flags(POST_EXT_VISITED);
      post->account->xdata().add_flags(ACCOUNT_EXT_VISITED);
    }

    if (all_null)
      return false;             // ignore this xact completely
    else if (some_null)
      throw_(balance_error,
             _("There cannot be null amounts after balancing a transaction"));
  }

  VERIFY(valid());

  return true;
}

bool xact_base_t::verify()
{
  // Scan through and compute the total balance for the xact.

  value_t  balance;

  foreach (post_t * post, posts) {
    if (! post->must_balance())
      continue;

    amount_t& p(post->cost ? *post->cost : post->amount);
    assert(! p.is_null());

    // If the amount was a cost, it very likely has the "keep_precision" flag
    // set, meaning commodity display precision is ignored when displaying the
    // amount.  We never want this set for the balance, so we must clear the
    // flag in a temporary to avoid it propagating into the balance.
    add_or_set_value(balance, p.keep_precision() ?
                     p.rounded().reduced() : p.reduced());
  }
  VERIFY(balance.valid());

  // Now that the post list has its final form, calculate the balance once
  // more in terms of total cost, accounting for any possible gain/loss
  // amounts.

  foreach (post_t * post, posts) {
    if (! post->cost)
      continue;

    if (post->amount.commodity() == post->cost->commodity())
      throw_(amount_error,
             _("A posting's cost must be of a different commodity than its amount"));
  }

  if (! balance.is_null() && ! balance.is_zero()) {
    add_error_context(item_context(*this, _("While balancing transaction")));
    add_error_context(_("Unbalanced remainder is:"));
    add_error_context(value_context(balance));
    add_error_context(_("Amount to balance against:"));
    add_error_context(value_context(magnitude()));
    throw_(balance_error, _("Transaction does not balance"));
  }

  VERIFY(valid());

  return true;
}

xact_t::xact_t(const xact_t& e)
  : xact_base_t(e), code(e.code), payee(e.payee)
{
  TRACE_CTOR(xact_t, "copy");
}

void xact_t::add_post(post_t * post)
{
  post->xact = this;
  xact_base_t::add_post(post);
}

namespace {
  value_t get_magnitude(xact_t& xact) {
    return xact.magnitude();
  }

  value_t get_code(xact_t& xact) {
    if (xact.code)
      return string_value(*xact.code);
    else
      return NULL_VALUE;
  }

  value_t get_payee(xact_t& xact) {
    return string_value(xact.payee);
  }

  template <value_t (*Func)(xact_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<xact_t>(scope));
  }

  value_t fn_any(call_scope_t& args)
  {
    post_t& post(args.context<post_t>());
    expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

    foreach (post_t * p, post.xact->posts) {
      bind_scope_t bound_scope(args, *p);
      if (expr->calc(bound_scope, args.locus, args.depth).to_boolean())
        return true;
    }
    return false;
  }

  value_t fn_all(call_scope_t& args)
  {
    post_t& post(args.context<post_t>());
    expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

    foreach (post_t * p, post.xact->posts) {
      bind_scope_t bound_scope(args, *p);
      if (! expr->calc(bound_scope, args.locus, args.depth).to_boolean())
        return false;
    }
    return true;
  }
}

expr_t::ptr_op_t xact_t::lookup(const symbol_t::kind_t kind,
                                const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return item_t::lookup(kind, name);

  switch (name[0]) {
  case 'a':
    if (name == "any")
      return WRAP_FUNCTOR(&fn_any);
    else if (name == "all")
      return WRAP_FUNCTOR(&fn_all);
    break;

  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    break;

  case 'm':
    if (name == "magnitude")
      return WRAP_FUNCTOR(get_wrapper<&get_magnitude>);
    break;

  case 'p':
    if (name[1] == '\0' || name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    break;
  }

  return item_t::lookup(kind, name);
}

bool xact_t::valid() const
{
  if (! _date) {
    DEBUG("ledger.validate", "xact_t: ! _date");
    return false;
  }

  foreach (post_t * post, posts)
    if (post->xact != this || ! post->valid()) {
      DEBUG("ledger.validate", "xact_t: post not valid");
      return false;
    }

  return true;
}

extern "C" unsigned char *SHA512(
  void *data, unsigned int data_len, unsigned char *digest);

namespace {
  std::string bufferToHex(const unsigned char* buffer, std::size_t size) {
      std::ostringstream oss;
      oss << std::hex << std::setfill('0');
      for(std::size_t i = 0; i < size; ++i)
          oss << std::setw(2) << static_cast<int>(buffer[i]);
      return oss.str();
  }
}

string xact_t::hash(string nonce, hash_type_t hash_type) const {
  std::ostringstream repr;

  repr << nonce;
  repr << date();
  repr << aux_date();
  repr << code;
  repr << payee;

  std::vector<std::string> strings;

  posts_list all_posts(posts.begin(), posts.end());
  foreach (post_t * post, all_posts) {
    std::ostringstream posting;
    posting << post->account->fullname();
    if (! post->amount.is_null())
      posting << post->amount.to_fullstring();
    if (post->cost)
      posting << post->cost->to_fullstring();
    posting << post->checkin;
    posting << post->checkout;
    strings.push_back(posting.str());
  }

  std::sort(strings.begin(), strings.end());

  foreach (string& str, strings) {
    repr << str;
  }

  unsigned char data[128];
  string repr_str(repr.str());

  SHA512((void *)repr_str.c_str(), repr_str.length(), data);

  return bufferToHex(
    data, hash_type == HASH_SHA512 ? 64 : 32 /*SHA512_DIGEST_LENGTH*/);
}

namespace {
  bool post_pred(expr_t::ptr_op_t op, post_t& post)
  {
    switch (op->kind) {
    case expr_t::op_t::VALUE:
      return op->as_value().to_boolean();

    case expr_t::op_t::O_MATCH:
      if (op->left()->kind == expr_t::op_t::IDENT &&
          op->left()->as_ident() == "account" &&
          op->right()->kind == expr_t::op_t::VALUE &&
          op->right()->as_value().is_mask())
        return op->right()->as_value().as_mask()
          .match(post.reported_account()->fullname());
      else
        break;

    case expr_t::op_t::O_EQ:
      return post_pred(op->left(), post) == post_pred(op->right(), post);

    case expr_t::op_t::O_NOT:
      return ! post_pred(op->left(), post);

    case expr_t::op_t::O_AND:
      return post_pred(op->left(), post) && post_pred(op->right(), post);

    case expr_t::op_t::O_OR:
      return post_pred(op->left(), post) || post_pred(op->right(), post);

    case expr_t::op_t::O_QUERY:
      if (post_pred(op->left(), post))
        return post_pred(op->right()->left(), post);
      else
        return post_pred(op->right()->right(), post);

    default:
      break;
    }

    throw_(calc_error, _("Unhandled operator"));
    return false;
  }
}

static string apply_format(const string& str, scope_t& scope)
{
  if (contains(str, "%(")) {
    format_t str_format(str);
    std::ostringstream buf;
    buf << str_format(scope);
    return buf.str();
  } else {
    return str;
  }
}

void auto_xact_t::extend_xact(xact_base_t& xact, parse_context_t& context)
{
  posts_list initial_posts(xact.posts.begin(), xact.posts.end());

  try {

  bool needs_further_verification = false;

  foreach (post_t * initial_post, initial_posts) {
    if (initial_post->has_flags(ITEM_GENERATED))
      continue;

    bind_scope_t bound_scope(*scope_t::default_scope, *initial_post);

    bool matches_predicate = false;
    if (try_quick_match) {
      try {
        bool found_memoized_result = false;
        if (! memoized_results.empty()) {
          std::map<string, bool>::iterator i =
            memoized_results.find(initial_post->account->fullname());
          if (i != memoized_results.end()) {
            found_memoized_result = true;
            matches_predicate = (*i).second;
          }
        }

        // Since the majority of people who use automated transactions simply
        // match against account names, try using a *much* faster version of
        // the predicate evaluator.
        if (! found_memoized_result) {
          matches_predicate = post_pred(predicate.get_op(), *initial_post);
          memoized_results.insert
            (std::pair<string, bool>(initial_post->account->fullname(),
                                     matches_predicate));
        }
      }
      catch (...) {
        DEBUG("xact.extend.fail",
              "The quick matcher failed, going back to regular eval");
        try_quick_match   = false;
        matches_predicate = predicate(bound_scope);
      }
    } else {
      matches_predicate = predicate(bound_scope);
    }

    if (matches_predicate) {
      if (deferred_notes) {
        foreach (deferred_tag_data_t& data, *deferred_notes) {
          if (data.apply_to_post == NULL)
            initial_post->append_note(
              apply_format(data.tag_data, bound_scope).c_str(),
              bound_scope, data.overwrite_existing);
        }
      }

      if (check_exprs) {
        foreach (expr_t::check_expr_pair& pair, *check_exprs) {
          if (pair.second == expr_t::EXPR_GENERAL) {
            pair.first.calc(bound_scope);
          }
          else if (! pair.first.calc(bound_scope).to_boolean()) {
            if (pair.second == expr_t::EXPR_ASSERTION)
              throw_(parse_error,
                     _f("Transaction assertion failed: %1%") % pair.first);
            else
              context.warning(_f("Transaction check failed: %1%") % pair.first);
          }
        }
      }

      foreach (post_t * post, posts) {
        amount_t post_amount;
        if (post->amount.is_null()) {
          if (! post->amount_expr)
            throw_(amount_error,
                   _("Automated transaction's posting has no amount"));

          value_t result(post->amount_expr->calc(bound_scope));
          if (result.is_long()) {
            post_amount = result.to_amount();
          } else {
            if (! result.is_amount())
              throw_(amount_error,
                     _("Amount expressions must result in a simple amount"));
            post_amount = result.as_amount();
          }
        } else {
          post_amount = post->amount;
        }

        amount_t amt;
        if (! post_amount.commodity())
          amt = initial_post->amount * post_amount;
        else
          amt = post_amount;

#if DEBUG_ON
        IF_DEBUG("xact.extend") {
          DEBUG("xact.extend",
                "Initial post on line " << initial_post->pos->beg_line << ": "
                << "amount " << initial_post->amount << " (precision "
                << initial_post->amount.precision() << ")");

          if (initial_post->amount.keep_precision())
            DEBUG("xact.extend", "  precision is kept");

          DEBUG("xact.extend",
                "Posting on line " << post->pos->beg_line << ": "
                << "amount " << post_amount << ", amt " << amt
                << " (precision " << post_amount.precision()
                << " != " << amt.precision() << ")");

          if (post_amount.keep_precision())
            DEBUG("xact.extend", "  precision is kept");
          if (amt.keep_precision())
            DEBUG("xact.extend", "  amt precision is kept");
        }
#endif // DEBUG_ON

        account_t * account  = post->account;
        string fullname = account->fullname();
        assert(! fullname.empty());

        if (contains(fullname, "$account")) {
          fullname = regex_replace(fullname, regex("\\$account\\>"),
                                   initial_post->account->fullname());
          while (account->parent)
            account = account->parent;
          account = account->find_account(fullname);
        }
        else if (contains(fullname, "%(")) {
          format_t account_name(fullname);
          std::ostringstream buf;
          buf << account_name(bound_scope);
          while (account->parent)
            account = account->parent;
          account = account->find_account(buf.str());
        }

        // Copy over details so that the resulting post is a mirror of
        // the automated xact's one.
        post_t * new_post = new post_t(account, amt);
        new_post->copy_details(*post);
        if(post->cost)
          new_post->cost = post->cost;

        // A Cleared transaction implies all of its automatic posting are cleared
        // CPR 2012/10/23
        if (xact.state() == item_t::CLEARED) {
          DEBUG("xact.extend.cleared", "CLEARED");
          new_post->set_state(item_t::CLEARED);
        }

        new_post->add_flags(ITEM_GENERATED);
        new_post->account =
          journal->register_account(account->fullname(), new_post,
                                    journal->master);

        if (deferred_notes) {
          foreach (deferred_tag_data_t& data, *deferred_notes) {
            if (! data.apply_to_post || data.apply_to_post == post) {
              new_post->append_note(
                apply_format(data.tag_data, bound_scope).c_str(),
                bound_scope, data.overwrite_existing);
            }
          }
        }

        extend_post(*new_post, *journal);

        xact.add_post(new_post);
        new_post->account->add_post(new_post);

        // Add flags so this post updates the account balance
        new_post->xdata().add_flags(POST_EXT_VISITED);
        new_post->account->xdata().add_flags(ACCOUNT_EXT_VISITED);

        if (new_post->must_balance())
          needs_further_verification = true;
      }
    }
  }

  if (needs_further_verification)
    xact.verify();

  }
  catch (const std::exception&) {
    add_error_context(item_context(*this, _("While applying automated transaction")));
    add_error_context(item_context(xact, _("While extending transaction")));
    throw;
  }
}

void put_xact(property_tree::ptree& st, const xact_t& xact)
{
  if (xact.state() == item_t::CLEARED)
    st.put("<xmlattr>.state", "cleared");
  else if (xact.state() == item_t::PENDING)
    st.put("<xmlattr>.state", "pending");

  if (xact.has_flags(ITEM_GENERATED))
    st.put("<xmlattr>.generated", "true");

  if (xact._date)
    put_date(st.put("date", ""), *xact._date);
  if (xact._date_aux)
    put_date(st.put("aux-date", ""), *xact._date_aux);

  if (xact.code)
    st.put("code", *xact.code);

  st.put("payee", xact.payee);

  if (xact.note)
    st.put("note", *xact.note);

  if (xact.metadata)
    put_metadata(st.put("metadata", ""),  *xact.metadata);
}

} // namespace ledger
