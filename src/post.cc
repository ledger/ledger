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
 * @file   post.cc
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Implementation of posting operations
 *
 * This file implements the post_t methods for tag inheritance, date
 * resolution, payee resolution, expression binding, value accumulation,
 * valuation expression attachment, and XML serialization.  Together these
 * form the core logic that connects journal entries to the reporting
 * pipeline.
 */

#include <system.hh>

#include "post.h"
#include "xact.h"
#include "account.h"
#include "journal.h"
#include "format.h"
#include "pool.h"

namespace ledger {

/*--- Tag Inheritance ---*/
// Postings inherit tags from their parent transaction.  When a posting
// does not have a given tag itself, and inherit is true, the lookup
// delegates to the parent xact.  This lets users attach metadata at the
// transaction level (e.g., ; Payee: ...) and have it apply to all postings.

bool post_t::has_tag(const string& tag, bool inherit) const {
  if (item_t::has_tag(tag))
    return true;
  if (inherit && xact)
    return xact->has_tag(tag);
  return false;
}

bool post_t::has_tag(const mask_t& tag_mask, const std::optional<mask_t>& value_mask,
                     bool inherit) const {
  if (item_t::has_tag(tag_mask, value_mask))
    return true;
  if (inherit && xact)
    return xact->has_tag(tag_mask, value_mask);
  return false;
}

std::optional<value_t> post_t::get_tag(const string& tag, bool inherit) const {
  if (std::optional<value_t> value = item_t::get_tag(tag))
    return value;
  if (inherit && xact)
    return xact->get_tag(tag);
  return std::nullopt;
}

std::optional<value_t> post_t::get_tag(const mask_t& tag_mask,
                                       const std::optional<mask_t>& value_mask,
                                       bool inherit) const {
  if (std::optional<value_t> value = item_t::get_tag(tag_mask, value_mask))
    return value;
  if (inherit && xact)
    return xact->get_tag(tag_mask, value_mask);
  return std::nullopt;
}

/*--- Date Resolution ---*/
// Date resolution follows a priority chain that varies by method:
//
//   value_date(): xdata override -> date()
//   date():       xdata override -> aux_date (if use_aux_date) -> primary_date()
//   primary_date(): xdata override -> posting _date -> xact date -> CURRENT_DATE
//   aux_date():  posting aux_date -> xact aux_date
//
// The xdata date override lets the reporting pipeline assign custom dates
// (e.g., for --now or period-based reporting) without modifying the journal.
// The --aux-date flag toggles use_aux_date, making auxiliary dates primary.

/**
 * @brief The date to use for valuation (price lookup) purposes.
 *
 * Checks xdata for a pipeline-assigned valuation date first, then
 * falls back to the posting's effective date().
 */
date_t post_t::value_date() const {
  if (xdata_ && is_valid(xdata_->value_date))
    return xdata_->value_date;
  return date();
}

/**
 * @brief The effective date of this posting for reporting purposes.
 *
 * When --aux-date is active and an auxiliary date exists (on the posting
 * or its parent transaction), that date is used.  Otherwise falls through
 * to primary_date().
 */
date_t post_t::date() const {
  if (xdata_ && is_valid(xdata_->date))
    return xdata_->date;

  if (item_t::use_aux_date) {
    if (optional<date_t> aux = aux_date())
      return *aux;
  }

  return primary_date();
}

/**
 * @brief The primary date of this posting.
 *
 * If the posting has its own date (from a per-posting date override in
 * the journal), that is returned.  Otherwise the parent transaction's
 * date is used.  As a last resort, CURRENT_DATE() provides a fallback
 * for programmatically created postings with no transaction.
 */
date_t post_t::primary_date() const {
  if (xdata_ && is_valid(xdata_->date))
    return xdata_->date;

  if (!_date) {
    if (xact)
      return xact->date();
    else
      return CURRENT_DATE();
  }
  return *_date;
}

/**
 * @brief The auxiliary (secondary) date for this posting.
 *
 * Checks the posting's own aux_date first, then the parent transaction's.
 * Auxiliary dates are commonly used for the settlement date of a trade
 * when the primary date is the trade date.
 */
optional<date_t> post_t::aux_date() const {
  optional<date_t> date = item_t::aux_date();
  if (!date && xact)
    return xact->aux_date();
  return date;
}

/*--- Payee Resolution ---*/
// The payee for a posting is resolved through a three-level cascade:
//   1. Explicit _payee set via set_payee() (used by automated transactions)
//   2. A "Payee:" metadata tag on the posting or inherited from the transaction
//   3. The parent transaction's payee field
//
// This allows individual postings to override the transaction payee,
// which is useful for split transactions involving different payees.

/**
 * @brief Extract a payee from the Payee metadata tag.
 * @return The tag value as a string, or empty string if not found.
 */
string post_t::payee_from_tag() const {
  if (std::optional<value_t> post_payee = get_tag(_("Payee")))
    return post_payee->as_string();
  else
    return "";
}

/**
 * @brief Resolve the effective payee for this posting.
 *
 * Checks: (1) _payee field, (2) Payee metadata tag, (3) xact payee.
 */
string post_t::payee() const {
  if (_payee)
    return *_payee;

  string post_payee = payee_from_tag(); // NOLINT(bugprone-unused-local-non-trivial-variable)

  return post_payee != "" ? post_payee : xact ? xact->payee : "";
}

/*--- Expression Bindings ---*/
// This anonymous namespace defines getter functions that bridge between
// Ledger's expression language and the C++ post_t data model.  Each getter
// extracts a specific field or computed value from a posting, making it
// available to user expressions like `amount`, `account`, `payee`, etc.
//
// The get_wrapper template adapts simple post_t& getters into call_scope_t
// functors expected by the expression engine.

namespace {

/** @brief Return the posting itself as a scope value. */
value_t get_this(post_t& post) {
  return scope_value(&post);
}

value_t get_is_calculated(post_t& post) {
  return post.has_flags(POST_CALCULATED);
}

value_t get_is_cost_calculated(post_t& post) {
  return post.has_flags(POST_COST_CALCULATED);
}

/** @brief True if the posting is virtual (parenthesized account). */
value_t get_virtual(post_t& post) {
  return post.has_flags(POST_VIRTUAL);
}

/** @brief True if the posting is real (not virtual). */
value_t get_real(post_t& post) {
  return !post.has_flags(POST_VIRTUAL);
}

/** @brief Return the parent transaction as a scope value. */
value_t get_xact(post_t& post) {
  return scope_value(post.xact);
}

value_t get_xact_id(post_t& post) {
  return static_cast<long>(post.xact_id());
}

/** @brief Return the transaction code, or NULL_VALUE if none. */
value_t get_code(post_t& post) {
  if (post.xact && post.xact->code)
    return string_value(*post.xact->code);
  else
    return NULL_VALUE;
}

value_t get_payee(post_t& post) {
  return string_value(post.payee());
}

/**
 * @brief Combine the posting's note with its transaction's note.
 *
 * If both exist, they are joined with a newline.  This mirrors the
 * tag inheritance model: posting notes supplement transaction notes.
 */
value_t get_note(post_t& post) {
  if (post.note || (post.xact && post.xact->note)) {
    string note = post.note ? *post.note : empty_string;
    if (post.xact && post.xact->note) {
      if (!note.empty())
        note += '\n';
      note += *post.xact->note;
    }
    return string_value(note);
  } else {
    return NULL_VALUE;
  }
}

value_t get_magnitude(post_t& post) {
  if (post.xact)
    return post.xact->magnitude();
  else
    return NULL_VALUE;
}

/**
 * @brief Return the posting amount, respecting compound value overrides.
 *
 * If POST_EXT_COMPOUND is set, returns compound_value (which may be a
 * balance or other complex value set by a filter).  Otherwise returns
 * the raw posting amount, or 0 if null.
 */
value_t get_amount(post_t& post) {
  if (post.has_xdata() && post.xdata().has_flags(POST_EXT_COMPOUND))
    return post.xdata().compound_value;
  else if (post.amount.is_null())
    return 0L;
  else
    return post.amount;
}

value_t get_use_direct_amount(post_t& post) {
  return post.has_xdata() && post.xdata().has_flags(POST_EXT_DIRECT_AMT);
}

/**
 * @brief Return the commodity of the posting amount.
 *
 * Accepts an optional amount argument; if provided, returns that amount's
 * commodity instead of the posting's.  Annotations are stripped to return
 * the base commodity symbol.
 */
value_t get_commodity(call_scope_t& args) {
  if (args.has<amount_t>(0)) {
    value_t val(args[0]);
    if (val.is_balance()) {
      const balance_t& bal(val.as_balance());
      if (bal.single_amount())
        return bal.amounts.begin()->second.commodity().strip_annotations(keep_details_t{});
      return string_value(empty_string);
    }
    return args.get<amount_t>(0).commodity().strip_annotations(keep_details_t{});
  } else {
    post_t& post(args.context<post_t>());
    if (post.has_xdata() && post.xdata().has_flags(POST_EXT_COMPOUND))
      return post.xdata().compound_value.to_amount().commodity().strip_annotations(
          keep_details_t{});
    else
      return post.amount.commodity().strip_annotations(keep_details_t{});
  }
}

value_t get_commodity_is_primary(post_t& post) {
  if (post.has_xdata() && post.xdata().has_flags(POST_EXT_COMPOUND))
    return post.xdata().compound_value.to_amount().commodity().has_flags(COMMODITY_PRIMARY);
  else
    return post.amount.commodity().has_flags(COMMODITY_PRIMARY);
}

value_t get_has_cost(post_t& post) {
  return post.cost ? true : false;
}

/**
 * @brief Return the cost of the posting, falling back to the amount.
 *
 * If a cost is recorded (currency conversion or lot price), returns it.
 * Otherwise returns the compound value or raw amount, since a posting
 * in a single commodity has cost equal to amount.
 */
value_t get_cost(post_t& post) {
  if (post.cost)
    return *post.cost;
  else if (post.has_xdata() && post.xdata().has_flags(POST_EXT_COMPOUND))
    return post.xdata().compound_value;
  else if (post.amount.is_null())
    return 0L;
  else
    return post.amount;
}

/**
 * @brief Return the per-unit price from the amount's annotation.
 *
 * If the amount has a lot price annotation, returns that price.
 * Otherwise falls back to the cost (which equals the amount when
 * there is no currency conversion).
 */
value_t get_price(post_t& post) {
  if (post.amount.is_null())
    return 0L;
  if (post.amount.has_annotation() && post.amount.annotation().price)
    return *post.amount.price();
  else
    return get_cost(post);
}

/**
 * @brief Return the running total, or the amount if no total is accumulated.
 */
value_t get_total(post_t& post) {
  if (post.xdata_ && !post.xdata_->total.is_null())
    return post.xdata_->total;
  else if (post.amount.is_null())
    return 0L;
  else
    return post.amount;
}

value_t get_count(post_t& post) {
  if (post.xdata_)
    return long(post.xdata_->count);
  else
    return 1L;
}

/**
 * @brief Return the posting's account, with optional formatting.
 *
 * With no arguments, returns the full account name (or the account as a
 * scope when the type context is SCOPE).  With a numeric argument > 2,
 * truncates the account name to that width.  With a string or mask
 * argument, looks up a different account by name or pattern.
 */
value_t get_account(call_scope_t& args) {
  post_t& post(args.context<post_t>());
  account_t& account(*post.reported_account());
  string name;

  if (args.has(0)) {
    if (args[0].is_long()) {
      if (args.get<long>(0) > 2)
        name =
            format_t::truncate(account.fullname(), static_cast<std::size_t>(args.get<long>(0) - 2),
                               /* account_abbrev_length= */ 2);
      else
        name = account.fullname();
    } else {
      account_t* acct = nullptr;
      account_t* master = &account;
      while (master->parent)
        master = master->parent;

      if (args[0].is_string()) {
        name = args.get<string>(0);
        acct = master->find_account(name, false);
      } else if (args[0].is_mask()) {
        name = args.get<mask_t>(0).str();
        acct = master->find_account_re(name);
      } else {
        throw_(std::runtime_error,
               _f("Expected string or mask for argument 1, but received %1%") % args[0].label());
      }

      if (!acct)
        throw_(std::runtime_error, _f("Could not find an account matching '%1%'") % args[0]);

      return value_t(static_cast<scope_t*>(acct));
    }
  } else if (args.type_context() == value_t::SCOPE) {
    return scope_value(&account);
  } else {
    name = account.fullname();
  }
  return string_value(name);
}

/**
 * @brief Return the display account name, decorated with virtual indicators.
 *
 * Virtual postings are wrapped in parentheses, balanced virtual postings
 * in brackets, matching the journal syntax the user originally wrote.
 */
value_t get_display_account(call_scope_t& args) {
  value_t acct = get_account(args);
  if (acct.is_string()) {
    post_t& post(args.context<post_t>());
    if (post.has_flags(POST_VIRTUAL)) {
      if (post.must_balance())
        acct = string_value(string("[") + acct.as_string() + "]");
      else
        acct = string_value(string("(") + acct.as_string() + ")");
    }
  }
  return acct;
}

value_t get_account_id(post_t& post) {
  return static_cast<long>(post.account_id());
}

value_t get_account_base(post_t& post) {
  return string_value(post.reported_account()->name);
}

value_t get_account_depth(post_t& post) {
  return long(post.reported_account()->depth);
}

value_t get_value_date(post_t& post) {
  if (post.has_xdata()) {
    post_t::xdata_t& xdata(post.xdata());
    if (!xdata.value_date.is_not_a_date())
      return xdata.value_date;
  }
  return post.date();
}
value_t get_datetime(post_t& post) {
  return (!post.xdata().datetime.is_not_a_date_time() ? post.xdata().datetime
                                                      : datetime_t(post.date()));
}
value_t get_checkin(post_t& post) {
  return post.checkin ? *post.checkin : NULL_VALUE;
}
value_t get_checkout(post_t& post) {
  return post.checkout ? *post.checkout : NULL_VALUE;
}

/** @brief Adapt a simple post_t& getter into a call_scope_t functor. */
template <value_t (*Func)(post_t&)>
value_t get_wrapper(call_scope_t& scope) {
  return (*Func)(find_scope<post_t>(scope));
}

/**
 * @brief Test whether any posting in the same transaction satisfies an expression.
 *
 * Iterates over all postings in the parent transaction and evaluates the
 * given expression in each posting's scope.  Returns true as soon as one
 * matches.
 *
 * An optional second argument (false) excludes the current posting from
 * consideration, which is useful for queries like "does any *other*
 * posting in this transaction match?"
 */
value_t fn_any(call_scope_t& args) {
  post_t& post(args.context<post_t>());
  expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

  if (!post.xact)
    return false;

  for (post_t* p : post.xact->posts) {
    bind_scope_t bound_scope(args, *p);
    if (p == &post && args.has(1) && !args.get<bool>(1)) {
      // If the user specifies any(EXPR, false), and the context is a
      // posting, then that posting isn't considered by the test.
      ; // skip it
    } else if (expr->calc(bound_scope, args.locus, args.depth).to_boolean()) {
      return true;
    }
  }
  return false;
}

/**
 * @brief Test whether all postings in the same transaction satisfy an expression.
 *
 * Like fn_any, but returns false as soon as any posting fails the test.
 * Returns true if no transaction exists (vacuous truth).
 *
 * An optional second argument (false) excludes the current posting from
 * consideration.
 */
value_t fn_all(call_scope_t& args) {
  post_t& post(args.context<post_t>());
  expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

  if (!post.xact)
    return true;

  for (post_t* p : post.xact->posts) {
    bind_scope_t bound_scope(args, *p);
    if (p == &post && args.has(1) && !args.get<bool>(1)) {
      // If the user specifies any(EXPR, false), and the context is a
      // posting, then that posting isn't considered by the test.
      ; // skip it
    } else if (!expr->calc(bound_scope, args.locus, args.depth).to_boolean()) {
      return false;
    }
  }
  return true;
}
} // namespace

/**
 * @brief Map expression names to getter implementations for the posting scope.
 *
 * This dispatch table maps expression identifiers to the appropriate getter
 * functions defined above.  It uses a first-character switch for fast lookup.
 *
 * Single-character aliases (used in format strings and expressions):
 *   - `a` = amount
 *   - `b` = cost (mnemonic: "basis")
 *   - `n` / `N` = count
 *   - `O` = total
 *   - `R` = real (not virtual)
 *
 * Falls through to item_t::lookup() for inherited bindings such as date,
 * note, state, has_tag, etc.
 */
expr_t::ptr_op_t post_t::lookup(const symbol_t::kind_t kind, const string& name) {
  if (kind != symbol_t::FUNCTION)
    return item_t::lookup(kind, name);

  // NOLINTBEGIN(bugprone-branch-clone)
  switch (name[0]) {
  case 'a':
    if (name[1] == '\0' || name == "amount")
      return WRAP_FUNCTOR(get_wrapper<&get_amount>);
    else if (name == "account")
      return WRAP_FUNCTOR(get_account);
    else if (name == "account_base")
      return WRAP_FUNCTOR(get_wrapper<&get_account_base>);
    else if (name == "account_id")
      return WRAP_FUNCTOR(get_wrapper<&get_account_id>);
    else if (name == "any")
      return WRAP_FUNCTOR(&fn_any);
    else if (name == "all")
      return WRAP_FUNCTOR(&fn_all);
    break;

  case 'b':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_cost>);
    break;

  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    else if (name == "cost")
      return WRAP_FUNCTOR(get_wrapper<&get_cost>);
    else if (name == "cost_calculated")
      return WRAP_FUNCTOR(get_wrapper<&get_is_cost_calculated>);
    else if (name == "count")
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    else if (name == "calculated")
      return WRAP_FUNCTOR(get_wrapper<&get_is_calculated>);
    else if (name == "commodity")
      return WRAP_FUNCTOR(&get_commodity);
    else if (name == "checkin")
      return WRAP_FUNCTOR(get_wrapper<&get_checkin>);
    else if (name == "checkout")
      return WRAP_FUNCTOR(get_wrapper<&get_checkout>);
    break;

  case 'd':
    if (name == "display_account")
      return WRAP_FUNCTOR(get_display_account);
    else if (name == "depth")
      return WRAP_FUNCTOR(get_wrapper<&get_account_depth>);
    else if (name == "datetime")
      return WRAP_FUNCTOR(get_wrapper<&get_datetime>);
    break;

  case 'h':
    if (name == "has_cost")
      return WRAP_FUNCTOR(get_wrapper<&get_has_cost>);
    break;

  case 'i':
    if (name == "index")
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    break;

  case 'm':
    if (name == "magnitude")
      return WRAP_FUNCTOR(get_wrapper<&get_magnitude>);
    break;

  case 'n':
    if (name == "note")
      return WRAP_FUNCTOR(get_wrapper<&get_note>);
    else if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    break;

  case 'p':
    if (name == "post")
      return WRAP_FUNCTOR(get_wrapper<&get_this>);
    else if (name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    else if (name == "primary")
      return WRAP_FUNCTOR(get_wrapper<&get_commodity_is_primary>);
    else if (name == "price")
      return WRAP_FUNCTOR(get_wrapper<&get_price>);
    else if (name == "parent")
      return WRAP_FUNCTOR(get_wrapper<&get_xact>);
    break;

  case 'r':
    if (name == "real")
      return WRAP_FUNCTOR(get_wrapper<&get_real>);
    break;

  case 't':
    if (name == "total")
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    break;

  case 'u':
    if (name == "use_direct_amount")
      return WRAP_FUNCTOR(get_wrapper<&get_use_direct_amount>);
    break;

  case 'v':
    if (name == "virtual")
      return WRAP_FUNCTOR(get_wrapper<&get_virtual>);
    else if (name == "value_date")
      return WRAP_FUNCTOR(get_wrapper<&get_value_date>);
    break;

  case 'x':
    if (name == "xact")
      return WRAP_FUNCTOR(get_wrapper<&get_xact>);
    else if (name == "xact_id")
      return WRAP_FUNCTOR(get_wrapper<&get_xact_id>);
    break;

  case 'N':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    break;

  case 'O':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    break;

  case 'R':
    if (name[1] == '\0')
      return WRAP_FUNCTOR(get_wrapper<&get_real>);
    break;
  }
  // NOLINTEND(bugprone-branch-clone)

  return item_t::lookup(kind, name);
}

/**
 * @brief Evaluate an expression in a scope bound to this posting.
 *
 * Creates a bind_scope_t pairing the given scope with this posting,
 * then calculates the expression.  Long results are promoted to amounts.
 *
 * @param scope  The parent scope (typically the report scope).
 * @param expr   The expression to evaluate.
 * @return The result as an amount_t.
 * @throws amount_error if the result is neither a long nor an amount.
 */
amount_t post_t::resolve_expr(scope_t& scope, expr_t& expr) {
  bind_scope_t bound_scope(scope, *this);
  value_t result(expr.calc(bound_scope));
  if (result.is_long()) {
    return result.to_amount();
  } else {
    if (!result.is_amount())
      throw_(amount_error, _("Amount expressions must result in a simple amount"));
    return result.as_amount();
  }
}

/**
 * @brief Return the 1-based index of this posting within its transaction.
 */
std::size_t post_t::xact_id() const {
  std::size_t id = 1;
  for (post_t* p : xact->posts) {
    if (p == this)
      return id;
    id++;
  }
  assert(false && "Failed to find posting within its transaction");
  return 0;
}

/**
 * @brief Return the 1-based index of this posting within its account.
 */
std::size_t post_t::account_id() const {
  std::size_t id = 1;
  for (post_t* p : account->posts) {
    if (p == this)
      return id;
    id++;
  }
  assert(false && "Failed to find posting within its account");
  return 0;
}

/**
 * @brief Validate the internal consistency of this posting.
 *
 * Checks that the posting has a parent transaction, that it can be
 * found within that transaction's posting list, that it has an account,
 * and that the amount and cost (if present) are valid.
 */
bool post_t::valid() const {
  if (!xact) {
    DEBUG("ledger.validate", "post_t: ! xact");
    return false;
  }

  posts_list::const_iterator i = std::find(xact->posts.begin(), xact->posts.end(), this);
  if (i == xact->posts.end()) {
    DEBUG("ledger.validate", "post_t: ! found");
    return false;
  }

  if (!account) {
    DEBUG("ledger.validate", "post_t: ! account");
    return false;
  }

  if (!amount.valid()) {
    DEBUG("ledger.validate", "post_t: ! amount.valid()");
    return false;
  }

  if (cost) {
    if (!cost->valid()) {
      DEBUG("ledger.validate", "post_t: cost && ! cost->valid()");
      return false;
    }
    if (!cost->keep_precision()) {
      DEBUG("ledger.validate", "post_t: ! cost->keep_precision()");
      return false;
    }
  }

  return true;
}

/*--- Value Accumulation ---*/
// add_to_value is called by the reporting pipeline to accumulate posting
// values into running totals.  The logic has several branches to handle
// compound values, fast-path amount reads, expression evaluation, and
// visited-value fallbacks.

/**
 * @brief Accumulate this posting's value into a running total.
 *
 * The evaluation priority is:
 *   1. If POST_EXT_COMPOUND is set, use the compound_value (set by filters
 *      that transform the posting value, e.g., market valuation).
 *   2. If an expression is provided and its fast_path is POST_AMOUNT,
 *      read the amount directly -- this avoids creating scope objects
 *      and evaluating through the AST interpreter for the common case.
 *   3. Otherwise evaluate the expression in a bound scope.
 *   4. If no expression and the posting was visited, use visited_value.
 *   5. Fall back to the raw posting amount.
 *
 * @param value  The accumulator to add this posting's value to.
 * @param expr   Optional expression to evaluate (e.g., `amount` or `cost`).
 */
void post_t::add_to_value(value_t& value, const optional<expr_t&>& expr) const {
  // NOLINTBEGIN(bugprone-branch-clone)
  if (xdata_ && xdata_->has_flags(POST_EXT_COMPOUND)) {
    if (!xdata_->compound_value.is_null())
      add_or_set_value(value, xdata_->compound_value);
  } else if (expr) {
    if (expr->fast_path() == expr_t::fast_path_t::POST_AMOUNT) {
      // Fast path: directly read the post amount without creating
      // scope objects or evaluating through the AST interpreter.
      // This is equivalent to get_amount(post) when POST_EXT_COMPOUND
      // is not set (which is guaranteed by the if-branch above).
      if (amount.is_null())
        add_or_set_value(value, value_t(0L));
      else
        add_or_set_value(value, amount);
    } else {
      scope_t* ctx = expr->get_context();
      bind_scope_t bound_scope(*ctx, const_cast<post_t&>(*this));
#if 1
      value_t temp(expr->calc(bound_scope));
      add_or_set_value(value, temp);
      expr->set_context(ctx);
#else
      if (!xdata_)
        xdata_ = xdata_t();
      xdata_->value = expr->calc(bound_scope);
      xdata_->add_flags(POST_EXT_COMPOUND);

      add_or_set_value(value, xdata_->value);
#endif
    }
  } else if (xdata_ && xdata_->has_flags(POST_EXT_VISITED) && !xdata_->visited_value.is_null()) {
    add_or_set_value(value, xdata_->visited_value);
  } else {
    add_or_set_value(value, amount);
  }
  // NOLINTEND(bugprone-branch-clone)
}

/**
 * @brief Set the account used for display and register it for reporting.
 *
 * Overrides the account shown in reports (stored in xdata) and adds this
 * posting to the target account's reported_posts list.
 */
void post_t::set_reported_account(account_t* acct) {
  xdata().account = acct;
  acct->xdata().reported_posts.push_back(this);
}

/*--- Valuation Expression Cascade ---*/
// extend_post attaches a valuation expression to the posting's commodity
// annotation.  This is the mechanism behind --market, --exchange, and the
// Value: metadata tag.  The cascade checks four sources in priority order,
// stopping at the first one found.

/**
 * @brief Attach a valuation expression to the posting's commodity.
 *
 * When the user runs `--market` or uses a `Value:` tag, this function
 * determines which valuation expression to attach to the commodity
 * annotation.  The cascade checks four sources:
 *
 *   1. `Value:` metadata tag on the posting (or inherited from the
 *      transaction).  When a typed expression (`Value::`) returns an
 *      amount, it is divided by the posting quantity to convert from
 *      total value to per-unit price.
 *   2. The account's value_expr.
 *   3. The commodity's value_expr.
 *   4. The journal's value_expr.
 *
 * If the commodity already has a valuation expression in its annotation,
 * this function does nothing.  Otherwise, the selected expression is
 * either set on the existing annotation or wrapped into a new annotated
 * commodity created via the commodity pool.
 *
 * @param post     The posting whose commodity should be annotated.
 * @param journal  The journal providing the fallback value_expr.
 */
void extend_post(post_t& post, journal_t& journal) {
  commodity_t& comm(post.amount.commodity());

  annotation_t* details = (comm.has_annotation() ? &as_annotated_commodity(comm).details : nullptr);

  if (!details || !details->value_expr) {
    std::optional<expr_t> value_expr;

    if (std::optional<value_t> data = post.get_tag(_("Value"))) {
      // When the Value:: tag uses a typed expression (::), the expression is
      // evaluated at parse time and the result is the total converted value
      // (e.g., market(amount, post.date, exchange) returns amount * rate).
      // Since value_expr is used as a per-unit price, we must divide by the
      // posting quantity to convert from total value to per-unit price.
      if (data->is_amount() && !post.amount.is_zero()) {
        amount_t per_unit(data->as_amount());
        per_unit /= post.amount.number();
        value_expr = expr_t(per_unit.to_string());
      } else {
        value_expr = expr_t(data->to_string());
      }
    }

    if (!value_expr)
      value_expr = post.account->value_expr;

    if (!value_expr)
      value_expr = post.amount.commodity().value_expr();

    if (!value_expr)
      value_expr = journal.value_expr;

    if (value_expr) {
      if (!details) {
        annotation_t new_details;
        new_details.value_expr = value_expr;

        commodity_t* new_comm = commodity_pool_t::current_pool->find_or_create(comm, new_details);
        post.amount.set_commodity(*new_comm);
      } else {
        details->value_expr = value_expr;
      }
    }
  }
}

/*--- XML Serialization ---*/
// put_post serializes a posting into an XML property tree for the xml
// output format.  It emits all fields that have meaningful values.

/**
 * @brief Serialize a posting to an XML property tree.
 *
 * Outputs the posting's state (cleared/pending), virtual flag, dates,
 * payee override, account (with hex address reference), amount or
 * compound value, cost, balance assertion/assignment, note, metadata,
 * and running total.
 *
 * @param st    The property tree node to populate.
 * @param post  The posting to serialize.
 */
void put_post(property_tree::ptree& st, const post_t& post) {
  if (post.state() == item_t::CLEARED)
    st.put("<xmlattr>.state", "cleared");
  else if (post.state() == item_t::PENDING)
    st.put("<xmlattr>.state", "pending");

  if (post.has_flags(POST_VIRTUAL))
    st.put("<xmlattr>.virtual", "true");
  if (post.has_flags(ITEM_GENERATED))
    st.put("<xmlattr>.generated", "true");

  if (post._date)
    put_date(st.put("date", ""), *post._date);
  if (post._date_aux)
    put_date(st.put("aux-date", ""), *post._date_aux);

  if (post.payee_from_tag() != "")
    st.put("payee", post.payee_from_tag());

  if (post.account) {
    property_tree::ptree& t(st.put("account", ""));

    std::ostringstream buf;
    buf.width(sizeof(intptr_t) * 2);
    buf.fill('0');
    buf << std::hex << reinterpret_cast<intptr_t>(post.account);

    t.put("<xmlattr>.ref", buf.str());
    t.put("name", post.account->fullname());
  }

  {
    property_tree::ptree& t(st.put("post-amount", ""));
    if (post.has_xdata() && post.xdata().has_flags(POST_EXT_COMPOUND))
      put_value(t, post.xdata().compound_value);
    else
      put_amount(t.put("amount", ""), post.amount);
  }

  if (post.cost)
    put_amount(st.put("cost", ""), *post.cost);

  if (post.assigned_amount) {
    if (post.has_flags(POST_CALCULATED))
      put_amount(st.put("balance-assertion", ""), *post.assigned_amount);
    else
      put_amount(st.put("balance-assignment", ""), *post.assigned_amount);
  }

  if (post.note)
    st.put("note", *post.note);

  if (post.metadata)
    put_metadata(st.put("metadata", ""), *post.metadata);

  if (post.xdata_ && !post.xdata_->total.is_null())
    put_value(st.put("total", ""), post.xdata_->total);
}

} // namespace ledger
