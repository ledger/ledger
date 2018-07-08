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

#include <system.hh>

#include "post.h"
#include "xact.h"
#include "account.h"
#include "journal.h"
#include "format.h"
#include "pool.h"

namespace ledger {

bool post_t::has_tag(const string& tag, bool inherit) const
{
  if (item_t::has_tag(tag))
    return true;
  if (inherit && xact)
    return xact->has_tag(tag);
  return false;
}

bool post_t::has_tag(const mask_t&           tag_mask,
                     const optional<mask_t>& value_mask,
                     bool                    inherit) const
{
  if (item_t::has_tag(tag_mask, value_mask))
    return true;
  if (inherit && xact)
    return xact->has_tag(tag_mask, value_mask);
  return false;
}

optional<value_t> post_t::get_tag(const string& tag, bool inherit) const
{
  if (optional<value_t> value = item_t::get_tag(tag))
    return value;
  if (inherit && xact)
    return xact->get_tag(tag);
  return none;
}

optional<value_t> post_t::get_tag(const mask_t&           tag_mask,
                                  const optional<mask_t>& value_mask,
                                  bool                    inherit) const
{
  if (optional<value_t> value = item_t::get_tag(tag_mask, value_mask))
    return value;
  if (inherit && xact)
    return xact->get_tag(tag_mask, value_mask);
  return none;
}

date_t post_t::value_date() const
{
  if (xdata_ && is_valid(xdata_->value_date))
    return xdata_->value_date;
  return date();
}

date_t post_t::date() const
{
  if (xdata_ && is_valid(xdata_->date))
    return xdata_->date;

  if (item_t::use_aux_date) {
    if (optional<date_t> aux = aux_date())
      return *aux;
  }

  return primary_date();
}

date_t post_t::primary_date() const
{
  if (xdata_ && is_valid(xdata_->date))
    return xdata_->date;

  if (! _date) {
    assert(xact);
    return xact->date();
  }
  return *_date;
}

optional<date_t> post_t::aux_date() const
{
  optional<date_t> date = item_t::aux_date();
  if (! date && xact)
    return xact->aux_date();
  return date;
}

string post_t::payee() const
{
  if (optional<value_t> post_payee = get_tag(_("Payee")))
    return post_payee->as_string();
  else
    return xact->payee;
}

namespace {
  value_t get_this(post_t& post) {
    return scope_value(&post);
  }

  value_t get_is_calculated(post_t& post) {
    return post.has_flags(POST_CALCULATED);
  }

  value_t get_is_cost_calculated(post_t& post) {
    return post.has_flags(POST_COST_CALCULATED);
  }

  value_t get_virtual(post_t& post) {
    return post.has_flags(POST_VIRTUAL);
  }

  value_t get_real(post_t& post) {
    return ! post.has_flags(POST_VIRTUAL);
  }

  value_t get_xact(post_t& post) {
    return scope_value(post.xact);
  }

  value_t get_xact_id(post_t& post) {
    return static_cast<long>(post.xact_id());
  }

  value_t get_code(post_t& post) {
    if (post.xact->code)
      return string_value(*post.xact->code);
    else
      return NULL_VALUE;
  }

  value_t get_payee(post_t& post) {
    return string_value(post.payee());
  }

  value_t get_note(post_t& post)
  {
    if (post.note || post.xact->note) {
      string note = post.note ? *post.note : empty_string;
      note += post.xact->note ? *post.xact->note : empty_string;
      return string_value(note);
    } else {
      return NULL_VALUE;
    }
  }

  value_t get_magnitude(post_t& post) {
    return post.xact->magnitude();
  }

  value_t get_amount(post_t& post)
  {
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

  value_t get_commodity(call_scope_t& args)
  {
    if (args.has<amount_t>(0)) {
      return string_value(args.get<amount_t>(0).commodity().symbol());
    } else {
      post_t& post(args.context<post_t>());
      if (post.has_xdata() && post.xdata().has_flags(POST_EXT_COMPOUND))
        return string_value(post.xdata().compound_value.to_amount()
                            .commodity().symbol());
      else
        return string_value(post.amount.commodity().symbol());
    }
  }

  value_t get_commodity_is_primary(post_t& post)
  {
    if (post.has_xdata() &&
        post.xdata().has_flags(POST_EXT_COMPOUND))
      return post.xdata().compound_value.to_amount()
        .commodity().has_flags(COMMODITY_PRIMARY);
    else
      return post.amount.commodity().has_flags(COMMODITY_PRIMARY);
  }

  value_t get_has_cost(post_t& post) {
    return post.cost ? true : false;
  }

  value_t get_cost(post_t& post) {
    if (post.cost)
      return *post.cost;
    else if (post.has_xdata() &&
             post.xdata().has_flags(POST_EXT_COMPOUND))
      return post.xdata().compound_value;
    else if (post.amount.is_null())
      return 0L;
    else
      return post.amount;
  }

  value_t get_price(post_t& post) {
    if (post.amount.is_null())
      return 0L;
    if (post.amount.has_annotation() && post.amount.annotation().price)
      return *post.amount.price();
    else
      return get_cost(post);
  }

  value_t get_total(post_t& post) {
    if (post.xdata_ && ! post.xdata_->total.is_null())
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

  value_t get_account(call_scope_t& args)
  {
    post_t&    post(args.context<post_t>());
    account_t& account(*post.reported_account());
    string     name;

    if (args.has(0)) {
      if (args[0].is_long()) {
        if (args.get<long>(0) > 2)
          name = format_t::truncate
            (account.fullname(), static_cast<std::size_t>(args.get<long>(0) - 2),
             /* account_abbrev_length= */ 2);
        else
          name = account.fullname();
      } else {
        account_t * acct   = NULL;
        account_t * master = &account;
        while (master->parent)
          master = master->parent;

        if (args[0].is_string()) {
          name = args.get<string>(0);
          acct = master->find_account(name, false);
        }
        else if (args[0].is_mask()) {
          name = args.get<mask_t>(0).str();
          acct = master->find_account_re(name);
        }
        else {
          throw_(std::runtime_error,
                 _f("Expected string or mask for argument 1, but received %1%")
                 % args[0].label());
        }

        if (! acct)
          throw_(std::runtime_error,
                 _f("Could not find an account matching '%1%'") % args[0]);

        return value_t(static_cast<scope_t *>(acct));
      }
    }
    else if (args.type_context() == value_t::SCOPE) {
      return scope_value(&account);
    }
    else {
      name = account.fullname();
    }
    return string_value(name);
  }

  value_t get_display_account(call_scope_t& args)
  {
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
      if (! xdata.value_date.is_not_a_date())
        return xdata.value_date;
    }
    return post.date();
  }
  value_t get_datetime(post_t& post) {
    return (! post.xdata().datetime.is_not_a_date_time() ?
            post.xdata().datetime : datetime_t(post.date()));
  }
  value_t get_checkin(post_t& post) {
    return post.checkin ? *post.checkin : NULL_VALUE;
  }
  value_t get_checkout(post_t& post) {
    return post.checkout ? *post.checkout : NULL_VALUE;
  }

  template <value_t (*Func)(post_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<post_t>(scope));
  }

  value_t fn_any(call_scope_t& args)
  {
    post_t& post(args.context<post_t>());
    expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

    foreach (post_t * p, post.xact->posts) {
      bind_scope_t bound_scope(args, *p);
      if (p == &post && args.has<expr_t::ptr_op_t>(1) &&
          ! args.get<expr_t::ptr_op_t>(1)
            ->calc(bound_scope, args.locus, args.depth).to_boolean()) {
        // If the user specifies any(EXPR, false), and the context is a
        // posting, then that posting isn't considered by the test.
        ;                       // skip it
      }
      else if (expr->calc(bound_scope, args.locus,
                          args.depth).to_boolean()) {
        return true;
      }
    }
    return false;
  }

  value_t fn_all(call_scope_t& args)
  {
    post_t& post(args.context<post_t>());
    expr_t::ptr_op_t expr(args.get<expr_t::ptr_op_t>(0));

    foreach (post_t * p, post.xact->posts) {
      bind_scope_t bound_scope(args, *p);
      if (p == &post && args.has<expr_t::ptr_op_t>(1) &&
          ! args.get<expr_t::ptr_op_t>(1)
            ->calc(bound_scope, args.locus, args.depth).to_boolean()) {
        // If the user specifies any(EXPR, false), and the context is a
        // posting, then that posting isn't considered by the test.
        ;                       // skip it
      }
      else if (! expr->calc(bound_scope, args.locus,
                            args.depth).to_boolean()) {
        return false;
      }
    }
    return true;
  }
}

expr_t::ptr_op_t post_t::lookup(const symbol_t::kind_t kind,
                                const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return item_t::lookup(kind, name);

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

  return item_t::lookup(kind, name);
}

amount_t post_t::resolve_expr(scope_t& scope, expr_t& expr)
{
  bind_scope_t bound_scope(scope, *this);
  value_t result(expr.calc(bound_scope));
  if (result.is_long()) {
    return result.to_amount();
  } else {
    if (! result.is_amount())
      throw_(amount_error,
             _("Amount expressions must result in a simple amount"));
    return result.as_amount();
  }
}

std::size_t post_t::xact_id() const
{
  std::size_t id = 1;
  foreach (post_t * p, xact->posts) {
    if (p == this)
      return id;
    id++;
  }
  assert("Failed to find posting within its transaction" == NULL);
  return 0;
}

std::size_t post_t::account_id() const
{
  std::size_t id = 1;
  foreach (post_t * p, account->posts) {
    if (p == this)
      return id;
    id++;
  }
  assert("Failed to find posting within its transaction" == NULL);
  return 0;
}

bool post_t::valid() const
{
  if (! xact) {
    DEBUG("ledger.validate", "post_t: ! xact");
    return false;
  }

  posts_list::const_iterator i =
    std::find(xact->posts.begin(),
              xact->posts.end(), this);
  if (i == xact->posts.end()) {
    DEBUG("ledger.validate", "post_t: ! found");
    return false;
  }

  if (! account) {
    DEBUG("ledger.validate", "post_t: ! account");
    return false;
  }

  if (! amount.valid()) {
    DEBUG("ledger.validate", "post_t: ! amount.valid()");
    return false;
  }

  if (cost) {
    if (! cost->valid()) {
      DEBUG("ledger.validate", "post_t: cost && ! cost->valid()");
      return false;
    }
    if (! cost->keep_precision()) {
      DEBUG("ledger.validate", "post_t: ! cost->keep_precision()");
      return false;
    }
  }

  return true;
}

void post_t::add_to_value(value_t& value, const optional<expr_t&>& expr) const
{
  if (xdata_ && xdata_->has_flags(POST_EXT_COMPOUND)) {
    if (! xdata_->compound_value.is_null())
      add_or_set_value(value, xdata_->compound_value);
  }
  else if (expr) {
    bind_scope_t bound_scope(*expr->get_context(),
                             const_cast<post_t&>(*this));
#if 1
    value_t temp(expr->calc(bound_scope));
    add_or_set_value(value, temp);
#else
    if (! xdata_) xdata_ = xdata_t();
    xdata_->value = expr->calc(bound_scope);
    xdata_->add_flags(POST_EXT_COMPOUND);

    add_or_set_value(value, xdata_->value);
#endif
  }
  else if (xdata_ && xdata_->has_flags(POST_EXT_VISITED) &&
           ! xdata_->visited_value.is_null()) {
    add_or_set_value(value, xdata_->visited_value);
  }
  else {
    add_or_set_value(value, amount);
  }
}

void post_t::set_reported_account(account_t * acct)
{
  xdata().account = acct;
  acct->xdata().reported_posts.push_back(this);
}

void extend_post(post_t& post, journal_t& journal)
{
  commodity_t& comm(post.amount.commodity());

  annotation_t * details =
    (comm.has_annotation() ?
     &as_annotated_commodity(comm).details : NULL);

  if (! details || ! details->value_expr) {
    optional<expr_t> value_expr;

    if (optional<value_t> data = post.get_tag(_("Value")))
      value_expr = expr_t(data->to_string());

    if (! value_expr)
      value_expr = post.account->value_expr;

    if (! value_expr)
      value_expr = post.amount.commodity().value_expr();

    if (! value_expr)
      value_expr = journal.value_expr;

    if (value_expr) {
      if (! details) {
        annotation_t new_details;
        new_details.value_expr = value_expr;

        commodity_t * new_comm =
          commodity_pool_t::current_pool->find_or_create(comm, new_details);
        post.amount.set_commodity(*new_comm);
      } else {
        details->value_expr = value_expr;
      }
    }
  }
}

void put_post(property_tree::ptree& st, const post_t& post)
{
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

  if (post.account) {
    property_tree::ptree& t(st.put("account", ""));

    std::ostringstream buf;
    buf.width(sizeof(unsigned long) * 2);
    buf.fill('0');
    buf << std::hex << reinterpret_cast<unsigned long>(post.account);

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
    put_metadata(st.put("metadata", ""),  *post.metadata);

  if (post.xdata_ && ! post.xdata_->total.is_null())
    put_value(st.put("total", ""), post.xdata_->total);
}

} // namespace ledger
