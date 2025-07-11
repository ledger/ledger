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

#include "filters.h"
#include "iterators.h"
#include "journal.h"
#include "report.h"
#include "compare.h"
#include "pool.h"

namespace ledger {

void post_splitter::print_title(const value_t& val)
{
  if (! report.HANDLED(no_titles)) {
    std::ostringstream buf;
    val.print(buf);
    post_chain->title(buf.str());
  }
}

void post_splitter::flush()
{
  foreach (value_to_posts_map::value_type& pair, posts_map) {
    preflush_func(pair.first);

    foreach (post_t * post, pair.second)
      (*post_chain)(*post);

    post_chain->flush();
    post_chain->clear();

    if (postflush_func)
      (*postflush_func)(pair.first);
  }
}

void post_splitter::operator()(post_t& post)
{
  bind_scope_t bound_scope(report, post);
  value_t      result(group_by_expr.calc(bound_scope));

  if (! result.is_null()) {
    value_to_posts_map::iterator i = posts_map.find(result);
    if (i != posts_map.end()) {
      (*i).second.push_back(&post);
    } else {
      std::pair<value_to_posts_map::iterator, bool> inserted
        = posts_map.insert(value_to_posts_map::value_type(result, posts_list()));
      assert(inserted.second);
      (*inserted.first).second.push_back(&post);
    }
  }
}

void truncate_xacts::flush()
{
  if (! posts.size())
    return;

  xact_t * xact = (*posts.begin())->xact;

  int l = 0;
  foreach (post_t * post, posts)
    if (xact != post->xact) {
      l++;
      xact = post->xact;
    }
  l++;

  xact = (*posts.begin())->xact;

  int i = 0;
  foreach (post_t * post, posts) {
    if (xact != post->xact) {
      xact = post->xact;
      i++;
    }

    bool print = false;
    if (head_count) {
      if (head_count > 0 && i < head_count)
        print = true;
      else if (head_count < 0 && i >= - head_count)
        print = true;
    }

    if (! print && tail_count) {
      if (tail_count > 0 && l - i <= tail_count)
        print = true;
      else if (tail_count < 0 && l - i > - tail_count)
        print = true;
    }

    if (print)
      item_handler<post_t>::operator()(*post);
  }
  posts.clear();

  item_handler<post_t>::flush();
}

void truncate_xacts::operator()(post_t& post)
{
  if (completed)
    return;

  if (last_xact != post.xact) {
    if (last_xact)
      xacts_seen++;
    last_xact = post.xact;
  }

  if (tail_count == 0 && head_count > 0 &&
      static_cast<int>(xacts_seen) >= head_count) {
    flush();
    completed = true;
    return;
  }

  posts.push_back(&post);
}

void sort_posts::post_accumulated_posts()
{
  std::stable_sort(posts.begin(), posts.end(),
                   compare_items<post_t>(sort_order, report));

  foreach (post_t * post, posts) {
    post->xdata().drop_flags(POST_EXT_SORT_CALC);
    item_handler<post_t>::operator()(*post);
  }

  posts.clear();
}

namespace {
  void split_string(const string& str, const char ch,
                    std::list<string>& strings)
  {
    const char * b = str.c_str();
    for (const char * p = b; *p; p++) {
      if (*p == ch) {
        strings.push_back(string(b, static_cast<std::string::size_type>(p - b)));
        b = p + 1;
      }
    }
    strings.push_back(string(b));
  }

  account_t * create_temp_account_from_path(std::list<string>& account_names,
                                            temporaries_t&     temps,
                                            account_t *        master)
  {
    account_t * new_account = NULL;
    foreach (const string& name, account_names) {
      if (new_account) {
        new_account = new_account->find_account(name);
      } else {
        new_account = master->find_account(name, false);
        if (! new_account)
          new_account = &temps.create_account(name, master);
      }
    }

    assert(new_account != NULL);
    return new_account;
  }
}

void anonymize_posts::render_commodity(amount_t& amt)
{
  commodity_t& comm(amt.commodity());

  std::size_t id;
  bool newly_added = false;

  commodity_index_map::iterator i = comms.find(&comm);
  if (i == comms.end()) {
    id = next_comm_id++;
    newly_added = true;
    comms.insert(commodity_index_map::value_type(&comm, id));
  } else {
    id = (*i).second;
  }

  std::ostringstream buf;
  do {
    buf << static_cast<char>('A' + (id % 26));
    id /= 26;
  }
  while (id > 0);

  if (amt.has_annotation())
    amt.set_commodity
      (*commodity_pool_t::current_pool->find_or_create(buf.str(),
                                                       amt.annotation()));
  else
    amt.set_commodity
      (*commodity_pool_t::current_pool->find_or_create(buf.str()));

  if (newly_added) {
    amt.commodity().set_flags(comm.flags());
    amt.commodity().set_precision(comm.precision());
  }
}

void anonymize_posts::operator()(post_t& post)
{
  bool         copy_xact_details = false;

  if (last_xact != post.xact) {
    temps.copy_xact(*post.xact);
    last_xact = post.xact;
    copy_xact_details = true;
  }
  xact_t& xact = temps.last_xact();
  xact.code = none;

  if (copy_xact_details) {
    xact.copy_details(*post.xact);

    std::ostringstream buf;
    buf << reinterpret_cast<boost::uintmax_t>(post.xact->payee.c_str())
        << integer_gen() << post.xact->payee.c_str();
    xact.payee = sha1sum(buf.str(), 8);
    xact.note  = none;
  } else {
    xact.journal = post.xact->journal;
  }

  std::list<string> account_names;

  for (account_t * acct = post.account;
       acct;
       acct = acct->parent) {
    std::ostringstream buf;
    buf << integer_gen() << acct << acct->fullname();
    account_names.push_front(sha1sum(buf.str(), 8));
  }

  account_t * new_account =
    create_temp_account_from_path(account_names, temps, xact.journal->master);
  post_t& temp = temps.copy_post(post, xact, new_account);
  temp.note = none;
  temp.add_flags(POST_ANONYMIZED);

  render_commodity(temp.amount);
  if (temp.amount.has_annotation()) {
    temp.amount.annotation().tag = none;
    if (temp.amount.annotation().price)
      render_commodity(*temp.amount.annotation().price);
  }

  if (temp.cost)
    render_commodity(*temp.cost);
  if (temp.assigned_amount)
    render_commodity(*temp.assigned_amount);

  (*handler)(temp);
}

void calc_posts::operator()(post_t& post)
{
  post_t::xdata_t& xdata(post.xdata());

  if (last_post) {
    assert(last_post->has_xdata());
    if (calc_running_total)
      xdata.total = last_post->xdata().total;
    xdata.count = last_post->xdata().count + 1;
  } else {
    xdata.count = 1;
  }

  post.add_to_value(xdata.visited_value, amount_expr);
  xdata.add_flags(POST_EXT_VISITED);

  account_t * acct = post.reported_account();
  acct->xdata().add_flags(ACCOUNT_EXT_VISITED);

  if (calc_running_total)
    add_or_set_value(xdata.total, xdata.visited_value);

  item_handler<post_t>::operator()(post);

  last_post = &post;
}

namespace {
  void handle_value(const value_t&   value,
                    account_t *      account,
                    xact_t *         xact,
                    temporaries_t&   temps,
                    post_handler_ptr handler,
                    const date_t&    date          = date_t(),
                    const bool       act_date_p    = true,
                    const value_t&   total         = value_t(),
                    const bool       direct_amount = false,
                    const bool       mark_visited  = false,
                    const bool       bidir_link    = true)
  {
    post_t& post = temps.create_post(*xact, account, bidir_link);
    post.add_flags(ITEM_GENERATED);

    // If the account for this post is all virtual, then report the post as
    // such.  This allows subtotal reports to show "(Account)" for accounts
    // that contain only virtual posts.
    if (account && account->has_xdata() &&
        account->xdata().has_flags(ACCOUNT_EXT_AUTO_VIRTUALIZE)) {
      if (! account->xdata().has_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS)) {
        post.add_flags(POST_VIRTUAL);
        if (! account->xdata().has_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS))
          post.add_flags(POST_MUST_BALANCE);
      }
    }

    post_t::xdata_t& xdata(post.xdata());

    if (is_valid(date)) {
      if (act_date_p)
        xdata.date = date;
      else
        xdata.value_date = date;
    }

    value_t temp(value);

    switch (value.type()) {
    case value_t::BOOLEAN:
    case value_t::INTEGER:
      temp.in_place_cast(value_t::AMOUNT);
      // fall through...

    case value_t::AMOUNT:
      post.amount = temp.as_amount();
      break;

    case value_t::BALANCE:
    case value_t::SEQUENCE:
      xdata.compound_value = temp;
      xdata.add_flags(POST_EXT_COMPOUND);
      break;

    case value_t::DATETIME:
    case value_t::DATE:
    default:
      assert(false);
      break;
    }

    if (! total.is_null())
      xdata.total = total;

    if (direct_amount)
      xdata.add_flags(POST_EXT_DIRECT_AMT);

    DEBUG("filters.changed_value.rounding", "post.amount = " << post.amount);

    (*handler)(post);

    if (mark_visited) {
      post.xdata().add_flags(POST_EXT_VISITED);
      post.account->xdata().add_flags(ACCOUNT_EXT_VISITED);
    }
  }
}

void collapse_posts::report_subtotal()
{
  if (! count)
    return;

  std::size_t displayed_count = 0;
  foreach (post_t * post, component_posts) {
    bind_scope_t bound_scope(report, *post);
    if (only_predicate(bound_scope) && display_predicate(bound_scope))
      displayed_count++;
  }

  if (collapse_depth == 0 && displayed_count == 1) {
    item_handler<post_t>::operator()(*last_post);
  }
  else if (only_collapse_if_zero && ! subtotal.is_zero()) {
    foreach (post_t * post, component_posts)
      item_handler<post_t>::operator()(*post);
  }
  else {
    date_t earliest_date;
    date_t latest_date;

    foreach (post_t * post, component_posts) {
      date_t date       = post->date();
      date_t value_date = post->value_date();
      if (! is_valid(earliest_date) || date < earliest_date)
        earliest_date = date;
      if (! is_valid(latest_date) || value_date > latest_date)
        latest_date = value_date;
    }

    xact_t& xact = temps.create_xact();
    xact.payee     = last_xact->payee;
    xact._date     = (is_valid(earliest_date) ?
                      earliest_date : last_xact->_date);

    DEBUG("filters.collapse", "Pseudo-xact date = " << *xact._date);
    DEBUG("filters.collapse", "earliest date    = " << earliest_date);
    DEBUG("filters.collapse", "latest date      = " << latest_date);

    foreach (totals_map::value_type& pat, totals) {
      handle_value(/* value=      */ pat.second,
                   /* account=    */ pat.first,
                   /* xact=       */ &xact,
                   /* temps=      */ temps,
                   /* handler=    */ handler,
                   /* date=       */ latest_date,
                   /* act_date_p= */ false);
    }

  }

  totals.clear();
  component_posts.clear();

  last_xact = NULL;
  last_post = NULL;
  subtotal  = 0L;
  count     = 0;
}

value_t& collapse_posts::find_totals(account_t* account)
{
  if (collapse_depth == 0)
    return totals[global_totals_account];

  if (account->depth <= collapse_depth)
    return totals[account];

  //else recurse
  return find_totals(account->parent);
}

void collapse_posts::operator()(post_t& post)
{
  // If we've reached a new xact, report on the subtotal
  // accumulated thus far.

  if (last_xact != post.xact && count > 0)
    report_subtotal();

  post.add_to_value(subtotal, amount_expr);
  post.add_to_value(find_totals(post.account), amount_expr);

  component_posts.push_back(&post);

  last_xact = post.xact;
  last_post = &post;
  count++;
}

void related_posts::flush()
{
  if (posts.size() > 0) {
    foreach (post_t * post, posts) {
      assert(post->xact);
      foreach (post_t * r_post, post->xact->posts) {
        post_t::xdata_t& xdata(r_post->xdata());
        if (! xdata.has_flags(POST_EXT_HANDLED) &&
            (! xdata.has_flags(POST_EXT_RECEIVED) ?
             ! r_post->has_flags(ITEM_GENERATED | POST_VIRTUAL) :
             also_matching)) {
          xdata.add_flags(POST_EXT_HANDLED);
          item_handler<post_t>::operator()(*r_post);
        }
      }
    }
  }

  item_handler<post_t>::flush();
}

display_filter_posts::display_filter_posts(post_handler_ptr handler,
                                           report_t&        _report,
                                           bool             _show_rounding)
  : item_handler<post_t>(handler), report(_report),
    display_amount_expr(report.HANDLER(display_amount_).expr),
    display_total_expr(report.HANDLER(display_total_).expr),
    show_rounding(_show_rounding)
{
  create_accounts();
  TRACE_CTOR(display_filter_posts, "post_handler_ptr, report_t&, bool");
}

bool display_filter_posts::output_rounding(post_t& post)
{
  bind_scope_t bound_scope(report, post);
  value_t      new_display_total;

  if (show_rounding) {
    new_display_total = (display_total_expr.calc(bound_scope)
                         .strip_annotations(report.what_to_keep()));

    DEBUG("filters.changed_value.rounding",
          "rounding.new_display_total     = " << new_display_total);
  }

  // Allow the posting to be displayed if:
  //  1. Its display_amount would display as non-zero, or
  //  2. The --empty option was specified, or
  //  3. a) The account of the posting is <Revalued>, and
  //     b) the revalued option is specified, and
  //     c) the --no-rounding option is not specified.

  if (post.account == revalued_account) {
    if (show_rounding)
      last_display_total = new_display_total;
    return true;
  }

  if (value_t repriced_amount = (display_amount_expr.calc(bound_scope)
                                 .strip_annotations(report.what_to_keep()))) {
    if (! last_display_total.is_null()) {
      DEBUG("filters.changed_value.rounding",
            "rounding.repriced_amount       = " << repriced_amount);

      value_t precise_display_total(new_display_total.truncated() -
                                    repriced_amount.truncated());

      DEBUG("filters.changed_value.rounding",
            "rounding.precise_display_total = " << precise_display_total);
      DEBUG("filters.changed_value.rounding",
            "rounding.last_display_total    = " << last_display_total);

      if (value_t diff = precise_display_total - last_display_total) {
        DEBUG("filters.changed_value.rounding",
              "rounding.diff                  = " << diff);

        handle_value(/* value=         */ diff,
                     /* account=       */ rounding_account,
                     /* xact=          */ post.xact,
                     /* temps=         */ temps,
                     /* handler=       */ handler,
                     /* date=          */ date_t(),
                     /* act_date_p=    */ true,
                     /* total=         */ precise_display_total,
                     /* direct_amount= */ true,
                     /* mark_visited=  */ false,
                     /* bidir_link=    */ false);
      }
    }
    if (show_rounding)
      last_display_total = new_display_total;
    return true;
  } else {
    return report.HANDLED(empty);
  }
}

void display_filter_posts::operator()(post_t& post)
{
  if (output_rounding(post))
    item_handler<post_t>::operator()(post);
}

changed_value_posts::changed_value_posts
  (post_handler_ptr       handler,
   report_t&              _report,
   bool                   _for_accounts_report,
   bool                   _show_unrealized,
   display_filter_posts * _display_filter)
  : item_handler<post_t>(handler), report(_report),
    total_expr(report.HANDLED(revalued_total_) ?
               report.HANDLER(revalued_total_).expr :
               report.HANDLER(display_total_).expr),
    display_total_expr(report.HANDLER(display_total_).expr),
    changed_values_only(report.HANDLED(revalued_only)),
    historical_prices_only(report.HANDLED(historical)),
    for_accounts_report(_for_accounts_report),
    show_unrealized(_show_unrealized), last_post(NULL),
    display_filter(_display_filter)
{
  string gains_equity_account_name;
  if (report.HANDLED(unrealized_gains_))
    gains_equity_account_name = report.HANDLER(unrealized_gains_).str();
  else
    gains_equity_account_name = _("Equity:Unrealized Gains");
  gains_equity_account =
    report.session.journal->master->find_account(gains_equity_account_name);
  gains_equity_account->add_flags(ACCOUNT_GENERATED);

  string losses_equity_account_name;
  if (report.HANDLED(unrealized_losses_))
    losses_equity_account_name = report.HANDLER(unrealized_losses_).str();
  else
    losses_equity_account_name = _("Equity:Unrealized Losses");
  losses_equity_account =
    report.session.journal->master->find_account(losses_equity_account_name);
  losses_equity_account->add_flags(ACCOUNT_GENERATED);

  create_accounts();

  TRACE_CTOR(changed_value_posts,
             "post_handler_ptr, report_t&, bool, bool, display_filter_posts *");
}

void changed_value_posts::flush()
{
  if (last_post && last_post->date() <= report.terminus.date()) {
    if (! historical_prices_only) {
      if (! for_accounts_report)
        output_intermediate_prices(*last_post, report.terminus.date());
      output_revaluation(*last_post, report.terminus.date());
    }
    last_post = NULL;
  }
  item_handler<post_t>::flush();
}

void changed_value_posts::output_revaluation(post_t& post, const date_t& date)
{
  if (is_valid(date))
    post.xdata().date = date;

  try {
    bind_scope_t bound_scope(report, post);
    repriced_total = total_expr.calc(bound_scope);
  }
  catch (...) {
    post.xdata().date = date_t();
    throw;
  }
  post.xdata().date = date_t();

  DEBUG("filters.changed_value",
        "output_revaluation(last_total)     = " << last_total);
  DEBUG("filters.changed_value",
        "output_revaluation(repriced_total) = " << repriced_total);

  if (! last_total.is_null()) {
    if (value_t diff = repriced_total - last_total) {
      DEBUG("filters.changed_value", "output_revaluation(strip(diff)) = "
            << diff.strip_annotations(report.what_to_keep()));

      xact_t& xact = temps.create_xact();
      xact.payee = _("Commodities revalued");
      xact._date = is_valid(date) ? date : post.value_date();

      if (! for_accounts_report) {
        handle_value
          (/* value=         */ diff,
           /* account=       */ revalued_account,
           /* xact=          */ &xact,
           /* temps=         */ temps,
           /* handler=       */ handler,
           /* date=          */ *xact._date,
           /* act_date_p=    */ true,
           /* total=         */ repriced_total);
      }
      else if (show_unrealized) {
        handle_value
          (/* value=         */ - diff,
           /* account=       */ (diff < 0L ?
                                 losses_equity_account :
                                 gains_equity_account),
           /* xact=          */ &xact,
           /* temps=         */ temps,
           /* handler=       */ handler,
           /* date=          */ *xact._date,
           /* act_date_p=    */ true,
           /* total=         */ value_t(),
           /* direct_amount= */ false,
           /* mark_visited=  */ true);
      }
    }
  }
}

namespace {
  struct insert_prices_in_map {
    price_map_t& all_prices;

    insert_prices_in_map(price_map_t& _all_prices)
      : all_prices(_all_prices) {}

    void operator()(const datetime_t& date, const amount_t& price) {
      all_prices.insert(price_map_t::value_type(date, price));
    }
  };
}

void changed_value_posts::output_intermediate_prices(post_t&       post,
                                                     const date_t& current)
{
  // To fix BZ#199, examine the balance of last_post and determine whether the
  // price of that amount changed after its date and before the new post's
  // date.  If so, generate an output_revaluation for that price change.
  // Mostly this is only going to occur if the user has a series of pricing
  // entries, since a posting-based revaluation would be seen here as a post.

  value_t display_total(last_total);

  if (display_total.type() == value_t::SEQUENCE) {
    xact_t& xact(temps.create_xact());

    xact.payee = _("Commodities revalued");
    xact._date = is_valid(current) ? current : post.value_date();

    post_t& temp(temps.copy_post(post, xact));
    temp.add_flags(ITEM_GENERATED);

    post_t::xdata_t& xdata(temp.xdata());
    if (is_valid(current))
      xdata.date = current;

    DEBUG("filters.revalued", "intermediate last_total = " << last_total);

    switch (last_total.type()) {
    case value_t::BOOLEAN:
    case value_t::INTEGER:
      last_total.in_place_cast(value_t::AMOUNT);
      // fall through...

    case value_t::AMOUNT:
      temp.amount = last_total.as_amount();
      break;

    case value_t::BALANCE:
    case value_t::SEQUENCE:
      xdata.compound_value = last_total;
      xdata.add_flags(POST_EXT_COMPOUND);
      break;

    case value_t::DATETIME:
    case value_t::DATE:
    default:
      assert(false);
      break;
    }

    bind_scope_t inner_scope(report, temp);
    display_total = display_total_expr.calc(inner_scope);

    DEBUG("filters.revalued", "intermediate display_total = " << display_total);
  }

  switch (display_total.type()) {
  case value_t::VOID:
  case value_t::INTEGER:
  case value_t::SEQUENCE:
    break;

  case value_t::AMOUNT:
    display_total.in_place_cast(value_t::BALANCE);
    // fall through...

  case value_t::BALANCE: {
    price_map_t all_prices;

    foreach (const balance_t::amounts_map::value_type& amt_comm,
             display_total.as_balance().amounts)
      amt_comm.first->map_prices(insert_prices_in_map(all_prices),
                                 datetime_t(current),
                                 datetime_t(post.value_date()), true);

    // Choose the last price from each day as the price to use
    typedef std::map<const date_t, bool> date_map;
    date_map pricing_dates;

    BOOST_REVERSE_FOREACH(const price_map_t::value_type& price, all_prices) {
      // This insert will fail if a later price has already been inserted
      // for that date.
      DEBUG("filters.revalued",
            "re-inserting " << price.second << " at " << price.first.date());
      pricing_dates.insert(date_map::value_type(price.first.date(), true));
    }

    // Go through the time-sorted prices list, outputting a revaluation for
    // each price difference.
    foreach (const date_map::value_type& price, pricing_dates) {
      output_revaluation(post, price.first);
      last_total = repriced_total;
    }
    break;
  }
  default:
    assert(false);
    break;
  }
}

void changed_value_posts::operator()(post_t& post)
{
  if (last_post) {
    if (! for_accounts_report && ! historical_prices_only)
      output_intermediate_prices(*last_post, post.value_date());
    output_revaluation(*last_post, post.value_date());
  }

  if (changed_values_only)
    post.xdata().add_flags(POST_EXT_DISPLAYED);

  item_handler<post_t>::operator()(post);

  bind_scope_t bound_scope(report, post);
  last_total = total_expr.calc(bound_scope);
  last_post  = &post;
}

void subtotal_posts::report_subtotal(const char *                     spec_fmt,
                                     const optional<date_interval_t>& interval)
{
  if (component_posts.empty())
    return;

  optional<date_t> range_start  = interval ? interval->start : none;
  optional<date_t> range_finish = interval ? interval->inclusive_end() : none;

  if (! range_start || ! range_finish) {
    foreach (post_t * post, component_posts) {
      date_t date       = post->date();
      date_t value_date = post->value_date();
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
      if (! range_start || date < *range_start)
        range_start = date;
      if (! range_finish || value_date > *range_finish)
        range_finish = value_date;
#if defined(__GNUC__) && __GNUC__ >= 4 && __GNUC_MINOR__ >= 7
#pragma GCC diagnostic pop
#endif
    }
  }
  component_posts.clear();

  std::ostringstream out_date;
  if (spec_fmt) {
    out_date << format_date(*range_finish, FMT_CUSTOM, spec_fmt);
  }
  else if (date_format) {
    out_date << "- " << format_date(*range_finish, FMT_CUSTOM,
                                    date_format->c_str());
  }
  else {
    out_date << "- " << format_date(*range_finish);
  }

  xact_t& xact = temps.create_xact();
  xact.payee = out_date.str();
  xact._date = *range_start;

  foreach (values_map::value_type& pair, values)
    handle_value(/* value=      */ pair.second.value,
                 /* account=    */ pair.second.account,
                 /* xact=       */ &xact,
                 /* temps=      */ temps,
                 /* handler=    */ handler,
                 /* date=       */ *range_finish,
                 /* act_date_p= */ false);

  values.clear();
}

void subtotal_posts::operator()(post_t& post)
{
  component_posts.push_back(&post);

  account_t * acct = post.reported_account();
  assert(acct);

  value_t amount(post.amount);

  post.xdata().compound_value = amount;
  post.xdata().add_flags(POST_EXT_COMPOUND);

  values_map::iterator i = values.find(acct->fullname());
  if (i == values.end()) {
#if DEBUG_ON
    std::pair<values_map::iterator, bool> result =
#endif
      values.insert(values_pair
                    (acct->fullname(),
                     acct_value_t(acct, amount, post.has_flags(POST_VIRTUAL),
                                  post.has_flags(POST_MUST_BALANCE))));
#if DEBUG_ON
    assert(result.second);
#endif
  } else {
    if (post.has_flags(POST_VIRTUAL) != (*i).second.is_virtual)
      throw_(std::logic_error,
             _("'equity' cannot accept virtual and "
               "non-virtual postings to the same account"));

    add_or_set_value((*i).second.value, amount);
  }

  // If the account for this post is all virtual, mark it as
  // such, so that `handle_value' can show "(Account)" for accounts
  // that contain only virtual posts.

  post.reported_account()->xdata().add_flags(ACCOUNT_EXT_AUTO_VIRTUALIZE);

  if (! post.has_flags(POST_VIRTUAL))
    post.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS);
  else if (! post.has_flags(POST_MUST_BALANCE))
    post.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS);
}

void interval_posts::report_subtotal(const date_interval_t& ival)
{
  if (exact_periods)
    subtotal_posts::report_subtotal();
  else
    subtotal_posts::report_subtotal(NULL, ival);
}

namespace {
  struct sort_posts_by_date {
    bool operator()(post_t * left, post_t * right) const {
      return left->date() < right->date();
    }
  };
}

void interval_posts::operator()(post_t& post)
{
  // If there is a duration (such as weekly), we must generate the
  // report in two passes.  Otherwise, we only have to check whether the
  // post falls within the reporting period.

  if (interval.duration) {
    all_posts.push_back(&post);
  }
  else if (interval.find_period(post.date(), align_intervals)) {
    item_handler<post_t>::operator()(post);
  }
}

void interval_posts::flush()
{
  if (! interval.duration) {
    item_handler<post_t>::flush();
    return;
  }

  // Sort all the postings we saw by date ascending
  std::stable_sort(all_posts.begin(), all_posts.end(),
                   sort_posts_by_date());

  // only if the interval has no start use the earliest post
  if (!(interval.begin() && interval.find_period(*interval.begin(), align_intervals)))
    // Determine the beginning interval by using the earliest post
    if (all_posts.size() > 0 && all_posts.front()
        && !interval.find_period(all_posts.front()->date(), align_intervals))
      throw_(std::logic_error, _("Failed to find period for interval report"));

  // Walk the interval forward reporting all posts within each one
  // before moving on, until we reach the end of all_posts
  bool saw_posts = false;
  for (std::deque<post_t *>::iterator i = all_posts.begin();
       i != all_posts.end(); ) {
    post_t * post(*i);

    DEBUG("filters.interval",
          "Considering post " << post->date() << " = " << post->amount);
#if DEBUG_ON
    DEBUG("filters.interval", "interval is:");
    debug_interval(interval);
#endif
    assert(! interval.finish || post->date() < *interval.finish);

    if (interval.within_period(post->date())) {
      DEBUG("filters.interval", "Calling subtotal_posts::operator()");
      subtotal_posts::operator()(*post);
      ++i;
      saw_posts = true;
    } else {
      if (saw_posts) {
        DEBUG("filters.interval",
              "Calling subtotal_posts::report_subtotal()");
        report_subtotal(interval);
        saw_posts = false;
      }
      else if (generate_empty_posts) {
        // Generate a null posting, so the intervening periods can be
        // seen when -E is used, or if the calculated amount ends up
        // being non-zero
        xact_t& null_xact = temps.create_xact();
        null_xact._date = interval.inclusive_end();

        post_t& null_post = temps.create_post(null_xact, empty_account);
        null_post.add_flags(POST_CALCULATED);
        null_post.amount = 0L;

        subtotal_posts::operator()(null_post);
        report_subtotal(interval);
      }

      DEBUG("filters.interval", "Advancing interval");
      ++interval;
    }
  }

  // If the last postings weren't reported, do so now.
  if (saw_posts) {
    DEBUG("filters.interval",
          "Calling subtotal_posts::report_subtotal() at end");
    report_subtotal(interval);
  }

  // Tell our parent class to flush
  subtotal_posts::flush();
}

namespace {
  struct create_post_from_amount
  {
    post_handler_ptr handler;
    xact_t&          xact;
    account_t&       balance_account;
    temporaries_t&   temps;

    explicit create_post_from_amount(post_handler_ptr _handler,
                                     xact_t&          _xact,
                                     account_t&       _balance_account,
                                     temporaries_t&   _temps)
      : handler(_handler), xact(_xact),
        balance_account(_balance_account), temps(_temps) {
      TRACE_CTOR(create_post_from_amount,
                 "post_handler_ptr, xact_t&, account_t&, temporaries_t&");
    }
    create_post_from_amount(const create_post_from_amount& other)
      : handler(other.handler), xact(other.xact),
        balance_account(other.balance_account), temps(other.temps) {
      TRACE_CTOR(create_post_from_amount, "copy");
    }
    ~create_post_from_amount() throw() {
      TRACE_DTOR(create_post_from_amount);
    }

    void operator()(const amount_t& amount) {
      if(amount.is_zero()) return;
      post_t& balance_post = temps.create_post(xact, &balance_account);
      balance_post.amount = - amount;
      (*handler)(balance_post);
    }
  };
}

void posts_as_equity::report_subtotal()
{
  date_t finish;
  foreach (post_t * post, component_posts) {
    date_t date = post->date();
    if (! is_valid(finish) || date > finish)
      finish = date;
  }
  component_posts.clear();

  xact_t& xact = temps.create_xact();
  xact.payee = _("Opening Balances");
  xact._date = finish;

  value_t total = 0L;
  foreach (values_map::value_type& pair, values) {
    value_t value(pair.second.value.strip_annotations(report.what_to_keep()));
    if (unround)
      value.in_place_unround();
    if (! value.is_zero()) {
      if (value.is_balance()) {
        value.as_balance_lval().map_sorted_amounts
          ([&](const amount_t& amt) {
             if (! amt.is_zero())
               handle_value(/* value=      */ amt,
                            /* account=    */ pair.second.account,
                            /* xact=       */ &xact,
                            /* temps=      */ temps,
                            /* handler=    */ handler,
                            /* date=       */ finish,
                            /* act_date_p= */ false);
           });
      } else {
        handle_value(/* value=      */ value.to_amount(),
                     /* account=    */ pair.second.account,
                     /* xact=       */ &xact,
                     /* temps=      */ temps,
                     /* handler=    */ handler,
                     /* date=       */ finish,
                     /* act_date_p= */ false);
      }
    }

    if (! pair.second.is_virtual || pair.second.must_balance)
      total += value;
  }
  values.clear();

  // This last part isn't really needed, since an Equity:Opening
  // Balances posting with a null amount will automatically balance with
  // all the other postings generated.  But it does make the full
  // balancing amount clearer to the user.
  if (! total.is_zero()) {
    create_post_from_amount post_creator(handler, xact,
                                         *balance_account, temps);
    if (total.is_balance())
      total.as_balance_lval().map_sorted_amounts(post_creator);
    else
      post_creator(total.to_amount());
  }
}

void by_payee_posts::flush()
{
  foreach (payee_subtotals_map::value_type& pair, payee_subtotals)
    pair.second->report_subtotal(pair.first.c_str());

  item_handler<post_t>::flush();

  payee_subtotals.clear();
}

void by_payee_posts::operator()(post_t& post)
{
  payee_subtotals_map::iterator i = payee_subtotals.find(post.payee());
  if (i == payee_subtotals.end()) {
    payee_subtotals_pair
      temp(post.payee(),
           shared_ptr<subtotal_posts>(new subtotal_posts(handler, amount_expr)));
    std::pair<payee_subtotals_map::iterator, bool> result
      = payee_subtotals.insert(temp);

    assert(result.second);
    if (! result.second)
      return;
    i = result.first;
  }

  (*(*i).second)(post);
}

void transfer_details::operator()(post_t& post)
{
  xact_t& xact = temps.copy_xact(*post.xact);
  xact._date = post.date();

  post_t& temp = temps.copy_post(post, xact);
  temp.set_state(post.state());

  bind_scope_t bound_scope(scope, temp);
  value_t      substitute(expr.calc(bound_scope));

  if (! substitute.is_null()) {
    switch (which_element) {
    case SET_DATE:
      temp._date = substitute.to_date();
      break;

    case SET_ACCOUNT: {
      string account_name = substitute.to_string();
      if (! account_name.empty() &&
          account_name[account_name.length() - 1] != ':') {
        account_t * prev_account = temp.account;
        temp.account->remove_post(&temp);

        account_name += ':';
        account_name += prev_account->fullname();

        std::list<string> account_names;
        split_string(account_name, ':', account_names);
        temp.account = create_temp_account_from_path(account_names, temps,
                                                     xact.journal->master);
        temp.account->add_post(&temp);

        temp.account->add_flags(prev_account->flags());
        if (prev_account->has_xdata())
          temp.account->xdata().add_flags(prev_account->xdata().flags());
      }
      break;
    }

    case SET_PAYEE:
      xact.payee = substitute.to_string();
      break;
    }
  }

  item_handler<post_t>::operator()(temp);
}

void day_of_week_posts::flush()
{
  for (int i = 0; i < 7; i++) {
    foreach (post_t * post, days_of_the_week[i])
      subtotal_posts::operator()(*post);
    subtotal_posts::report_subtotal("%As");
    days_of_the_week[i].clear();
  }

  subtotal_posts::flush();
}

void generate_posts::add_period_xacts(period_xacts_list& period_xacts)
{
  foreach (period_xact_t * xact, period_xacts)
    foreach (post_t * post, xact->posts)
      add_post(xact->period, *post);
}

void generate_posts::add_post(const date_interval_t& period, post_t& post)
{
  pending_posts.push_back(pending_posts_pair(period, &post));
}

void budget_posts::report_budget_items(const date_t& date)
{
  { // Cleanup pending items that finished before date
    // We have to keep them until the last day they apply because operator() needs them to see if a
    // posting is budgeted or not
    std::list<pending_posts_list::iterator> posts_to_erase;
    for (pending_posts_list::iterator i = pending_posts.begin(); i != pending_posts.end(); i++) {
      pending_posts_list::value_type& pair(*i);
      if (pair.first.finish && ! pair.first.start && pair.first.finish < date) {
        posts_to_erase.push_back(i);
      }
    }
    foreach (pending_posts_list::iterator& i, posts_to_erase)
      pending_posts.erase(i);
  }

  if (pending_posts.size() == 0)
    return;

  bool reported;
  do {
    reported = false;
    for (pending_posts_list::iterator i = pending_posts.begin();
         i != pending_posts.end();
         i++) {
      pending_posts_list::value_type& pair(*i);

      if (pair.first.finish && ! pair.first.start)
        continue;               // skip expired posts

      optional<date_t> begin = pair.first.start;
      if (! begin) {
        optional<date_t> range_begin;
        if (pair.first.range)
          range_begin = pair.first.range->begin();

        DEBUG("budget.generate", "Finding period for pending post");
        if (! pair.first.find_period(range_begin ? *range_begin : date))
          continue;
        if (! pair.first.start)
          throw_(std::logic_error,
                 _("Failed to find period for periodic transaction"));
        begin = pair.first.start;
      }

#if DEBUG_ON
      DEBUG("budget.generate", "begin = " << *begin);
      DEBUG("budget.generate", "date  = " << date);
      if (pair.first.finish)
        DEBUG("budget.generate", "pair.first.finish = " << *pair.first.finish);
#endif

      if (*begin <= date &&
          (! pair.first.finish || *begin < *pair.first.finish)) {
        post_t& post = *pair.second;

        ++pair.first;
        DEBUG("budget.generate", "Reporting budget for "
              << post.reported_account()->fullname());

        xact_t& xact = temps.create_xact();
        xact.payee = _("Budget transaction");
        xact._date = begin;

        post_t& temp = temps.copy_post(post, xact);
        temp.amount.in_place_negate();

        if (flags & BUDGET_WRAP_VALUES) {
          value_t seq;
          seq.push_back(0L);
          seq.push_back(temp.amount);

          temp.xdata().compound_value = seq;
          temp.xdata().add_flags(POST_EXT_COMPOUND);
        }

        item_handler<post_t>::operator()(temp);

        reported = true;
      }
    }
  } while (reported);
}

void budget_posts::operator()(post_t& post)
{
  bool post_in_budget = false;

  foreach (pending_posts_list::value_type& pair, pending_posts) {
    for (account_t * acct = post.reported_account();
         acct;
         acct = acct->parent) {
      if (acct == (*pair.second).reported_account()) {
        post_in_budget = true;
        // Report the post as if it had occurred in the parent account.
        if (post.reported_account() != acct)
          post.set_reported_account(acct);
        goto handle;
      }
    }
  }

 handle:
  if (post_in_budget && flags & BUDGET_BUDGETED) {
    report_budget_items(post.date());
    item_handler<post_t>::operator()(post);
  }
  else if (! post_in_budget && flags & BUDGET_UNBUDGETED) {
    item_handler<post_t>::operator()(post);
  }
}

void budget_posts::flush()
{
  if (flags & BUDGET_BUDGETED)
    report_budget_items(terminus);

  item_handler<post_t>::flush();
}

void forecast_posts::add_post(const date_interval_t& period, post_t& post)
{
  date_interval_t i(period);
  if (! i.start && ! i.find_period(CURRENT_DATE()))
    return;

  generate_posts::add_post(i, post);

  // Advance the period's interval until it is at or beyond the current
  // date.
  while (*i.start < CURRENT_DATE())
    ++i;
}

void forecast_posts::flush()
{
  posts_list passed;
  date_t     last = CURRENT_DATE();

  // If there are period transactions to apply in a continuing series until
  // the forecast condition is met, generate those transactions now.  Note
  // that no matter what, we abandon forecasting beyond the next 5 years.
  //
  // It works like this:
  //
  // Earlier, in forecast_posts::add_period_xacts, we cut up all the periodic
  // transactions into their components postings, so that we have N "periodic
  // postings".  For example, if the user had this:
  //
  // ~ daily
  //   Expenses:Food       $10
  //   Expenses:Auto:Gas   $20
  // ~ monthly
  //   Expenses:Food       $100
  //   Expenses:Auto:Gas   $200
  //
  // We now have 4 periodic postings in `pending_posts'.
  //
  // Each periodic postings gets its own copy of its parent transaction's
  // period, which is modified as we go.  This is found in the second member
  // of the pending_posts_list for each posting.
  //
  // The algorithm below works by iterating through the N periodic postings
  // over and over, until each of them meets the termination criteria for the
  // forecast and is removed from the set.

  while (pending_posts.size() > 0) {
    // At each step through the loop, we find the first periodic posting whose
    // period contains the earliest starting date.
    pending_posts_list::iterator least = pending_posts.begin();
    for (pending_posts_list::iterator i = ++pending_posts.begin();
         i != pending_posts.end();
         i++) {
      assert((*i).first.start);
      assert((*least).first.start);
      if (*(*i).first.start < *(*least).first.start)
        least = i;
    }

    if ((*least).first.finish)
      assert(*(*least).first.start < *(*least).first.finish);

    // If the next date in the series for this periodic posting is more than 5
    // years beyond the last valid post we generated, drop it from further
    // consideration.
    date_t& next(*(*least).first.next);
    assert(next > *(*least).first.start);

    if (static_cast<std::size_t>((next - last).days()) >
        static_cast<std::size_t>(365U) * forecast_years) {
      DEBUG("filters.forecast",
            "Forecast transaction exceeds " << forecast_years
            << " years beyond today");
      pending_posts.erase(least);
      continue;
    }

    // `post' refers to the posting defined in the period transaction.  We
    // make a copy of it within a temporary transaction with the payee
    // "Forecast transaction".
    post_t& post = *(*least).second;
    xact_t& xact = temps.create_xact();
    xact.payee   = _("Forecast transaction");
    xact._date   = next;
    post_t& temp = temps.copy_post(post, xact);

    // Submit the generated posting
    DEBUG("filters.forecast",
          "Forecast transaction: " << temp.date()
          << " " << temp.account->fullname()
          << " " << temp.amount);
    item_handler<post_t>::operator()(temp);

    // If the generated posting matches the user's report query, check whether
    // it also fails to match the continuation condition for the forecast.  If
    // it does, drop this periodic posting from consideration.
    if (temp.has_xdata() && temp.xdata().has_flags(POST_EXT_MATCHES)) {
      DEBUG("filters.forecast", "  matches report query");
      bind_scope_t bound_scope(context, temp);
      if (! pred(bound_scope)) {
        DEBUG("filters.forecast", "  fails to match continuation criteria");
        pending_posts.erase(least);
        continue;
      }
    }

    // Increment the 'least', but remove it from pending_posts if it
    // exceeds its own boundaries.
    ++(*least).first;
    if (! (*least).first.start) {
      pending_posts.erase(least);
      continue;
    }
  }

  item_handler<post_t>::flush();
}

inject_posts::inject_posts(post_handler_ptr handler,
                           const string&    tag_list,
                           account_t *      master)
  : item_handler<post_t>(handler)
{
  scoped_array<char> buf(new char[tag_list.length() + 1]);
  std::strcpy(buf.get(), tag_list.c_str());

  for (char * q = std::strtok(buf.get(), ",");
       q;
       q = std::strtok(NULL, ",")) {
    std::list<string> account_names;
    split_string(q, ':', account_names);

    account_t * account =
      create_temp_account_from_path(account_names, temps, master);
    account->add_flags(ACCOUNT_GENERATED);

    tags_list.push_back
      (tags_list_pair(q, tag_mapping_pair(account, tag_injected_set())));
  }

  TRACE_CTOR(inject_posts, "post_handler_ptr, string, account_t *");
}

void inject_posts::operator()(post_t& post)
{
  foreach (tags_list_pair& pair, tags_list) {
    optional<value_t> tag_value = post.get_tag(pair.first, false);
    // When checking if the transaction has the tag, only inject once
    // per transaction.
    if (! tag_value &&
        pair.second.second.find(post.xact) == pair.second.second.end() &&
        (tag_value = post.xact->get_tag(pair.first)))
      pair.second.second.insert(post.xact);

    if (tag_value) {
      xact_t& xact = temps.copy_xact(*post.xact);
      xact._date = post.date();
      xact.add_flags(ITEM_GENERATED);
      post_t& temp = temps.copy_post(post, xact);

      temp.account = pair.second.first;
      temp.amount  = tag_value->to_amount();
      temp.add_flags(ITEM_GENERATED);

      item_handler<post_t>::operator()(temp);
    }
  }

  item_handler<post_t>::operator()(post);
}

} // namespace ledger
