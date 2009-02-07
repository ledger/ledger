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

#include "report.h"
#include "iterators.h"
#include "filters.h"
#include "chain.h"
#include "output.h"
#include "precmd.h"

namespace ledger {

report_t::report_t(session_t& _session) : session(_session)
{
  // Setup default values for some of the option handlers
  HANDLER(date_format_).on("%y-%b-%d");

  HANDLER(register_format_).on(
    "%-.9(display_date) %-.20(payee)"
    " %-.23(truncate(account, 23, 2))"
    " %!12(print_balance(strip(display_amount), 12, 67))"
    " %!12(print_balance(strip(display_total), 12, 80, true))\n%/"
    "%31|%-.23(truncate(account, 23, 2))"
    " %!12(print_balance(strip(display_amount), 12, 67))"
    " %!12(print_balance(strip(display_total), 12, 80, true))\n");

  // jww (2009-02-06): Most of these still need to be defined
  HANDLER(wide_register_format_).on(
    "%-.9D  %-.35P %-.39A %22.108t %!22.132T\n%/"
    "%48|%-.38A %22.108t %!22.132T\n");

  HANDLER(print_format_).on(
    "%(display_date)%(entry.cleared ? \" *\" : (entry.uncleared ? \"\" : \" !\"))"
    "%(code ? \" (\" + code + \")\" : \"\") %(payee)%(entry.comment | \"\")\n"
    "    %(cleared ? \" *\" : (uncleared ? \"\" : \" !\"))%-34(account)"
    "  %12(amount)%(comment | \"\")\n%/"
    "    %(cleared ? \" *\" : (uncleared ? \"\" : \" !\"))%-34(account)"
    "  %12(amount)%(comment | \"\")\n%/\n");

  HANDLER(balance_format_).on(
    "%20(strip(display_total))  %(depth_spacer)%-(partial_account)\n");

  HANDLER(equity_format_).on("\n%D %Y%C%P\n%/    %-34W  %12t\n");
  HANDLER(plot_amount_format_).on("%D %(S(t))\n");
  HANDLER(plot_total_format_).on("%D %(S(T))\n");
  HANDLER(write_hdr_format_).on("%d %Y%C%P\n");
  HANDLER(write_xact_format_).on("    %-34W  %12o%n\n");
  HANDLER(prices_format_).on("%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n");
  HANDLER(pricesdb_format_).on("P %[%Y/%m/%d %H:%M:%S] %A %t\n");
}

void report_t::xacts_report(xact_handler_ptr handler)
{
  session_xacts_iterator walker(session);
  pass_down_xacts(chain_xact_handlers(*this, handler), walker);
  session.clean_xacts();
}

void report_t::entry_report(xact_handler_ptr handler, entry_t& entry)
{
  entry_xacts_iterator walker(entry);
  pass_down_xacts(chain_xact_handlers(*this, handler), walker);
  session.clean_xacts(entry);
}

void report_t::sum_all_accounts()
{
  session_xacts_iterator walker(session);
  pass_down_xacts
    (chain_xact_handlers(*this, xact_handler_ptr(new set_account_value), false),
     walker);

  session.master->calculate_sums(HANDLER(amount_).expr, *this);
}

void report_t::accounts_report(acct_handler_ptr handler)
{
  sum_all_accounts();

  if (! HANDLED(sort_)) {
    basic_accounts_iterator walker(*session.master);
    pass_down_accounts(handler, walker,
		       item_predicate<account_t>("total", what_to_keep()));
  } else {
    sorted_accounts_iterator walker(*session.master, HANDLER(sort_).str());
    pass_down_accounts(handler, walker,
		       item_predicate<account_t>("total", what_to_keep()));
  }
    
  session.clean_xacts();
  session.clean_accounts();
}

void report_t::commodities_report(const string& format)
{
}

value_t report_t::fn_amount_expr(call_scope_t& scope)
{
  return HANDLER(amount_).expr.calc(scope);
}

value_t report_t::fn_total_expr(call_scope_t& scope)
{
  return HANDLER(total_).expr.calc(scope);
}

value_t report_t::fn_display_amount(call_scope_t& scope)
{
  return HANDLER(display_amount_).expr.calc(scope);
}

value_t report_t::fn_display_total(call_scope_t& scope)
{
  return HANDLER(display_total_).expr.calc(scope);
}

namespace {
  value_t fn_market_value(call_scope_t& args)
  {
    var_t<datetime_t> date(args, 1);
    var_t<string>	    in_terms_of(args, 2);

    commodity_t * commodity = NULL;
    if (in_terms_of)
      commodity = amount_t::current_pool->find_or_create(*in_terms_of);

    DEBUG("report.market", "getting market value of: " << args[0]);

    value_t result =
      args[0].value(date ? optional<datetime_t>(*date) : optional<datetime_t>(),
		    commodity ? optional<commodity_t&>(*commodity) :
		    optional<commodity_t&>());

    DEBUG("report.market", "result is: " << result);
    return result;
  }

  value_t fn_print_balance(call_scope_t& args)
  {
    report_t& report(find_scope<report_t>(args));

    var_t<long>	first_width(args, 1);
    var_t<long>	latter_width(args, 2);
#if 0
    var_t<bool>	bold_negative(args, 3);
#endif

    std::ostringstream out;
    args[0].strip_annotations(report.what_to_keep())
      .print(out, *first_width, *latter_width,
	     report.HANDLED(date_format_) ?
	     report.HANDLER(date_format_).str() : optional<string>());

    return string_value(out.str());
  }

  value_t fn_strip(call_scope_t& args)
  {
    report_t& report(find_scope<report_t>(args));
    return args[0].strip_annotations(report.what_to_keep());
  }

  value_t fn_truncate(call_scope_t& args)
  {
    report_t& report(find_scope<report_t>(args));

    var_t<long>	width(args, 1);
    var_t<long>	account_abbrev(args, 2);

    return string_value(format_t::truncate(args[0].as_string(), *width,
					   account_abbrev ? *account_abbrev : -1));
  }

  value_t fn_display_date(call_scope_t& args)
  {
    report_t& report(find_scope<report_t>(args));
    item_t&   item(find_scope<item_t>(args));

    date_t when;
    if (report.HANDLED(effective)) {
      if (optional<date_t> date = item.effective_date())
	when = *date;
      else
	when = item.date();
    } else {
      when = item.date();
    }

    if (report.HANDLED(date_format_))
      return string_value(format_date(when,
				      report.HANDLER(date_format_).str()));
    else
      return string_value(format_date(when));
  }

  template <class Type        = xact_t,
	    class handler_ptr = xact_handler_ptr,
	    void (report_t::*report_method)(handler_ptr) =
	      &report_t::xacts_report>
  class reporter
  {
    shared_ptr<item_handler<Type> > handler;

  public:
    reporter(item_handler<Type> * _handler)
      : handler(_handler) {}

    value_t operator()(call_scope_t& args)
    {
      report_t& report(find_scope<report_t>(args));

      if (args.value().size() > 0)
	report.append_predicate
	  (args_to_predicate_expr(args.value().as_sequence().begin(),
				  args.value().as_sequence().end()));

      DEBUG("report.predicate",
	    "Predicate = " << report.HANDLER(limit_).str());

      (report.*report_method)(handler_ptr(handler));

      return true;
    }
  };
}

#if 0
// Commands yet to implement:
//
// entry
// prices
// pricesdb
// csv
// emacs | lisp
#endif

expr_t::ptr_op_t report_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'a':
    METHOD(report_t, amount_expr);
    break;

  case 'c':
    if (WANT_CMD()) { p += CMD_PREFIX_LEN;
      switch (*p) {
      case 'b':
	if (*(p + 1) == '\0' ||
	    std::strcmp(p, "bal") == 0 ||
	    std::strcmp(p, "balance") == 0)
	  return expr_t::op_t::wrap_functor
	    (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	     (new format_accounts(*this, HANDLER(balance_format_).str())));
	break;

      case 'e':
	if (std::strcmp(p, "equity") == 0)
	  return expr_t::op_t::wrap_functor
	    (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	     (new format_equity(*this, HANDLER(print_format_).str())));
	break;

      case 'p':
	if (*(p + 1) == '\0' || std::strcmp(p, "print") == 0)
	  return WRAP_FUNCTOR
	    (reporter<>(new format_xacts(*this, HANDLER(print_format_).str())));
	break;

      case 'r':
	if (*(p + 1) == '\0' ||
	    std::strcmp(p, "reg") == 0 ||
	    std::strcmp(p, "register") == 0)
	  return WRAP_FUNCTOR
	    (reporter<>(new format_xacts(*this, HANDLER(register_format_).str())));
	break;
      }
    }
    break;

  case 'd':
    METHOD(report_t, display_total);
    else METHOD(report_t, display_amount);
    else FUNCTION(display_date);
    break;

  case 'm':
    FUNCTION(market_value);
    break;

  case 'o':
    if (WANT_OPT()) { p += OPT_PREFIX_LEN;
      switch (*p) {
      case 'a':
	OPT(amount_);
	else OPT(anon);
	else OPT(account_);
	break;

      case 'b':
	OPT_(begin_);
	else OPT(base);
	else OPT(by_payee);
	break;

      case 'c':
	OPT_(current);
	else OPT(collapse);
	else OPT(cleared);
	else OPT(cost);
	else OPT(comm_as_payee);
	else OPT(code_as_payee);
	break;

      case 'd':
	OPT(daily);
	else OPT(dow);
	else OPT(date_format_);
	break;

      case 'e':
	OPT_(end_);
	else OPT(empty);
	else OPT(effective);
	break;

      case 'f':
	OPT(format_);
	break;

      case 'h':
	OPT(head_);
	break;

      case 'i':
	OPT(invert);
	break;

      case 'j':
	OPT_CH(amount_data);
	break;

      case 'l':
	OPT_(limit_);
	break;

      case 'm':
	OPT(monthly);
	else OPT(market);
	break;

      case 'n':
	OPT_CH(collapse);
	break;

      case 'o':
	OPT_(output_);
	break;

      case 'p':
	OPT_(period_);
	else OPT(period_sort_);
	else OPT(price);
	else OPT(pager_);
	break;

      case 'q':
	OPT(quarterly);
	else OPT(quantity);
	break;

      case 'r':
	OPT_(related);
	else OPT_(related_all);
	break;

      case 's':
	OPT_(subtotal);
	else OPT(sort_);
	else OPT(sort_all_);
	else OPT(sort_entries_);
	break;

      case 't':
	OPT_CH(amount_);
	else OPT(total_);
	else OPT(totals);
	else OPT(tail_);
	break;

      case 'u':
	OPT(uncleared);
	break;

      case 'w':
	OPT(weekly);
	break;

      case 'x':
	OPT_CH(comm_as_payee); // jww (2009-02-05): ???
	break;

      case 'y':
	OPT_CH(date_format_);
	else OPT(yearly);
	break;

      case 'B':
	OPT_CH(cost);
	break;
      case 'C':
	OPT_CH(cleared);
	break;
      case 'E':
	OPT_CH(empty);
	break;
      case 'F':
	OPT_CH(format_);
	break;
      case 'I':
	OPT_CH(price);
	break;
      case 'J':
	OPT_CH(total_data);
	break;
      case 'M':
	OPT_CH(monthly);
	break;
      case 'O':
	OPT_CH(quantity);
	break;
      case 'P':
	OPT_CH(by_payee);
	break;
      case 'S':
	OPT_CH(sort_);
	break;
      case 'T':
	OPT_CH(total_);
	break;
      case 'U':
	OPT_CH(uncleared);
	break;
      case 'V':
	OPT_CH(market);
	break;
      case 'W':
	OPT_CH(weekly);
	break;
      case 'Y':
	OPT_CH(yearly);
	break;
      }
    }
    break;

  case 'p':
    if (WANT_PRECMD()) { p += PRECMD_PREFIX_LEN;
      switch (*p) {
      case 'a':
	COMMAND(args);
	break;
      case 'p':
	COMMAND(parse);
	else COMMAND(period);
	break;
      case 'e':
	COMMAND(eval);
	break;
      case 'f':
	COMMAND(format);
	break;
      }
    }
    else FUNCTION(print_balance);
    break;

  case 's':
    FUNCTION(strip);
    break;

  case 't':
    FUNCTION(truncate);
    else METHOD(report_t, total_expr);
    break;
  }

  return session.lookup(name);
}

} // namespace ledger
