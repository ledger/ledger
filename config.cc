#include "config.h"
#include "option.h"
#include "quotes.h"

#include <fstream>

namespace ledger {

config_t config;

std::string bal_fmt	 = "%20T  %2_%-n\n";
std::string reg_fmt
  = "%D %-.20P %-.22N %12.66t %12.80T\n%/%32|%-.22N %12.66t %12.80T\n";
std::string plot_value_fmt = "%D %t\n";
std::string plot_total_fmt = "%D %T\n";
std::string print_fmt
  = "\n%D %X%C%P\n    %-34N  %12o\n%/    %-34N  %12o\n";
std::string equity_fmt
  = "\n%D %X%C%P\n%/    %-34N  %12t\n";

config_t::config_t()
{
  if (const char * p = std::getenv("HOME"))
    init_file = cache_file = price_db = p;

  init_file  += "/.ledgerrc";
  cache_file += "/.ledger";
  price_db   += "/.pricedb";

  value_expr	     = "a";
  total_expr	     = "O";
  pricing_leeway     = 24 * 3600;
  show_collapsed     = false;
  show_subtotal      = false;
  show_related       = false;
  show_all_related   = false;
  show_inverted      = false;
  show_empty	     = false;
  days_of_the_week   = false;
  show_revalued      = false;
  show_revalued_only = false;
  download_quotes    = false;
  use_cache	     = false;
  cache_dirty        = false;
}

static void
regexps_to_predicate(config_t& config,
		     std::list<std::string>::const_iterator begin,
		     std::list<std::string>::const_iterator end,
		     const bool account_regexp		= false,
		     const bool add_account_short_masks = false)
{
  std::string regexps[2];

  // Treat the remaining command-line arguments as regular
  // expressions, used for refining report results.

  for (std::list<std::string>::const_iterator i = begin;
       i != end;
       i++)
    if ((*i)[0] == '-') {
      if (! regexps[1].empty())
	regexps[1] += "|";
      regexps[1] += (*i).substr(1);
    }
    else if ((*i)[0] == '+') {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += (*i).substr(1);
    }
    else {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += *i;
    }

  for (int i = 0; i < 2; i++) {
    if (regexps[i].empty())
      continue;

    if (! config.predicate.empty())
      config.predicate += "&";

    if (i == 1) {
      config.predicate += "!";
    }
    else if (add_account_short_masks) {
      if (regexps[i].find(':') != std::string::npos) {
	config.show_subtotal = true;
      } else {
	if (! config.display_predicate.empty())
	  config.display_predicate += "&";
	else if (! config.show_empty)
	  config.display_predicate += "T&";

	config.display_predicate += "///(?:";
	config.display_predicate += regexps[i];
	config.display_predicate += ")/";
      }
    }

    if (! account_regexp)
      config.predicate += "/";
    config.predicate += "/(?:";
    config.predicate += regexps[i];
    config.predicate += ")/";
  }
}

void config_t::process_options(const std::string&     command,
			       strings_list::iterator arg,
			       strings_list::iterator args_end)
{
  // Configure some other options depending on report type

  if (command == "p" || command == "e") {
    show_related     =
    show_all_related = true;
  }
  else if (command == "E") {
    show_subtotal = true;
  }
  else if (show_related) {
    if (command == "r") {
      show_inverted = true;
    } else {
      show_subtotal    = true;
      show_all_related = true;
    }
  }

  // Process remaining command-line arguments

  if (command != "e") {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    std::list<std::string>::iterator i = arg;
    for (; i != args_end; i++)
      if (*i == "--")
	break;

    regexps_to_predicate(*this, arg, i, true,
			 (command == "b" && ! show_subtotal &&
			  display_predicate.empty()));
    if (i != args_end)
      regexps_to_predicate(*this, i, args_end);
  }

  // Setup the default value for the display predicate

  if (display_predicate.empty()) {
    if (command == "b") {
      if (! show_empty)
	display_predicate = "T";
      if (! show_subtotal) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	display_predicate += "l<=1";
      }
    }
    else if (command == "E") {
      display_predicate = "t";
    }
  }

  // Parse the sort_string

  if (! sort_order.get() && ! sort_string.empty()) {
    try {
      std::istringstream stream(sort_string);
      sort_order.reset(parse_value_expr(stream));
      if (stream.peek() != -1) {
	throw value_expr_error(std::string("Unexpected character '") +
			       char(stream.peek()) + "'");
      }
      else if (! sort_order.get()) {
	throw error("Failed to parse sort criteria!");
      }
    }
    catch (const value_expr_error& err) {
      throw error(std::string("In sort criteria: ") + err.what());
    }
  }

  // Setup the values of %t and %T, used in format strings

  try {
    if (! format_t::value_expr)
      format_t::value_expr = parse_value_expr(value_expr);
    if (! format_t::value_expr)
      throw value_expr_error(std::string("Failed to parse '") +
			     value_expr + "'");
  }
  catch (const value_expr_error& err) {
    throw error(std::string("In value expression to -t: ") + err.what());
  }

  try {
    if (! format_t::total_expr)
      format_t::total_expr = parse_value_expr(total_expr);
    if (! format_t::total_expr)
      throw value_expr_error(std::string("Failed to parse '") +
			     total_expr + "'");
  }
  catch (const value_expr_error& err) {
    throw error(std::string("In value expression to -T: ") + err.what());
  }

  // If downloading is to be supported, configure the updater

  if (! commodity_t::updater && download_quotes)
    commodity_t::updater = new quotes_by_script(price_db,
						pricing_leeway,
						cache_dirty);

  // Configure the output stream

  if (! output_stream.get() && ! output_file.empty())
    output_stream.reset(new std::ofstream(output_file.c_str()));

  // Parse the interval specifier, if provided

  if (! report_interval && ! interval_text.empty()) {
    try {
      std::istringstream stream(interval_text);

      report_interval.parse(stream);

      if (report_interval.begin) {
	if (! predicate.empty())
	  predicate += "&";
	char buf[32];
	std::sprintf(buf, "d>=%lu", report_interval.begin);
	predicate += buf;
      }

      if (report_interval.end) {
	if (! predicate.empty())
	  predicate += "&";
	char buf[32];
	std::sprintf(buf, "d<%lu", report_interval.end);
	predicate += buf;
      }
    }
    catch (const interval_expr_error& err) {
      throw error(std::string("In interval (-z) specifier: ") + err.what());
    }
  }

  if (format_t::date_format.empty() && ! date_format.empty())
    format_t::date_format = date_format;

  // Compile the format strings

  const char * f;
  if (! format_string.empty())
    f = format_string.c_str();
  else if (command == "b")
    f = bal_fmt.c_str();
  else if (command == "r")
    f = reg_fmt.c_str();
  else if (command == "E")
    f = equity_fmt.c_str();
  else
    f = print_fmt.c_str();

  std::string first_line_format;
  std::string next_lines_format;

  if (const char * p = std::strstr(f, "%/")) {
    first_line_format = std::string(f, 0, p - f);
    next_lines_format = std::string(p + 2);
  } else {
    first_line_format = next_lines_format = f;
  }

  format.reset(first_line_format);
  nformat.reset(next_lines_format);
}

static void show_version(std::ostream& out)
{
  out
    << "Ledger " << ledger::version << ", the command-line accounting tool\n\n"
    << "Copyright (c) 2003-2004, New Artisans LLC. All rights reserved.\n\n"
    << "This program is made available under the terms of the BSD Public\n"
    << "License.  See the LICENSE file included with the distribution for\n"
    << "details and disclaimer.\n";
}

void option_help(std::ostream& out)
{
  out
    << "usage: ledger [options] COMMAND [ACCT REGEX]... [-- [PAYEE REGEX]...]\n\n\
Basic options:\n\
  -h, --help            display this help text\n\
  -v, --version         show version information\n\
  -i, --init FILE       initialize ledger by loading FILE (def: ~/.ledgerrc)\n\
      --cache FILE      use FILE as a binary cache when --file is not used\n\
  -f, --file FILE       read ledger data from FILE\n\
  -o, --output FILE     write output to FILE\n\
  -z, --set-price CONV  specify a commodity conversion: \"COMM=AMOUNT\"\n\
  -a, --account NAME    specify the default account (useful with QIF files)\n\n\
Report filtering:\n\
  -b, --begin-date DATE set report begin date\n\
  -e, --end-date DATE   set report end date\n\
  -c, --current         show only current and past entries (not future)\n\
  -C, --cleared         consider only cleared transactions\n\
  -U, --uncleared       consider only uncleared transactions\n\
  -R, --real            consider only non-virtual transactions\n\
  -r, --related         calculate report using related transactions\n\n\
Output customization:\n\
  -F, --format STR      use STR as the format; for each report type, use:\n\
			  --balance-format   --equity-format\n\
			  --register-format  --plot-value-format\n\
			  --print-format     --plot-total-format\n\
  -y, --date-format STR use STR as the date format (def: %Y/%m/%d)\n\
  -E, --empty           balance: show accounts with zero balance\n\
  -n, --collapse        register: collapse entries with multiple transactions\n\
  -s, --subtotal        balance: show sub-accounts; register: show subtotals\n\
  -S, --sort EXPR       sort report according to the value expression EXPR\n\
  -p, --interval STR    report by interval (period), based on STR\n\
      --dow             show a days-of-the-week report\n\
  -W, --weekly          show weekly sub-totals\n\
  -M, --monthly         show monthly sub-totals\n\
  -Y, --yearly          show yearly sub-totals\n\
  -l, --limit EXPR      calculate only transactions matching EXPR\n\
  -d, --display EXPR    display only transactions matching EXPR\n\
  -t, --value EXPR      set the value expression for all report types\n\
  -T, --total EXPR      set the total expression for all report types\n\
  -j, --value-data      print only raw value data (useful when scripting)\n\
  -J, --total-data      print only raw total data\n\n\
Commodity reporting:\n\
  -P, --price-db FILE   sets the price database to FILE (def: ~/.pricedb)\n\
  -L, --price-exp MINS  download quotes only if newer than MINS (def: 1440)\n\
  -Q, --download        download price information when needed\n\
  -O, --quantity        report commodity totals (this is the default)\n\
  -B, --basis           report commodity cost basis\n\
  -V, --market          report commodity market value\n\
  -G, --gain            report commodity gain/loss\n\
  -A, --average         report average transaction amount\n\
  -D, --deviation       report deviation from the average\n\
  -X, --trend           report average deviation from the average\n\
  -Z, --weighted-trend  same as trend, but older values are less significant\n\
			(-D, -X and -Z make little sense in balance reports)\n\
Commands:\n\
  balance [REGEXP]...   show balance totals for matching accounts\n\
  register [REGEXP]...  show register of matching transactions\n\
  print [REGEXP]...     print all matching entries\n\
  equity [REGEXP]...    output equity entries for matching accounts\n\
  entry DATE PAYEE AMT  output a derived entry, based on the arguments\n";
}

//////////////////////////////////////////////////////////////////////
//
// Basic options

OPT_BEGIN(help, "h") {
  option_help(std::cout);
  throw 0;
} OPT_END(help);

OPT_BEGIN(version, "v") {
  show_version(std::cout);
  throw 0;
} OPT_END(version);

OPT_BEGIN(init, "i:") {
  config.init_file = optarg;
} OPT_END(init);

OPT_BEGIN(file, "f:") {
  config.data_file = optarg;
} OPT_END(file);

OPT_BEGIN(cache, ":") {
  config.cache_file = optarg;
} OPT_END(cache);

OPT_BEGIN(output, "o:") {
  if (std::string(optarg) != "-")
    config.output_file = optarg;
} OPT_END(output);

OPT_BEGIN(set_price, "z:") {
  if (std::strchr(optarg, '='))
    config.price_settings.push_back(optarg);
  else
    throw error(std::string("Invalid price setting: ") + optarg);
} OPT_END(set_price);

OPT_BEGIN(account, "a:") {
  config.account = optarg;
} OPT_END(account);

//////////////////////////////////////////////////////////////////////
//
// Report filtering

OPT_BEGIN(begin_date, "b:") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d>=[";
  config.predicate += optarg;
  config.predicate += "]";
} OPT_END(begin_date);

OPT_BEGIN(end_date, "e:") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d<[";
  config.predicate += optarg;
  config.predicate += "]";
} OPT_END(end_date);

OPT_BEGIN(current, "c") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d<=m";
} OPT_END(current);

OPT_BEGIN(cleared, "C") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "X";
} OPT_END(cleared);

OPT_BEGIN(uncleared, "U") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "!X";
} OPT_END(uncleared);

OPT_BEGIN(real, "R") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "R";
} OPT_END(real);

//////////////////////////////////////////////////////////////////////
//
// Output customization

OPT_BEGIN(format, "F:") {
  config.format_string = optarg;
} OPT_END(format);

OPT_BEGIN(date_format, "y:") {
  config.date_format = optarg;
} OPT_END(date_format);

OPT_BEGIN(balance_format, ":") {
  bal_fmt = optarg;
} OPT_END(balance_format);

OPT_BEGIN(register_format, ":") {
  reg_fmt = optarg;
} OPT_END(register_format);

OPT_BEGIN(plot_value_format, ":") {
  plot_value_fmt = optarg;
} OPT_END(plot_value_format);

OPT_BEGIN(plot_total_format, ":") {
  plot_total_fmt = optarg;
} OPT_END(plot_total_format);

OPT_BEGIN(print_format, ":") {
  print_fmt = optarg;
} OPT_END(print_format);

OPT_BEGIN(equity_format, ":") {
  equity_fmt = optarg;
} OPT_END(equity_format);

OPT_BEGIN(empty, "E") {
  config.show_empty = true;
} OPT_END(empty);

OPT_BEGIN(collapse, "n") {
  config.show_collapsed = true;
} OPT_END(collapse);

OPT_BEGIN(subtotal, "s") {
  config.show_subtotal = true;
} OPT_END(subtotal);

OPT_BEGIN(sort, "S:") {
  config.sort_string = optarg;
} OPT_END(sort);

OPT_BEGIN(related, "r") {
  config.show_related = true;
} OPT_END(related);

OPT_BEGIN(interval, "p:") {
  config.interval_text = optarg;
} OPT_END(interval);

OPT_BEGIN(weekly, "W") {
  config.interval_text = "weekly";
} OPT_END(weekly);

OPT_BEGIN(dow, "") {
  config.days_of_the_week = true;
} OPT_END(dow);

OPT_BEGIN(monthly, "M") {
  config.interval_text = "monthly";
} OPT_END(monthly);

OPT_BEGIN(yearly, "Y") {
  config.interval_text = "yearly";
} OPT_END(yearly);

OPT_BEGIN(limit, "l:") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "(";
  config.predicate += optarg;
  config.predicate += ")";
} OPT_END(limit);

OPT_BEGIN(display, "d:") {
  if (! config.display_predicate.empty())
    config.display_predicate += "&";
  config.display_predicate += "(";
  config.display_predicate += optarg;
  config.display_predicate += ")";
} OPT_END(display);

OPT_BEGIN(value, "t:") {
  config.value_expr = optarg;
} OPT_END(value);

OPT_BEGIN(total, "T:") {
  config.total_expr = optarg;
} OPT_END(total);

OPT_BEGIN(value_data, "j") {
  config.value_expr    = "S" + config.value_expr;
  config.format_string = plot_value_fmt;
} OPT_END(value_data);

OPT_BEGIN(total_data, "J") {
  config.total_expr    = "S" + config.total_expr;
  config.format_string = plot_total_fmt;
} OPT_END(total_data);

//////////////////////////////////////////////////////////////////////
//
// Commodity reporting

OPT_BEGIN(price_db, "P:") {
  config.price_db = optarg;
} OPT_END(price_db);

OPT_BEGIN(price_exp, "L:") {
  config.pricing_leeway = std::atol(optarg) * 60;
} OPT_END(price_exp);

OPT_BEGIN(download, "Q") {
  config.download_quotes = true;
} OPT_END(download);

OPT_BEGIN(quantity, "O") {
  config.value_expr = "a";
  config.total_expr = "O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  config.value_expr = "c";
  config.total_expr = "C";
} OPT_END(basis);

OPT_BEGIN(market, "V") {
  config.show_revalued = true;

  config.value_expr = "v";
  config.total_expr = "V";
} OPT_END(market);

OPT_BEGIN(gain, "G") {
  config.show_revalued      =
  config.show_revalued_only = true;

  config.value_expr = "a";
  config.total_expr = "G";
} OPT_END(gain);

OPT_BEGIN(average, "A") {
  config.value_expr = "a";
  config.total_expr = "MO";
} OPT_END(average);

OPT_BEGIN(deviation, "D") {
  config.value_expr = "a";
  config.total_expr = "DMO";
} OPT_END(deviation);

OPT_BEGIN(trend, "X") {
  config.value_expr = "a";
  config.total_expr = "MDMO";
} OPT_END(trend);

OPT_BEGIN(weighted_trend, "Z") {
  config.value_expr = "a";
  config.total_expr
    = "MD(MO/(1+(((m-d)/(30*86400))<0?0:((m-d)/(30*86400)))))";
} OPT_END(weighted_trend);

} // namespace ledger
