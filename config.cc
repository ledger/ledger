#include "config.h"
#include "option.h"

namespace ledger {

config_t * config = NULL;

const std::string bal_fmt	 = "%20T  %2_%-n\n";
const std::string reg_fmt
  = "%D %-.20P %-.22N %12.66t %12.80T\n\
%/                                %-.22N %12.66t %12.80T\n";
const std::string plot_value_fmt = "%D %t\n";
const std::string plot_total_fmt = "%D %T\n";
const std::string print_fmt
  = "\n%D %X%C%P\n    %-34N  %12o\n%/    %-34N  %12o\n";
const std::string equity_fmt
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
  show_subtotals     = true;
  show_expanded      = false;
  show_related       = false;
  show_inverted      = false;
  show_empty	     = false;
  days_of_the_week   = false;
  show_revalued      = false;
  show_revalued_only = false;
  download_quotes    = false;
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
    << "usage: ledger [options] COMMAND [options] [REGEXPS]\n\n\
Basic options:\n\
  -h, --help            display this help text\n\
  -v, --version         display version information\n\
  -i, --init FILE       initialize ledger by loading FILE\n\
  -f, --file FILE       specify pathname of ledger data file\n\
  -o, --output FILE     write all output to FILE\n\
  -p, --set-price CONV  specifies commodity conversion: COMM=AMOUNT\n\n\
Report filtering:\n\
  -b, --begin-date DATE specify a beginning date\n\
  -e, --end-date DATE   specify an ending date\n\
  -c, --current         do not show future entries (same as -e TODAY)\n\
  -C, --cleared         show only cleared transactions and balances\n\
  -U, --uncleared       show only uncleared transactions and balances\n\
  -R, --real            do not consider virtual transactions: real only\n\n\
Output customization:\n\
  -F, --format STR      \n\
  -y, --date-format STR \n\
  -E, --empty           balance: also show accounts that total to zero\n\
  -n, --collapse        balance: no parent account totals; register: collapse\n\
  -s, --show-all        balance: show sub-accounts; register: show subtotals\n\
  -S, --sort EXPR       sort report according to value EXPR\n\
  -r, --related         \n\
  -z, --interval EXPR   \n\
  -w, --dow             print register using day of week sub-totals\n\
  -W, --weekly                 \"   \"         weekly sub-totals\n\
  -M, --monthly                \"   \"         monthly sub-totals\n\
  -Y, --yearly                 \"   \"         yearly sub-totals\n\
  -l, --limit EXPR      don't calculate entries for which EXPR yields 0\n\
  -d, --display EXPR    don't print entries for which EXPR yields 0\n\
  -t, --value EXPR      \n\
  -T, --total EXPR      \n\
  -j, --value-data      \n\
  -J, --total-data      \n\n\
Commodity reporting:\n\
  -P, --price-db FILE   sets the price database\n\
  -L, --price-exp MINS  with -Q, fetch quotes only if data is older than MINS\n\
  -Q, --download        download price information from the Internet\n\
			(works by running \"getquote SYMBOL\")\n\
  -O, --quantity        \n\
  -B, --basis           report cost basis of commodities\n\
  -V, --market          report the market value of commodities\n\
  -G, --gain            \n\
  -A, --average         \n\
  -D, --deviation       \n\
  -X, --trend           \n\
  -Z, --weighted-trend  \n\n\
Commands:\n\
  balance       show balance totals\n\
  register      display a register for ACCOUNT\n\
  print         print all ledger entries\n\
  entry         output a newly formed entry, based on arguments\n\
  equity        output equity entries for specified accounts\n";
}

//////////////////////////////////////////////////////////////////////
//
// Basic options

DEF_OPT_HANDLERS();

OPT_BEGIN(help, "h") {
  option_help(std::cout);
  std::exit(0);
} OPT_END(help);

OPT_BEGIN(version, "v") {
  show_version(std::cout);
  std::exit(0);
} OPT_END(version);

OPT_BEGIN(init, "i:") {
  config->init_file = optarg;
} OPT_END(init);

OPT_BEGIN(file, "f:") {
  char * buf = new char[std::strlen(optarg) + 1];
  std::strcpy(buf, optarg);
  for (char * p = std::strtok(buf, ":");
       p;
       p = std::strtok(NULL, ":"))
    config->files.push_back(p);
  delete[] buf;
} OPT_END(file);

OPT_BEGIN(cache, ":") {
  config->cache_file = optarg;
} OPT_END(cache);

OPT_BEGIN(output, "o:") {
  if (std::string(optarg) != "-")
    config->output_file = optarg;
} OPT_END(output);

OPT_BEGIN(set_price, "p:") {
  if (std::strchr(optarg, '='))
    config->price_settings.push_back(optarg);
  else
    std::cerr << "Error: Invalid price setting: " << optarg << std::endl;
} OPT_END(set_price);

//////////////////////////////////////////////////////////////////////
//
// Report filtering

OPT_BEGIN(begin_date, "b:") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "(d>=[";
  config->predicate += optarg;
  config->predicate += "])";
} OPT_END(begin_date);

OPT_BEGIN(end_date, "e:") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "(d<[";
  config->predicate += optarg;
  config->predicate += "])";
} OPT_END(end_date);

OPT_BEGIN(current, "c") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "(d<=N)";
} OPT_END(current);

OPT_BEGIN(cleared, "C") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "X";
} OPT_END(cleared);

OPT_BEGIN(uncleared, "U") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "!X";
} OPT_END(uncleared);

OPT_BEGIN(real, "R") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "R";
} OPT_END(real);

//////////////////////////////////////////////////////////////////////
//
// Output customization

OPT_BEGIN(format, "F:") {
  config->format_string = optarg;
} OPT_END(format);

OPT_BEGIN(date_format, "y:") {
  config->date_format = optarg;
} OPT_END(date_format);

OPT_BEGIN(empty, "E") {
  config->show_empty = true;
} OPT_END(empty);

OPT_BEGIN(collapse, "n") {
  config->show_subtotals = false;
} OPT_END(collapse);

OPT_BEGIN(show_all, "s") {
  config->show_expanded = true;
} OPT_END(show_all);

OPT_BEGIN(sort, "S:") {
  config->sort_string = optarg;
} OPT_END(sort);

OPT_BEGIN(related, "r") {
  config->show_related = true;
} OPT_END(related);

OPT_BEGIN(interval, "z:") {
  config->interval_text = optarg;
} OPT_END(interval);

OPT_BEGIN(weekly, "W") {
  config->interval_text = "weekly";
} OPT_END(weekly);

OPT_BEGIN(dow, "w") {
  config->days_of_the_week = true;
} OPT_END(dow);

OPT_BEGIN(monthly, "M") {
  config->interval_text = "monthly";
} OPT_END(monthly);

OPT_BEGIN(yearly, "Y") {
  config->interval_text = "yearly";
} OPT_END(yearly);

OPT_BEGIN(limit, "l:") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "(";
  config->predicate += optarg;
  config->predicate += ")";
} OPT_END(limit);

OPT_BEGIN(display, "d:") {
  if (! config->display_predicate.empty())
    config->display_predicate += "&";
  config->display_predicate += "(";
  config->display_predicate += optarg;
  config->display_predicate += ")";
} OPT_END(display);

OPT_BEGIN(value, "t:") {
  config->value_expr = optarg;
} OPT_END(value);

OPT_BEGIN(total, "T:") {
  config->total_expr = optarg;
} OPT_END(total);

OPT_BEGIN(value_data, "j") {
  config->value_expr    = "S" + config->value_expr;
  config->format_string = plot_value_fmt;
} OPT_END(value_data);

OPT_BEGIN(total_data, "J") {
  config->total_expr    = "S" + config->total_expr;
  config->format_string = plot_total_fmt;
} OPT_END(total_data);

//////////////////////////////////////////////////////////////////////
//
// Commodity reporting

OPT_BEGIN(price_db, "P:") {
  config->price_db = optarg;
} OPT_END(price_db);

OPT_BEGIN(price_exp, "L:") {
  config->pricing_leeway = std::atol(optarg) * 60;
} OPT_END(price_exp);

OPT_BEGIN(download, "Q") {
  config->download_quotes = true;
} OPT_END(download);

OPT_BEGIN(quantity, "O") {
  config->value_expr = "a";
  config->total_expr = "O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  config->value_expr = "c";
  config->total_expr = "C";
} OPT_END(basis);

OPT_BEGIN(market, "V") {
  config->show_revalued = true;

  config->value_expr = "v";
  config->total_expr = "V";
} OPT_END(market);

OPT_BEGIN(gain, "G") {
  config->show_revalued      =
  config->show_revalued_only = true;

  config->value_expr = "a";
  config->total_expr = "G";
} OPT_END(gain);

OPT_BEGIN(average, "A") {
  config->value_expr = "a";
  config->total_expr = "MO";
} OPT_END(average);

OPT_BEGIN(deviation, "D") {
  config->value_expr = "a";
  config->total_expr = "DMO";
} OPT_END(deviation);

OPT_BEGIN(trend, "X") {
  config->value_expr = "a";
  config->total_expr = "MDMO";
} OPT_END(trend);

OPT_BEGIN(weighted_trend, "Z") {
  config->value_expr = "a";
  config->total_expr
    = "MD(MO/(1+(((t-d)/(30*86400))<0?0:((t-d)/(30*86400)))))";
} OPT_END(weighted_trend);

} // namespace ledger
