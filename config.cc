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
  show_collapsed     = false;
  show_subtotal      = false;
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
    << "usage: ledger [options] COMMAND [ACCT REGEX]... [-- [PAYEE REGEX]...]\n\n\
Basic options:\n\
  -h, --help            display this help text\n\
  -v, --version         show version information\n\
  -i, --init FILE       initialize ledger by loading FILE (def: ~/.ledgerrc)\n\
  -f, --file FILE       read ledger data from FILE\n\
  -o, --output FILE     write output to FILE\n\
  -p, --set-price CONV  specify a commodity conversion: \"COMM=AMOUNT\"\n\n\
Report filtering:\n\
  -b, --begin-date DATE set report begin date\n\
  -e, --end-date DATE   set report end date\n\
  -c, --current         show only current and past entries (not future)\n\
  -C, --cleared         consider only cleared transactions\n\
  -U, --uncleared       consider only uncleared transactions\n\
  -R, --real            consider only non-virtual transactions\n\n\
  -r, --related         calculate report using related transactions\n\
Output customization:\n\
  -F, --format STR      use STR as the report format\n\
  -y, --date-format STR use STR as the date format (def: %Y/%m/%d)\n\
  -E, --empty           balance: show accounts with zero balance\n\
  -n, --collapse        register: collapse entries with multiple transactions\n\
  -s, --subtotal        balance: show sub-accounts; register: show subtotals\n\
  -S, --sort EXPR       sort report according to the value expression EXPR\n\
  -z, --interval STR    report by interval, based on interval expression STR\n\
  -w, --dow             show a days-of-the-week report\n\
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
  -Z, --weighted-trend  same as trend, but older values less significant\n\n\
Commands:\n\
  balance [REGEXP]...   show balance totals for matching accounts\n\
  register [REGEXP]...  show register of matching transactions\n\
  print [REGEXP]...     print all matching entries\n\
  equity [REGEXP]...    output equity entries for matching accounts\n\
  entry DATE PAYEE AMT  output a derived entry, based on the arguments\n\n\
For commands that accepts a list of regular expressions, these match against\n\
the account.  If the separator \"--\" is specified, regexps after it are\n\
matched against the payee name.  For even more sophisticated entry matching,\n\
use --limit; to affect display only (not calculation), use --display.\n";
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
  config->predicate += "d>=[";
  config->predicate += optarg;
  config->predicate += "]";
} OPT_END(begin_date);

OPT_BEGIN(end_date, "e:") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "d<[";
  config->predicate += optarg;
  config->predicate += "]";
} OPT_END(end_date);

OPT_BEGIN(current, "c") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "d<=N";
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
  config->show_collapsed = true;
} OPT_END(collapse);

OPT_BEGIN(subtotal, "s") {
  config->show_subtotal = true;
} OPT_END(subtotal);

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
