#include "ledger.h"
#include "error.h"
#include "textual.h"
#include "binary.h"
#include "valexpr.h"
#include "format.h"
#include "walk.h"

#include <fstream>
#include <cstring>
#include <unistd.h>
#include <ctime>

namespace ledger {


//////////////////////////////////////////////////////////////////////
//
// The command-line balance report
//

static const std::string bal_fmt = "%20T  %2_%-n\n";

#if 0

unsigned int show_balances(std::ostream&   out,
			   items_deque&	   items,
			   const node_t *  predicate,
			   const node_t *  sort_order,
			   const format_t& format,
			   const bool      show_expanded,
			   const item_t *  displayed_parent)
{
  unsigned int    headlines = 0;
  value_predicate pred_obj(predicate);

  for (items_deque::const_iterator i = items.begin();
       i != items.end();
       i++) {
    const item_t * parent = displayed_parent;

    if (pred_obj(*i) &&
	((*i)->subitems.size() != 1 ||
	 (*i)->total != (*i)->subitems[0]->total)) {
      format.format_elements(out, *i, parent);
      parent = *i;

      if (! displayed_parent->parent)
	headlines++;
    }

    if (sort_order)
      (*i)->sort(sort_order);

    if (show_expanded)
      headlines += show_balances(out, (*i)->subitems, predicate,
				 sort_order, format, true, parent);
  }

  return headlines;
}

void balance_report(std::ostream&   out,
		    item_t *	    top,
		    const node_t *  predicate,
		    const node_t *  sort_order,
		    const format_t& format,
		    const bool      show_expanded,
		    const bool      show_subtotals)
{
  if (sort_order)
    top->sort(sort_order);

  unsigned int headlines = show_balances(out, top->subitems, predicate,
					 sort_order, format, show_expanded,
					 top);

  if (show_subtotals && headlines > 1 && top->total) {
    std::cout << "--------------------\n";
    format.format_elements(std::cout, top);
  }
}

#endif

//////////////////////////////////////////////////////////////////////
//
// The command-line register and print report
//

static const std::string reg_fmt
  = "%10d %-.20p %-.22N %12.66t %12.80T\n\
%/                                %-.22N %12.66t %12.80T\n";

static const std::string print_fmt
  = "\n%10d %X%C%p\n    %-34N  %12o\n%/    %-34N  %12o\n";

static bool show_commodities_revalued      = false;
static bool show_commodities_revalued_only = false;

#if 0

static void report_value_change(std::ostream&         out,
				const std::time_t     date,
				const balance_pair_t& balance,
				const balance_pair_t& prev_balance,
				const node_t *        predicate,
				const format_t&       first_line_format,
				const format_t&       next_lines_format)
{
  static std::time_t prev_date = -1;
  if (prev_date == -1) {
    prev_date = date;
    return;
  }

  item_t temp;
  temp.date  = prev_date;
  temp.total = prev_balance;
  balance_t prev_bal = format_t::compute_total(&temp);

  temp.date  = date;
  temp.total = balance;
  balance_t cur_bal = format_t::compute_total(&temp);

  if (balance_t diff = cur_bal - prev_bal) {
    temp.value = diff;
    temp.total = balance;
    temp.payee = "Commodities revalued";

    if (value_predicate(predicate)(&temp)) {
      first_line_format.format_elements(out, &temp);
      next_lines_format.format_elements(out, &temp);
    }
  }

  prev_date = date;
}

void register_report(std::ostream&   out,
		     item_t *	     top,
		     const node_t *  predicate,
		     const node_t *  sort_order,
		     const format_t& first_line_format,
		     const format_t& next_lines_format,
		     const bool      show_expanded)
{
  if (sort_order)
    top->sort(sort_order);

  balance_pair_t  balance;
  balance_pair_t  last_reported;
  account_t       splits(NULL, "<Total>");
  value_predicate pred_obj(predicate);

  for (items_deque::const_iterator i = top->subitems.begin();
       i != top->subitems.end();
       i++) {
    bool first = true;

    if ((*i)->subitems.size() > 1 && ! show_expanded) {
      item_t summary(*i);
      summary.parent  = *i;
      summary.account = &splits;

      summary.value   = 0;
      for (items_deque::const_iterator j = (*i)->subitems.begin();
	   j != (*i)->subitems.end();
	   j++)
	summary.value += (*j)->value;
      summary.total = balance + summary.value;

      bool show = pred_obj(&summary);
      if (show && show_commodities_revalued)
	report_value_change(out, summary.date, balance, last_reported,
			    predicate, first_line_format, next_lines_format);

      balance += summary.value;

      if (show) {
	if (! show_commodities_revalued_only)
	  first_line_format.format_elements(out, &summary, top);

	if (show_commodities_revalued)
	  last_reported = balance;
      }
    } else {
      for (items_deque::const_iterator j = (*i)->subitems.begin();
	   j != (*i)->subitems.end();
	   j++) {
	(*j)->total = balance + (*j)->value;

	bool show = pred_obj(*j);
	if (show && first && show_commodities_revalued) {
	  report_value_change(out, (*i)->date, balance, last_reported,
			      predicate, first_line_format, next_lines_format);
	  if (show_commodities_revalued_only)
	    first = false;
	}

	balance += (*j)->value;

	if (show) {
	  if (! show_commodities_revalued_only) {
	    if (first) {
	      first = false;
	      first_line_format.format_elements(out, *j, *i);
	    } else {
	      next_lines_format.format_elements(out, *j, *i);
	    }
	  }

	  if (show_commodities_revalued)
	    last_reported = balance;
	}
      }
    }
  }

  if (show_commodities_revalued)
    report_value_change(out, -1, balance, last_reported, predicate,
			first_line_format, next_lines_format);
}

#endif

void set_price_conversion(const std::string& setting)
{
  char buf[128];
  std::strcpy(buf, setting.c_str());

  assert(setting.length() < 128);

  char * c = buf;
  char * p = std::strchr(buf, '=');
  if (! p) {
    std::cerr << "Warning: Invalid price setting: " << setting << std::endl;
  } else {
    *p++ = '\0';

    amount_t price;
    price.parse(p);

    commodity_t * commodity = commodity_t::find_commodity(c, true);
    commodity->set_conversion(price);
  }
}


static long	   pricing_leeway = 24 * 3600;
static std::string price_db;
static bool        cache_dirty    = false;

void download_price_quote(commodity_t *	    commodity,
			  const std::time_t age,
			  const amount_t&   price,
			  const std::time_t moment)
{
  std::time_t now = std::time(NULL); // the time of the query

  if (! (commodity->flags & COMMODITY_STYLE_CONSULTED) &&
      std::difftime(now, moment) < pricing_leeway &&
      (! price || std::difftime(moment, age) > pricing_leeway)) {
    using namespace std;

    // Only consult the Internet once for any commodity
    commodity->flags |= COMMODITY_STYLE_CONSULTED;
    cache_dirty = true;

    char buf[256];
    buf[0] = '\0';

    if (FILE * fp = popen((string("getquote ") +
			   commodity->symbol).c_str(), "r")) {
      if (feof(fp) || ! fgets(buf, 255, fp)) {
	fclose(fp);
	return;
      }
      fclose(fp);
    }

    if (buf[0]) {
      char * p = strchr(buf, '\n');
      if (p) *p = '\0';

      amount_t current;
      current.parse(buf);

      commodity->add_price(now, current);

      if (! price_db.empty()) {
	char buf[128];
	strftime(buf, 127, "%Y/%m/%d %H:%M:%S", localtime(&now));
	ofstream database(price_db.c_str(), ios_base::out | ios_base::app);
	database << "P " << buf << " " << commodity->symbol << " "
		 << current << endl;
      }
    }
  }
}

} // namespace ledger


static void show_help(std::ostream& out)
{
  std::cerr
    << "usage: ledger [options] COMMAND [options] [REGEXPS]" << std::endl
    << std::endl
    << "Basic options:" << std::endl
    << "  -h        display this help text" << std::endl
    << "  -v        display version information" << std::endl
    << "  -f FILE   specify pathname of ledger data file" << std::endl
    << std::endl
    << "Report filtering:" << std::endl
    << "  -a REGEX  specify an account regex for \"print\"" << std::endl
    << "  -b DATE   specify a beginning date" << std::endl
    << "  -e DATE   specify an ending date" << std::endl
    << "  -c        do not show future entries (same as -e TODAY)" << std::endl
    << "  -d DATE   specify a date mask ('-d mon', for all mondays)" << std::endl
    << "  -C        show only cleared transactions and balances" << std::endl
    << "  -U        show only uncleared transactions and balances" << std::endl
    << "  -R        do not consider virtual transactions: real only" << std::endl
    << "  -l EXPR   don't print entries for which EXPR yields 0" << std::endl
    << std::endl
    << "Customizing output:" << std::endl
    << "  -n        do not calculate parent account totals" << std::endl
    << "  -s        show sub-accounts in balance, and splits in register" << std::endl
    << "  -M        print register using monthly sub-totals" << std::endl
    << "  -E        show accounts that total to zero" << std::endl
    << "  -S EXPR   sort entry output based on EXPR" << std::endl
    << std::endl
    << "Commodity prices:" << std::endl
    << "  -T        report commodity totals, not their market value" << std::endl
    << "  -B        report cost basis of commodities" << std::endl
    << "  -V        report the market value of commodities" << std::endl
    << "  -P FILE   sets the price database, for reading/writing price info" << std::endl
    << "  -Q        download new price information (when needed) from the Internet" << std::endl
    << "            (works by running \"getquote SYMBOL\")" << std::endl
    << "  -L MINS   with -Q, fetch quotes only if data is older than MINS" << std::endl
    << "  -p STR    specifies a direct commodity conversion: COMM=AMOUNT" << std::endl
    << std::endl
    << "commands:" << std::endl
    << "  balance   show balance totals" << std::endl
    << "  register  display a register for ACCOUNT" << std::endl
    << "  print     print all ledger entries" << std::endl
    << "  entry     output a newly formed entry, based on arguments" << std::endl
    << "  equity    output equity entries for specified accounts" << std::endl;
}

int main(int argc, char * argv[])
{
  std::auto_ptr<ledger::ledger_t> journal(new ledger::ledger_t);
  std::list<std::string>	  files;
  std::auto_ptr<ledger::node_t>   predicate;
  std::auto_ptr<ledger::node_t>   display_predicate;
  std::auto_ptr<ledger::node_t>   sort_order;

  std::string predicate_string;
  std::string display_predicate_string;
  std::string format_string;
  std::string sort_string;
  std::string value_expr = "a";
  std::string total_expr = "T";

  bool show_subtotals = true;
  bool show_expanded  = false;
  bool show_related   = false;
  bool show_inverted  = false;
  bool show_empty     = false;

#ifdef DEBUG
  bool debug = false;
#endif

  // Initialize some variables based on environment variable settings

  if (char * p = std::getenv("PRICE_HIST"))
    ledger::price_db = p;

  if (char * p = std::getenv("PRICE_EXP"))
    ledger::pricing_leeway = std::atol(p) * 60;

  // A ledger data file must be specified

  bool use_cache = std::getenv("LEDGER") != NULL;

  if (use_cache) {
    for (int i = 0; i < argc; i++)
      if (std::strcmp(argv[i], "-f") == 0) {
	use_cache = false;
	break;
      }

    ledger::cache_dirty = true;

    if (use_cache)
      if (const char * p = std::getenv("LEDGER_CACHE"))
	if (access(p, R_OK) != -1) {
	  std::ifstream instr(p);
	  if (! ledger::read_binary_ledger(instr, std::getenv("LEDGER"),
					   journal.get())) {
	    // Throw away what's been read, and create a new journal
	    journal.reset(new ledger::ledger_t);
	  } else {
	    ledger::cache_dirty = false;
	  }
	}
  }

  // Parse the command-line options

  int c, index;
  while (-1 !=
	 (c = getopt(argc, argv,
		     "+ABb:Ccd:DEe:F:f:Ghi:L:l:MnoOP:p:QRS:st:T:UVvWXZz"))) {
    switch (char(c)) {
    // Basic options
    case 'h':
      show_help(std::cout);
      break;

#ifdef DEBUG
    case 'z':
      debug = 1;
      break;
#endif

    case 'v':
      std::cout
	<< "Ledger " << ledger::version
	<< ", the command-line accounting tool" << std::endl
	<< "  Copyright (c) 2003-2004, New Artisans LLC. All rights reserved."
	<< std::endl << std::endl
	<< "This program is made available under the terms of the BSD Public"
	<< std::endl
	<< "License.  See the LICENSE file included with the distribution for"
	<< std::endl
	<< "details and disclaimer." << std::endl;
      return 0;

    case 'f':
      files.push_back(optarg);
      use_cache = false;
      break;

    case 'p':
      ledger::set_price_conversion(optarg);
      break;

    case 'b':
      if (! predicate_string.empty())
	predicate_string += "&";
      predicate_string += "(d>=[";
      predicate_string += optarg;
      predicate_string += "])";
      break;

    case 'e':
      if (! predicate_string.empty())
	predicate_string += "&";
      predicate_string += "(d<[";
      predicate_string += optarg;
      predicate_string += "])";
      break;

    case 'c': {
      if (! predicate_string.empty())
	predicate_string += "&";
      predicate_string += "(d<";
      std::ostringstream now;
      now << std::time(NULL);
      predicate_string += now.str();
      predicate_string += ")";
      break;
    }

    case 'C':
      if (! predicate_string.empty())
	predicate_string += "&";
      predicate_string += "X";
      break;

    case 'U':
      if (! predicate_string.empty())
	predicate_string += "&";
      predicate_string += "!X";
      break;

    case 'R':
      if (! predicate_string.empty())
	predicate_string += "&";
      predicate_string += "R";
      break;

    // Customizing output
    case 'F':
      format_string = optarg;
      break;

    case 'E':
      show_empty = true;
      break;

    case 'n':
      show_subtotals = false;
      break;

    case 's':
      show_expanded = true;
      break;

    case 'S':
      sort_string = optarg;
      break;

    case 'o':
      show_related = true;
      break;

    case 'l':
      if (! predicate_string.empty())
	predicate_string += "&";
      predicate_string += "(";
      predicate_string += optarg;
      predicate_string += ")";
      break;

    case 'd':
      if (! display_predicate_string.empty())
	display_predicate_string += "&";
      display_predicate_string += "(";
      display_predicate_string += optarg;
      display_predicate_string += ")";
      break;

    // Commodity reporting
    case 'P':
      ledger::price_db = optarg;
      break;

    case 'L':
      ledger::pricing_leeway = std::atol(optarg) * 60;
      break;

    case 'Q':
      ledger::commodity_t::updater = ledger::download_price_quote;
      break;

    case 't':
      value_expr = optarg;
      break;

    case 'T':
      total_expr = optarg;
      break;

    case 'O':
      value_expr = "a";
      total_expr = "T";
      break;

    case 'B':
      value_expr = "c";
      total_expr = "C";
      break;

    case 'V':
      ledger::show_commodities_revalued = true;

      value_expr = "v";
      total_expr = "V";
      break;

    case 'G':
      ledger::show_commodities_revalued      =
      ledger::show_commodities_revalued_only = true;

      value_expr = "c";
      total_expr = "G";
      break;

    case 'A':
      value_expr = "a";
      total_expr = "MT";
      break;

    case 'D':
      value_expr = "a";
      total_expr = "DMT";
      break;

    case 'Z':
      value_expr = "a";
      total_expr = "MDMT";
      break;

#if 0
    case 'W':
      value_expr = "a";
      total_expr = "MD(MT*(d-b/e-b))";
      break;

    case 'X':
      value_expr = "a";
      total_expr = "a+MD(MT*(d-b/e-b))";
      break;
#endif

    default:
      assert(0);
      break;
    }
  }

  if (optind == argc) {
    show_help(std::cout);
    return 1;
  }

  index = optind;

  // Read the ledger file, unless we already read it from the cache

  if (! use_cache || ledger::cache_dirty) {
    int entry_count = 0;

    try {
      if (files.empty()) {
	if (char * p = std::getenv("LEDGER"))
	  for (p = std::strtok(p, ":"); p; p = std::strtok(NULL, ":"))
	    entry_count += parse_ledger_file(p, journal.get());
      } else {
	for (std::list<std::string>::iterator i = files.begin();
	     i != files.end(); i++) {
	  char buf[4096];
	  char * p = buf;
	  std::strcpy(p, (*i).c_str());
	  entry_count += parse_ledger_file(p, journal.get());
	}
      }

      // Read prices from their own ledger file, after all others have
      // been read.

      if (! ledger::price_db.empty()) {
	const char * path = ledger::price_db.c_str();
	std::ifstream db(path);
	journal->sources.push_back(path);
	entry_count += ledger::parse_textual_ledger(db, journal.get(),
						    journal->master);
      }
    }
    catch (ledger::error& err) {
      std::cerr << "Fatal: " << err.what() << std::endl;
      return 1;
    }

    if (entry_count == 0) {
      std::cerr << ("Please specify ledger file(s) using -f option "
		    "or LEDGER environment variable.") << std::endl;
      return 1;
    }
  }

  // Read the command word, and then check and simplify it

  std::string command = argv[index++];

  if (command == "balance" || command == "bal" || command == "b")
    command = "b";
  else if (command == "register" || command == "reg" || command == "r")
    command = "r";
  else if (command == "print" || command == "p")
    command = "p";
  else if (command == "entry")
    command = "e";
  else if (command == "equity")
    command = "E";
  else {
    std::cerr << "Error: Unrecognized command '" << command << "'."
	      << std::endl;
    return 1;
  }

  // Process the remaining command-line arguments

  std::auto_ptr<ledger::entry_t> new_entry;
  if (command == "entry") {
    new_entry.reset(journal->derive_entry(argc - index, &argv[index]));
  } else {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    bool have_regexps = index < argc;
    bool first	      = true;

    for (; index < argc; index++) {
      if (std::strcmp(argv[index], "--") == 0) {
	index++;
	if (! first && index < argc)
	  predicate_string += ")";
	break;
      }

      if (! show_expanded && command == "b")
	show_expanded = true;

      if (first) {
	if (! predicate_string.empty())
	  predicate_string += "&(";
	else
	  predicate_string += "(";
	first = false;
      }
      else if (argv[index][0] == '-') {
	predicate_string += "&";
      }
      else {
	predicate_string += "|";
      }

      if (argv[index][0] == '-') {
	predicate_string += "!/";
	predicate_string += argv[index] + 1;
      } else {
	predicate_string += "/";
	predicate_string += argv[index];
      }
      predicate_string += "/";
    }

    if (index < argc) {
      if (! predicate_string.empty())
	predicate_string += "&(";
      else
	predicate_string += "(";
    }

    first = true;
    for (; index < argc; index++) {
      if (! show_expanded && command == "b")
	show_expanded = true;

      if (first)
	first = false;
      else if (argv[index][0] == '-')
	predicate_string += "&";
      else
	predicate_string += "|";

      if (argv[index][0] == '-') {
	predicate_string += "!//";
	predicate_string += argv[index] + 1;
      } else {
	predicate_string += "//";
	predicate_string += argv[index];
      }
      predicate_string += "/";
    }

    if (have_regexps)
      predicate_string += ")";
  }

  // Compile the predicates

  if (! predicate_string.empty()) {
#ifdef DEBUG
    if (debug)
      std::cerr << "predicate = " << predicate_string << std::endl;
#endif
    predicate.reset(ledger::parse_expr(predicate_string));
  }

  if (display_predicate_string.empty() && command == "b" && ! show_empty)
    display_predicate_string = "T";

  if (! display_predicate_string.empty()) {
#ifdef DEBUG
    if (debug)
      std::cerr << "display predicate = " << display_predicate_string
		<< std::endl;
#endif
    display_predicate.reset(ledger::parse_expr(display_predicate_string));
  }

  // Compile the sorting string

  if (! sort_string.empty())
    sort_order.reset(ledger::parse_expr(sort_string));

  // Setup the meaning of %t and %T encountered in format strings

  ledger::format_t::value_expr.reset(ledger::parse_expr(value_expr));
  ledger::format_t::total_expr.reset(ledger::parse_expr(total_expr));

  // Now handle the command that was identified above.

  if (command == "p" || command == "e") {
    show_related  = true;
    show_expanded = true;
  }
  else if (command == "E") {
    show_expanded = true;
  }
  else if (show_related && command == "r") {
    show_inverted = true;
  }

  const char * f;
  if (! format_string.empty())
    f = format_string.c_str();
  else if (command == "b")
    f = ledger::bal_fmt.c_str();
  else if (command == "r")
    f = ledger::reg_fmt.c_str();
  else
    f = ledger::print_fmt.c_str();

  if (command == "b") {
    std::auto_ptr<ledger::format_t> format(new ledger::format_t(f));

    ledger::walk_accounts(journal->master,
			  ledger::format_account(std::cout, *format.get()),
			  predicate.get(), show_related, show_inverted,
			  show_subtotals, display_predicate.get());

    if (! display_predicate.get() ||
	ledger::item_predicate(display_predicate.get())(journal->master)) {
      std::string end_format = "--------------------\n";
      end_format += f;
      format.get()->elements.reset(ledger::format_t::parse_elements(end_format));
      ledger::format_account(std::cout, *format.get())(journal->master, true);
    }
  } else {
    std::string first_line_format;
    std::string next_lines_format;

    if (const char * p = std::strstr(f, "%/")) {
      first_line_format = std::string(f, 0, p - f);
      next_lines_format = std::string(p + 2);
    } else {
      first_line_format = next_lines_format = f;
    }

    std::auto_ptr<ledger::format_t>
      format(new ledger::format_t(first_line_format));
    std::auto_ptr<ledger::format_t>
      nformat(new ledger::format_t(next_lines_format));

    ledger::walk_entries(journal->entries.begin(), journal->entries.end(),
			 ledger::format_transaction(std::cout,
						    first_line_format,
						    next_lines_format),
			 predicate.get(), show_related, show_inverted,
			 display_predicate.get());
  }

  // Save the cache, if need be

  if (use_cache && ledger::cache_dirty)
    if (const char * p = std::getenv("LEDGER_CACHE")) {
      std::ofstream outstr(p);
      assert(std::getenv("LEDGER"));
      ledger::write_binary_ledger(outstr, journal.get(),
				  std::getenv("LEDGER"));
    }

  return 0;
}

// main.cc ends here.
