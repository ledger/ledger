#include "ledger.h"

#include <fstream>

namespace ledger {
  extern bool parse_ledger(std::istream& in, bool compute_balances);
#ifdef READ_GNUCASH
  extern bool parse_gnucash(std::istream& in, bool compute_balances);
#endif

  extern void report_balances(int argc, char ** argv, regexps_t& regexps,
			      std::ostream& out);
  extern void print_register(int argc, char ** argv, regexps_t& regexps,
			     std::ostream& out);
  extern void print_ledger(int argc, char ** argv, regexps_t& regexps,
			   std::ostream& out);
  extern void equity_ledger(int argc, char ** argv, regexps_t& regexps,
			    std::ostream& out);

  bool        show_cleared;
  bool        get_quotes;

  std::time_t begin_date;
  bool        have_beginning;
  std::time_t end_date;
  bool        have_ending;
}

using namespace ledger;

static void show_help(std::ostream& out)
{
  std::cerr
    << "usage: ledger [options] COMMAND [options] [REGEXPS]" << std::endl
    << std::endl
    << "ledger options:" << std::endl
    << "  -C        also show cleared transactions" << std::endl
    << "  -d DATE   specify an implicit date range (e.g., -d april)"
    << std::endl
    << "  -b DATE   specify a beginning date" << std::endl
    << "  -e DATE   specify an ending date" << std::endl
    << "  -c        do not show future entries (same as -e TODAY)" << std::endl
    << "  -f FILE   specify pathname of ledger data file" << std::endl
    << "  -h        display this help text" << std::endl
#ifdef HUQUQULLAH
    << "  -H        do not auto-compute Huququ'llah" << std::endl
#endif
    << "  -i FILE   read the list of inclusion regexps from FILE" << std::endl
    << "  -p FILE   read the list of prices from FILE" << std::endl
    << "  -P        download price quotes from the Internet" << std::endl
    << "            (works by running the command \"getquote SYMBOL\")"
    << std::endl
    << "  -v        display version information" << std::endl
    << "  -w        print out warnings where applicable" << std::endl
    << std::endl
    << "commands:" << std::endl
    << "  balance   show balance totals" << std::endl
    << "  register  display a register for ACCOUNT" << std::endl
    << "  print     print all ledger entries" << std::endl
    << "  equity    generate equity ledger for all entries" << std::endl
    << std::endl
    << "`balance' options:" << std::endl
    << "  -F        print each account's full name" << std::endl
    << "  -n        do not generate totals for parent accounts" << std::endl
    << "  -s        show sub-accounts in balance totals" << std::endl
    << "  -S        show empty accounts in balance totals" << std::endl;
}

static const char *formats[] = {
  "%Y/%m/%d",
  "%m/%d",
  "%Y.%m.%d",
  "%m.%d",
  "%a",
  "%A",
  "%b",
  "%B",
  "%Y",
  NULL
};

static bool parse_date(const std::string& date_str, std::time_t * result)
{
  struct std::tm when;

  std::time_t now = std::time(NULL);
  struct std::tm * now_tm = std::localtime(&now);

  for (const char ** f = formats; *f; f++) {
    memset(&when, INT_MAX, sizeof(struct std::tm));
    if (strptime(date_str.c_str(), *f, &when)) {
      when.tm_hour = 0;
      when.tm_min  = 0;
      when.tm_sec  = 0;

      if (when.tm_year == -1)
	when.tm_year = now_tm->tm_year;

      if (std::strcmp(*f, "%Y") == 0) {
	when.tm_mon  = 0;
	when.tm_mday = 1;
      } else {
	if (when.tm_mon == -1)
	  when.tm_mon = now_tm->tm_mon;
	if (when.tm_mday == -1)
	  when.tm_mday = now_tm->tm_mday;
      }
      *result = std::mktime(&when);
      return true;
    }
  }
  return false;
}

//////////////////////////////////////////////////////////////////////
//
// Command-line parser and top-level logic.
//

int main(int argc, char * argv[])
{
  std::istream * file = NULL;

  regexps_t regexps;

#ifdef HUQUQULLAH
  bool compute_huquq = true;
#endif
  have_beginning = false;
  have_ending    = false;
  show_cleared   = false;

  // Parse the command-line options

  int c;
  while (-1 != (c = getopt(argc, argv, "+b:e:d:cChHwf:i:p:Pv"))) {
    switch (char(c)) {
    case 'b':
    case 'e': {
      std::time_t when;
      if (! parse_date(optarg, &when)) {
	std::cerr << "Error: Bad date string: " << optarg << std::endl;
	return 1;
      }

      if (c == 'b') {
	begin_date     = when;
	have_beginning = true;
      } else {
	end_date       = when;
	have_ending    = true;
      }
      break;
    }

    case 'd': {
      if (! parse_date(optarg, &begin_date)) {
	std::cerr << "Error: Bad date string: " << optarg << std::endl;
	return 1;
      }
      have_beginning = true;

      struct std::tm when, then;
      std::memset(&then, 0, sizeof(struct std::tm));

      std::time_t now = std::time(NULL);
      struct std::tm * now_tm = std::localtime(&now);

      for (const char ** f = formats; *f; f++) {
	memset(&when, INT_MAX, sizeof(struct std::tm));
	if (strptime(optarg, *f, &when)) {
	  then.tm_hour = 0;
	  then.tm_min  = 0;
	  then.tm_sec  = 0;

	  if (when.tm_year != -1)
	    then.tm_year = when.tm_year + 1;
	  else
	    then.tm_year = now_tm->tm_year;

	  if (std::strcmp(*f, "%Y") == 0) {
	    then.tm_mon  = 0;
	    then.tm_mday = 1;
	  } else {
	    if (when.tm_mon != -1)
	      then.tm_mon  = when.tm_mon + 1;
	    else
	      then.tm_mon = now_tm->tm_mon;

	    if (when.tm_mday != -1)
	      then.tm_mday = when.tm_mday + 1;
	    else
	      then.tm_mday = now_tm->tm_mday;
	  }

	  end_date = std::mktime(&then);
	  have_ending = true;
	  break;
	}
      }
      break;
    }

    case 'c':
      end_date = std::time(NULL);
      have_ending = true;
      break;

    case 'C': show_cleared = true; break;
    case 'h': show_help(std::cout); break;
#ifdef HUQUQULLAH
    case 'H': compute_huquq = false; break;
#endif
    case 'w': use_warnings = true; break;
    case 'f': file = new std::ifstream(optarg); break;

    // -i path-to-file-of-regexps
    case 'i':
      read_regexps(optarg, regexps);
      break;

    // -p "COMMODITY=PRICE"
    // -p path-to-price-database
    case 'p':
      if (access(optarg, R_OK) != -1) {
	std::ifstream pricedb(optarg);

	while (! pricedb.eof()) {
	  char buf[80];
	  pricedb.getline(buf, 79);
	  if (*buf && ! std::isspace(*buf))
	    main_ledger.record_price(buf);
	}
      } else {
	main_ledger.record_price(optarg);
      }
      break;

    case 'P':
      get_quotes = true;
      break;

    case 'v':
      std::cout
	<< "Ledger Accouting Tool 1.0" << std::endl
	<< "    Copyright (c) 2003 John Wiegley <johnw@newartisans.com>"
	<< std::endl << std::endl
	<< "This program is made available under the terms of the BSD"
	<< std::endl
	<< "Public License.  See the LICENSE file included with the"
	<< std::endl
	<< "distribution for details and disclaimer." << std::endl;
      return 0;
    }
  }

  if (optind == argc) {
    show_help(std::cout);
    return 1;
  }

  if (use_warnings && (have_beginning || have_ending)) {
    std::cout << "Reporting";

    if (have_beginning) {
      char buf[32];
      std::strftime(buf, 31, "%Y.%m.%d", std::localtime(&begin_date));
      std::cout << " from " << buf;
    }

    if (have_ending) {
      char buf[32];
      std::strftime(buf, 31, "%Y.%m.%d", std::localtime(&end_date));
      std::cout << " until " << buf;
    }

    std::cout << std::endl;
  }

  // A ledger data file must be specified

  if (! file) {
    const char * p = std::getenv("LEDGER");
    if (p)
      file = new std::ifstream(p);

    if (! file || ! *file) {
      std::cerr << "Please specify the ledger file using the -f option."
		<< std::endl;
      return 1;
    }
  }

  // Read the command word

  const std::string command = argv[optind];

  // Parse the ledger

#ifdef READ_GNUCASH
  char buf[32];
  file->get(buf, 31);
  file->seekg(0);

  if (std::strncmp(buf, "<?xml version=\"1.0\"?>", 21) == 0)
    parse_gnucash(*file, command == "equity");
  else
#endif
  {
#ifdef HUQUQULLAH
    if (command == "register" && argv[optind + 1] &&
	std::string(argv[optind + 1]) != "Huququ'llah" &&
	std::string(argv[optind + 1]) != "Expenses:Huququ'llah")
      compute_huquq = false;

    if (compute_huquq) {
      main_ledger.compute_huquq = true;

      read_regexps(".huquq", main_ledger.huquq_categories);

      main_ledger.huquq_account = main_ledger.find_account("Huququ'llah");
      main_ledger.huquq_expenses_account =
	main_ledger.find_account("Expenses:Huququ'llah");
    }
#endif

    parse_ledger(*file, command == "equity");
  }

#ifdef DO_CLEANUP
  delete file;
#endif

  // Process the command

  if (command == "balance")
    report_balances(argc - optind, &argv[optind], regexps, std::cout);
  else if (command == "register")
    print_register(argc - optind, &argv[optind], regexps, std::cout);
  else if (command == "print")
    print_ledger(argc - optind, &argv[optind], regexps, std::cout);
  else if (command == "equity")
    equity_ledger(argc - optind, &argv[optind], regexps, std::cout);
}

// main.cc ends here.
