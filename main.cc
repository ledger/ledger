#include "ledger.h"

#include <fstream>

namespace ledger {
  extern bool parse_ledger(std::istream& in);
  extern bool parse_gnucash(std::istream& in);

  extern void report_balances(int argc, char **argv, std::ostream& out);
  extern void print_register(int argc, char **argv, std::ostream& out);
  extern void print_ledger(int argc, char *argv[], std::ostream& out);
  extern void equity_ledger(int argc, char **argv, std::ostream& out);

  bool show_cleared;

  std::time_t begin_date;
  bool        have_beginning;
  std::time_t end_date;
  bool        have_ending;
}

using namespace ledger;

void show_help(std::ostream& out)
{
  out << "usage: ledger [options] DATA_FILE COMMAND [ARGS]"
      << std::endl
      << "options:" << std::endl
      << "  -s  show sub-accounts in balance totals" << std::endl
      << "  -S  show empty accounts in balance totals" << std::endl
      << "commands:" << std::endl
      << "  balance  show balance totals" << std::endl
      << "  print    print all ledger entries" << std::endl;
}

//////////////////////////////////////////////////////////////////////
//
// Command-line parser and top-level logic.
//

int main(int argc, char *argv[])
{
  // Global defaults

  commodity * usd = new commodity("$", true, false, true, false, 2);
  main_ledger.commodities.insert(commodities_entry("USD", usd));

  // Parse the command-line options

  std::istream * file = NULL;

#ifdef HUQUQULLAH
  compute_huquq  = true;
#endif
  have_beginning = false;
  have_ending    = false;
  show_cleared   = false;

  int c;
  while (-1 != (c = getopt(argc, argv, "+b:e:cChHwf:i:p:"))) {
    switch (char(c)) {
    case 'b': {
      struct tm * when = getdate(optarg);
      if (! when) {
	std::cerr << "Error: Bad begin date string: " << optarg
		  << std::endl;
      } else {
	begin_date = std::mktime(when);
	have_beginning = true;
      }
      break;
    }
    case 'e': {
      struct tm * when = getdate(optarg);
      if (! when) {
	std::cerr << "Error: Bad end date string: " << optarg
		  << std::endl;
      } else {
	end_date = std::mktime(when);
	have_ending = true;
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
    }
  }

  if (optind == argc) {
    std::cerr
      << "usage: ledger [options] COMMAND [options] [REGEXPS]" << std::endl
      << std::endl
      << "ledger options:" << std::endl
      << "  -C       also show cleared transactions" << std::endl
      << "  -b DATE  specify a beginning date" << std::endl
      << "  -c       do not show future entries (same as -e TODAY)" << std::endl
      << "  -e DATE  specify an ending date" << std::endl
      << "  -f FILE  specify pathname of ledger data file" << std::endl
      << "  -h       display this help text" << std::endl
#ifdef HUQUQULLAH
      << "  -H       do not auto-compute Huququ'llah" << std::endl
#endif
      << "  -i FILE  read the list of inclusion regexps from FILE" << std::endl
      << "  -p FILE  read the list of prices from FILE" << std::endl
      << "  -w       print out warnings where applicable" << std::endl
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
    return 1;
  }

  // The -f option is required

  if (! file || ! *file) {
    std::cerr << "Please specify the ledger file using the -f option."
	      << std::endl;
    return 1;
  }

  // Read the command word

  const std::string command = argv[optind];

#ifdef HUQUQULLAH
  if (command == "register")
    compute_huquq = false;

  if (compute_huquq) {
    new commodity("H", true, true, true, false, 2);
    new commodity("mithqal", false, true, true, false, 1);

    read_regexps(".huquq", huquq_categories);

    main_ledger.record_price("H=" DEFAULT_COMMODITY "0.19");
    main_ledger.record_price("troy=8.5410148523 mithqal");
  }
#endif

  // Parse the ledger

  char buf[32];
  file->get(buf, 31);
  file->seekg(0);

  if (std::strncmp(buf, "<?xml version=\"1.0\"?>", 21) == 0)
    parse_gnucash(*file);
  else
    parse_ledger(*file);

  delete file;

  // Process the command

  if (command == "balance")
    report_balances(argc - optind, &argv[optind], std::cout);
  else if (command == "register")
    print_register(argc - optind, &argv[optind], std::cout);
  else if (command == "print")
    print_ledger(argc - optind, &argv[optind], std::cout);
  else if (command == "equity")
    equity_ledger(argc - optind, &argv[optind], std::cout);
}

// main.cc ends here.
