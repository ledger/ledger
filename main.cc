#include "ledger.h"

#include <fstream>

namespace ledger {
  extern bool parse_ledger(std::istream& in);
  extern bool parse_gnucash(std::istream& in);

  extern void report_balances(int argc, char **argv, std::ostream& out);
  extern void print_ledger(int argc, char *argv[], std::ostream& out);
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
  // Parse the command-line options

  std::istream * file = NULL;

#ifdef HUQUQULLAH
  compute_huquq = true;
#endif

  int c;
  while (-1 != (c = getopt(argc, argv, "+hHwf:"))) {
    switch (char(c)) {
    case 'h': show_help(std::cout); break;
#ifdef HUQUQULLAH
    case 'H': compute_huquq = false; break;
#endif
    case 'w': use_warnings = true; break;
    case 'f': file = new std::ifstream(optarg); break;
    }
  }

  if (optind == argc) {
    std::cerr << "usage: ledger [options] COMMAND [options] [ARGS]" << std::endl
	      << std::endl
	      << "ledger options:" << std::endl
	      << "  -f FILE  specify pathname of ledger data file" << std::endl
	      << std::endl
	      << "commands:" << std::endl
	      << "  balance  show balance totals" << std::endl
	      << "  print    print all ledger entries" << std::endl
	      << std::endl
	      << "`balance' command options:" << std::endl
	      << "  -s       show sub-accounts in balance totals" << std::endl
	      << "  -S       show empty accounts in balance totals" << std::endl;
    return 1;
  }

  // The -f option is required

  if (! file) {
    std::cerr << "Please specify the ledger file using the -f option."
	      << std::endl;
    return 1;
  }

  // Global defaults

  commodity * usd = new commodity("$", true, false, true, false, 2);
  commodities.insert(commodities_entry("USD", usd));

#ifdef HUQUQULLAH
  if (compute_huquq) {
    new commodity("H", true, true, true, false, 2);
    new commodity("mithqal", false, true, true, false, 1);

    read_regexps(".huquq", huquq_categories);
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

  const std::string command = argv[optind];

  if (command == "balance")
    report_balances(argc - optind, &argv[optind], std::cout);
  else if (command == "print")
    print_ledger(argc - optind, &argv[optind], std::cout);
}

// main.cc ends here.
