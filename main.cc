#include <fstream>
#include <vector>
#include <cassert>

#include <pcre.h>               // Perl regular expression library

#include "ledger.h"

//////////////////////////////////////////////////////////////////////
//
// Command-line parser and top-level logic.
//

namespace ledger {
  extern bool parse_ledger(std::istream& in, std::vector<entry *>& ledger);
  extern bool parse_gnucash(std::istream& in, std::vector<entry *>& ledger);
  extern void report_balances(std::ostream& out, std::vector<entry *>& ledger,
			      bool show_children, bool show_empty);
  extern void print_ledger(std::ostream& out, std::vector<entry *>& ledger);
}

using namespace ledger;

int main(int argc, char *argv[])
{
  // Setup global defaults

  commodity_usd = new commodity("$", true, false, 2);
  commodities.insert(commodities_entry("$", commodity_usd));
  commodities.insert(commodities_entry("USD", commodity_usd));

  // Parse the command-line options

  bool show_children = false;
  bool show_empty = false;

  int c;
  while (-1 != (c = getopt(argc, argv, "sS"))) {
    switch (char(c)) {
    case 's': show_children = true; break;
    case 'S': show_empty = true; break;
    }
  }

  if (optind == argc) {
    std::cerr << "usage: ledger [options] DATA_FILE COMMAND [ARGS]"
	      << std::endl
	      << "options:" << std::endl
	      << "  -s  show sub-accounts in balance totals" << std::endl
	      << "  -S  show empty accounts in balance totals" << std::endl
	      << "commands:" << std::endl
	      << "  balance  show balance totals" << std::endl
	      << "  print    print all ledger entries" << std::endl;
    std::exit(1);
  }

  // Parse the ledger

  std::ifstream file(argv[optind++]);
  std::vector<entry *> ledger;

  char buf[256];
  file.get(buf, 255);
  file.seekg(0);

  if (std::strncmp(buf, "<?xml version=\"1.0\"?>", 21) == 0)
    parse_gnucash(file, ledger);
  else
    parse_ledger(file, ledger);

  // Read the command word

  if (optind == argc) {
    std::cerr << "Command word missing" << std::endl;
    return 1;
  }

  const std::string command = argv[optind++];

  // Process the command

  if (command == "balance")
    report_balances(std::cout, ledger, show_children, show_empty);
  else if (command == "print")
    print_ledger(std::cout, ledger);
}
