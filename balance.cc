#include "ledger.h"

#include <fstream>
#include <unistd.h>
#include <pcre.h>               // Perl regular expression library

namespace ledger {

static bool show_current  = false;
static bool show_cleared  = false;
static bool show_children = false;
static bool show_empty    = false;
static bool no_subtotals  = false;

struct mask
{
  bool   exclude;
  pcre * regexp;

  mask(bool exc, pcre * re) : exclude(exc), regexp(re) {}
};

static inline bool matches(const std::list<mask>& regexps,
			   const std::string& str)
{
  // If the first pattern is an exclude, we assume all patterns match
  // if they don't match the exclude.  If the first pattern is an
  // include, then only accounts matching the include will match.

  bool match = (*regexps.begin()).exclude;

  for (std::list<mask>::const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
    int ovec[3];
    if (pcre_exec((*r).regexp, NULL, str.c_str(), str.length(),
		  0, 0, ovec, 3) >= 0)
      match = ! (*r).exclude;
  }

  return match;
}

static void display_total(std::ostream& out, totals& total_balance,
			  const account * acct,
			  const std::map<account *, totals *>& balances,
			  const std::list<mask>& regexps)
{
  std::map<account *, totals *>::const_iterator b =
    balances.find(const_cast<account *>(acct));
  if (b != balances.end()) {
    totals * balance = (*b).second;

    if (balance && (show_empty || *balance)) {
      bool match = true;
      if (! regexps.empty()) {
	if (show_children) {
	  match = false;
	  for (const account * a = acct; a; a = a->parent) {
	    if (matches(regexps, a->name)) {
	      match = true;
	      break;
	    }
	  }
	} else {
	  match = matches(regexps, acct->name);
	}
      }

      if (match) {
	out << *balance;

	if (acct->parent) {
	  for (const account * a = acct; a; a = a->parent)
	    out << "  ";
	  out << acct->name << std::endl;
	} else {
	  out << "  " << *acct << std::endl;

	  // For top-level accounts, update the total running balance
	  total_balance.credit(*balance);
	}
      }
    }
  }

  // Display balances for all child accounts

  for (account::const_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    display_total(out, total_balance, (*i).second, balances, regexps);
}

static void record_price(char * setting,
			 std::map<const std::string, amount *>& prices)
{
  char * c = setting;
  char * p = std::strchr(setting, '=');
  if (! p) {
    std::cerr << "Warning: Invalid price setting: " << setting << std::endl;
  } else {
    *p++ = '\0';
    amount * price = create_amount(p);
    prices.insert(std::pair<const std::string, amount *>(c, price));
  }
}

static void record_regexp(char * pattern, std::list<mask>& regexps)
{
  bool exclude = false;

  char * pat = pattern;
  if (*pat == '-') {
    exclude = true;
    pat++;
    while (std::isspace(*pat))
      pat++;
  }
  else if (*pat == '+') {
    pat++;
    while (std::isspace(*pat))
      pat++;
  }

  const char *error;
  int erroffset;
  pcre * re = pcre_compile(pat, PCRE_CASELESS, &error, &erroffset, NULL);
  if (! re)
    std::cerr << "Warning: Failed to compile regexp: " << pattern
	      << std::endl;
  else
    regexps.push_back(mask(exclude, re));
}

//////////////////////////////////////////////////////////////////////
//
// Balance reporting code
//

void report_balances(int argc, char **argv, std::ostream& out)
{
  std::map<const std::string, amount *> prices;
  std::list<mask> regexps;

  int c;
  optind = 1;
  while (-1 != (c = getopt(argc, argv, "cCsSni:p:"))) {
    switch (char(c)) {
    case 'c': show_current  = true; break;
    case 'C': show_cleared  = true; break;
    case 's': show_children = true; break;
    case 'S': show_empty    = true; break;
    case 'n': no_subtotals  = true; break;

    // -i path-to-file-of-regexps
    case 'i':
      if (access(optarg, R_OK) != -1) {
	std::ifstream include(optarg);

	while (! include.eof()) {
	  char buf[80];
	  include.getline(buf, 79);
	  if (*buf && ! std::isspace(*buf))
	    record_regexp(buf, regexps);
	}
      }
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
	    record_price(buf, prices);
	}
      } else {
	record_price(optarg, prices);
      }
      break;
    }
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // Walk through all of the ledger entries, computing the account
  // totals

  std::map<account *, totals *> balances;
  std::time_t now = std::time(NULL);

  for (ledger_iterator i = ledger.begin(); i != ledger.end(); i++) {
    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      account * acct = (*x)->acct;

      for (; acct; acct = no_subtotals ? NULL : acct->parent) {
	if (! show_children && acct->parent)
	  continue;

	totals * balance = NULL;

	std::map<account *, totals *>::iterator t = balances.find(acct);
	if (t == balances.end()) {
	  balance = new totals;
	  balances.insert(std::pair<account *, totals *>(acct, balance));
	} else {
	  balance = (*t).second;
	}

	bool do_credit = false;

	if (show_current) {
	  if (difftime((*i)->date, now) < 0)
	    do_credit = true;
	}
	else if (show_cleared) {
	  if ((*i)->cleared)
	    do_credit = true;
	}
	else {
	  do_credit = true;
	}

	if (! do_credit)
	  continue;

	std::map<const std::string, amount *>::iterator pi
	  = prices.find((*x)->cost->comm_symbol());

	if (pi == prices.end()) {
	  balance->credit((*x)->cost);
	} else {
	  amount * value = (*x)->cost->value((*pi).second);
	  balance->credit(value);
	  delete value;
	}
      }
    }
  }

  // Walk through all the top-level accounts, giving the balance
  // report for each, and then for each of their children.

  totals total_balance;

  for (accounts_iterator i = accounts.begin(); i != accounts.end(); i++)
    display_total(out, total_balance, (*i).second, balances, regexps);

  // Print the total of all the balances shown

  if (! no_subtotals)
    out << "--------------------" << std::endl
	<< total_balance << std::endl;

  // Free up temporary variables created on the heap

  for (std::map<account *, totals *>::iterator i = balances.begin();
       i != balances.end();
       i++)
    delete (*i).second;

  for (std::map<const std::string, amount *>::iterator i = prices.begin();
       i != prices.end();
       i++)
    delete (*i).second;
}

} // namespace ledger
