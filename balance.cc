#include "ledger.h"

#include <fstream>
#include <unistd.h>

namespace ledger {

static bool show_cleared  = false;
static bool show_children = false;
static bool show_empty    = false;
static bool no_subtotals  = false;

static std::time_t begin_date;
static bool have_beginning;
static std::time_t end_date;
static bool have_ending;

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
	    if (matches(regexps, a->as_str())) {
	      match = true;
	      break;
	    }
	  }
	} else {
	  match = matches(regexps, acct->as_str());
	}
      }

      if (match) {
	out << *balance;

	// jww (2003-09-30): Don't check "! acct->parent", but simply
	// make sure this is the parent which matched the regexp.
	if (! show_children || ! acct->parent)
	  total_balance.credit(*balance);

	if (acct->parent && ! no_subtotals) {
	  for (const account * a = acct; a; a = a->parent)
	    out << "  ";
	  out << acct->name << std::endl;
	} else {
	  out << "  " << *acct << std::endl;
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

//////////////////////////////////////////////////////////////////////
//
// Balance reporting code
//

void report_balances(int argc, char **argv, std::ostream& out)
{
  std::map<const std::string, amount *> prices;
  std::list<mask> regexps;

#ifdef HUQUQULLAH
  if (compute_huquq) {
    prices.insert(std::pair<const std::string, amount *>
		  ("H", create_amount("$0.19")));
    prices.insert(std::pair<const std::string, amount *>
		  ("troy", create_amount("8.5410148523 mithqal")));
  }
#endif

  have_beginning = false;
  have_ending    = false;

  int c;
  optind = 1;
  while (-1 != (c = getopt(argc, argv, "b:e:cCsSni:p:G:"))) {
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

    case 'C': show_cleared  = true; break;
    case 's': show_children = true; break;
    case 'S': show_empty    = true; break;
    case 'n': no_subtotals  = true; break;

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
	    record_price(buf, prices);
	}
      } else {
	record_price(optarg, prices);
      }
      break;

#ifdef HUQUQULLAH
    case 'G': {
      double gold = std::atof(optarg);
      gold = 1 / gold;
      char buf[256];
      std::sprintf(buf, "$=%f troy", gold);
      record_price(buf, prices);
      break;
    }
#endif
    }
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // Walk through all of the ledger entries, computing the account
  // totals

  std::map<account *, totals *> balances;

  for (ledger_iterator i = ledger.begin(); i != ledger.end(); i++) {
    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      account * acct = (*x)->acct;

      for (; acct; acct = no_subtotals ? NULL : acct->parent) {
	if (! show_children && acct->parent && regexps.empty())
	  continue;

	totals * balance = NULL;

	std::map<account *, totals *>::iterator t = balances.find(acct);
	if (t == balances.end()) {
	  balance = new totals;
	  balances.insert(std::pair<account *, totals *>(acct, balance));
	} else {
	  balance = (*t).second;
	}

	bool do_credit = true;

	if (have_beginning && difftime((*i)->date, begin_date) < 0)
	  do_credit = false;
	else if (have_ending && difftime((*i)->date, end_date) > 0)
	  do_credit = false;
	else if (show_cleared && ! (*i)->cleared)
	  do_credit = false;

	if (! do_credit)
	  continue;

	amount * cost = (*x)->cost;

	bool allocated = false;
	for (int cycles = 0; cost && cycles < 10; cycles++) {
	  std::map<const std::string, amount *>::iterator pi
	    = prices.find(cost->comm_symbol());

	  if (pi == prices.end()) {
	    balance->credit(cost);
	    if (allocated)
	      delete cost;
	    break;
	  } else {
	    amount * temp = cost;
	    cost = temp->value((*pi).second);
	    if (allocated)
	      delete temp;
	    allocated = true;
	  }
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
