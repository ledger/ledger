#include "ledger.h"

namespace ledger {

static void equity_entry(account * acct, regexps_t& regexps,
			 std::ostream& out)
{
  if (! acct->balance.is_zero() &&
      (regexps.empty() || matches(regexps, acct->as_str()))) {
    entry opening;

    opening.date    = std::time(NULL);
    opening.cleared = true;
    opening.desc    = "Opening Balance";

    transaction * xact;
    for (totals::const_iterator i = acct->balance.amounts.begin();
	 i != acct->balance.amounts.end();
	 i++) {
      // Skip it, if there is a zero balance for the commodity
      if ((*i).second->is_zero())
	continue;

      xact = new transaction();
      xact->acct = const_cast<account *>(acct);
      xact->cost = (*i).second->street();
      opening.xacts.push_back(xact);

      xact = new transaction();
      xact->acct = main_ledger.find_account("Equity:Opening Balances");
      xact->cost = (*i).second->street();
      xact->cost->negate();
      opening.xacts.push_back(xact);
    }

    opening.print(out);
  }

  // Display balances for all child accounts

  for (accounts_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    equity_entry((*i).second, regexps, out);
}

//////////////////////////////////////////////////////////////////////
//
// Create an Equity file based on a ledger.  This is used for
// archiving past years, and starting out a new year with compiled
// balances.
//

void equity_ledger(int argc, char ** argv, regexps_t& regexps,
		   std::ostream& out)
{
  optind = 1;

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // The account have their current totals already generated as a
  // result of parsing.  We just have to output those values.
  // totals

  for (accounts_iterator i = main_ledger.accounts.begin();
       i != main_ledger.accounts.end();
       i++)
    equity_entry((*i).second, regexps, out);
}

} // namespace ledger
