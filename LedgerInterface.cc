//
//  JournalController.mm
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import "LedgerInterface.h"

using namespace ledger;

ledger_interface::ledger_interface()
{
  config = new config_t;
  std::list<std::string> args;
  config->process_options("r", args.begin(), args.end());

  formatter_ptrs = new std::list<item_handler<transaction_t> *>;
}

ledger_interface::~ledger_interface()
{
  clear_formatter_ptrs();
  delete formatter_ptrs;
  delete config;
}

void ledger_interface::clear_formatter_ptrs()
{
  for (std::list<item_handler<transaction_t> *>::iterator i
	 = formatter_ptrs->begin();
       i != formatter_ptrs->end();
       i++)
    delete *i;

  formatter_ptrs->clear();
}

static void setup_predicates(config_t * config, const std::string& query,
			     bool for_account)
{
  std::list<std::string> args;

  if (! query.empty()) {
    const char * text = query.c_str();
    const char * pch  = std::strtok(const_cast<char *>(text), " ");
    while (pch != NULL) {
      args.push_back(pch);
      pch = strtok(NULL, " ");
    }
  }

  config->regexps_to_predicate("r", args.begin(), args.end(),
				      for_account);
}

void ledger_interface::set_query_predicates
  (const std::string& account_predicate,
   const std::string& payee_predicate)
{
  // Reset Ledger's predicates before determining them again
  config->predicate	   = "";
  config->display_predicate = "";

  setup_predicates(config, account_predicate, true);
  setup_predicates(config, payee_predicate, false);
}

void ledger_interface::set_report_period(int period)
{
  switch (period) {
  case PERIOD_NONE:
    config->report_period = "";
    break;
  case PERIOD_DAILY:
    config->report_period = "daily";
    break;
  case PERIOD_WEEKLY:
    config->report_period = "weekly";
    break;
  case PERIOD_MONTHLY:
    config->report_period = "monthly";
    break;
  case PERIOD_QUARTERLY:
    config->report_period = "quarterly";
    break;
  case PERIOD_YEARLY:
    config->report_period = "yearly";
    break;
  }
}

void ledger_interface::set_query_option(int option, bool enable)
{
  switch (option) {
  case OPTION_COLLAPSED:
    config->show_collapsed = enable;
    break;
  case OPTION_RELATED:
    config->show_related = enable;
    break;
  case OPTION_BUDGET:
    if (enable)
      config->budget_flags = BUDGET_BUDGETED;
    else
      config->budget_flags = BUDGET_NO_BUDGET;
    break;
  case OPTION_BY_WEEKDAY:
    config->days_of_the_week = enable;
    break;
  case OPTION_SUBTOTALED:
    config->show_subtotal = enable;
    break;
  case OPTION_BY_PAYEE:
    config->by_payee = enable;
    break;
  }
}

void ledger_interface::perform_query
  (journal_t * journal,
   item_handler<account_t> * accounts_functor,
   item_handler<transaction_t> * entries_functor)
{
  // Remove all (possible) previous query results for the given
  // journal
  clear_query(journal);

  // Collect all the revelant transactions
  clear_formatter_ptrs();
  item_handler<transaction_t> * formatter
    = config->chain_xact_handlers("r", entries_functor, journal,
				  journal->master, *formatter_ptrs);
  walk_entries(journal->entries, *formatter);
  formatter->flush();

  // Leave clean of the `entries_functor' to the caller
  formatter_ptrs->remove(entries_functor);

  // Sum the account balances
  sum_accounts(*journal->master);
  walk_accounts(*journal->master, *accounts_functor, config->sort_string);
  accounts_functor->flush();

  // Propogate the master account total if there is one
  if (account_has_xdata(*journal->master)) {
    account_xdata_t& xdata = account_xdata(*journal->master);
    if (xdata.total)
      xdata.value = xdata.total;
  }
}
