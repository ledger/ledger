//
//  LedgerInterface.h
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#include <ledger.h>

using namespace ledger;

#define PERIOD_NONE	 0
#define PERIOD_DAILY	 1
#define PERIOD_WEEKLY	 2
#define PERIOD_MONTHLY	 3
#define PERIOD_QUARTERLY 4
#define PERIOD_YEARLY	 5

#define OPTION_COLLAPSED  0
#define OPTION_RELATED	  1
#define OPTION_BUDGET	  2
#define OPTION_BY_WEEKDAY 3
#define OPTION_SUBTOTALED 4
#define OPTION_BY_PAYEE	  5

class ledger_interface
{
  config_t * config;

  std::list<item_handler<transaction_t> *> * formatter_ptrs;

  void clear_formatter_ptrs();

public:
  ledger_interface();
  ~ledger_interface();

  void set_query_predicates(const std::string& account_predicate,
			    const std::string& payee_predicate);

  void set_sort_string(const std::string& sort_order) {
    config->sort_string = sort_order;
  }
  void set_report_period_sort(const std::string& sort_order) {
    config->report_period_sort = sort_order;
  }

  void set_report_period(int period);
  void set_query_option(int option, bool enable);

  void perform_query(journal_t * journal,
		     item_handler<account_t> * accounts_functor,
		     item_handler<transaction_t> * entries_functor);
};
