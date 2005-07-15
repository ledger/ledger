//
//  LedgerInterface.h
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#include <ledger.h>

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
#define OPTION_AVERAGE    6
#define OPTION_DEVIATION  7

#define REPORT_COMMODITY  0
#define REPORT_MARKET     1
#define REPORT_BASIS      2
#define REPORT_CUSTOM     3

class ledger_interface
{
  ledger::config_t * config;
  std::string	     base_total_expr;

  std::list<ledger::item_handler<ledger::transaction_t> *> * formatter_ptrs;
  void clear_formatter_ptrs();

public:
  std::auto_ptr<ledger::value_expr_t> amount_expr;
  std::auto_ptr<ledger::value_expr_t> total_expr;

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
  void set_report_type(int type, bool show_revalued   = false,
                       const std::string& amount_expr = "a",
                       const std::string& total_expr  = "O");

  void perform_query
    (ledger::journal_t * journal,
     ledger::item_handler<ledger::account_t> * accounts_functor,
     ledger::item_handler<ledger::transaction_t> * entries_functor);

  void clear_query(ledger::journal_t * journal) {
    clear_formatter_ptrs();
    clear_journal_xdata(journal);
  }
};
