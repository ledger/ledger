//
//  LedgerFunctors.h
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#include <Cocoa/Cocoa.h>

#include <ledger.h>

using namespace ledger;

class AddAccountsToArray : public item_handler<account_t>
{
  item_predicate<account_t> disp_pred;
  account_t * master;

 public:
  NSMutableArray *	records;
  NSMutableDictionary * names;

  AddAccountsToArray(NSMutableArray *	   _records,
		     NSMutableDictionary * _names,
		     account_t *	   _master,
		     const std::string&    display_predicate = "T")
    : disp_pred(display_predicate), master(_master),
      records(_records), names(_names) {}

  void GetAccountsData(NSMutableArray * array, account_t * account);

  virtual void flush();
  virtual void operator()(account_t& xact);
};

class AddEntriesToArray : public item_handler<transaction_t>
{
  entry_t * last_entry;

 public:
  NSMutableArray * records;

  AddEntriesToArray(NSMutableArray * _records)
    : last_entry(NULL), records(_records) {}

  void register_last_entry();

  virtual void flush() {
    if (last_entry) {
      register_last_entry();
      last_entry = NULL;
    }
  }

  virtual void operator()(transaction_t& xact);
};
