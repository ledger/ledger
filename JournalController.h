//
//  JournalController.h
//  LedgerPro
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include <ledger.h>

using namespace ledger;

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
    item_handler<transaction_t>::flush();
  }

  virtual void operator()(transaction_t& xact);
};

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

#define PERIOD_NONE	 0
#define PERIOD_DAILY	 1
#define PERIOD_WEEKLY	 2
#define PERIOD_MONTHLY	 3
#define PERIOD_QUARTERLY 4
#define PERIOD_YEARLY	 5

@interface JournalController : NSWindowController
{
  IBOutlet NSOutlineView * accountsList;
  IBOutlet NSTableView *   entriesList;
  IBOutlet NSView *	   inFlowPie;
  IBOutlet NSView *	   outFlowPie;
  IBOutlet NSView *	   balanceGraph;
  IBOutlet NSFormCell *    accountQuery;
  IBOutlet NSFormCell *    payeeQuery;

  NSMutableArray *	accounts;
  NSMutableDictionary * accountNames;
  NSMutableArray *	entries;

  std::list<item_handler<transaction_t> *> * formatter_ptrs;
}

- (IBAction)setQueryPredicate:(id)sender;
- (IBAction)setQueryPeriod:(id)sender;
- (IBAction)invokeQuery:(id)sender;
@end
