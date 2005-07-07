//
//  JournalController.mm
//  LedgerPro
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import "JournalController.h"
#import "JournalDocument.h"

#include <list>
#include <string>
#include <strstream>
#include <cstring>
#include <exception>

void AddEntriesToArray::register_last_entry()
{
  for (transactions_list::const_iterator i = last_entry->transactions.begin();
       i != last_entry->transactions.end();
       i++) {
    transaction_t& xact = **i;
    if (transaction_has_xdata(xact) &&
	transaction_xdata_(xact).dflags & TRANSACTION_TO_DISPLAY) {
      // Calculate totals for the parent account
      add_transaction_to(xact, account_xdata(*xact.account).value);
      account_xdata_(*xact.account).count++;
      if (xact.flags & TRANSACTION_VIRTUAL)
	account_xdata_(*xact.account).virtuals++;

      // Add the transaction as an NSValue object within an array
      [records addObject:[NSValue valueWithPointer:&xact]];
    }
  }
}

void AddEntriesToArray::operator()(transaction_t& xact)
{
  transaction_xdata(xact).dflags |= TRANSACTION_TO_DISPLAY;

  if (last_entry && xact.entry != last_entry)
    register_last_entry();

  last_entry = xact.entry;
}

void AddAccountsToArray::GetAccountsData(NSMutableArray * array,
					 account_t *      account)
{
  NSMutableArray * info = NULL;

  if (account_xdata(*account).dflags & ACCOUNT_DISPLAYED) {
    info = [[NSMutableArray alloc] init];
    [info addObject:[NSValue valueWithPointer:account]];
  }

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    GetAccountsData(info ? info : array, (*i).second);

  if (info) {
    [array addObject:info];
    [info release];
  }
}

void dumpAccountsArray(NSMutableArray * array, int index = 0)
{
  NSEnumerator * enumerator = [array objectEnumerator];

  NSValue * value = [enumerator nextObject];
  account_t * account = reinterpret_cast<account_t *>([value pointerValue]);
  NSLog(@"%*sAccount %s", index, " ", account->fullname().c_str());

  while (NSMutableArray * child = [enumerator nextObject])
    dumpAccountsArray(child, index + 2);
}

void AddAccountsToArray::flush()
{
  NSMutableArray * array = [[NSMutableArray alloc] init];

  GetAccountsData(array, master);

  if ([array count] > 0) {
    NSEnumerator * enumerator = [array objectEnumerator];
    while (NSMutableArray * subarray = [enumerator nextObject])
      [records addObject:subarray];
  }
  [array release];

  item_handler<account_t>::flush();
}

void AddAccountsToArray::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    if (! account.parent) {
      account_xdata(account).dflags |= ACCOUNT_TO_DISPLAY;
    } else {
      std::string name;
      std::ostringstream name_stream;
      format_t("%-a").format(name_stream, details_t(account));
      name_stream.flush();

      NSString * accountName
	= [[NSString alloc] initWithCString:name_stream.str().c_str()];
      NSString * fullname
	= [[NSString alloc] initWithCString:account.fullname().c_str()];
      [names setValue:accountName forKey:fullname];
      [fullname release];
      [accountName release];

      account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
    }
  }
}

@implementation JournalController

- (void)windowDidLoad
{
  [super windowDidLoad];

  accounts     = [[NSMutableArray alloc] init];
  accountNames = [[NSMutableDictionary alloc] init];
  entries      = [[NSMutableArray alloc] init];

  formatter_ptrs = new std::list<item_handler<transaction_t> *>;

  ledger_config = new config_t;
  std::list<std::string> args;
  ledger_config->process_options("r", args.begin(), args.end());

  [self invokeQuery:self];
}

void clear_formatter_ptrs(std::list<item_handler<transaction_t> *>&
			  formatter_ptrs)
{
  for (std::list<item_handler<transaction_t> *>::iterator i
	 = formatter_ptrs.begin();
       i != formatter_ptrs.end();
       i++)
    delete *i;

  formatter_ptrs.clear();
}

- (void)dealloc
{
  [accounts release];
  [accountNames release];
  [entries release];

  clear_formatter_ptrs(*formatter_ptrs);
  delete formatter_ptrs;
  delete ledger_config;

  [super dealloc];
}

- (void)awakeFromNib
{
  NSString * date_format =
    [NSString stringWithCString:(format_t::date_format.c_str())];
  NSDateFormatter * formatter =
    [[NSDateFormatter alloc]
     initWithDateFormat:date_format allowNaturalLanguage:NO];

  [[[entriesList tableColumnWithIdentifier:@"date"] dataCell] 
   setFormatter:formatter];
  [formatter release];
}

static void setup_predicates(config_t * ledger_config,
			     NSString * query, bool for_account)
{
  std::list<std::string> args;

  if ([query length] > 0) {
    const char * text = [query cString];
    const char * pch  = std::strtok(const_cast<char *>(text), " ");
    while (pch != NULL) {
      args.push_back(pch);
      pch = strtok(NULL, " ");
    }
  }

  ledger_config->regexps_to_predicate("r", args.begin(), args.end(),
				      for_account);
}

- (IBAction)setQueryPredicate:(id)sender
{
  // Reset Ledger's predicates before determining them again
  ledger_config->predicate	   = "";
  ledger_config->display_predicate = "";

  setup_predicates(ledger_config, [accountQuery stringValue], true);
  setup_predicates(ledger_config, [payeeQuery stringValue], false);

  [self invokeQuery:self];
}

- (IBAction)setQueryPeriod:(id)sender
{
  switch ([[sender selectedCell] tag]) {
  case PERIOD_NONE:
    ledger_config->report_period = "";
    break;
  case PERIOD_DAILY:
    ledger_config->report_period = "daily";
    break;
  case PERIOD_WEEKLY:
    ledger_config->report_period = "weekly";
    break;
  case PERIOD_MONTHLY:
    ledger_config->report_period = "monthly";
    break;
  case PERIOD_QUARTERLY:
    ledger_config->report_period = "quarterly";
    break;
  case PERIOD_YEARLY:
    ledger_config->report_period = "yearly";
    break;
  }

  [self invokeQuery:self];
}

- (IBAction)setQuerySort:(id)sender
{
  ledger_config->sort_string	    = [[sortBy stringValue] cString];
  ledger_config->report_period_sort = [[sortPeriodBy stringValue] cString];

  [self invokeQuery:self];
}

- (IBAction)setQueryOption:(id)sender
{
  NSButton * button = [sender selectedCell];
  switch ([button tag]) {
  case OPTION_COLLAPSED:
    ledger_config->show_collapsed = [button state] == NSOnState;
    break;
  case OPTION_RELATED:
    ledger_config->show_related = [button state] == NSOnState;
    break;
  case OPTION_BUDGET:
    if ([button state] == NSOnState)
      ledger_config->budget_flags = BUDGET_BUDGETED;
    else
      ledger_config->budget_flags = BUDGET_NO_BUDGET;
    break;
  case OPTION_BY_WEEKDAY:
    ledger_config->days_of_the_week = [button state] == NSOnState;
    break;
  case OPTION_SUBTOTALED:
    ledger_config->show_subtotal = [button state] == NSOnState;
    break;
  case OPTION_BY_PAYEE:
    ledger_config->by_payee = [button state] == NSOnState;
    break;
  }

  [self invokeQuery:self];
}

- (IBAction)invokeQuery:(id)sender
{
  try {
    // Remove all (possible) previous query results
    clear_all_xdata();

    [accounts removeAllObjects];
    [accountNames removeAllObjects];
    [entries removeAllObjects];

    // Find out which journal we're talking to
    journal_t * journal = [[self document] journalPointer];

    // Collect all the revelant transactions
    {
      AddEntriesToArray * functor = new AddEntriesToArray(entries);

      clear_formatter_ptrs(*formatter_ptrs);
      item_handler<transaction_t> * formatter
	= ledger_config->chain_xact_handlers("r", functor, journal,
					     journal->master, *formatter_ptrs);
      walk_entries(journal->entries, *formatter);
      formatter->flush();
    }

    // Sum the account balances
    {
      AddAccountsToArray functor(accounts, accountNames, journal->master);
      sum_accounts(*journal->master);
      walk_accounts(*journal->master, functor, ledger_config->sort_string);
      functor.flush();
    }

    if (account_has_xdata(*journal->master)) {
      account_xdata_t& xdata = account_xdata(*journal->master);
      if (xdata.total)
	xdata.value = xdata.total;
    }
  }
  catch (const std::exception& err) {
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert setMessageText:@"Caught unhandled ledger exception"];
    [alert setInformativeText:[NSString stringWithCString:err.what()]];
    [alert setAlertStyle:NSCriticalAlertStyle];
    [alert runModal];
    [alert release];
  }

  // Inform the outlineView that it's time to redisplay
  [accountsList reloadData];
  [entriesList reloadData];
}

static NSString * getValueString(amount_t& amount)
{
  std::string amount_string;
  std::ostringstream amount_stream(amount_string);
  amount_stream << amount;
  amount_stream.flush();
  return [NSString stringWithCString:amount_stream.str().c_str()];
}

static NSString * getValueString(value_t& value)
{
  std::string value_string;
  std::ostringstream value_stream(value_string);

  switch (value.type) {
  case value_t::BOOLEAN:
  case value_t::INTEGER:
    assert(0);
    break;

  case value_t::AMOUNT:
    value_stream << *((amount_t *) value.data);
    break;

  case value_t::BALANCE:
  case value_t::BALANCE_PAIR:
    //NSLog(@"Balances cannot be displayed yet");
    break;
  }

  value_stream.flush();
  return [NSString stringWithCString:value_stream.str().c_str()];
}

#if 0
static void calculate_subtotal()
{
  value_t subtotal;

  int len = [item count];
  for (int i = 0; i < len; i++) {
    value = [item objectAtIndex:i];
    xact  = reinterpret_cast<transaction_t *>([value pointerValue]);
    add_transaction_to(*xact, subtotal);
  }
  return getValueString(subtotal);
}
#endif

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
  return [entries count];
}

- (id)tableView:(NSTableView *)tableView
    objectValueForTableColumn:(NSTableColumn *)tableColumn
    row:(int)rowIndex
{
  NSString * ident  = [tableColumn identifier];
  NSValue  * value  = [entries objectAtIndex:rowIndex];

  transaction_t * xact
    = reinterpret_cast<transaction_t *>([value pointerValue]);

  int index = 0;
  for (transactions_list::const_iterator i
	 = xact->entry->transactions.begin();
       i != xact->entry->transactions.end();
       i++) {
    transaction_t& x = **i;
    if (transaction_has_xdata(x) &&
	transaction_xdata_(x).dflags & TRANSACTION_TO_DISPLAY) {
      if (&x == xact)
	break;
      index++;
    }
  }
  bool first = index == 0;

  if (first && [ident isEqualToString:@"date"])
    return [NSDate dateWithTimeIntervalSince1970:xact->entry->date];
  else if (first && [ident isEqualToString:@"payee"])
    return [NSString stringWithCString:xact->entry->payee.c_str()];
  else if ([ident isEqualToString:@"account"])
    return [NSString stringWithCString:xact->account->fullname().c_str()];
  else if ([ident isEqualToString:@"amount"])
    return getValueString(xact->amount);
  else if ([ident isEqualToString:@"total"])
    return getValueString(transaction_xdata(*xact).total);

  return nil;
}

- (BOOL)outlineView:(NSOutlineView *)ov isItemExpandable:(id)item
{
  return item && [item count] > 1 ? YES : NO;
}

- (int)outlineView:(NSOutlineView *)ov numberOfChildrenOfItem:(id)item
{
  if (! item)
    return [accounts count];
  else
    return [item count] - 1;
}

- (id)outlineView:(NSOutlineView *)ov child:(int)index ofItem:(id)item
{
  if (! item)
    return [accounts objectAtIndex:index];
  else
    return [item objectAtIndex:index + 1];
}

- (id)outlineView:(NSOutlineView *)ov objectValueForTableColumn:
    (NSTableColumn *)tableColumn byItem:(id)item
{
  NSString * ident  = [tableColumn identifier];
  NSValue  * value  = [item objectAtIndex:0];

  account_t * account = reinterpret_cast<account_t *>([value pointerValue]);
  assert(account_xdata(*account).dflags & ACCOUNT_DISPLAYED);

  if ([ident isEqualToString:@"total"]) {
    return getValueString(account_xdata(*account).total);
  }
  else if ([ident isEqualToString:@"account"]) {
    NSString * fullname
      = [NSString stringWithCString:account->fullname().c_str()];
    return [accountNames valueForKey:fullname];
  }
  return nil;
}

@end
