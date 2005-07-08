//
//  JournalController.mm
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import "JournalController.h"
#import "JournalDocument.h"
#import "LedgerFunctors.h"

@implementation JournalController

- (void)windowDidLoad
{
  [super windowDidLoad];

  accounts     = [[NSMutableArray alloc] init];
  accountNames = [[NSMutableDictionary alloc] init];
  entries      = [[NSMutableArray alloc] init];
  ledgerConfig = new ledger_interface;

  [self invokeQuery:self];
}

- (void)dealloc
{
  [accounts release];
  [accountNames release];
  [entries release];
  delete ledgerConfig;

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

- (IBAction)setQueryPredicate:(id)sender
{
  ledgerConfig->set_query_predicates([[accountQuery stringValue] cString],
				     [[payeeQuery stringValue] cString]);
  [self invokeQuery:self];
}

- (IBAction)setQueryPeriod:(id)sender
{
  ledgerConfig->set_report_period([[sender selectedCell] tag]);
  [self invokeQuery:self];
}

- (IBAction)setQuerySort:(id)sender
{
  ledgerConfig->set_sort_string([[sender stringValue] cString]);
  [self invokeQuery:self];
}

- (IBAction)setQueryPeriodSort:(id)sender
{
  ledgerConfig->set_report_period_sort([[sender stringValue] cString]);
  [self invokeQuery:self];
}

- (IBAction)setQueryOption:(id)sender
{
  NSButton * button = [sender selectedCell];
  ledgerConfig->set_query_option([button tag], [button state] == NSOnState);
  [self invokeQuery:self];
}

- (IBAction)invokeQuery:(id)sender
{
  try {
    [accounts removeAllObjects];
    [accountNames removeAllObjects];
    [entries removeAllObjects];

    // Find out which journal we're talking to
    journal_t * journal = [[self document] journal];

    std::auto_ptr<AddAccountsToArray>
      accounts_functor(new AddAccountsToArray(accounts, accountNames,
					      journal->master));
    std::auto_ptr<AddEntriesToArray>
      entries_functor(new AddEntriesToArray(entries));

    ledgerConfig->perform_query(journal,
				accounts_functor.get(), entries_functor.get());
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
