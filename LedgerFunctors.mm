//
//  LedgerFunctors.cc
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import "LedgerFunctors.h"

void AddAccountsToArray::GetAccountsData(NSMutableArray * array,
					 account_t *      account)
{
  NSMutableArray * info = nil;

  if (account_xdata(*account).dflags & ACCOUNT_DISPLAYED) {
    info = [[NSMutableArray alloc] init];
    [info addObject:[NSValue valueWithPointer:account]];
  }

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    GetAccountsData(info != nil ? info : array, (*i).second);

  if (info != nil) {
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

      [names setValue:[NSString stringWithCString:name_stream.str().c_str()]
             forKey:[NSString stringWithCString:account.fullname().c_str()]];

      account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
    }
  }
}

void AddEntriesToArray::register_last_entry()
{
  for (transactions_list::const_iterator i = last_entry->transactions.begin();
       i != last_entry->transactions.end();
       i++) {
    transaction_t& xact = **i;
    if (transaction_has_xdata(xact) &&
	transaction_xdata_(xact).dflags & TRANSACTION_TO_DISPLAY) {
      // Calculate totals for the parent account
      account_xdata_t& xdata = account_xdata(xact_account(xact));
      add_transaction_to(xact, xdata.value);
      xdata.count++;
      if (xact.flags & TRANSACTION_VIRTUAL)
	xdata.virtuals++;

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
