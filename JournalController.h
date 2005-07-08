//
//  JournalController.h
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include "LedgerInterface.h"

@interface JournalController : NSWindowController
{
  IBOutlet NSOutlineView * accountsList;
  IBOutlet NSTableView *   entriesList;
  IBOutlet NSView *	   inFlowPie;
  IBOutlet NSView *	   outFlowPie;
  IBOutlet NSView *	   balanceGraph;
  IBOutlet NSFormCell *    accountQuery;
  IBOutlet NSFormCell *    payeeQuery;

  NSMutableArray *	   accounts;
  NSMutableDictionary *    accountNames;
  NSMutableArray *	   entries;

  ledger_interface *       ledgerConfig;
}

- (IBAction)setQueryPredicate:(id)sender;
- (IBAction)setQueryPeriod:(id)sender;
- (IBAction)setQuerySort:(id)sender;
- (IBAction)setQueryPeriodSort:(id)sender;
- (IBAction)setQueryOption:(id)sender;

- (IBAction)invokeQuery:(id)sender;
@end
