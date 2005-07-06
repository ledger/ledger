//
//  JournalDocument.mm
//  LedgerPro
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import "JournalDocument.h"
#import "JournalController.h"

@implementation JournalDocument

- (id)init
{
  self = [super init];
  if (self) {
    journal = new journal_t;
    if (! journal)
      return nil;
  }
  return self;
}

- (void)dealloc
{
  if (journal) {
    clear_all_xdata();
    delete journal;
  }

  [super dealloc];
}

- (ledger::journal_t *)journalPointer
{
  return journal;
}

- (NSString *)windowNibName
{
  return @"JournalWindow";
}

- (void)makeWindowControllers
{
  JournalController * ctl = [[JournalController alloc]
			     initWithWindowNibName:[self windowNibName]];
  [self addWindowController:ctl];
  [ctl release];
}

- (void)windowControllerDidLoadNib:(NSWindowController *)aController
{
  [super windowControllerDidLoadNib:aController];
  // Add any code here that needs to be executed once the
  // windowController has loaded the document's window.
}

- (BOOL)writeToFile:(NSString *)fileName ofType:(NSString *)docType
{
  // jww (2005-07-04): NYI
  return NO;
}

- (BOOL)readFromFile:(NSString *)fileName ofType:(NSString *)docType
{
  // Parse initialization files, ledger data, price database, etc.

  bool dirty;
  if (parse_ledger_data(journal, "", [fileName cString], "",
			false, "", dirty, "") > 0)
    return YES;
  else
    return NO;
}

@end
