//
//  JournalDocument.mm
//  MoneyWise
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
  if (self)
    _journal = new ledger::journal_t;
  return self;
}

- (void)dealloc
{
  delete _journal;

  [super dealloc];
}

- (ledger::journal_t *)journal
{
  return _journal;
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
  if (ledger::parse_ledger_data(_journal, [fileName cString]) > 0)
    return YES;
  else
    return NO;
}

@end
