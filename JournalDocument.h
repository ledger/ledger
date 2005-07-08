//
//  JournalDocument.h
//  MoneyWise
//
//  Created by John Wiegley on Sun Jul 03 2005.
//  Copyright (c) 2005 New Artisans LLC. All rights reserved.
//

#import <Cocoa/Cocoa.h>

#include <ledger.h>

@interface JournalDocument : NSDocument
{
  ledger::journal_t * journal;
}

- (ledger::journal_t *)journalPointer;
@end
