#import "MWApplication.h"

@implementation MWApplication

- (void)orderFrontStandardAboutPanel:(id)sender
{
  if (aboutPanel == nil) {
    [NSBundle loadNibNamed:@"AboutPanel" owner:self];
  }
  [aboutPanel makeKeyAndOrderFront:self];
}

@end
