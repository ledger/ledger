#import "GraphView.h"

@implementation GraphView

- (id)initWithFrame:(NSRect)frameRect
{
  self = [super initWithFrame:frameRect];
  if (self) {
    displayList = [[NSMutableArray alloc] init];
  }
  return self;
}

- (void)dealloc
{
  [displayList release];
}

- (void)drawRect:(NSRect)rect
{
  id obj = nil;
  NSEnumerator * en;
  NSSize sz;

  [[NSColor whiteColor] set];
  NSRectFill(rect);

  sz = [self convertSize:NSMakeSize(1, 1) fromView:nil];
  [NSBezierPath setDefaultLineWidth:MAX(sz.width, sz.height)];

  en = [displayList objectEnumerator];
  while (obj = [en nextObject])
    if (NSIntersectsRect(rect, [obj bounds]))
      [obj stroke];
}

- (void)clear
{
  [displayList removeAllObjects];
  [self setNeedsDisplay:YES];
}

- (void)addGraphElement:(id)element
{
  [displayList addObject:element];
  [self setNeedsDisplayInRect:[element bounds]];
}

@end
