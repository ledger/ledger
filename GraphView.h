/* GraphView */

#import <Cocoa/Cocoa.h>

@protocol GraphViewElement
- (int)tag;
- (void)setTag:(int)aTag;
- (void)stroke;
- (NSRect)bounds;
- (void)setColor:(NSColor *)aColor;
- (NSColor *)color;
@end

@interface GraphView : NSView
{
  NSMutableArray * displayList;
}
@end
