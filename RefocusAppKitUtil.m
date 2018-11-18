#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

// Do we need to use @autoreleasepool everywhere?
// Maybe not since refocus is a short lived process.

NSRunningApplication* getActiveApp() {
  return [[NSWorkspace sharedWorkspace] frontmostApplication];
}

const char* getAppName(NSRunningApplication* app) {
  return [[app localizedName] UTF8String];
}

const char* getActiveAppName() {
  return getAppName(getActiveApp());
}

bool focusApp(NSRunningApplication* app) {
  return [app activateWithOptions:NSApplicationActivateIgnoringOtherApps];
}

NSArray* getRunningApps() {
  return [[NSWorkspace sharedWorkspace] runningApplications];
}

int nsarrayLength(NSArray* arr) {
  return [arr count];
}

NSEnumerator* nsarrayEnumerator(NSArray* arr) {
  return [arr objectEnumerator];
}

id enumeratorNextObject(NSEnumerator* e) {
  return [e nextObject];
}
