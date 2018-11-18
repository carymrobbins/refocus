#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

// Do we need to use @autoreleasepool everywhere?
// Maybe not since refocus is a short lived process.

NSRunningApplication* getActiveApp() {
  return [[NSWorkspace sharedWorkspace] frontmostApplication];
}

const char* getAppName(NSRunningApplication* app) {
  if (!app) return NULL;
  return [[app localizedName] UTF8String];
}

const char* getActiveAppName() {
  const char* res = getAppName(getActiveApp());
  return res;
}

void focusApp(NSRunningApplication* app) {
  if (!app) return;
  [app activateWithOptions:NSApplicationActivateIgnoringOtherApps];
}

NSRunningApplication* getAppByName(const char* name) {
  for (NSRunningApplication* app in
         [[NSWorkspace sharedWorkspace] runningApplications]) {
    if (strcmp(getAppName(app), name) == 0) return app;
  }
  return NULL;
}

void focusAppByName(const char* name) {
  NSRunningApplication* app = getAppByName(name);
  if (!app) return;
  focusApp(app);
}
