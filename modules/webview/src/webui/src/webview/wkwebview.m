//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
/*
  WebUI Library
  https://webui.me
  https://github.com/webui-dev/webui
  Copyright (c) 2020-2026 Hassan Draga.
  Licensed under MIT License.
  All rights reserved.
  Canada.
*/

#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
extern void nelson_configure_figure_printing(WKUserContentController* controller);

#define WEBUI_MAX_IDS (256) // Max windows, servers and threads
extern bool webui_interface_is_app_running(void);

void (*closed_callback)(int index) = NULL;
bool (*should_close_callback)(int index) = NULL;
void _webui_macos_wv_check_exit();

// NELSON PATCH: set by `webui_set_ui_thread_dispatcher()` when WebUI runs inside
// a host application that owns NSApplication and its run loop (Nelson's Qt
// desktop). WebUI otherwise assumes the process is its own: it installs itself
// as the application delegate, re-runs `finishLaunching`, and stops the run loop
// when its last window closes -- all of which take the application away from the
// host. When hosted, WebUI stays a guest: it keeps its per-window delegate, and
// leaves the application object and the run loop to the host.
static bool hostedUiLoop = false;

void _webui_macos_wv_set_hosted(bool status) {
    hostedUiLoop = status;
}

@interface WebUIWebView : WKWebView
@end

@implementation WebUIWebView

- (BOOL)acceptsFirstMouse:(NSEvent*)event
{
    (void)event;
    return YES;
}

- (void)magnifyWithEvent:(NSEvent*)event
{
    (void)event;
}

- (void)smartMagnifyWithEvent:(NSEvent*)event
{
    (void)event;
}

@end

@interface AppDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate, WKNavigationDelegate,
                                      WKScriptMessageHandler>
{
    NSWindow *windows[WEBUI_MAX_IDS];
    WKWebView *webViews[WEBUI_MAX_IDS];
    NSMenu *nativeMenus[WEBUI_MAX_IDS];
    NSMenu *fallbackMainMenu;
}
- (NSWindow *)windowAtIndex:(int)index;
- (void)setWindow:(NSWindow *)window atIndex:(int)index;
- (WKWebView *)webViewAtIndex:(int)index;
- (void)setWebView:(WKWebView *)webView atIndex:(int)index;
- (void)nativeMenuItemSelected:(NSMenuItem *)sender;
@end

@implementation AppDelegate

- (instancetype)init {
    self = [super init];
    if (self) {
        for (int i = 0; i < WEBUI_MAX_IDS; i++) {
            windows[i] = nil;
            webViews[i] = nil;
            nativeMenus[i] = nil;
        }
        fallbackMainMenu = nil;

        [NSTimer scheduledTimerWithTimeInterval:1.0
            target:self
            selector:@selector(_webui_macos_wv_timer)
            userInfo:nil
            repeats:YES];
    }
    return self;
}

- (void)_webui_macos_wv_timer {
    _webui_macos_wv_check_exit();
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\tapplicationDidFinishLaunching()\n");
    #endif
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    [[NSApplication sharedApplication] setActivationPolicy:NSApplicationActivationPolicyRegular];
    [[NSApplication sharedApplication] activateIgnoringOtherApps:YES];
}

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication*)sender
{
    (void)sender;
    if (!webui_interface_is_app_running()) {
        return NSTerminateNow;
    }
    for (int i = 0; i < WEBUI_MAX_IDS; i++) {
        if (windows[i] != nil) {
            [windows[i] performClose:nil];
        }
    }
    return NSTerminateCancel;
}

- (void)windowWillClose:(NSNotification *)notification {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\twindowWillClose()\n");
    #endif
    for (int i = 0; i < WEBUI_MAX_IDS; i++) {
        if (windows[i] == [notification object]) {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\twindowWillClose() -> Index [%d]\n", i);
            #endif
            if (closed_callback) {
                closed_callback(i);
            }
            WKWebView* closingWebView = webViews[i];
            windows[i] = nil;
            webViews[i] = nil;
            if (closingWebView != nil) {
                [closingWebView stopLoading];
                [closingWebView setNavigationDelegate:nil];
                [[[closingWebView configuration] userContentController]
                    removeScriptMessageHandlerForName:@"webuiNativeMenu"];
                [closingWebView removeFromSuperview];
            }
            if (nativeMenus[i] != nil) {
                if ([NSApp mainMenu] == nativeMenus[i]) {
                    [NSApp setMainMenu:fallbackMainMenu];
                }
                [nativeMenus[i] release];
                nativeMenus[i] = nil;
            }
            break;
        }
    }

    BOOL anyWindowOpen = NO;
    for (int i = 0; i < WEBUI_MAX_IDS; i++) {
        if (windows[i] != nil) {
            anyWindowOpen = YES;
            break;
        }
    }

    // NELSON PATCH: closing the last WebUI window must not stop the host's run
    // loop -- that would end the host application, not just this window.
    if (!anyWindowOpen && !hostedUiLoop) {
        [NSApp stop:nil];
    }
}

- (BOOL)windowShouldClose:(NSWindow*)sender
{
    int index = [self indexForWindow:sender];
    if (index < 0 || should_close_callback == NULL) {
        return YES;
    }
    return should_close_callback(index) ? YES : NO;
}

- (int)indexForWindow:(NSWindow *)window {
    for (int i = 0; i < WEBUI_MAX_IDS; i++) {
        if (windows[i] == window) {
            return i;
        }
    }
    return -1;
}

- (BOOL)isManagedNativeMenu:(NSMenu *)menu {
    if (menu == nil) {
        return NO;
    }
    for (int i = 0; i < WEBUI_MAX_IDS; i++) {
        if (nativeMenus[i] == menu) {
            return YES;
        }
    }
    return NO;
}

- (void)activateNativeMenuAtIndex:(int)index {
    if (index < 0 || index >= WEBUI_MAX_IDS || nativeMenus[index] == nil) {
        return;
    }
    NSMenu *current = [NSApp mainMenu];
    if (current != nativeMenus[index] && ![self isManagedNativeMenu:current]) {
        [fallbackMainMenu release];
        fallbackMainMenu = [current retain];
    }
    NSMenuItem *applicationMenuItem = [nativeMenus[index] itemAtIndex:0];
    NSString *applicationMenuTitle = [[applicationMenuItem submenu] title];
    if (applicationMenuTitle.length > 0) {
        [applicationMenuItem setTitle:applicationMenuTitle];
    }
    [NSApp setMainMenu:nativeMenus[index]];
}

- (void)dispatchEvent:(NSString *)name detail:(NSDictionary *)detail toWebView:(WKWebView *)webView {
    if (webView == nil || name.length == 0) {
        return;
    }
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:(detail ?: @{}) options:0 error:nil];
    if (jsonData == nil) {
        return;
    }
    NSString *json = [[[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding]
        autorelease];
    NSString *script = [NSString stringWithFormat:
        @"window.dispatchEvent(new CustomEvent('%@',{detail:%@}));", name, json];
    [webView evaluateJavaScript:script completionHandler:nil];
}

- (void)windowDidBecomeKey:(NSNotification *)notification {
    int index = [self indexForWindow:[notification object]];
    if (index < 0) {
        return;
    }
    [self activateNativeMenuAtIndex:index];
    [self dispatchEvent:@"webui:native-menu-refresh" detail:@{} toWebView:webViews[index]];
}

- (void)windowDidResignKey:(NSNotification *)notification {
    int index = [self indexForWindow:[notification object]];
    if (index < 0 || nativeMenus[index] == nil) {
        return;
    }
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *keyWindow = [NSApp keyWindow];
        int keyIndex = [self indexForWindow:keyWindow];
        if (keyIndex >= 0 && nativeMenus[keyIndex] != nil) {
            [self activateNativeMenuAtIndex:keyIndex];
        } else if ([NSApp mainMenu] == nativeMenus[index]) {
            [NSApp setMainMenu:fallbackMainMenu];
        }
    });
}

- (void)configureShortcut:(NSString *)shortcut forItem:(NSMenuItem *)item {
    if (![shortcut isKindOfClass:[NSString class]] || shortcut.length == 0) {
        return;
    }
    NSArray *parts = [[shortcut lowercaseString] componentsSeparatedByString:@"+"];
    NSEventModifierFlags modifiers = 0;
    NSString *key = @"";
    for (NSString *part in parts) {
        if ([part isEqualToString:@"cmd"] || [part isEqualToString:@"command"]) {
            modifiers |= NSEventModifierFlagCommand;
        } else if ([part isEqualToString:@"shift"]) {
            modifiers |= NSEventModifierFlagShift;
        } else if ([part isEqualToString:@"alt"] || [part isEqualToString:@"option"]) {
            modifiers |= NSEventModifierFlagOption;
        } else if ([part isEqualToString:@"ctrl"] || [part isEqualToString:@"control"]) {
            modifiers |= NSEventModifierFlagControl;
        } else if (part.length > 0) {
            key = part;
        }
    }
    [item setKeyEquivalent:key];
    [item setKeyEquivalentModifierMask:modifiers];
}

- (SEL)selectorForRole:(NSString *)role target:(id *)target {
    *target = nil;
    if ([role isEqualToString:@"hide"]) {
        *target = NSApp;
        return @selector(hide:);
    }
    if ([role isEqualToString:@"hideOthers"]) {
        *target = NSApp;
        return @selector(hideOtherApplications:);
    }
    if ([role isEqualToString:@"showAll"]) {
        *target = NSApp;
        return @selector(unhideAllApplications:);
    }
    if ([role isEqualToString:@"closeWindow"]) {
        return @selector(performClose:);
    }
    if ([role isEqualToString:@"minimize"]) {
        return @selector(performMiniaturize:);
    }
    if ([role isEqualToString:@"zoom"]) {
        return @selector(performZoom:);
    }
    return NULL;
}

- (NSMenu *)buildMenuWithTitle:(NSString *)title items:(NSArray *)items index:(int)index {
    NSMenu *menu = [[NSMenu alloc] initWithTitle:(title ?: @"")];
    [menu setAutoenablesItems:NO];
    for (id rawItem in items) {
        if (![rawItem isKindOfClass:[NSDictionary class]]) {
            continue;
        }
        NSDictionary *descriptor = rawItem;
        if ([descriptor[@"separator"] boolValue]) {
            [menu addItem:[NSMenuItem separatorItem]];
            continue;
        }
        NSString *label = descriptor[@"label"];
        if (![label isKindOfClass:[NSString class]] || label.length == 0) {
            continue;
        }
        NSString *role = descriptor[@"role"];
        id target = nil;
        SEL selector = [self selectorForRole:role target:&target];
        NSString *action = descriptor[@"action"];
        if (selector == NULL && [action isKindOfClass:[NSString class]] && action.length > 0) {
            selector = @selector(nativeMenuItemSelected:);
            target = self;
        }
        NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:label action:selector
            keyEquivalent:@""];
        [item setTarget:target];
        if ([action isKindOfClass:[NSString class]] && action.length > 0) {
            [item setRepresentedObject:@{ @"action": action, @"index": @(index) }];
        }
        [item setEnabled:descriptor[@"enabled"] == nil || [descriptor[@"enabled"] boolValue]];
        if ([descriptor[@"checked"] boolValue]) {
            [item setState:NSControlStateValueOn];
        }
        [self configureShortcut:descriptor[@"shortcut"] forItem:item];
        NSArray *children = descriptor[@"items"];
        if ([children isKindOfClass:[NSArray class]]) {
            NSMenu *submenu = [self buildMenuWithTitle:label items:children index:index];
            [item setSubmenu:submenu];
            [submenu release];
        }
        [menu addItem:item];
        [item release];
    }
    return menu;
}

- (void)installNativeMenuDescriptor:(NSDictionary *)descriptor atIndex:(int)index {
    NSArray *menus = descriptor[@"menus"];
    if (index < 0 || index >= WEBUI_MAX_IDS || ![menus isKindOfClass:[NSArray class]]) {
        return;
    }
    NSMenu *mainMenu = [[NSMenu alloc] initWithTitle:@""];
    for (id rawMenu in menus) {
        if (![rawMenu isKindOfClass:[NSDictionary class]]) {
            continue;
        }
        NSString *label = rawMenu[@"label"];
        NSArray *items = rawMenu[@"items"];
        if (![label isKindOfClass:[NSString class]] || ![items isKindOfClass:[NSArray class]]) {
            continue;
        }
        NSMenuItem *rootItem = [[NSMenuItem alloc] initWithTitle:label action:nil keyEquivalent:@""];
        NSMenu *submenu = [self buildMenuWithTitle:label items:items index:index];
        [rootItem setSubmenu:submenu];
        [submenu release];
        [mainMenu addItem:rootItem];
        [rootItem release];
    }
    if (mainMenu.numberOfItems == 0) {
        [mainMenu release];
        return;
    }
    BOOL wasActive = [NSApp mainMenu] == nativeMenus[index];
    NSMenu *oldMenu = nativeMenus[index];
    nativeMenus[index] = mainMenu;
    if (wasActive || [NSApp keyWindow] == windows[index]) {
        [self activateNativeMenuAtIndex:index];
    }
    [oldMenu release];
}

- (void)userContentController:(WKUserContentController *)userContentController
      didReceiveScriptMessage:(WKScriptMessage *)message {
    (void)userContentController;
    if (![message.name isEqualToString:@"webuiNativeMenu"]
        || ![message.body isKindOfClass:[NSDictionary class]]) {
        return;
    }
    NSDictionary *body = message.body;
    if (![(body[@"type"] ?: @"") isEqualToString:@"set"]) {
        return;
    }
    for (int i = 0; i < WEBUI_MAX_IDS; i++) {
        if (webViews[i] == message.webView) {
            [self installNativeMenuDescriptor:body atIndex:i];
            break;
        }
    }
}

- (void)nativeMenuItemSelected:(NSMenuItem *)sender {
    NSDictionary *represented = sender.representedObject;
    NSString *action = represented[@"action"];
    int index = [represented[@"index"] intValue];
    if (![action isKindOfClass:[NSString class]] || index < 0 || index >= WEBUI_MAX_IDS) {
        return;
    }
    [self dispatchEvent:@"webui:native-menu-action" detail:@{ @"action": action }
        toWebView:webViews[index]];
}

#pragma mark - Accessors

- (NSWindow *)windowAtIndex:(int)index {
    return windows[index];
}

- (void)setWindow:(NSWindow *)window atIndex:(int)index {
    windows[index] = window;
}

- (WKWebView *)webViewAtIndex:(int)index {
    return webViews[index];
}

- (void)setWebView:(WKWebView *)webView atIndex:(int)index {
    webViews[index] = webView;
}

#pragma mark - WKNavigationDelegate

- (void)webView:(WKWebView *)webView didFinishNavigation:(WKNavigation *)navigation {
    NSString *title = webView.title;
    for (int i = 0; i < WEBUI_MAX_IDS; i++) {
        if (webViews[i] == webView) {
            NSWindow *window = windows[i];
            [window setTitle:title];
            [NSApp activateIgnoringOtherApps:YES];
            [window makeKeyAndOrderFront:nil];
            [window makeFirstResponder:webView];
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\tdidFinishNavigation([%d])\n", i);
            printf("[ObjC]\t\t\tdidFinishNavigation([%d]) -> Updated title [%s]\n", i, [title UTF8String]);
            #endif
            break;
        }
    }
}

@end

AppDelegate *delegate;
static bool applicationFinishedLaunching = false;

void _webui_macos_wv_start() {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_start()\n");
    #endif

    @autoreleasepool {
        NSApplication *app = [NSApplication sharedApplication];
        [app setActivationPolicy:NSApplicationActivationPolicyRegular];

        [app finishLaunching];
        [app run];

        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_start() -> Exit.\n");
        #endif
    }
}

bool _webui_macos_wv_iteration_do(void) {
    if (![NSThread isMainThread]) {
        __block bool didProcessEvent = false;
        dispatch_sync(dispatch_get_main_queue(), ^{
            didProcessEvent = _webui_macos_wv_iteration_do();
        });
        return didProcessEvent;
    }

    @autoreleasepool {
        NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
                                untilDate:[NSDate distantPast]
                                inMode:NSDefaultRunLoopMode
                                dequeue:YES];
        if (event) {
            [NSApp sendEvent:event];
            [NSApp updateWindows];
            return true; // One event processed
        }
        return false; // Event queue is empty
    }
}

bool _webui_macos_wv_new(int index, bool frameless, bool resizable,
    const unsigned char* iconData, size_t iconSize) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_new([%d], [%d], [%d])\n", index, frameless, resizable);
    #endif

    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_new([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    // AppKit objects belong to the main thread while evaluation runs on a worker.
    if (![NSThread isMainThread]) {
        __block bool success = false;
        dispatch_sync(dispatch_get_main_queue(), ^{
            success = _webui_macos_wv_new(index, frameless, resizable, iconData, iconSize);
        });
        return success;
    }

    NSApplication *app = [NSApplication sharedApplication];
    if (!delegate) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_new([%d]) -> Creating NSApplication and AppDelegate\n", index);
        #endif
        delegate = [[AppDelegate alloc] init];
        // NELSON PATCH: the object is still needed as the NSWindowDelegate of
        // every WebUI window, but under a host it must not become the
        // application delegate: that slot already holds the host's own (Qt's
        // QCocoaApplicationDelegate), which drives termination, the dock menu
        // and file-open events. Re-running `finishLaunching` on an application
        // the host has already launched is not ours to do either.
        if (!hostedUiLoop) {
            [app setDelegate:delegate];
            // Finish startup before the asynchronous WebUI loop pumps individual events.
            if (!applicationFinishedLaunching) {
                [app finishLaunching];
                applicationFinishedLaunching = true;
            }
        }
    }
    if (iconData != NULL && iconSize > 0) {
        NSData *data = [NSData dataWithBytes:iconData length:iconSize];
        NSImage *applicationIcon = [[[NSImage alloc] initWithData:data] autorelease];
        if (applicationIcon != nil) {
            [app setApplicationIconImage:applicationIcon];
            [[app dockTile] display];
        }
    }

    NSRect frame = NSMakeRect(0, 0, 800, 600);

    NSWindowStyleMask windowStyle;
    if (frameless) {
        windowStyle = NSWindowStyleMaskBorderless;
        if (resizable) {
            windowStyle |= NSWindowStyleMaskResizable; // Allows programmatic resizing
        }
    } else {
        windowStyle = NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable;
        if (resizable) {
            windowStyle |= NSWindowStyleMaskResizable; // Allows user resizing
        }
    }

    NSWindow *window = [[NSWindow alloc] initWithContentRect:frame
                                                   styleMask:windowStyle
                                                     backing:NSBackingStoreBuffered
                                                       defer:NO];
    if (!window) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_new([%d]) -> Failed to create NSWindow\n", index);
        #endif
        return false;
    }

    if (!frameless) {
        [window setTitle:@"Loading..."];
    } else {
        [window setMovableByWindowBackground:YES]; // Allow moving frameless windows
    }
    [window setDelegate:delegate];

    WKUserContentController *contentController = [[WKUserContentController alloc] init];
    nelson_configure_figure_printing(contentController);
    [contentController addScriptMessageHandler:delegate name:@"webuiNativeMenu"];
    WKWebViewConfiguration *configuration = [[WKWebViewConfiguration alloc] init];
    [configuration setUserContentController:contentController];
    // NELSON PATCH: grant clipboard read silently for local pages. Without
    // these preferences WebKit shows its "Paste" confirmation pill on every
    // navigator.clipboard.readText() call (console context-menu paste). The
    // keys are private WKPreferences properties set through KVC (the same
    // technique used by the webview/webview library); if an OS update removes
    // them, the KVC exception is swallowed and the pill simply comes back.
    @try {
        [[configuration preferences] setValue:@YES forKey:@"javaScriptCanAccessClipboard"];
        [[configuration preferences] setValue:@YES forKey:@"DOMPasteAllowed"];
    } @catch (NSException *exception) {
        (void)exception;
    }
    WKWebView* webView = [[WebUIWebView alloc] initWithFrame:[[window contentView] bounds]
                                               configuration:configuration];
    [configuration release];
    [contentController release];
    if (!webView) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_new([%d]) -> Failed to create WKWebView\n", index);
        #endif
        return false;
    }
    [webView setNavigationDelegate:delegate];
    [webView setAutoresizingMask:(NSViewWidthSizable | NSViewHeightSizable)];
    [window.contentView addSubview:webView];

    [delegate setWindow:window atIndex:index];
    [delegate setWebView:webView atIndex:index];
    [webView release];

    return true;
}

void _webui_macos_wv_new_thread_safe(int index, bool frameless, bool resizable,
    const unsigned char* iconData, size_t iconSize) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_new_thread_safe([%d])\n", index);
    #endif
    dispatch_async(dispatch_get_main_queue(), ^{
        _webui_macos_wv_new(index, frameless, resizable, iconData, iconSize);
    });
}

bool _webui_macos_wv_show(int index, const char* urlString, int x, int y, int width, int height) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_show([%d])\n", index);
    printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> url: [%s]\n", index, urlString);
    printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> x: [%d]\n", index, x);
    printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> y: [%d]\n", index, y);
    printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> width: [%d]\n", index, width);
    printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> height: [%d]\n", index, height);
    #endif
    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    __block bool success = false;
    void (^showWindow)(void) = ^{
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> dispatch_async\n", index);
        #endif
        NSWindow *window = [delegate windowAtIndex:index];
        WKWebView *webView = [delegate webViewAtIndex:index];

        if (window && webView) {
            NSRect frame = [window frame];
            frame.origin.x = x;
            frame.origin.y = y;
            frame.size.width = width;
            frame.size.height = height;
            [window setFrame:frame display:YES];

            NSString *nsUrlString = [NSString stringWithUTF8String:urlString];
            if (nsUrlString) {
                NSURL *url = [NSURL URLWithString:nsUrlString];
                if (url) {
                    NSURLRequest *request = [NSURLRequest requestWithURL:url];
                    [webView loadRequest:request];
                }
            }

            [NSApp activateIgnoringOtherApps:YES];
            [window makeKeyAndOrderFront:nil];
            [window makeFirstResponder:webView];
            success = true;
        }
        else {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\t_webui_macos_wv_show([%d]) -> Window or WebView not found.\n", index);
            #endif
        }
    };
    if ([NSThread isMainThread]) {
        showWindow();
    }
    else {
        dispatch_sync(dispatch_get_main_queue(), showWindow);
    }
    return success;
}

void _webui_macos_wv_process() {
    #ifdef WEBUI_LOG
    // printf("[ObjC]\t\t\t_webui_macos_wv_process()\n");
    #endif
    // NELSON PATCH: the host's run loop already dequeues and sends NSApp events.
    // Draining them here would steal events from it.
    if (hostedUiLoop)
        return;
    if (![NSThread isMainThread]) {
        dispatch_sync(dispatch_get_main_queue(), ^{
            _webui_macos_wv_process();
        });
        return;
    }

    NSApplication *app = [NSApplication sharedApplication];
    NSEvent *event;

    while ((event = [app nextEventMatchingMask:NSEventMaskAny
                        untilDate:[NSDate distantPast]
                        inMode:NSDefaultRunLoopMode
                        dequeue:YES])) {
        [app sendEvent:event];
    }
}

void _webui_macos_wv_stop() {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_stop()\n");
    #endif
    // NELSON PATCH: this is how WebUI ends its own `webui_wait()` run loop, and
    // it is reached from every window close and destroy (webui.c, the wait
    // signal helper) and from the delegate's exit timer. Under a host there is
    // no WebUI run loop to end, and stopping the one that is actually running
    // would take the host application down with the window.
    if (hostedUiLoop)
        return;
    dispatch_async(dispatch_get_main_queue(), ^{
        NSApplication *app = [NSApplication sharedApplication];
        [app stop:nil];

        NSEvent *event = [NSEvent otherEventWithType:NSEventTypeApplicationDefined
                            location:NSMakePoint(0, 0)
                            modifierFlags:0
                            timestamp:0
                            windowNumber:0
                            context:nil
                            subtype:0
                            data1:0
                            data2:0];
        [app postEvent:event atStart:YES];
    });
}

void _webui_macos_wv_check_exit() {
    if (!webui_interface_is_app_running()) {
        _webui_macos_wv_stop();
    }
}

bool _webui_macos_wv_close(int index) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_close([%d])\n", index);
    #endif
    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_close([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    __block bool success = false;
    void (^closeWindow)(void) = ^{
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_close([%d]) -> close\n", index);
        #endif
        NSWindow *window = [delegate windowAtIndex:index];
        if (window) {
            // A programmatic close must run the normal NSWindow teardown. Hiding
            // it leaves the delegate's window and WebView slots alive, so an
            // application shutdown can finish with an orphan Figure window.
            [window close];
            success = true;
        }
        else {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\t_webui_macos_wv_close([%d]) -> Window not found.\n", index);
            #endif
        }
    };
    if ([NSThread isMainThread]) {
        closeWindow();
    }
    else {
        dispatch_sync(dispatch_get_main_queue(), closeWindow);
    }
    return success;
}

bool _webui_macos_wv_is_main_thread(void) {
    return [NSThread isMainThread];
}

bool _webui_macos_wv_focus(int index) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_focus([%d])\n", index);
    #endif
    if (index < 0 || index >= WEBUI_MAX_IDS) {
        return false;
    }

    __block bool success = false;
    void (^focusWindow)(void) = ^{
        NSWindow *window = [delegate windowAtIndex:index];
        WKWebView *webView = [delegate webViewAtIndex:index];
        if (window) {
            if ([window isMiniaturized]) {
                [window deminiaturize:nil];
            }
            [NSApp activateIgnoringOtherApps:YES];
            [window makeKeyAndOrderFront:nil];
            if (webView) {
                [window makeFirstResponder:webView];
            }
            success = true;
        }
    };
    if ([NSThread isMainThread]) {
        focusWindow();
    } else {
        dispatch_sync(dispatch_get_main_queue(), focusWindow);
    }
    return success;
}

bool _webui_macos_wv_set_position(int index, int x, int y) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_set_position([%d])\n", index);
    printf("[ObjC]\t\t\t_webui_macos_wv_set_position([%d]) -> x: [%d]\n", index, x);
    printf("[ObjC]\t\t\t_webui_macos_wv_set_position([%d]) -> y: [%d]\n", index, y);
    #endif
    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_set_position([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    __block bool success = false;
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *window = [delegate windowAtIndex:index];
        if (window) {
            NSRect frame = [window frame];
            frame.origin.x = x;
            frame.origin.y = y;
            [window setFrame:frame display:YES];
            success = true;
        }
        else {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\t_webui_macos_wv_set_position([%d]) -> Window not found.\n", index);
            #endif
        }
    });
    return success;
}

bool _webui_macos_wv_minimize(int index) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_minimize([%d])\n", index);
    #endif

    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_minimize([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    __block bool success = false;
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *window = [delegate windowAtIndex:index];
        if (window) {
            [window miniaturize:nil];
            success = true;
        } else {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\t_webui_macos_wv_minimize([%d]) -> Window not found.\n", index);
            #endif
        }
    });

    return success;
}

bool _webui_macos_wv_maximize(int index) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_maximize([%d])\n", index);
    #endif

    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_maximize([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    __block bool success = false;
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *window = [delegate windowAtIndex:index];
        if (window) {
            if (![window isZoomed]) {
                [window zoom:nil];
            }
            success = true;
        } else {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\t_webui_macos_wv_maximize([%d]) -> Window not found.\n", index);
            #endif
        }
    });

    return success;
}

// NELSON PATCH: hide/show an already-shown window (post-show), for figure
// Visible on/off. See webui-vendored-patches.
bool _webui_macos_wv_set_hidden(int index, bool hidden) {
    if (index < 0 || index >= WEBUI_MAX_IDS) {
        return false;
    }
    __block bool success = false;
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *window = [delegate windowAtIndex:index];
        if (window) {
            if (hidden) {
                [window orderOut:nil];
            } else {
                [window makeKeyAndOrderFront:nil];
            }
            success = true;
        }
    });
    return success;
}

bool _webui_macos_wv_set_size(int index, int width, int height) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_set_size([%d])\n", index);
    printf("[ObjC]\t\t\t_webui_macos_wv_set_size([%d]) -> width: [%d]\n", index, width);
    printf("[ObjC]\t\t\t_webui_macos_wv_set_size([%d]) -> height: [%d]\n", index, height);
    #endif
    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_set_size([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    __block bool success = false;
    dispatch_async(dispatch_get_main_queue(), ^{
        NSWindow *window = [delegate windowAtIndex:index];
        if (window) {
            NSRect frame = [window frame];
            frame.size.width = width;
            frame.size.height = height;
            [window setFrame:frame display:YES];
            success = true;
        }
        else {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\t_webui_macos_wv_set_size([%d]) -> Window not found.\n", index);
            #endif
        }
    });
    return success;
}

bool _webui_macos_wv_get_geometry(int index, int* x, int* y,
    unsigned int* width, unsigned int* height, bool* maximized) {
    if (index < 0 || index >= WEBUI_MAX_IDS || !x || !y || !width || !height || !maximized) {
        return false;
    }

    __block bool success = false;
    void (^capture)(void) = ^{
        NSWindow *window = [delegate windowAtIndex:index];
        if (window) {
            NSRect frame = [window frame];
            *x = (int)frame.origin.x;
            *y = (int)frame.origin.y;
            *width = (unsigned int)frame.size.width;
            *height = (unsigned int)frame.size.height;
            *maximized = [window isZoomed];
            success = true;
        }
    };
    if ([NSThread isMainThread]) {
        capture();
    } else {
        dispatch_sync(dispatch_get_main_queue(), capture);
    }
    return success;
}

bool _webui_macos_wv_navigate(int index, const char* urlString) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_navigate([%d])\n", index);
    printf("[ObjC]\t\t\t_webui_macos_wv_navigate([%d]) -> url: [%s]\n", index, urlString);
    #endif
    if (index < 0 || index >= WEBUI_MAX_IDS) {
        #ifdef WEBUI_LOG
        printf("[ObjC]\t\t\t_webui_macos_wv_navigate([%d]) -> Invalid index\n", index);
        #endif
        return false;
    }

    __block bool success = false;
    void (^navigate)(void) = ^{
        WKWebView *webView = [delegate webViewAtIndex:index];
        if (webView) {
            NSString *nsUrlString = [NSString stringWithUTF8String:urlString];
            if (nsUrlString) {
                NSURL *url = [NSURL URLWithString:nsUrlString];
                if (url) {
                    NSURLRequest *request = [NSURLRequest requestWithURL:url];
                    [webView loadRequest:request];
                    success = true;
                }
            }
        }
        else {
            #ifdef WEBUI_LOG
            printf("[ObjC]\t\t\t_webui_macos_wv_navigate([%d]) -> WebView not found.\n", index);
            #endif
        }
    };
    if ([NSThread isMainThread]) {
        navigate();
    } else {
        dispatch_sync(dispatch_get_main_queue(), navigate);
    }
    return success;
}

void _webui_macos_wv_set_close_cb(void (*cb)(int index)) {
    #ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_set_close_cb()\n");
    #endif
    closed_callback = cb;
}

void
_webui_macos_wv_set_should_close_cb(bool (*cb)(int index))
{
#ifdef WEBUI_LOG
    printf("[ObjC]\t\t\t_webui_macos_wv_set_should_close_cb()\n");
#endif
    should_close_callback = cb;
}
