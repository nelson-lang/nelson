// Copyright (c) 2026-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>

// The embedded WebView does not host browser popup/print UI. Keep this bridge
// owned by its content controller and pass only an exported SVG and its title.
@interface NelsonFigurePrinter
    : NSObject <WKScriptMessageHandler, WKNavigationDelegate, NSWindowDelegate> {
    NSWindow* _preview;
    WKWebView* _view;
    BOOL _printing;
}
@end

@implementation NelsonFigurePrinter
- (void)cleanup
{
    [_view setNavigationDelegate:nil];
    [_view release];
    _view = nil;
    [_preview setDelegate:nil];
    [_preview close];
    [_preview release];
    _preview = nil;
}

- (void)dealloc
{
    [self cleanup];
    [super dealloc];
}

- (void)windowWillClose:(NSNotification*)notification
{
    (void)notification;
    [self retain];
    [self cleanup];
    [self release];
}

- (void)printOperationDidRun:(NSPrintOperation*)operation
                     success:(BOOL)success
                 contextInfo:(void*)context
{
    (void)operation;
    (void)success;
    (void)context;
    _printing = NO;
}

- (void)printFigure
{
    if (_printing || !_view || !_preview) {
        return;
    }
    NSPrintInfo* info = [[NSPrintInfo sharedPrintInfo] copy];
    [info setHorizontalPagination:NSPrintingPaginationModeFit];
    [info setVerticallyCentered:NO];
    NSPrintOperation* operation = [_view printOperationWithPrintInfo:info];
    [info release];
    if (operation) {
        _printing = YES;
        [operation runOperationModalForWindow:_preview
                                     delegate:self
                               didRunSelector:@selector(printOperationDidRun:success:contextInfo:)
                                  contextInfo:NULL];
    }
}

- (void)webView:(WKWebView*)webView didFinishNavigation:(WKNavigation*)navigation
{
    (void)webView;
    (void)navigation;
    [self printFigure];
}

- (void)userContentController:(WKUserContentController*)controller
      didReceiveScriptMessage:(WKScriptMessage*)message
{
    (void)controller;
    if (![message.frameInfo isMainFrame] || _printing) {
        return;
    }
    if (message.webView == _view && [message.body isEqual:@"print"]) {
        [self printFigure];
        return;
    }
    NSString* host = message.webView.URL.host;
    if (!([host isEqualToString:@"localhost"] || [host isEqualToString:@"127.0.0.1"])
        || ![message.body isKindOfClass:[NSDictionary class]]) {
        return;
    }
    NSString* svg = message.body[@"svg"];
    NSString* title = message.body[@"title"];
    if (![svg isKindOfClass:[NSString class]] || ![title isKindOfClass:[NSString class]]) {
        return;
    }
    [self cleanup];
    _preview = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 900, 650)
                                           styleMask:NSWindowStyleMaskTitled
        | NSWindowStyleMaskClosable | NSWindowStyleMaskResizable
                                             backing:NSBackingStoreBuffered
                                               defer:NO];
    [_preview setReleasedWhenClosed:NO];
    [_preview setTitle:[title stringByAppendingString:@" — Print"]];
    [_preview setDelegate:self];
    WKWebViewConfiguration* configuration = [[WKWebViewConfiguration alloc] init];
    [configuration.userContentController addScriptMessageHandler:self name:@"nelsonFigurePrint"];
    _view = [[WKWebView alloc] initWithFrame:_preview.contentView.bounds
                               configuration:configuration];
    [configuration release];
    [_view setAutoresizingMask:NSViewWidthSizable | NSViewHeightSizable];
    [_view setNavigationDelegate:self];
    [_preview setContentView:_view];
    NSString* encoded =
        [[svg dataUsingEncoding:NSUTF8StringEncoding] base64EncodedStringWithOptions:0];
    NSString* html = [NSString stringWithFormat:
            @"<!doctype html><meta charset='utf-8'><style>body{margin:16px;font:14px system-ui}"
             "img{display:block;max-width:100%%;height:auto}button{margin-bottom:16px;padding:6px "
             "16px}"
             "@media print{body{margin:0}button{display:none}img{break-inside:avoid}}</style>"
             "<button "
             "onclick=\"window.webkit.messageHandlers.nelsonFigurePrint.postMessage('print')\">"
             "Print</button>"
             "<img alt='Figure' src='data:image/svg+xml;base64,%@'>",
        encoded];
    [_view loadHTMLString:html baseURL:nil];
    [_preview center];
    [_preview makeKeyAndOrderFront:nil];
}
@end

void
nelson_configure_figure_printing(WKUserContentController* controller)
{
    NelsonFigurePrinter* printer = [[NelsonFigurePrinter alloc] init];
    [controller addScriptMessageHandler:printer name:@"nelsonFigurePrint"];
    [printer release];
}
