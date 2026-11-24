//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#import "MacHtmlRenderer.h"
#import <Cocoa/Cocoa.h>
#import <WebKit/WebKit.h>
#include <string.h>
//=============================================================================
// Poll `window.nelsonMarkdownReady` so MathJax/markdown pages finish before the
// PDF is produced; plain pages fall through once the polls are exhausted.
static const NSUInteger MAX_READY_POLLS = 40;
static const NSTimeInterval READY_POLL_DELAY = 0.05;
//=============================================================================
@interface NelsonPdfRenderer : NSObject <WKNavigationDelegate> {
    WKWebView* _webView;
    NSString* _pdfPath;
    dispatch_semaphore_t _semaphore;
    NSUInteger _readyPolls;
    BOOL _finished;
    BOOL _success;
}
- (instancetype)initWithFileUrl:(NSURL*)fileUrl
                        pdfPath:(NSString*)pdfPath
                      semaphore:(dispatch_semaphore_t)semaphore;
- (BOOL)success;
- (void)cleanup;
@end
//=============================================================================
@implementation NelsonPdfRenderer
//=============================================================================
- (instancetype)initWithFileUrl:(NSURL*)fileUrl
                        pdfPath:(NSString*)pdfPath
                      semaphore:(dispatch_semaphore_t)semaphore
{
    self = [super init];
    if (self) {
        _semaphore = semaphore;
        _pdfPath = [pdfPath copy];
        _webView = [[WKWebView alloc] initWithFrame:NSMakeRect(0, 0, 816, 1056)];
        [_webView setNavigationDelegate:self];
        NSURL* readAccess = [fileUrl URLByDeletingLastPathComponent];
        [_webView loadFileURL:fileUrl allowingReadAccessToURL:readAccess];
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 60 * NSEC_PER_SEC),
            dispatch_get_main_queue(), ^{ [self finish:NO]; });
    }
    return self;
}
//=============================================================================
- (void)finish:(BOOL)success
{
    if (_finished) {
        return;
    }
    _finished = YES;
    _success = success;
    dispatch_semaphore_signal(_semaphore);
}
//=============================================================================
- (void)printToPdf
{
    if (@available(macOS 11.0, *)) {
        [_webView createPDFWithConfiguration:nil
                           completionHandler:^(NSData* data, NSError* error) {
                               BOOL ok = NO;
                               if (error == nil && data != nil && [data length] > 0) {
                                   ok = [data writeToFile:_pdfPath atomically:YES];
                               }
                               [self finish:ok];
                           }];
    } else {
        [self finish:NO];
    }
}
//=============================================================================
- (void)pollUntilReady
{
    [_webView evaluateJavaScript:@"Boolean(window.nelsonMarkdownReady)"
               completionHandler:^(id value, NSError* error) {
                   if (error == nil && [value boolValue]) {
                       [self printToPdf];
                   } else if (++_readyPolls < MAX_READY_POLLS) {
                       dispatch_after(dispatch_time(DISPATCH_TIME_NOW,
                                          (int64_t)(READY_POLL_DELAY * NSEC_PER_SEC)),
                           dispatch_get_main_queue(), ^{ [self pollUntilReady]; });
                   } else {
                       // Not a markdown page (or MathJax absent): print as-is.
                       [self printToPdf];
                   }
               }];
}
//=============================================================================
- (void)webView:(WKWebView*)webView didFinishNavigation:(WKNavigation*)navigation
{
    (void)webView;
    (void)navigation;
    [self pollUntilReady];
}
//=============================================================================
- (void)webView:(WKWebView*)webView
    didFailNavigation:(WKNavigation*)navigation
            withError:(NSError*)error
{
    (void)webView;
    (void)navigation;
    (void)error;
    [self finish:NO];
}
//=============================================================================
- (void)webView:(WKWebView*)webView
    didFailProvisionalNavigation:(WKNavigation*)navigation
                       withError:(NSError*)error
{
    (void)webView;
    (void)navigation;
    (void)error;
    [self finish:NO];
}
//=============================================================================
- (BOOL)success
{
    return _success;
}
//=============================================================================
- (void)cleanup
{
    [_webView stopLoading];
    [_webView setNavigationDelegate:nil];
}
//=============================================================================
- (void)dealloc
{
    [_webView release];
    [_pdfPath release];
    [super dealloc];
}
//=============================================================================
@end
//=============================================================================
bool
NelsonHtmlFileToPdfWithWebKit(const char* htmlFilePath, const char* pdfFilePath)
{
    if (htmlFilePath == NULL || pdfFilePath == NULL) {
        return false;
    }
    dispatch_semaphore_t semaphore = dispatch_semaphore_create(0);
    __block NelsonPdfRenderer* renderer = nil;
    void (^startRenderer)(void) = ^{
        NSString* htmlPath = [NSString stringWithUTF8String:htmlFilePath];
        NSString* pdfPath = [NSString stringWithUTF8String:pdfFilePath];
        NSURL* fileUrl = [NSURL fileURLWithPath:htmlPath];
        renderer = [[NelsonPdfRenderer alloc] initWithFileUrl:fileUrl
                                                      pdfPath:pdfPath
                                                    semaphore:semaphore];
    };
    if ([NSThread isMainThread]) {
        startRenderer();
        while (dispatch_semaphore_wait(semaphore, DISPATCH_TIME_NOW) != 0) {
            [[NSRunLoop currentRunLoop] runMode:NSDefaultRunLoopMode
                                     beforeDate:[NSDate dateWithTimeIntervalSinceNow:0.01]];
        }
    } else {
        dispatch_sync(dispatch_get_main_queue(), startRenderer);
        dispatch_semaphore_wait(semaphore, DISPATCH_TIME_FOREVER);
    }
    const bool success = [renderer success];
    void (^cleanupRenderer)(void) = ^{
        [renderer cleanup];
        [renderer release];
    };
    if ([NSThread isMainThread]) {
        cleanupRenderer();
    } else {
        dispatch_sync(dispatch_get_main_queue(), cleanupRenderer);
    }
#if !OS_OBJECT_USE_OBJC
    dispatch_release(semaphore);
#endif
    return success;
}
//=============================================================================
