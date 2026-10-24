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
static const NSUInteger MAX_READY_POLLS = 100;
static const NSTimeInterval READY_POLL_DELAY = 0.05;
//=============================================================================
@interface NelsonHtmlRenderer : NSObject <WKNavigationDelegate> {
    WKWebView* _webView;
    NSString* _result;
    dispatch_semaphore_t _semaphore;
    NSUInteger _readyPolls;
    BOOL _finished;
}
- (instancetype)initWithHtml:(NSString*)html semaphore:(dispatch_semaphore_t)semaphore;
- (void)finishWithResult:(NSString*)result;
- (void)readRenderedHtml;
- (void)pollUntilReady;
- (NSString*)result;
- (void)cleanup;
@end
//=============================================================================
@implementation NelsonHtmlRenderer
//=============================================================================
- (instancetype)initWithHtml:(NSString*)html semaphore:(dispatch_semaphore_t)semaphore
{
    self = [super init];
    if (self) {
        _semaphore = semaphore;
        _webView = [[WKWebView alloc] initWithFrame:NSMakeRect(0, 0, 800, 600)];
        [_webView setNavigationDelegate:self];
        [_webView loadHTMLString:html baseURL:nil];
        dispatch_after(dispatch_time(DISPATCH_TIME_NOW, 6 * NSEC_PER_SEC),
            dispatch_get_main_queue(), ^{ [self finishWithResult:nil]; });
    }
    return self;
}
//=============================================================================
- (void)finishWithResult:(NSString*)result
{
    if (_finished) {
        return;
    }
    _finished = YES;
    _result = [result copy];
    dispatch_semaphore_signal(_semaphore);
}
//=============================================================================
- (void)readRenderedHtml
{
    NSString* script = @"(()=>{const n=document.getElementById('nelson-markdowndisp-root');"
                        "return n?n.innerHTML:document.body.innerHTML;})()";
    [_webView
        evaluateJavaScript:script
         completionHandler:^(id value, NSError* error) {
             [self finishWithResult:error == nil && [value isKindOfClass:[NSString class]] ? value
                                                                                           : nil];
         }];
}
//=============================================================================
- (void)pollUntilReady
{
    [_webView evaluateJavaScript:@"Boolean(window.nelsonMarkdownReady)"
               completionHandler:^(id value, NSError* error) {
                   if (error == nil && [value boolValue]) {
                       [self readRenderedHtml];
                   } else if (++_readyPolls < MAX_READY_POLLS) {
                       dispatch_after(dispatch_time(DISPATCH_TIME_NOW,
                                          (int64_t)(READY_POLL_DELAY * NSEC_PER_SEC)),
                           dispatch_get_main_queue(), ^{ [self pollUntilReady]; });
                   } else {
                       [self finishWithResult:nil];
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
    [self finishWithResult:nil];
}
//=============================================================================
- (NSString*)result
{
    return _result;
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
    [_result release];
    [super dealloc];
}
//=============================================================================
@end
//=============================================================================
bool
NelsonRenderHtmlWithWebKit(const char* html, char* output, size_t outputSize)
{
    if (html == NULL || output == NULL || outputSize == 0) {
        return false;
    }
    output[0] = '\0';
    dispatch_semaphore_t semaphore = dispatch_semaphore_create(0);
    __block NelsonHtmlRenderer* renderer = nil;
    void (^startRenderer)(void) = ^{
        NSString* source = [NSString stringWithUTF8String:html];
        renderer = [[NelsonHtmlRenderer alloc] initWithHtml:source semaphore:semaphore];
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
    NSString* result = [[renderer result] retain];
    const char* utf8 = [result UTF8String];
    const size_t length = utf8 == NULL ? 0 : strlen(utf8);
    const bool success = length > 0 && length < outputSize;
    if (success) {
        memcpy(output, utf8, length + 1);
    }
    void (^cleanupRenderer)(void) = ^{
        [renderer cleanup];
        [renderer release];
    };
    if ([NSThread isMainThread]) {
        cleanupRenderer();
    } else {
        dispatch_sync(dispatch_get_main_queue(), cleanupRenderer);
    }
    [result release];
#if !OS_OBJECT_USE_OBJC
    dispatch_release(semaphore);
#endif
    return success;
}
//=============================================================================
