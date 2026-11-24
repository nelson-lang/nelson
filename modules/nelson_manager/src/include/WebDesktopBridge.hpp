//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <string>
#include <cstdint>
#include <mutex>
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
class Evaluator;
//=============================================================================
// Figure WindowStyle carried across the web-figure lifecycle hook. The web
// desktop honors it like the Qt desktop: 'Normal'/'AlwaysOnTop'/'Modal' open a
// separate native window, 'Docked' opens a panel embedded in the desktop.
enum class FigureWindowStyle : int
{
    Normal = 0,
    Modal = 1,
    Docked = 2,
    AlwaysOnTop = 3
};
//=============================================================================
struct WebFigureFrameAttachment
{
    const char* id { nullptr };
    const unsigned char* data { nullptr };
    int length { 0 };
};
//=============================================================================
// Bridge between the engine/graphics low-level modules and the desktop
// (gui / webview). It is a registration point kept in nelson_manager so
// bottom-level modules (core, graphics, gui, text_editor) can push events to
// the browser desktop WITHOUT linking the high-level web modules. gui
// registers the hooks at startup; the other modules only call the push*/open*
// helpers, which are no-ops until a handler is registered.
class NLSNELSON_MANAGER_IMPEXP WebDesktopBridge
{
    //=============================================================================
public:
    //=============================================================================
    static WebDesktopBridge*
    getInstance();
    //=============================================================================
    void
    destroy();
    //=============================================================================
    // Desktop: the `terminal` builtin opens a browser shell panel, not a Qt window.
    void
    setWebTerminalOpener(void (*fn)(void*), void* ctx);
    bool
    openWebTerminal();
    //=============================================================================
    // Desktop: the `editor` builtin opens the Monaco panel, not the Qt text editor.
    // An empty path means "new file".
    void
    setWebEditorOpener(void (*fn)(void*, const wchar_t*, int, int), void* ctx);
    bool
    openWebEditor(const std::wstring& path, int line = 0, int column = 0);
    //=============================================================================
    // Desktop: the `edit` builtin opens a Notebook document (.nlx) in the Live
    // Script panel, not the Monaco text editor.
    void
    setWebNotebookOpener(void (*fn)(void*, const wchar_t*), void* ctx);
    bool
    openWebNotebook(const std::wstring& path);
    //=============================================================================
    // Native hosts install a window opener; the WebAssembly host serializes the
    // request as a UI action for the browser frontend.
    void
    setWebNFlowOpener(void (*fn)(void*, Evaluator*, const wchar_t*), void* ctx);
    bool
    openWebNFlow(Evaluator* eval, const std::wstring& modelPath = {});
    //=============================================================================
    // Returns true when a web handler was registered and called; graphics falls
    // back to the Qt figure otherwise.
    void
    setWebMessageBox(void (*fn)(void*, const wchar_t*, const wchar_t*, const wchar_t*), void* ctx);
    bool
    showWebMessageBox(
        const std::wstring& title, const std::wstring& message, const std::wstring& icon);
    //=============================================================================
    // drawnow pushes the figure frame (display-list JSON) to the browser so
    // animations play live.
    void
    setWebFigureFrameHook(void (*fn)(void*, long long, const char*, int), void* ctx);
    void
    setWebFigureFrameAttachmentHook(
        void (*fn)(void*, long long, const char*, int, const WebFigureFrameAttachment*, int),
        void* ctx);
    bool
    hasWebFigureFrameHook() const;
    bool
    hasWebFigureFrameAttachmentHook() const;
    void
    pushWebFigureFrame(long long handle, const char* json, int len);
    void
    pushWebFigureFrame(long long handle, const char* json, int len,
        const WebFigureFrameAttachment* attachments, int attachmentCount);
    //=============================================================================
    // nelson-adv-cli web figures: the native figure windows of the GuiServer must
    // be serviced on the interpreter thread and there is no event loop, so the
    // engine calls pumpWebEvents() at statement boundaries, drawnow and pause.
    // No-op until the server registers the hook.
    void
    setWebEventPumpHook(void (*fn)(void*), void* ctx);
    bool
    hasWebEventPumpHook() const;
    void
    pumpWebEvents();
    //=============================================================================
    // Keeps browser tabs synchronized while a command is running.
    void
    setWebFigureLifecycleHook(void (*fn)(void*, long long, bool, FigureWindowStyle), void* ctx);
    void
    pushWebFigureLifecycle(long long handle, bool created, FigureWindowStyle windowStyle);
    //=============================================================================
    // The desktop moves the figure between a native window and a docked panel.
    void
    setWebFigureWindowStyleHook(void (*fn)(void*, long long, FigureWindowStyle), void* ctx);
    void
    pushWebFigureWindowStyle(long long handle, FigureWindowStyle windowStyle);
    //=============================================================================
    // The desktop hides or shows the separate native window.
    void
    setWebFigureVisibleHook(void (*fn)(void*, long long, bool), void* ctx);
    void
    pushWebFigureVisible(long long handle, bool visible);
    //=============================================================================
    // Name / NumberTitle changed: the desktop renames the panel and the window.
    void
    setWebFigureTitleHook(void (*fn)(void*, long long, const char*), void* ctx);
    void
    pushWebFigureTitle(long long handle, const char* title);
    //=============================================================================
    // Position changed programmatically (f.Position = ...): the desktop slides the
    // separate native window so its drawing area follows [x y] (pixels, bottom-up).
    void
    setWebFigurePositionHook(void (*fn)(void*, long long, int, int, int, int), void* ctx);
    void
    pushWebFigurePosition(long long handle, int x, int y, int width, int height);
    //=============================================================================
    void
    setWebClipboardImageHook(void (*fn)(void*, const char*, int), void* ctx);
    bool
    pushWebClipboardImage(const std::string& pngBase64);
    //=============================================================================
    // See PackageUpdatesCount. The desktop refreshes its Package Manager badge.
    void
    setWebPackageUpdatesHook(void (*fn)(void*, int), void* ctx);
    void
    pushWebPackageUpdates(int count);
    //=============================================================================
    // The desktop minimizes/restores its own main window and reports its state so
    // ipc(pid, 'minimize', ...) controls the desktop like the Qt main window.
    // requestWebMinimize returns false when no handler is registered.
    void
    setWebMinimizeHook(void (*fn)(void*, bool), void* ctx);
    void
    setWebIsMinimizedHook(bool (*fn)(void*), void* ctx);
    bool
    hasWebMinimizeHook() const;
    bool
    requestWebMinimize(bool minimize);
    bool
    queryWebMinimized();
    //=============================================================================
    // The desktop reports, on demand, where a figure's drawing area really is
    // on screen (the native window the engine host no longer owns): [left bottom
    // width height] in logical pixels, bottom-up from the primary screen. Lets
    // __figure_native_geometry__ read the real window in web mode. The handler
    // fills the outputs and returns true, or returns false when the figure has no
    // measured native window (headless, hidden, not yet reported).
    void
    setWebFigureNativeGeometryHook(
        bool (*fn)(void*, long long, double*, double*, double*, double*), void* ctx);
    bool
    queryWebFigureNativeGeometry(
        long long handle, double* x, double* y, double* width, double* height);
    //=============================================================================
    void
    setWebLaunchMode(const std::string& mode);
    std::string
    getWebLaunchMode();
    void
    setWebLaunchUrlAndPort(const std::string& url, uint64_t port);
    std::string
    getWebLaunchUrl();
    uint64_t
    getWebLaunchPort();
    void
    setWebViewUnavailableAtLaunch(bool unavailable);
    bool
    wasWebViewUnavailableAtLaunch();
    //=============================================================================
private:
    WebDesktopBridge() = default;
    //=============================================================================
    static WebDesktopBridge* m_pInstance;
    //=============================================================================
    void (*m_webTerminalFn)(void*) = nullptr;
    void* m_webTerminalCtx = nullptr;
    void (*m_webEditorFn)(void*, const wchar_t*, int, int) = nullptr;
    void* m_webEditorCtx = nullptr;
    void (*m_webNotebookFn)(void*, const wchar_t*) = nullptr;
    void* m_webNotebookCtx = nullptr;
    void (*m_webNFlowFn)(void*, Evaluator*, const wchar_t*) = nullptr;
    void* m_webNFlowCtx = nullptr;
    void (*m_webFigureFrameFn)(void*, long long, const char*, int) = nullptr;
    void* m_webFigureFrameCtx = nullptr;
    void (*m_webFigureFrameAttachmentFn)(
        void*, long long, const char*, int, const WebFigureFrameAttachment*, int)
        = nullptr;
    void* m_webFigureFrameAttachmentCtx = nullptr;
    void (*m_webEventPumpFn)(void*) = nullptr;
    void* m_webEventPumpCtx = nullptr;
    void (*m_webFigureLifecycleFn)(void*, long long, bool, FigureWindowStyle) = nullptr;
    void* m_webFigureLifecycleCtx = nullptr;
    void (*m_webFigureWindowStyleFn)(void*, long long, FigureWindowStyle) = nullptr;
    void* m_webFigureWindowStyleCtx = nullptr;
    void (*m_webFigureVisibleFn)(void*, long long, bool) = nullptr;
    void* m_webFigureVisibleCtx = nullptr;
    void (*m_webFigureTitleFn)(void*, long long, const char*) = nullptr;
    void* m_webFigureTitleCtx = nullptr;
    void (*m_webFigurePositionFn)(void*, long long, int, int, int, int) = nullptr;
    void* m_webFigurePositionCtx = nullptr;
    void (*m_webMessageBoxFn)(void*, const wchar_t*, const wchar_t*, const wchar_t*) = nullptr;
    void* m_webMessageBoxCtx = nullptr;
    void (*m_webClipboardImageFn)(void*, const char*, int) = nullptr;
    void* m_webClipboardImageCtx = nullptr;
    void (*m_webPackageUpdatesFn)(void*, int) = nullptr;
    void* m_webPackageUpdatesCtx = nullptr;
    void (*m_webMinimizeFn)(void*, bool) = nullptr;
    void* m_webMinimizeCtx = nullptr;
    bool (*m_webIsMinimizedFn)(void*) = nullptr;
    void* m_webIsMinimizedCtx = nullptr;
    bool (*m_webFigureNativeGeometryFn)(void*, long long, double*, double*, double*, double*)
        = nullptr;
    void* m_webFigureNativeGeometryCtx = nullptr;
    std::mutex m_webLaunchModeMutex;
    std::string m_webLaunchMode { "none" };
    std::string m_webLaunchUrl;
    uint64_t m_webLaunchPort { 0 };
    bool m_webViewUnavailableAtLaunch { false };
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
