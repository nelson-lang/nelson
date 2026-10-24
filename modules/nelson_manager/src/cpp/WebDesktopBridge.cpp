//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <new>
#include "WebDesktopBridge.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
WebDesktopBridge* WebDesktopBridge::m_pInstance = nullptr;
//=============================================================================
WebDesktopBridge*
WebDesktopBridge::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new WebDesktopBridge();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
void
WebDesktopBridge::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
void
WebDesktopBridge::setWebTerminalOpener(void (*fn)(void*), void* ctx)
{
    this->m_webTerminalFn = fn;
    this->m_webTerminalCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::openWebTerminal()
{
    if (this->m_webTerminalFn == nullptr) {
        return false;
    }
    this->m_webTerminalFn(this->m_webTerminalCtx);
    return true;
}
//=============================================================================
void
WebDesktopBridge::setWebEditorOpener(void (*fn)(void*, const wchar_t*, int), void* ctx)
{
    this->m_webEditorFn = fn;
    this->m_webEditorCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::openWebEditor(const std::wstring& path, int line)
{
    if (this->m_webEditorFn == nullptr) {
        return false;
    }
    this->m_webEditorFn(this->m_webEditorCtx, path.c_str(), line);
    return true;
}
//=============================================================================
void
WebDesktopBridge::setWebNotebookOpener(void (*fn)(void*, const wchar_t*), void* ctx)
{
    this->m_webNotebookFn = fn;
    this->m_webNotebookCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::openWebNotebook(const std::wstring& path)
{
    if (this->m_webNotebookFn == nullptr) {
        return false;
    }
    this->m_webNotebookFn(this->m_webNotebookCtx, path.c_str());
    return true;
}
//=============================================================================
void
WebDesktopBridge::setWebNFlowOpener(void (*fn)(void*, Evaluator*, const wchar_t*), void* ctx)
{
    this->m_webNFlowFn = fn;
    this->m_webNFlowCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::openWebNFlow(Evaluator* eval, const std::wstring& modelPath)
{
    if (this->m_webNFlowFn == nullptr) {
        return false;
    }
    this->m_webNFlowFn(this->m_webNFlowCtx, eval, modelPath.c_str());
    return true;
}
//=============================================================================
void
WebDesktopBridge::setWebMessageBox(
    void (*fn)(void*, const wchar_t*, const wchar_t*, const wchar_t*), void* ctx)
{
    this->m_webMessageBoxFn = fn;
    this->m_webMessageBoxCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::showWebMessageBox(
    const std::wstring& title, const std::wstring& message, const std::wstring& icon)
{
    if (this->m_webMessageBoxFn == nullptr) {
        return false;
    }
    this->m_webMessageBoxFn(this->m_webMessageBoxCtx, title.c_str(), message.c_str(), icon.c_str());
    return true;
}
//=============================================================================
void
WebDesktopBridge::setWebFigureFrameHook(void (*fn)(void*, long long, const char*, int), void* ctx)
{
    this->m_webFigureFrameFn = fn;
    this->m_webFigureFrameCtx = ctx;
}
//=============================================================================
void
WebDesktopBridge::setWebFigureFrameAttachmentHook(
    void (*fn)(void*, long long, const char*, int, const WebFigureFrameAttachment*, int), void* ctx)
{
    this->m_webFigureFrameAttachmentFn = fn;
    this->m_webFigureFrameAttachmentCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::hasWebFigureFrameHook() const
{
    return this->m_webFigureFrameFn != nullptr || this->m_webFigureFrameAttachmentFn != nullptr;
}
//=============================================================================
bool
WebDesktopBridge::hasWebFigureFrameAttachmentHook() const
{
    return this->m_webFigureFrameAttachmentFn != nullptr;
}
//=============================================================================
void
WebDesktopBridge::pushWebFigureFrame(long long handle, const char* json, int len)
{
    if (this->m_webFigureFrameFn != nullptr) {
        this->m_webFigureFrameFn(this->m_webFigureFrameCtx, handle, json, len);
    }
}
//=============================================================================
void
WebDesktopBridge::pushWebFigureFrame(long long handle, const char* json, int len,
    const WebFigureFrameAttachment* attachments, int attachmentCount)
{
    if (this->m_webFigureFrameAttachmentFn != nullptr) {
        this->m_webFigureFrameAttachmentFn(
            this->m_webFigureFrameAttachmentCtx, handle, json, len, attachments, attachmentCount);
        return;
    }
    pushWebFigureFrame(handle, json, len);
}
//=============================================================================
void
WebDesktopBridge::setWebEventPumpHook(void (*fn)(void*), void* ctx)
{
    this->m_webEventPumpFn = fn;
    this->m_webEventPumpCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::hasWebEventPumpHook() const
{
    return this->m_webEventPumpFn != nullptr;
}
//=============================================================================
void
WebDesktopBridge::pumpWebEvents()
{
    if (this->m_webEventPumpFn != nullptr) {
        this->m_webEventPumpFn(this->m_webEventPumpCtx);
    }
}
//=============================================================================
void
WebDesktopBridge::setWebFigureLifecycleHook(
    void (*fn)(void*, long long, bool, FigureWindowStyle), void* ctx)
{
    this->m_webFigureLifecycleFn = fn;
    this->m_webFigureLifecycleCtx = ctx;
}
//=============================================================================
void
WebDesktopBridge::pushWebFigureLifecycle(
    long long handle, bool created, FigureWindowStyle windowStyle)
{
    if (this->m_webFigureLifecycleFn != nullptr) {
        this->m_webFigureLifecycleFn(this->m_webFigureLifecycleCtx, handle, created, windowStyle);
    }
}
//=============================================================================
void
WebDesktopBridge::setWebFigureWindowStyleHook(
    void (*fn)(void*, long long, FigureWindowStyle), void* ctx)
{
    this->m_webFigureWindowStyleFn = fn;
    this->m_webFigureWindowStyleCtx = ctx;
}
//=============================================================================
void
WebDesktopBridge::pushWebFigureWindowStyle(long long handle, FigureWindowStyle windowStyle)
{
    if (this->m_webFigureWindowStyleFn != nullptr) {
        this->m_webFigureWindowStyleFn(this->m_webFigureWindowStyleCtx, handle, windowStyle);
    }
}
//=============================================================================
void
WebDesktopBridge::setWebFigureVisibleHook(void (*fn)(void*, long long, bool), void* ctx)
{
    this->m_webFigureVisibleFn = fn;
    this->m_webFigureVisibleCtx = ctx;
}
//=============================================================================
void
WebDesktopBridge::pushWebFigureVisible(long long handle, bool visible)
{
    if (this->m_webFigureVisibleFn != nullptr) {
        this->m_webFigureVisibleFn(this->m_webFigureVisibleCtx, handle, visible);
    }
}
//=============================================================================
void
WebDesktopBridge::setWebFigureTitleHook(void (*fn)(void*, long long, const char*), void* ctx)
{
    this->m_webFigureTitleFn = fn;
    this->m_webFigureTitleCtx = ctx;
}
//=============================================================================
void
WebDesktopBridge::pushWebFigureTitle(long long handle, const char* title)
{
    if (this->m_webFigureTitleFn != nullptr) {
        this->m_webFigureTitleFn(this->m_webFigureTitleCtx, handle, title);
    }
}
//=============================================================================
void
WebDesktopBridge::setWebClipboardImageHook(void (*fn)(void*, const char*, int), void* ctx)
{
    this->m_webClipboardImageFn = fn;
    this->m_webClipboardImageCtx = ctx;
}
//=============================================================================
bool
WebDesktopBridge::pushWebClipboardImage(const std::string& pngBase64)
{
    if (this->m_webClipboardImageFn == nullptr) {
        return false;
    }
    this->m_webClipboardImageFn(
        this->m_webClipboardImageCtx, pngBase64.data(), static_cast<int>(pngBase64.size()));
    return true;
}
//=============================================================================
void
WebDesktopBridge::setWebPackageUpdatesHook(void (*fn)(void*, int), void* ctx)
{
    this->m_webPackageUpdatesFn = fn;
    this->m_webPackageUpdatesCtx = ctx;
}
//=============================================================================
void
WebDesktopBridge::pushWebPackageUpdates(int count)
{
    if (this->m_webPackageUpdatesFn != nullptr) {
        this->m_webPackageUpdatesFn(this->m_webPackageUpdatesCtx, count);
    }
}
//=============================================================================
void
WebDesktopBridge::setWebLaunchMode(const std::string& mode)
{
    std::lock_guard<std::mutex> lock(m_webLaunchModeMutex);
    m_webLaunchMode = mode;
}
//=============================================================================
std::string
WebDesktopBridge::getWebLaunchMode()
{
    std::lock_guard<std::mutex> lock(m_webLaunchModeMutex);
    return m_webLaunchMode;
}
//=============================================================================
void
WebDesktopBridge::setWebLaunchUrlAndPort(const std::string& url, uint64_t port)
{
    std::lock_guard<std::mutex> lock(m_webLaunchModeMutex);
    m_webLaunchUrl = url;
    m_webLaunchPort = port;
}
//=============================================================================
std::string
WebDesktopBridge::getWebLaunchUrl()
{
    std::lock_guard<std::mutex> lock(m_webLaunchModeMutex);
    return m_webLaunchUrl;
}
//=============================================================================
uint64_t
WebDesktopBridge::getWebLaunchPort()
{
    std::lock_guard<std::mutex> lock(m_webLaunchModeMutex);
    return m_webLaunchPort;
}
//=============================================================================
void
WebDesktopBridge::setWebViewUnavailableAtLaunch(bool unavailable)
{
    std::lock_guard<std::mutex> lock(m_webLaunchModeMutex);
    m_webViewUnavailableAtLaunch = unavailable;
}
//=============================================================================
bool
WebDesktopBridge::wasWebViewUnavailableAtLaunch()
{
    std::lock_guard<std::mutex> lock(m_webLaunchModeMutex);
    return m_webViewUnavailableAtLaunch;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
