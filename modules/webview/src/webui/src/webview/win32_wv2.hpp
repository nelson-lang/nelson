/*
  WebUI Library
  Win32 WebView2 C++ Implementation
  https://webui.me
  https://github.com/webui-dev/webui
  Copyright (c) 2020-2026 Hassan Draga.
  Licensed under MIT License.
  All rights reserved.
  Canada.
*/

#ifndef WEBUI_WIN32_WV2_HPP
#define WEBUI_WIN32_WV2_HPP

#ifdef _WIN32

#include <windows.h>
#include <wchar.h>
#include <stdbool.h>

#define WEBUI_WM_APPLY_GEOMETRY_AND_MAXIMIZE (WM_APP + 1)

#ifdef __cplusplus
extern "C"
{
#endif

    typedef struct _webui_window_t _webui_window_t;
    typedef void* _webui_win32_wv2_handle;

    bool
    _webui_win32_wv2_check_loader_dll(void);
    bool
    _webui_win32_wv2_check_runtime(void);
    _webui_win32_wv2_handle
    _webui_win32_wv2_create(void);
    void
    _webui_win32_wv2_free(_webui_win32_wv2_handle handle);
    bool
    _webui_win32_wv2_navigate(_webui_win32_wv2_handle handle, wchar_t* url);
    bool
    _webui_win32_wv2_set_position(_webui_win32_wv2_handle handle, int x, int y);
    bool
    _webui_win32_wv2_set_size(_webui_win32_wv2_handle handle, int width, int height);
    bool
    _webui_win32_wv2_maximize(_webui_win32_wv2_handle handle);
    bool
    _webui_win32_wv2_minimize(_webui_win32_wv2_handle handle);
    // NELSON PATCH: hide/show an already-shown window (post-show), for figure
    // Visible on/off. See webui-vendored-patches.
    bool
    _webui_win32_wv2_set_hidden(_webui_win32_wv2_handle handle, bool hidden);
    HWND
    _webui_win32_wv2_get_hwnd(_webui_win32_wv2_handle handle);
    void
    _webui_win32_wv2_set_hwnd(_webui_win32_wv2_handle handle, HWND hwnd);
    void
    _webui_win32_wv2_set_stop(_webui_win32_wv2_handle handle, bool stop);
    bool
    _webui_win32_wv2_get_stop(_webui_win32_wv2_handle handle);
    void
    _webui_win32_wv2_set_navigate_flag(_webui_win32_wv2_handle handle, bool navigate);
    void
    _webui_win32_wv2_set_size_flag(_webui_win32_wv2_handle handle, bool size);
    void
    _webui_win32_wv2_set_position_flag(_webui_win32_wv2_handle handle, bool position);
    bool
    _webui_win32_wv2_get_navigate_flag(_webui_win32_wv2_handle handle);
    bool
    _webui_win32_wv2_get_size_flag(_webui_win32_wv2_handle handle);
    bool
    _webui_win32_wv2_get_position_flag(_webui_win32_wv2_handle handle);
    void
    _webui_win32_wv2_set_url(_webui_win32_wv2_handle handle, wchar_t* url);
    wchar_t*
    _webui_win32_wv2_get_url(_webui_win32_wv2_handle handle);
    void
    _webui_win32_wv2_set_dimensions(
        _webui_win32_wv2_handle handle, int x, int y, int width, int height);
    void
    _webui_win32_wv2_get_dimensions(
        _webui_win32_wv2_handle handle, int* x, int* y, int* width, int* height);
    bool
    _webui_win32_wv2_create_environment(_webui_win32_wv2_handle handle, wchar_t* cacheFolder);
    // NELSON PATCH: one WebView2 environment shared by every native window. All
    // figure windows then run in a single browser process on a single user-data
    // folder instead of spawning a fresh msedgewebview2.exe family (and paying a
    // cold-profile first-run) per window. The environment, its controllers and
    // every HWND must live on one STA thread (the webview host thread), so these
    // are only ever called from that thread. See webui-vendored-patches.
    // Kick off shared-environment creation once (idempotent). Returns false only
    // when creation could not be started (loader missing) or previously failed.
    bool
    _webui_win32_wv2_ensure_shared_environment(wchar_t* cacheFolder);
    // 0 = none, 1 = pending, 2 = ready, 3 = failed.
    int
    _webui_win32_wv2_shared_environment_state(void);
    // Create a controller for this window's HWND from the shared environment
    // (async; completion sets is_ready / has_failed). Requires state == ready.
    bool
    _webui_win32_wv2_create_controller(_webui_win32_wv2_handle handle);
    // Did the last shared-environment controller attempt hit a transient
    // 0x8007139F (ERROR_INVALID_STATE) that the host thread should retry?
    bool
    _webui_win32_wv2_needs_controller_retry(_webui_win32_wv2_handle handle);
    // Drop the shared environment (host thread teardown).
    void
    _webui_win32_wv2_release_shared_environment(void);
    void*
    _webui_win32_wv2_get_controller(_webui_win32_wv2_handle handle);
    bool
    _webui_win32_wv2_is_ready(_webui_win32_wv2_handle handle);
    bool
    _webui_win32_wv2_has_failed(_webui_win32_wv2_handle handle);
    HRESULT
    _webui_win32_wv2_get_last_error(_webui_win32_wv2_handle handle);

#ifdef __cplusplus
}
#endif

#endif // _WIN32
#endif // WEBUI_WIN32_WV2_HPP
