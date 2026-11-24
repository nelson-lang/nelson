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
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <Windows.h>
#endif
#include <cmath>
#include <webui.h>
//=============================================================================
namespace Nelson {
//=============================================================================
// DPI of the display the windows open on. It is 96 when the process is not DPI
// aware (the system then scales its windows) and outside Windows, where the
// window sizes are already logical.
inline int
webUiDisplayDpi()
{
#ifdef _WIN32
    const UINT dpi = GetDpiForSystem();
    return dpi > 0 ? static_cast<int>(dpi) : 96;
#else
    return 96;
#endif
}
//=============================================================================
// A length in logical pixels, in the pixels the WebUI window calls take.
inline unsigned int
webUiScaledToDisplay(int logicalPixels)
{
    return static_cast<unsigned int>(
        std::lround(static_cast<double>(logicalPixels) * webUiDisplayDpi() / 96.0));
}
//=============================================================================
struct WebUiWindowGeometry
{
    int x = 0;
    int y = 0;
    int width = 0;
    int height = 0;
    bool maximized = false;
    bool valid = false;
};
//=============================================================================
inline bool
validateWebUiWindowGeometry(WebUiWindowGeometry& geometry, int minimumWidth = 800,
    int minimumHeight = 600, int maximumSize = 16384)
{
    geometry.valid = geometry.width >= minimumWidth && geometry.width <= maximumSize
        && geometry.height >= minimumHeight && geometry.height <= maximumSize;
    return geometry.valid;
}
//=============================================================================
// Keeps a window reachable: no larger than the work area of the nearest monitor
// and placed inside it. A saved geometry can come from a display that is gone
// or had another size. Windows only: elsewhere the window manager does it.
inline void
fitWebUiWindowGeometryToScreen(WebUiWindowGeometry& geometry)
{
#ifdef _WIN32
    RECT wanted
        = { geometry.x, geometry.y, geometry.x + geometry.width, geometry.y + geometry.height };
    MONITORINFO monitor;
    monitor.cbSize = sizeof(monitor);
    if (!GetMonitorInfoW(MonitorFromRect(&wanted, MONITOR_DEFAULTTONEAREST), &monitor)) {
        return;
    }
    const RECT& work = monitor.rcWork;
    // A window rect includes its invisible resize borders (left, right, bottom).
    const int border = GetSystemMetrics(SM_CXSIZEFRAME) + GetSystemMetrics(SM_CXPADDEDBORDER);
    const int maximumWidth = (work.right - work.left) + 2 * border;
    const int maximumHeight = (work.bottom - work.top) + border;
    if (geometry.width > maximumWidth) {
        geometry.width = maximumWidth;
    }
    if (geometry.height > maximumHeight) {
        geometry.height = maximumHeight;
    }
    const int maximumX = work.right + border - geometry.width;
    const int maximumY = work.bottom + border - geometry.height;
    if (geometry.x > maximumX) {
        geometry.x = maximumX;
    }
    if (geometry.x < work.left - border) {
        geometry.x = work.left - border;
    }
    if (geometry.y > maximumY) {
        geometry.y = maximumY;
    }
    if (geometry.y < work.top) {
        geometry.y = work.top;
    }
#else
    (void)geometry;
#endif
}
//=============================================================================
inline WebUiWindowGeometry
captureWebUiWindowGeometry(size_t window)
{
    WebUiWindowGeometry geometry;
    unsigned int width = 0;
    unsigned int height = 0;
    if (!webui_get_window_geometry(
            window, &geometry.x, &geometry.y, &width, &height, &geometry.maximized)) {
        return geometry;
    }
    geometry.width = static_cast<int>(width);
    geometry.height = static_cast<int>(height);
    validateWebUiWindowGeometry(geometry);
    return geometry;
}
//=============================================================================
inline bool
restoreWebUiWindowGeometry(size_t window, const WebUiWindowGeometry& saved)
{
    if (!saved.valid) {
        return false;
    }
    WebUiWindowGeometry geometry = saved;
    fitWebUiWindowGeometryToScreen(geometry);
    webui_set_size(window, static_cast<unsigned int>(geometry.width),
        static_cast<unsigned int>(geometry.height));
    webui_set_position(
        window, static_cast<unsigned int>(geometry.x), static_cast<unsigned int>(geometry.y));
    if (geometry.maximized) {
        webui_maximize(window);
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
