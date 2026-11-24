//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicsEnvironment.hpp"
#include <cmath>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#elif defined(__APPLE__)
#include <CoreGraphics/CoreGraphics.h>
#elif defined(__linux__) && !defined(__EMSCRIPTEN__)
#include <dlfcn.h>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static GraphicsEnvironmentHooks environmentHooks;
//=============================================================================
#if defined(__linux__) && !defined(__EMSCRIPTEN__)
// X11 is loaded at run time: the graphics module keeps no link dependency on it.
static bool
x11ScreenResolution(int& width, int& height)
{
    // the SONAME shipped by every distribution first, the development link name as a fallback
    void* x11 = dlopen("libX11.so.6", RTLD_LAZY);
    if (x11 == nullptr) {
        x11 = dlopen("libX11.so", RTLD_LAZY);
    }
    if (x11 == nullptr) {
        return false;
    }
    using OpenDisplayFunction = void* (*)(const char*);
    using CloseDisplayFunction = int (*)(void*);
    using DefaultScreenFunction = int (*)(void*);
    using DisplaySizeFunction = int (*)(void*, int);
    auto openDisplay = reinterpret_cast<OpenDisplayFunction>(dlsym(x11, "XOpenDisplay"));
    auto closeDisplay = reinterpret_cast<CloseDisplayFunction>(dlsym(x11, "XCloseDisplay"));
    auto defaultScreen = reinterpret_cast<DefaultScreenFunction>(dlsym(x11, "XDefaultScreen"));
    auto displayWidth = reinterpret_cast<DisplaySizeFunction>(dlsym(x11, "XDisplayWidth"));
    auto displayHeight = reinterpret_cast<DisplaySizeFunction>(dlsym(x11, "XDisplayHeight"));
    bool found = false;
    if (openDisplay && closeDisplay && defaultScreen && displayWidth && displayHeight) {
        void* display = openDisplay(nullptr);
        if (display != nullptr) {
            const int screen = defaultScreen(display);
            width = displayWidth(display, screen);
            height = displayHeight(display, screen);
            closeDisplay(display);
            found = width > 0 && height > 0;
        }
    }
    dlclose(x11);
    return found;
}
#endif
//=============================================================================
#ifdef _WIN32
static double
nativeMonitorDpi(HMONITOR monitor)
{
    // shcore is resolved at run time so the module keeps its link dependencies.
    using GetDpiForMonitorFunction = HRESULT(WINAPI*)(HMONITOR, int, UINT*, UINT*);
    static const auto getDpiForMonitor = reinterpret_cast<GetDpiForMonitorFunction>(
        GetProcAddress(LoadLibraryW(L"shcore.dll"), "GetDpiForMonitor"));
    constexpr int MDT_EFFECTIVE = 0;
    UINT dpiX = 0;
    UINT dpiY = 0;
    if (getDpiForMonitor != nullptr
        && SUCCEEDED(getDpiForMonitor(monitor, MDT_EFFECTIVE, &dpiX, &dpiY)) && dpiX > 0) {
        return static_cast<double>(dpiX);
    }
    return 96.0;
}
#endif
//=============================================================================
static bool
fallbackCurrentScreenResolution(int& width, int& height)
{
#ifdef _WIN32
    // Screen sizes are logical pixels (1/96 in). A DPI-unaware thread gets
    // them scaled already; a DPI-aware process would otherwise get physical
    // pixels, so the result would depend on who set the awareness first.
    DPI_AWARENESS_CONTEXT previous = SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_UNAWARE);
    width = GetSystemMetrics(SM_CXSCREEN);
    height = GetSystemMetrics(SM_CYSCREEN);
    if (previous != nullptr) {
        SetThreadDpiAwarenessContext(previous);
    }
    return width > 0 && height > 0;
#elif defined(__APPLE__)
    CGRect bounds = CGDisplayBounds(CGMainDisplayID());
    width = static_cast<int>(CGRectGetWidth(bounds));
    height = static_cast<int>(CGRectGetHeight(bounds));
    return width > 0 && height > 0;
#else
#if defined(__linux__) && !defined(__EMSCRIPTEN__)
    // The Qt adapter, which installs the screen hook, only loads with the first figure: until
    // then X11 itself answers, so the root and the figures agree on the screen size.
    if (x11ScreenResolution(width, height)) {
        return true;
    }
#endif
    // No windowing toolkit answer (headless or placeholder display): return a sane
    // default instead of -1 x -1, so screen-derived values (ScreenSize,
    // DefaultFigurePosition, figure placement) stay valid and consistent.
    width = 1920;
    height = 1080;
    return true;
#endif
}
//=============================================================================
void
setGraphicsEnvironmentHooks(const GraphicsEnvironmentHooks& hooks)
{
    environmentHooks = hooks;
}
//=============================================================================
void
clearGraphicsEnvironmentHooks()
{
    environmentHooks = GraphicsEnvironmentHooks {};
}
//=============================================================================
namespace GraphicsEnvironment {
    //=============================================================================
    double
    primaryScreenDpi()
    {
        return environmentHooks.primaryScreenDpi ? environmentHooks.primaryScreenDpi() : 96.0;
    }
    //=============================================================================
    double
    frameCaptureDeviceRatio()
    {
        if (environmentHooks.frameCaptureDeviceRatio) {
            return environmentHooks.frameCaptureDeviceRatio();
        }
        // No windowing backend registered: report the primary display's scale as the
        // operating system reports it at process start, so a figure's DevicePixelRatio
        // matches the real display the way a windowed backend would, at any scale
        // (1.0 at 100 %, a no-op there). Computed once: the native query is not free
        // and the start-time scale is the reference the point-to-pixel layout uses.
        static const double headlessRatio = [] {
            std::wstring deviceName;
            int x = 0;
            int y = 0;
            int width = 0;
            int height = 0;
            double dpi = 96.0;
            if (nativePrimaryDisplay(deviceName, x, y, width, height, dpi) && dpi > 0.0) {
                return dpi / 96.0;
            }
            return 1.0;
        }();
        return headlessRatio;
    }
    //=============================================================================
    int
    primaryScreenDepth()
    {
        return environmentHooks.primaryScreenDepth ? environmentHooks.primaryScreenDepth() : 32;
    }
    //=============================================================================
    bool
    currentScreenResolution(int& width, int& height)
    {
        if (environmentHooks.currentScreenResolution) {
            bool found = environmentHooks.currentScreenResolution(width, height);
            if (found && width > 0 && height > 0) {
                return true;
            }
        }
        return fallbackCurrentScreenResolution(width, height);
    }
    //=============================================================================
    int
    currentScreenWidth()
    {
        int w = 0;
        int h = 0;
        currentScreenResolution(w, h);
        return w;
    }
    //=============================================================================
    int
    currentScreenHeight()
    {
        int w = 0;
        int h = 0;
        currentScreenResolution(w, h);
        return h;
    }
    //=============================================================================
    void
    pointerScreenPosition(int& x, int& y)
    {
        if (environmentHooks.pointerScreenPosition) {
            environmentHooks.pointerScreenPosition(x, y);
            return;
        }
        x = 0;
        y = 0;
    }
    //=============================================================================
    void
    setPointerScreenPosition(int x, int y)
    {
        if (environmentHooks.setPointerScreenPosition) {
            environmentHooks.setPointerScreenPosition(x, y);
        }
    }
    //=============================================================================
    std::wstring
    defaultSansSubstituteFontFamily()
    {
        if (environmentHooks.defaultSansSubstituteFontFamily) {
            return environmentHooks.defaultSansSubstituteFontFamily();
        }
        return L"Helvetica";
    }
    //=============================================================================
    void
    flushGuiEvents(int maxTimeMs)
    {
        if (environmentHooks.flushGuiEvents) {
            environmentHooks.flushGuiEvents(maxTimeMs);
        }
    }
    //=============================================================================
    void
    processGuiEvents(bool waitEvents)
    {
        if (environmentHooks.processGuiEvents) {
            environmentHooks.processGuiEvents(waitEvents);
        }
    }
    //=============================================================================
    void
    saveGuiFocus()
    {
        if (environmentHooks.saveGuiFocus) {
            environmentHooks.saveGuiFocus();
        }
    }
    //=============================================================================
    void
    restoreGuiFocus()
    {
        if (environmentHooks.restoreGuiFocus) {
            environmentHooks.restoreGuiFocus();
        }
    }
    //=============================================================================
    bool
    platformReportsWindowPosition()
    {
        if (environmentHooks.platformReportsWindowPosition) {
            return environmentHooks.platformReportsWindowPosition();
        }
        return true;
    }
    //=============================================================================
    bool
    desktopPrefersDockedFigures()
    {
        if (environmentHooks.desktopPrefersDockedFigures) {
            return environmentHooks.desktopPrefersDockedFigures();
        }
        return false;
    }
    //=============================================================================
    bool
    nativePrimaryDisplay(
        std::wstring& deviceName, int& x, int& y, int& width, int& height, double& dpi)
    {
#ifdef _WIN32
        // Per-monitor awareness for this thread only: physical pixels and the
        // monitor's own dpi instead of values virtualized for the process.
        DPI_AWARENESS_CONTEXT previous
            = SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
        HMONITOR monitor = MonitorFromPoint(POINT { 0, 0 }, MONITOR_DEFAULTTOPRIMARY);
        MONITORINFOEXW info {};
        info.cbSize = sizeof(info);
        const bool found = monitor != nullptr && GetMonitorInfoW(monitor, &info) != FALSE;
        if (found) {
            deviceName = info.szDevice;
            x = info.rcMonitor.left;
            y = info.rcMonitor.top;
            width = info.rcMonitor.right - info.rcMonitor.left;
            height = info.rcMonitor.bottom - info.rcMonitor.top;
            dpi = nativeMonitorDpi(monitor);
        }
        if (previous != nullptr) {
            SetThreadDpiAwarenessContext(previous);
        }
        return found;
#elif defined(__linux__) && !defined(__EMSCRIPTEN__)
        // X11 itself answers before any display is opened; one logical pixel per
        // pixel, the same units reported without an Xft.dpi setting.
        if (x11ScreenResolution(width, height)) {
            deviceName.clear();
            x = 0;
            y = 0;
            dpi = 96.0;
            return true;
        }
        return false;
#else
        return false;
#endif
    }
    //=============================================================================
} // namespace GraphicsEnvironment
//=============================================================================
} // namespace Nelson
//=============================================================================
