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
        return environmentHooks.frameCaptureDeviceRatio ? environmentHooks.frameCaptureDeviceRatio()
                                                        : 1.0;
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
} // namespace GraphicsEnvironment
//=============================================================================
} // namespace Nelson
//=============================================================================
