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
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
static GraphicsEnvironmentHooks environmentHooks;
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
