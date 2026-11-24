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
#include <string>
#include "nlsGraphics_exports.h"
//=============================================================================
// Environment services the drawing model needs from the desktop (screen
// geometry, mouse pointer, event pumping, focus, user preferences). A native
// windowing backend registers implementations at load time; without a
// registration (nelson-webview, headless) the defaults below apply. This keeps
// libnlsGraphics free of any windowing-toolkit import.
//=============================================================================
namespace Nelson {
//=============================================================================
struct GraphicsEnvironmentHooks
{
    double (*primaryScreenDpi)() = nullptr;
    double (*frameCaptureDeviceRatio)() = nullptr;
    int (*primaryScreenDepth)() = nullptr;
    bool (*currentScreenResolution)(int& width, int& height) = nullptr;
    void (*pointerScreenPosition)(int& x, int& y) = nullptr;
    void (*setPointerScreenPosition)(int x, int y) = nullptr;
    std::wstring (*defaultSansSubstituteFontFamily)() = nullptr;
    void (*flushGuiEvents)(int maxTimeMs) = nullptr;
    void (*processGuiEvents)(bool waitEvents) = nullptr;
    void (*saveGuiFocus)() = nullptr;
    void (*restoreGuiFocus)() = nullptr;
    bool (*platformReportsWindowPosition)() = nullptr;
    bool (*desktopPrefersDockedFigures)() = nullptr;
};
//=============================================================================
NLSGRAPHICS_IMPEXP void
setGraphicsEnvironmentHooks(const GraphicsEnvironmentHooks& hooks);
NLSGRAPHICS_IMPEXP void
clearGraphicsEnvironmentHooks();
//=============================================================================
namespace GraphicsEnvironment {
    //=============================================================================
    NLSGRAPHICS_IMPEXP double
    primaryScreenDpi(); // 96.0 headless
    NLSGRAPHICS_IMPEXP double
    frameCaptureDeviceRatio(); // 1.0 headless; screen devicePixelRatio when a
                               // windowing backend is registered (getframe only)
    NLSGRAPHICS_IMPEXP int
    primaryScreenDepth(); // 32 headless
    NLSGRAPHICS_IMPEXP bool
    currentScreenResolution(int& width, int& height); // false / -1 headless
    NLSGRAPHICS_IMPEXP int
    currentScreenWidth();
    NLSGRAPHICS_IMPEXP int
    currentScreenHeight();
    NLSGRAPHICS_IMPEXP void
    pointerScreenPosition(int& x, int& y); // 0, 0 headless
    NLSGRAPHICS_IMPEXP void
    setPointerScreenPosition(int x, int y); // no-op headless
    NLSGRAPHICS_IMPEXP std::wstring
    defaultSansSubstituteFontFamily(); // L"Helvetica" headless
    NLSGRAPHICS_IMPEXP void
    flushGuiEvents(int maxTimeMs); // no-op headless
    NLSGRAPHICS_IMPEXP void
    processGuiEvents(bool waitEvents = false); // no-op headless
    NLSGRAPHICS_IMPEXP void
    saveGuiFocus(); // no-op headless
    NLSGRAPHICS_IMPEXP void
    restoreGuiFocus(); // no-op headless
    NLSGRAPHICS_IMPEXP bool
    platformReportsWindowPosition(); // true headless
    NLSGRAPHICS_IMPEXP bool
    desktopPrefersDockedFigures(); // false headless
    //=============================================================================
    // Primary display as the operating system reports it, whatever the DPI
    // awareness of the calling process: device name, rectangle in physical
    // pixels and effective dpi (96 = 100 % scale). Queries the OS directly (no
    // windowing backend needed), so it answers the same headless. False where no
    // native query exists (web / wasm).
    NLSGRAPHICS_IMPEXP bool
    nativePrimaryDisplay(
        std::wstring& deviceName, int& x, int& y, int& width, int& height, double& dpi);
    //=============================================================================
} // namespace GraphicsEnvironment
//=============================================================================
} // namespace Nelson
//=============================================================================
