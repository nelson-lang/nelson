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
#include "nlsGui_exports.h"
//=============================================================================
// Active-screen geometry queries (Qt), shared by the drawing model and the
// windowing layer (the model must not depend on the windowing
// module for these).
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGUI_IMPEXP bool
currentScreenResolution(int& width, int& height);
//=============================================================================
NLSGUI_IMPEXP int
getCurrentScreenHeight();
//=============================================================================
NLSGUI_IMPEXP int
getCurrentScreenWidth();
//=============================================================================
// Primary-screen properties (dpi / bit depth).
NLSGUI_IMPEXP double
primaryScreenDpi();
NLSGUI_IMPEXP int
primaryScreenDepth();
//=============================================================================
// Device pixel ratio of the primary screen (1.0 without a screen). Kept
// separate from the logical dpi so frame capture can render at device
// resolution without perturbing the point-to-pixel layout, which stays on the
// 96-dpi logical grid.
NLSGUI_IMPEXP double
primaryScreenDevicePixelRatio();
//=============================================================================
// Primary display as the operating system reports it, whatever the DPI
// awareness of the calling process: device name, rectangle in physical pixels
// and effective dpi (96 = 100 % scale). False where no native query exists.
NLSGUI_IMPEXP bool
nativePrimaryDisplay(
    std::wstring& deviceName, int& x, int& y, int& width, int& height, double& dpi);
//=============================================================================
// Called before the QApplication exists. While a remote session is minimized
// or disconnected, Windows exposes a single placeholder display ("WinDisc")
// whose dpi Qt ignores (it assumes 96), so a process started then would get a
// device pixel ratio of 1 and a physical-pixel ScreenSize. This hands Qt the
// display's real scale for that placeholder only (real monitors keep their own
// dpi). Returns true when the environment was changed; pass it to
// restoreDisconnectedDisplayScale() once the QApplication is built so child
// processes compute their own value.
NLSGUI_IMPEXP bool
applyDisconnectedDisplayScale();
NLSGUI_IMPEXP void
restoreDisconnectedDisplayScale(bool applied);
//=============================================================================
// Mouse pointer position in screen pixels (top-left origin).
NLSGUI_IMPEXP void
pointerScreenPosition(int& x, int& y);
NLSGUI_IMPEXP void
setPointerScreenPosition(int x, int y);
//=============================================================================
// Preferred substitute for the default sans family when the exact face is
// unavailable (uses the platform font database).
NLSGUI_IMPEXP std::wstring
defaultSansSubstituteFontFamily();
//=============================================================================
// Pump pending GUI events (deferred deletes included) for at most maxTimeMs;
// safe no-op without a QApplication.
NLSGUI_IMPEXP void
flushGuiEvents(int maxTimeMs);
//=============================================================================
// Remember / restore the widget that had keyboard focus (used around figure
// show so opening a figure does not steal focus from the console).
NLSGUI_IMPEXP void
saveGuiFocus();
NLSGUI_IMPEXP void
restoreGuiFocus();
//=============================================================================
// False on platforms that never report real window positions (Wayland).
NLSGUI_IMPEXP bool
platformReportsWindowPosition();
//=============================================================================
// User preference: open new figures docked into the desktop (QSettings-backed;
// declared here so the Qt-free drawing model can query it).
NLSGUI_IMPEXP bool
DesktopPrefersDockedFigures();
//=============================================================================
} // namespace Nelson
//=============================================================================
