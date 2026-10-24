//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtGui/QWindow>
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGUI_IMPEXP void
forceWindowsTitleBarToDark(WId winId);
// Ensure the window has the WS_EX_APPWINDOW extended style so it appears
// in the taskbar as a separate top-level window on Windows.
NLSGUI_IMPEXP void
forceWindowsUseAppWindow(WId winId);
//=============================================================================
// Re-applies the title bar theme of every already created top level window.
// Needed when the theme changes at runtime: forceWindowsTitleBarToDark() is only
// called when a window is built, so existing windows would keep the former one.
NLSGUI_IMPEXP void
refreshWindowsTitleBarsTheme();
//=============================================================================
} // namespace Nelson
//=============================================================================
