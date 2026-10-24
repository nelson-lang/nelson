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
#include <QtCore/QList>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QMainWindow>
#include "nlsGui_exports.h"
//=============================================================================
// Dock a tool window (figure, text editor, ...) into the Nelson desktop.
// The client window is hosted inside a QDockWidget while docked; undocking
// reparents it back to a real top-level window with native decorations, so a
// window that is never docked (or undocked again) keeps its normal look.
//=============================================================================
namespace Nelson {
//=============================================================================
// true when the Nelson desktop main window exists (GUI mode).
NLSGUI_IMPEXP bool
DesktopDockingAvailable();
//=============================================================================
// hosts the client window in a dock of the desktop; tabbed with the other
// docked client windows. The client keeps its own menu bar and toolbars.
NLSGUI_IMPEXP bool
DesktopDockWindow(QMainWindow* client, const QString& dockObjectName);
//=============================================================================
// reparents the client back to a top-level native window, restoring the
// geometry it had before docking; the hosting dock is destroyed.
NLSGUI_IMPEXP bool
DesktopUndockWindow(QMainWindow* client, bool showAsWindow = true);
//=============================================================================
NLSGUI_IMPEXP bool
DesktopIsWindowDocked(const QWidget* client);
//=============================================================================
// shows and raises the dock tab hosting the client (docked windows only).
NLSGUI_IMPEXP bool
DesktopShowDockedWindow(QMainWindow* client);
//=============================================================================
// hides the dock hosting the client (docked windows only).
NLSGUI_IMPEXP bool
DesktopHideDockedWindow(QMainWindow* client);
//=============================================================================
// docks currently hosting a client window (docked figures, editor, ...);
// used by the desktop Windows menu to list them.
NLSGUI_IMPEXP QList<QDockWidget*>
DesktopDockedClientWindows();
//=============================================================================
// number of docked client windows that are actually open (not floating, not
// hidden), i.e. the ones the tile / tab operations would act on. The Windows
// menu uses it to enable those entries only when they would do something.
NLSGUI_IMPEXP int
DesktopOpenClientWindowCount();
//=============================================================================
// arranges the open docked client windows side by side in a grid, for visual
// comparison. Returns false with fewer than two open docked client windows.
NLSGUI_IMPEXP bool
DesktopTileClientWindows();
//=============================================================================
// groups the open docked client windows back into a single tab stack.
NLSGUI_IMPEXP bool
DesktopTabClientWindows();
//=============================================================================
} // namespace Nelson
//=============================================================================
