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
#include <QtCore/QString>
#include <QtCore/QtGlobal>
#include <QtGui/QColor>
#include <QtGui/QIcon>
#include <QtGui/QKeySequence>
#include <QtGui/QPalette>
//=============================================================================
class QToolBar;
class QMainWindow;
class QAction;
class QActionGroup;
//=============================================================================
struct DesktopUiColors
{
    QColor window;
    QColor panel;
    QColor title;
    QColor border;
    QColor tab;
    QColor hover;
    QColor sidebar;
    QColor visible;
    QColor accent;
    QColor text;
    QColor muted;
};
//=============================================================================
DesktopUiColors
desktopUiColors(const QPalette& palette);
QString
desktopShellStyleSheet(const QPalette& palette);
QIcon
desktopTintedIcon(const QString& filename, const QPalette& palette);
QList<qreal>
desktopIconDevicePixelRatios();
QString
desktopAccessibleActionText(const QAction* action);
QKeySequence
desktopCommandShortcut(int key, bool shift = false);
void
desktopClearExclusiveActionSelection(QActionGroup* group, const QList<QAction*>& actions);
void
applyDesktopToolbarMetrics(QToolBar* toolbar);
void
applyDesktopWindowChrome(QMainWindow* window, QToolBar* toolbar);
//=============================================================================
