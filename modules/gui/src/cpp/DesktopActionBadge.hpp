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
#include <QtCore/QObject>
#include <QtGui/QAction>
#include <QtGui/QIcon>
#include <QtCore/QString>
//=============================================================================
// A count shown on a desktop action: a small badge painted on its icon and
// "(count)" after its label, so the toolbar, menu, panel rail and command
// palette (which all follow the action) show it. Owned by the action.
class DesktopActionBadge : public QObject
{
public:
    explicit DesktopActionBadge(QAction* action);
    // The desktop re-tints action icons on a theme change: take the new icon
    // as the base and paint the badge on it again.
    void
    setBaseIcon(const QIcon& icon);
    // No-op when the count did not change (the desktop polls it).
    void
    setCount(int count);

private:
    QAction* action;
    QIcon baseIcon;
    QString baseText;
    int count = 0;
    void
    apply();
};
//=============================================================================
