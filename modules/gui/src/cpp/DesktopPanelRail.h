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
#include <QtCore/QHash>
#include <QtCore/QPointer>
#include <QtWidgets/QToolBar>
//=============================================================================
class QAction;
class QDockWidget;
class QMenu;
//=============================================================================
class DesktopPanelRail : public QToolBar
{
    Q_OBJECT

public:
    explicit DesktopPanelRail(QWidget* parent = nullptr);

    void
    addPanelAction(QAction* sourceAction);
    void
    addCommandAction(QAction* sourceAction);
    void
    addRailSeparator();
    void
    bindDock(QAction* sourceAction, QDockWidget* dock);
    void
    setActiveDock(QDockWidget* dock);
    void
    complete();

signals:
    void
    panelRequested(QAction* sourceAction);

private:
    QAction*
    createProxyAction(QAction* sourceAction, bool panelAction);
    void
    refreshAction(QAction* sourceAction);
    void
    refreshButtonProperties();

    QHash<QAction*, QAction*> m_proxies;
    QHash<QAction*, QPointer<QDockWidget>> m_docks;
    QPointer<QDockWidget> m_activeDock;
    QMenu* m_moreMenu;
};
//=============================================================================
