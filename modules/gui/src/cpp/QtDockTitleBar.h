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
#include <functional>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLabel>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QWidget>
//=============================================================================
// Custom dock title bar with a collapse chevron and a compact actions menu.
// Collapsing hides the dock content and shrinks the
// dock to its title bar so sibling docks of the same column reclaim the
// space. The collapsed state and the expanded height are persisted in
// QSettings under "<prefix>_Collapsed" / "<prefix>_ExpandedHeight"; an empty
// prefix disables persistence (dynamic docks such as shell terminals).
//
// When an undock callback is provided, the title bar hosts a docked client
// window (figure, editor): the collapse chevron is hidden and an undock
// button (also bound to double-click) invokes the callback, which reparents
// the client back to a real top-level window.
//=============================================================================
class QtDockTitleBar : public QWidget
{
    Q_OBJECT

public:
    QtDockTitleBar(QDockWidget* dock, const QString& settingsPrefix,
        std::function<void()> undockCallback = nullptr);

    bool
    isCollapsed() const;
    void
    setCollapsed(bool collapsed);

    void
    restoreState();
    void
    saveState();

protected:
    void
    mouseDoubleClickEvent(QMouseEvent* event) override;

private slots:
    void
    toggleCollapsed();
    void
    toggleFloating();
    void
    onTopLevelChanged(bool floating);
    void
    rebuildActionsMenu();

private:
    void
    applyCollapsed(bool collapsed);
    void
    updateCollapseButton();
    void
    moveDock(Qt::DockWidgetArea area);

    QDockWidget* m_dock;
    QString m_settingsPrefix;
    QToolButton* m_collapseButton;
    QLabel* m_titleLabel;
    QToolButton* m_actionsButton;
    std::function<void()> m_undockCallback;
    bool m_collapsed;
    int m_expandedHeight;
    int m_expandedMinimumHeight;
};
//=============================================================================
