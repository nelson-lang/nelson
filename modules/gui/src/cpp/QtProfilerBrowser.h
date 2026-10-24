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
#include <QtCore/QtGlobal>
#include <QtCore/QTimer>
#include <QtGui/QAction>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLabel>
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QToolBar>
#include <QtGui/QCloseEvent>
//=============================================================================
class QtProfilerBrowser : public QDockWidget
{
    Q_OBJECT

public:
    explicit QtProfilerBrowser(QWidget* parent = nullptr);

signals:
    void
    closeProfilerBrowser();

protected:
    void
    closeEvent(QCloseEvent* event) override;

private slots:
    void
    toggleProfiler();
    void
    clearProfiler();
    void
    exportProfiler();
    void
    refreshProfiler();
    void
    syncProfilerState();
    void
    openProfileRow(int row, int column);

private:
    void
    setupUi();
    void
    updateRunAction();

    QToolBar* m_toolBar;
    QAction* m_runAction;
    QAction* m_clearAction;
    QAction* m_exportAction;
    QAction* m_refreshAction;
    QLabel* m_stateLabel;
    QTableWidget* m_table;
    QTimer* m_stateTimer;
    bool m_lastProfilerState;
};
//=============================================================================
