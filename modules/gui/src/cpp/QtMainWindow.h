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
#include "QtTerminal.h"
#include "QtHistoryBrowser.h"
#include "Context.hpp"
#include <QtGui/QDragEnterEvent>
#include <QtGui/QDropEvent>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QToolBar>
#include <QtCore/QVariant>
//=============================================================================
class QtMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    QtMainWindow(bool minimized = false);
    ~QtMainWindow() override;

    QtTerminal*
    getQtTerminal();

    void
    declareAsClosed();

    void
    createDockWigdets(Evaluator* eval);

    bool
    isClose() const
    {
        return bClosed;
    }

private slots:
    void
    about();
    void
    help();
    void
    website();
    void
    bugAndRequest();
    void
    checkUpdate();
    void
    cutText();
    void
    copyText();
    void
    pasteText();
    void
    selectAllText();
    void
    emptyClipboard();

    void
    clearConsole();

    void
    runFile();
    void
    loadWorkspace();
    void
    saveWorkspace();
    void
    pwdDisplay();
    void
    changeDir();

    void
    textEditor();

    void
    historyBrowserToggle();
    void
    fileBrowserToggle();
    void
    workspaceBrowserToggle();

    void
    onLayoutTerminalOnly();
    void
    onLayoutDefaultAction();
    void
    onLayoutTwoColumnsAction();

private:
    void
    createMenus();
    void
    createToolbars();

    void
    destroyDockWigdets();

    void
    closeEvent(QCloseEvent* event) override;

    QString nelsonPath;

    QMenuBar* mainMenuBar;
    QMenu* fileMenu;
    QAction* runAction;
    QAction* loadWorkspaceAction;
    QAction* saveWorkspaceAction;
    QAction* pwdAction;
    QAction* chdirAction;
    QAction* exitAction;

    QMenu* editMenu;
    QAction* cutAction;
    QAction* copyAction;
    QAction* pasteAction;
    QAction* selectAllAction;
    QAction* emptyClipboardAction;
    QAction* clearConsoleAction;

    QMenu* windowsMenu;
    QAction* historyBrowserAction;
    QAction* fileBrowserAction;
    QAction* workspaceBrowserAction;

    QMenu* layoutMenu;
    QAction* layoutTerminalOnlyAction;
    QAction* layoutDefaultAction;
    QAction* layoutTwoColumnsAction;

    QMenu* helpMenu;
    QAction* helpAction;
    QAction* webAction;
    QAction* bugAction;
    QAction* checkUpdateAction;

    QAction* aboutAction;

    QAction* editorAction;

    QtTerminal* qtTerminal;

    QToolBar* toolBar;

    bool bClosed;

    void
    dragEnterEvent(QDragEnterEvent* event) override;
    void
    dropEvent(QDropEvent* event) override;

    void
    saveDockWidgetPositions();
    void
    restoreDockWidgetPositions();

public Q_SLOTS:

    void
    onCloseWorkspaceBrowser();
    void
    onCloseHistoryBrowser();
    void
    onCloseFileBrowser();
    void
    onCloseVariablesEditor();
};
//=============================================================================
