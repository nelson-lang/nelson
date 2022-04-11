//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "QtTerminal.h"
#include <QtGui/QDragEnterEvent>
#include <QtGui/QDropEvent>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QToolBar>
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
    editor();

private:
    void
    createMenus();
    void
    createToolbars();
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

    QMenu* helpMenu;
    QAction* helpAction;
    QAction* webAction;
    QAction* bugAction;

    QAction* aboutAction;

    QAction* editorAction;

    QtTerminal* qtTerminal;

    QToolBar* toolBar;

    bool bClosed;

    void
    dragEnterEvent(QDragEnterEvent* event) override;
    void
    dropEvent(QDropEvent* event) override;
};
//=============================================================================
