//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    QtMainWindow();
    ~QtMainWindow() override;

    QtTerminal*
    getQtTerminal();

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
