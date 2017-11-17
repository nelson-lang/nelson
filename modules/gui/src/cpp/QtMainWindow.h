//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QToolBar>
#include "QtTerminal.h"
//=============================================================================
class QtMainWindow : public QMainWindow {
    Q_OBJECT

public:
    QtMainWindow();
    ~QtMainWindow();

    QtTerminal *getQtTerminal();

private slots:
    void about();
    void help();
    void website();
    void bugAndRequest();

    void cutText();
    void copyText();
    void pasteText();
    void selectAllText();
    void emptyClipboard();

    void clearConsole();

    void runFile();
    void pwdDisplay();
    void changeDir();

    void editor();

private:
    void createMenus();
    void createToolbars();
    virtual void closeEvent(QCloseEvent *event);

    QString nelsonPath;

    QMenuBar *mainMenuBar;
    QMenu *fileMenu;
    QAction *runAct;
    QAction *pwdAct;
    QAction *chdirAct;
    QAction *exitAct;

    QMenu *editMenu;
    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;
    QAction *selectAllAct;
    QAction *emptyClipboardAct;
    QAction *clearConsoleAct;

    QMenu *helpMenu;
    QAction *helpAct;
    QAction *webAct;
    QAction *bugAct;

    QAction *aboutAct;

    QAction *editorAct;

    QtTerminal *qtTerminal;

    QToolBar *toolBarEditor;

    bool bClosed;
};
//=============================================================================