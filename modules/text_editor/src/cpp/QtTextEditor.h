//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
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
#include "Evaluator.hpp"
#include "QtEditPane.h"
#include "QtTextEdit.h"
#include "QtTextIndent.h"
#include <QtCore/QFileSystemWatcher>
#include <QtGui/QDragEnterEvent>
#include <QtGui/QDropEvent>
#include <QtPrintSupport/QPrinter>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QTabWidget>
//=============================================================================
using namespace Nelson;
//=============================================================================
#define MAX_RECENT_FILES 5
//=============================================================================
class QtTextEditor : public QMainWindow
{
    Q_OBJECT
    QMenu *fileMenu, *editMenu;
    QToolBar *editToolBar, *fileToolBar;
    QAction *newAction, *openAction, *saveAction, *saveAsAction, *saveAllAction, *quitAction;
    QAction *closeAction, *closeAllAction;
    QAction *cutAction, *copyAction, *pasteAction, *fontAction;
    QAction* recentFileActions[MAX_RECENT_FILES];
    QAction* separatorAction;
    QAction *undoAction, *redoAction;

    QTabWidget* tab;
    QtTextEdit* prevEdit;
    QFont m_font;

    QAction* copyFullPathAction;
    QMenu* contextMenu;
    QAction* commentAction;
    QAction* uncommentAction;
    QAction* gotoLineAction;

    QAction* runFileAction;
    QAction* stopRunAction;
    QAction* evaluateSelectionAction;

    QAction* helpOnSelectionAction;
    QAction* smartIndentAction;

    QAction* printAction;

public:
    QtTextEditor(Evaluator* eval);
    ~QtTextEditor();
    void
    loadOrCreateFile(const QString& filename);
    void
    contextMenuEvent(QContextMenuEvent* event);

private:
    Evaluator* nlsEvaluator;
    std::wstring textEditorRootPath;
    wstringVector recentFilenames;
    void
    createActions();
    void
    createMenus();
    void
    createToolBars();
    void
    createStatusBar();
    bool
    maybeSave();
    bool
    saveFile(const QString& filename);
    void
    loadFile(const QString& filename);
    QString
    strippedName(const QString& fullfilename);
    void
    updateRecentFileActions();

    QtTextEdit*
    currentEditor();
    void
    setCurrentFile(const QString& filename);
    void
    setCurrentFilename(QString filename);
    QString
    currentFilename();
    QString
    shownName();
    void
    updateTitles();
    void
    readSettings();
    void
    writeSettings();
    void
    updateFont();

    void
    dragEnterEvent(QDragEnterEvent* event);
    void
    dropEvent(QDropEvent* event);

    QFileSystemWatcher fileWatcher;
    QStringList filesModifiedMessageDisplayedList;
    QString lastFilenameSaved;

    int
    getCurrentLineNumber();
    bool
    gotoLineNumber(int lineNumber);

private Q_SLOTS:
    bool
    save();
    bool
    saveAs();
    bool
    saveAll();
    void
    open();
    void
    font();
    void
    addTab();
    void
    addTabUntitled();
    void
    closeTab();
    void
    closeTab(int);
    void
    closeAllTabs();

    void
    tabChanged(int);
    void
    documentWasModified();
    void
    openRecentFile();

    void
    copyFullPath();
    void
    undo();
    void
    redo();

    void
    comment();
    void
    uncomment();

    void
    gotoLine();

    void
    runFile();
    void
    stopRun();
    void
    helpOnSelection();
    void
    smartIndent();

    void
    printDocument();
    void
    print(QPrinter* p);

    void
    evaluateSelection();

    void
    reloadFile(const QString);

public:
    void
    closeEvent(QCloseEvent* event);
};
//=============================================================================
