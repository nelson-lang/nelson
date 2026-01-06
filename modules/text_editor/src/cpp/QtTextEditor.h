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
#include "Evaluator.hpp"
#include "QtEditPane.h"
#include "QtTextEdit.h"
#include "QtTextIndent.h"
#include "QtDebugStackPanel.h"
#include "QtBreakpointPanel.h"
#include <QtCore/QFileSystemWatcher>
#include <QtGui/QDragEnterEvent>
#include <QtGui/QDropEvent>
#include <QtPrintSupport/QPrinter>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QLabel>
#include <QtWidgets/QDockWidget>
//=============================================================================
using namespace Nelson;
//=============================================================================
#define MAX_RECENT_FILES 5
//=============================================================================
class QtTextEditor : public QMainWindow
{
    Q_OBJECT
    QMenu *fileMenu, *editMenu, *debugMenu;
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

    QAction* exportToAction;
    QAction* runFileAction;
    QAction* stopRunAction;
    QAction* evaluateSelectionAction;

    // Debug actions
    QAction* dbStepAction;
    QAction* dbStepInAction;
    QAction* dbStepOutAction;
    QAction* dbContinueAction;

    // Debug panels
    QDockWidget* debugStackDock;
    QtDebugStackPanel* debugStackPanel;
    QDockWidget* breakpointDock;
    QtBreakpointPanel* breakpointPanel;

    QAction* showDebugStackAction;
    QAction* showBreakpointsPanelAction;

    QAction* helpOnSelectionAction;
    QAction* smartIndentAction;

    QAction* printAction;

    // Find feature members
    QAction* findAction;
    QAction* findNextAction;
    QAction* findPreviousAction;
    QString lastSearch;
    bool lastSearchCaseSensitive;

    // Debug state tracking
    QTimer* debugStateTimer;
    bool lastDebugState = false;
    std::wstring lastDebugFilename;
    size_t lastDebugLine = 0;
    QColor debugLineColor = QColor(255, 255, 0, 128); // Default semi-transparent yellow

public:
    QtTextEditor(Evaluator* eval);
    ~QtTextEditor();
    void
    loadOrCreateFile(const QString& filename);
    void
    contextMenuEvent(QContextMenuEvent* event);

    void
    updateDebugLineHighlight();

    void
    createTabUntitledWithText(const QString& text);

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
    setCurrentFilename(const QString& filename);
    QString
    currentFilename();
    void
    setCurrentEncoding(const QString& encoding);
    QString
    currentEncoding();
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

    QColor
    getDebugLineColor();
    void
    setDebugLineColor(const QColor& color);

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

    void
    exportToPdf(const QString& filename);

    // status label to show cursor position / encoding
    QLabel* statusLabel;

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
    onExportToAction();

    void
    gotoLine();

    void
    runFile();
    void
    stopRun();
    void
    helpOnSelection();

    // Debug slots
    void
    dbStep();
    void
    dbStepIn();
    void
    dbStepOut();
    void
    dbContinue();
    void
    checkDebugState();

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

    bool
    loadFileAsUtf8(const QString& filename, QString& data, QString& sourceEncoding);

    bool
    saveFileWithEncoding(
        const QString& filename, const QString& data, const QString& sourceEncoding);

    // Find feature slots
    void
    showFindDialog();
    void
    findNext();
    void
    findPrevious();

    // update status bar (cursor position / encoding)
    void
    updateCursorPosition();

public:
    void
    closeEvent(QCloseEvent* event);
};
//=============================================================================
