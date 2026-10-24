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
#include "CodeAnalyzer.hpp"
#include "QtEditPane.h"
#include "QtTextEdit.h"
#include "QtTextIndent.h"
#include "QtDebugStackPanel.h"
#include "QtBreakpointPanel.h"
#include <QtCore/QFileSystemWatcher>
#include <QtCore/QFutureWatcher>
#include <QtCore/QPointer>
#include <QtCore/QRegularExpression>
#include <QtGui/QDragEnterEvent>
#include <QtGui/QDropEvent>
#include <QtPrintSupport/QPrinter>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QLabel>
#include <QtWidgets/QDockWidget>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
#define MAX_RECENT_FILES 5
//=============================================================================
class QCheckBox;
class QComboBox;
class QLineEdit;
class QTableWidget;
class QTimer;
//=============================================================================
class QtTextEditor : public QMainWindow
{
    Q_OBJECT
    QMenu *fileMenu, *editMenu, *codeMenu, *debugMenu;
    QToolBar *editToolBar, *fileToolBar;
    QAction *newAction, *openAction, *saveAction, *saveAsAction, *saveAllAction, *quitAction;
    QAction *closeAction, *closeAllAction;
    QAction* dockAction;
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

    QAction* dbStepAction;
    QAction* dbStepInAction;
    QAction* dbStepOutAction;
    QAction* dbContinueAction;
    QAction* toggleBreakpointAction;

    QDockWidget* debugStackDock;
    QtDebugStackPanel* debugStackPanel;
    QDockWidget* breakpointDock;
    QtBreakpointPanel* breakpointPanel;

    QAction* showDebugStackAction;
    QAction* showBreakpointsPanelAction;
    QAction* showCodeIssuesPanelAction;
    QAction* enableCodeAnalyzerAction;

    QAction* helpOnSelectionAction;
    QAction* smartIndentAction;
    QAction* fixIssueAction;
    QAction* fixAllTrivialIssuesAction;
    QAction* suppressIssueOnLineAction;
    QAction* suppressIssueAboveLineAction;

    QAction* printAction;

    QAction* findAction;
    QAction* findNextAction;
    QAction* findPreviousAction;
    QAction* replaceAction;
    QString lastSearch;
    bool lastSearchCaseSensitive;

    QWidget* findBar;
    QWidget* replaceRow;
    QLineEdit* findEdit;
    QLineEdit* replaceEdit;
    QCheckBox* findCaseCheck;
    QCheckBox* findWholeWordCheck;
    QCheckBox* findRegexCheck;
    QLabel* findStatusLabel;

    QMenu* eolMenu;
    QAction* eolCrLfAction;
    QAction* eolLfAction;
    QAction* eolCrAction;
    // line ending detected by the last loadFileAsUtf8() call
    QString lastDetectedEol;
    bool lastDetectedEolMixed = false;

    QTimer* debugStateTimer;
    bool lastDebugState = false;
    std::wstring lastDebugFilename;
    size_t lastDebugLine = 0;
    size_t lastBreakpointCount = 0;
    QColor debugLineColor = QColor(255, 255, 0, 128);

    QTimer* codeAnalyzerTimer;
    QLabel* codeAnalyzerStatusLabel;
    QDockWidget* codeIssuesDock;
    QTableWidget* codeIssuesTable;
    QComboBox* codeIssuesScopeCombo;
    QLineEdit* codeIssuesSearchEdit;
    QCheckBox* codeIssuesErrorsCheck;
    QCheckBox* codeIssuesWarningsCheck;
    QCheckBox* codeIssuesInfosCheck;
    QFutureWatcher<std::vector<Nelson::CodeAnalyzerDiagnostic>>* codeAnalyzerWatcher;
    QPointer<QtEditPane> codeAnalyzerTargetPane;
    int codeAnalyzerGeneration = 0;
    int codeAnalyzerRunningGeneration = 0;
    bool codeAnalyzerPending = false;
    bool codeAnalyzerEnabled = true;

public:
    QtTextEditor(Evaluator* eval);
    ~QtTextEditor();

    // desktop docking: the editor window can be hosted inside the Nelson
    // desktop; undocked it stays a real top-level window.
    bool
    isDockedInDesktop() const;
    void
    setDockedInDesktop(bool docked);
    void
    restoreDockState();

    void
    loadOrCreateFile(const QString& filename);
    void
    contextMenuEvent(QContextMenuEvent* event) override;

    bool
    eventFilter(QObject* watched, QEvent* event) override;

    void
    updateDebugLineHighlight();

    void
    createTabUntitledWithText(const QString& text);
    bool
    gotoLineNumber(int lineNumber);

private:
    Evaluator* nlsEvaluator;
    std::wstring textEditorRootPath;
    wstringVector recentFilenames;
    void
    updateDockActionState();
    // top-level position/size remembered while docked, so preferences never
    // store dock-relative coordinates.
    QPoint m_undockedPos;
    QSize m_undockedSize;
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
    closeTabAt(int indexTab);
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
    void
    setCurrentEol(const QString& eol);
    QString
    currentEol();
    void
    changeEol(const QString& eol);
    void
    updateEolActions();

    void
    createFindBar();
    void
    showFindBar(bool withReplace);
    void
    hideFindBar();
    QRegularExpression
    currentFindExpression(bool& isValid) const;
    void
    clearFindHighlights();
    void
    updateFindHighlights();
    bool
    findFromBar(bool backward);
    void
    replaceCurrentMatch();
    void
    replaceAllMatches();
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
    // Re-reads the font, indent and code analyzer preferences from QSettings and
    // applies them to the open tabs. Called by the preferences dialog through
    // GuiPreferencesNotifier.
    void
    reloadPreferences();

    QColor
    getDebugLineColor();
    void
    setDebugLineColor(const QColor& color);

    void
    dragEnterEvent(QDragEnterEvent* event) override;
    void
    dropEvent(QDropEvent* event) override;

    QFileSystemWatcher fileWatcher;
    QStringList filesModifiedMessageDisplayedList;
    QString lastFilenameSaved;

    int
    getCurrentLineNumber();

    void
    exportToPdf(const QString& filename);

    void
    createCodeIssuesPanel();
    void
    scheduleCodeAnalysis();
    void
    clearCodeAnalyzerDiagnostics();
    void
    analyzeCurrentTab();
    void
    analyzePane(QtEditPane* pane);
    void
    refreshCodeIssuesPanel();
    void
    updateCodeAnalyzerStatus();
    void
    gotoIssueLine(int line);
    std::vector<Nelson::CodeAnalyzerDiagnostic>
    diagnosticsForCurrentPane() const;
    Nelson::CodeAnalyzerDiagnostic
    currentLineDiagnostic() const;
    QString
    codeAnalyzerConfigForFile(const QString& filename) const;
    void
    applyTrivialFix(const Nelson::CodeAnalyzerDiagnostic& diagnostic);
    void
    suppressDiagnostic(const Nelson::CodeAnalyzerDiagnostic& diagnostic, bool previousLine);

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

    void
    dbStep();
    void
    dbStepIn();
    void
    dbStepOut();
    void
    dbContinue();
    void
    toggleBreakpoint();
    void
    checkDebugState();

    void
    smartIndent();
    void
    onCodeIssueActivated(int row, int column);
    void
    setCodeAnalyzerEnabled(bool enabled);
    void
    fixIssue();
    void
    fixAllTrivialIssues();
    void
    suppressIssueOnLine();
    void
    suppressIssueAboveLine();

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

    void
    showFindDialog();
    void
    findNext();
    void
    findPrevious();

    void
    updateCursorPosition();

public:
    void
    closeEvent(QCloseEvent* event) override;
};
//=============================================================================
