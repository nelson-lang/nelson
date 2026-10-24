//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QtGlobal>
#include <QtCore/QFileInfo>
#include <QtCore/QDir>
#include <QtCore/QMimeData>
#include <QtCore/QUrl>
#include <QtCore/QTextStream>
#include <QtCore/QSettings>
#include <QtCore/QTimer>
#include <QtCore/QSignalBlocker>
#include <QtConcurrent/QtConcurrentRun>
#include <QtGui/QClipboard>
#include <QtGui/QKeyEvent>
#include <QtGui/QMouseEvent>
#include <QtGui/QTextBlock>
#include <QtGui/QTextDocumentFragment>
#include <QtGui/QPainter>
#include <QtPrintSupport/QPrintDialog>
#include <QtPrintSupport/QPrintPreviewDialog>
#include <QtPrintSupport/QPrinter>
#include <QtGui/QAction>
#include <QtWidgets/QAbstractItemView>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QFontDialog>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QTabBar>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QVBoxLayout>
#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include "CodeAnalyzer.hpp"
#include "StringHelpers.hpp"
#include "QtTextEditor.h"
#include "QtWaitCursor.hpp"
#include "PostCommand.hpp"
#include "ModulesManager.hpp"
#include "QStringConverter.hpp"
#include "QtBreakpointArea.h"
#include "QtCodeAnalyzerMarkerArea.h"
#include "QtLineNumber.h"
#include "QtTextIndent.h"
#include "QtTranslation.hpp"
#include "SmartIndent.h"
#include "NelsonException.hpp"
#include "TextEditorIndentPreferences.h"
#include "TextEditorPreferences.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "QtFileDialogHistory.hpp"
#include "PostCommand.hpp"
#include "i18n.hpp"
#include "NelsonPalette.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "NelsonEditorSettings.hpp"
#include "GuiPreferencesNotifier.hpp"
#include "DefaultFont.hpp"
#include <QtCore/QSettings>
#include "DesktopDocking.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
#define DEFAULT_FILENAME "untitled.m"
#define DEFAULT_DELAY_MSG 1500
#define DEBUG_STATE_CHECK_INTERVAL 200
//=============================================================================
const QString SETTING_TEXT_EDITOR_DOCKED = "te_DockedInDesktop";
//=============================================================================
static int
codeAnalyzerSeverityRank(const std::string& severity)
{
    if (severity == "error") {
        return 3;
    }
    if (severity == "warning") {
        return 2;
    }
    if (severity == "info") {
        return 1;
    }
    return 0;
}
//=============================================================================
static QColor
codeAnalyzerSeverityColor(const std::string& severity)
{
    if (severity == "error") {
        return QColor(211, 47, 47);
    }
    if (severity == "warning") {
        return QColor(245, 124, 0);
    }
    return QColor(25, 118, 210);
}
//=============================================================================
static bool
codeAnalyzerIsTrivialFix(const Nelson::CodeAnalyzerDiagnostic& diagnostic)
{
    return diagnostic.fixability == "trivial"
        && (diagnostic.checkId == "NLS0010" || diagnostic.checkId == "NLS0011");
}
//=============================================================================
// QTextCursor::selectedText() replaces line breaks by U+2029 (paragraph separator).
static QString
selectedTextAsPlainText(const QTextCursor& cursor)
{
    QString text = cursor.selectedText();
    text.replace(QChar(QChar::ParagraphSeparator), QLatin1Char('\n'));
    text.replace(QChar(QChar::LineSeparator), QLatin1Char('\n'));
    return text;
}
//=============================================================================
QtTextEditor::QtTextEditor(Evaluator* eval)
{
    setAcceptDrops(true);
    recentFilenames.clear();
    nlsEvaluator = eval;
    textEditorRootPath = Nelson::GetModulePath(L"text_editor");
    QString fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-open.svg"));
    setWindowIcon(QPixmap(fileNameIcon));
    prevEdit = nullptr;
    codeAnalyzerTimer = nullptr;
    codeAnalyzerStatusLabel = nullptr;
    codeIssuesDock = nullptr;
    codeIssuesTable = nullptr;
    codeIssuesScopeCombo = nullptr;
    codeIssuesSearchEdit = nullptr;
    codeIssuesErrorsCheck = nullptr;
    codeIssuesWarningsCheck = nullptr;
    codeIssuesInfosCheck = nullptr;
    codeAnalyzerWatcher = nullptr;
    enableCodeAnalyzerAction = nullptr;
    findAction = nullptr;
    findNextAction = nullptr;
    findPreviousAction = nullptr;
    findBar = nullptr;
    replaceRow = nullptr;
    findEdit = nullptr;
    replaceEdit = nullptr;
    findCaseCheck = nullptr;
    findWholeWordCheck = nullptr;
    findRegexCheck = nullptr;
    findStatusLabel = nullptr;
    replaceAction = nullptr;
    eolMenu = nullptr;
    eolCrLfAction = nullptr;
    eolLfAction = nullptr;
    eolCrAction = nullptr;
    statusLabel = nullptr;
    dockAction = nullptr;
    lastDetectedEol = QtEditPane::defaultEol();
    lastSearchCaseSensitive = false;
    tab = new QTabWidget(this);
    tab->setTabsClosable(true);
    tab->setMovable(true);
    tab->tabBar()->installEventFilter(this);
    // the tab widget and the Find/Replace bar share the central area
    QWidget* centralContainer = new QWidget(this);
    QVBoxLayout* centralLayout = new QVBoxLayout(centralContainer);
    centralLayout->setContentsMargins(0, 0, 0, 0);
    centralLayout->setSpacing(0);
    centralLayout->addWidget(tab);
    createFindBar();
    centralLayout->addWidget(findBar);
    setCentralWidget(centralContainer);
    readSettings();
    createActions();
    createMenus();
    createToolBars();
    createStatusBar();
    connect(tab, SIGNAL(currentChanged(int)), this, SLOT(tabChanged(int)));
    connect(tab, SIGNAL(tabCloseRequested(int)), this, SLOT(closeTab(int)));
    connect(&GuiPreferencesNotifier::instance(), &GuiPreferencesNotifier::editorPreferencesChanged,
        this, &QtTextEditor::reloadPreferences);
    addTab();
    setPalette(getNelsonPalette());

    debugStackPanel = new QtDebugStackPanel(nlsEvaluator, this);
    debugStackDock = new QDockWidget(tr("Call Stack"), this);
    debugStackDock->setWidget(debugStackPanel);
    debugStackDock->setAllowedAreas(
        Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea | Qt::BottomDockWidgetArea);
    addDockWidget(Qt::BottomDockWidgetArea, debugStackDock);
    debugStackDock->hide();

    breakpointPanel = new QtBreakpointPanel(nlsEvaluator, this);
    breakpointDock = new QDockWidget(tr("Breakpoints"), this);
    breakpointDock->setWidget(breakpointPanel);
    breakpointDock->setAllowedAreas(
        Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea | Qt::BottomDockWidgetArea);
    addDockWidget(Qt::RightDockWidgetArea, breakpointDock);
    breakpointDock->hide();

    connect(debugStackPanel, &QtDebugStackPanel::frameClicked, this,
        [this](const QString& filename, int line) {
            loadFile(filename);
            for (int i = 0; i < tab->count(); ++i) {
                QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
                if (pane && pane->getFileName() == filename) {
                    tab->setCurrentIndex(i);
                    if (pane->getEditor()) {
                        QTextBlock block
                            = pane->getEditor()->document()->findBlockByLineNumber(line - 1);
                        if (block.isValid()) {
                            QTextCursor cursor(block);
                            pane->getEditor()->setTextCursor(cursor);
                            pane->getEditor()->ensureCursorVisible();
                        }
                    }
                    break;
                }
            }
        });

    connect(breakpointPanel, &QtBreakpointPanel::breakpointClicked, this,
        [this](const QString& filename, int line) {
            QFileInfo requestedFile(filename);
            QString requestedPath = requestedFile.absoluteFilePath();

            bool fileFound = false;
            for (int i = 0; i < tab->count(); ++i) {
                QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
                if (pane) {
                    QFileInfo openFile(pane->getFileName());
                    if (openFile.absoluteFilePath() == requestedPath) {
                        tab->setCurrentIndex(i);
                        fileFound = true;
                        break;
                    }
                }
            }

            if (!fileFound) {
                loadFile(filename);
            }

            if (currentEditor()) {
                QTextBlock block = currentEditor()->document()->findBlockByLineNumber(line - 1);
                if (block.isValid()) {
                    QTextCursor cursor(block);
                    currentEditor()->setTextCursor(cursor);
                    currentEditor()->ensureCursorVisible();
                    currentEditor()->setFocus();
                }
            }
        });

    connect(breakpointPanel, &QtBreakpointPanel::breakpointsChanged, this, [this]() {
        for (int i = 0; i < tab->count(); ++i) {
            QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
            if (pane) {
                pane->refreshBreakpoints();
            }
        }
    });

    connect(showDebugStackAction, &QAction::toggled, debugStackDock, &QDockWidget::setVisible);
    connect(debugStackDock, &QDockWidget::visibilityChanged, showDebugStackAction,
        &QAction::setChecked);
    connect(
        showBreakpointsPanelAction, &QAction::toggled, breakpointDock, &QDockWidget::setVisible);
    connect(breakpointDock, &QDockWidget::visibilityChanged, showBreakpointsPanelAction,
        &QAction::setChecked);

    createCodeIssuesPanel();
    connect(showCodeIssuesPanelAction, &QAction::toggled, codeIssuesDock, &QDockWidget::setVisible);
    connect(codeIssuesDock, &QDockWidget::visibilityChanged, showCodeIssuesPanelAction,
        &QAction::setChecked);

    codeAnalyzerTimer = new QTimer(this);
    codeAnalyzerTimer->setSingleShot(true);
    codeAnalyzerTimer->setInterval(500);
    connect(codeAnalyzerTimer, &QTimer::timeout, this, &QtTextEditor::analyzeCurrentTab);
    codeAnalyzerWatcher = new QFutureWatcher<std::vector<Nelson::CodeAnalyzerDiagnostic>>(this);
    connect(codeAnalyzerWatcher,
        &QFutureWatcher<std::vector<Nelson::CodeAnalyzerDiagnostic>>::finished, this, [this]() {
            if (codeAnalyzerTargetPane && codeAnalyzerRunningGeneration == codeAnalyzerGeneration) {
                codeAnalyzerTargetPane->setCodeAnalyzerDiagnostics(codeAnalyzerWatcher->result());
                refreshCodeIssuesPanel();
                updateCodeAnalyzerStatus();
            }
            codeAnalyzerTargetPane.clear();
            if (codeAnalyzerPending) {
                codeAnalyzerPending = false;
                scheduleCodeAnalysis();
            }
        });
    scheduleCodeAnalysis();
    updateCodeAnalyzerStatus();

    debugStateTimer = new QTimer(this);
    connect(debugStateTimer, SIGNAL(timeout()), this, SLOT(checkDebugState()));
    debugStateTimer->start(DEBUG_STATE_CHECK_INTERVAL);
}
//=============================================================================
void
QtTextEditor::loadOrCreateFile(const QString& filename)
{
    if (filename.isEmpty()) {
        return;
    }
    QFile ff(filename);
    QFileInfo fileInfo(ff);
    bool isdir = (fileInfo.exists() && fileInfo.isDir());
    if (isdir) {
        QString msg = wstringToQString(
            Nelson::resolveMessageTemplate(
                L"Nelson:text_editor:cannotEditDirectory", L"Cannot edit the directory: %1"))
                          .arg(fileInfo.absoluteFilePath());
        statusBar()->showMessage(msg);
        return;
    }
    bool isfile = (fileInfo.exists() && fileInfo.isFile());
    if (!isfile) {
        if (QMessageBox::question(this, TR("Nelson"),
                TR("File %1 does not exists. Do you want to create it?")
                    .arg(fileInfo.absoluteFilePath()),
                QMessageBox::Yes | QMessageBox::Default, QMessageBox::No)
            == QMessageBox::No) {
            return;
        }
        addTabUntitled();
        setCurrentFile(filename);
        return;
    }
    loadFile(filename);
}
//=============================================================================
void
QtTextEditor::createActions()
{
    QString fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-new.svg"));
    newAction = new QAction(QIcon(fileNameIcon), TR("&New"), this);
    newAction->setShortcut(QKeySequence::New);
    connect(newAction, SIGNAL(triggered()), this, SLOT(addTab()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-open.svg"));
    openAction = new QAction(QIcon(fileNameIcon), TR("&Open"), this);
    openAction->setShortcut(QKeySequence::Open);
    connect(openAction, SIGNAL(triggered()), this, SLOT(open()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-save.svg"));
    saveAction = new QAction(QIcon(fileNameIcon), TR("&Save"), this);
    saveAction->setShortcut(Qt::Key_S | Qt::CTRL);
    connect(saveAction, SIGNAL(triggered()), this, SLOT(save()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-save-as.svg"));
    saveAsAction = new QAction(QIcon(fileNameIcon), TR("Save &As"), this);
    connect(saveAsAction, SIGNAL(triggered()), this, SLOT(saveAs()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-save-all.svg"));
    saveAllAction = new QAction(QIcon(fileNameIcon), TR("Save A&ll"), this);
    saveAllAction->setShortcut(QKeySequence(Qt::CTRL | Qt::SHIFT | Qt::Key_S));
    connect(saveAllAction, SIGNAL(triggered()), this, SLOT(saveAll()));
    for (auto& recentFileAction : recentFileActions) {
        recentFileAction = new QAction(this);
        recentFileAction->setVisible(false);
        connect(recentFileAction, SIGNAL(triggered()), this, SLOT(openRecentFile()));
    }
    closeAction = new QAction(TR("&Close"), this);
    closeAction->setShortcut(QKeySequence::Close);
    connect(closeAction, SIGNAL(triggered()), this, SLOT(closeTab()));
    closeAllAction = new QAction(TR("Close &All"), this);
    connect(closeAllAction, SIGNAL(triggered()), this, SLOT(closeAllTabs()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-exit.svg"));
    quitAction = new QAction(QIcon(fileNameIcon), TR("&Quit Editor"), this);
    quitAction->setShortcut(Qt::Key_F4 | Qt::ALT);
    connect(quitAction, SIGNAL(triggered()), this, SLOT(close()));
    dockAction = new QAction(TR("&Dock"), this);
    connect(dockAction, &QAction::triggered, this,
        [this]() { setDockedInDesktop(!isDockedInDesktop()); });
    updateDockActionState();
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-copy.svg"));
    copyAction = new QAction(QIcon(fileNameIcon), TR("&Copy"), this);
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-cut.svg"));
    cutAction = new QAction(QIcon(fileNameIcon), TR("C&ut"), this);
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-paste.svg"));
    pasteAction = new QAction(QIcon(fileNameIcon), TR("&Paste"), this);
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-undo.svg"));
    undoAction = new QAction(QIcon(fileNameIcon), TR("&Undo"), this);
    undoAction->setShortcut(Qt::Key_Z | Qt::CTRL);
    connect(undoAction, SIGNAL(triggered()), this, SLOT(undo()));
    undoAction->setEnabled(false);
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/edit-redo.svg"));
    redoAction = new QAction(QIcon(fileNameIcon), TR("&Redo"), this);
    redoAction->setShortcut(Qt::Key_Y | Qt::CTRL);
    connect(redoAction, SIGNAL(triggered()), this, SLOT(redo()));
    redoAction->setEnabled(false);
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/format-text-bold.svg"));
    fontAction = new QAction(QIcon(fileNameIcon), TR("&Font"), this);
    connect(fontAction, SIGNAL(triggered()), this, SLOT(font()));
    copyFullPathAction = new QAction(TR("Copy Full Path"), this);
    connect(copyFullPathAction, SIGNAL(triggered()), this, SLOT(copyFullPath()));
    commentAction = new QAction(TR("Comment"), this);
    commentAction->setShortcut(Qt::Key_R | Qt::CTRL);
    connect(commentAction, SIGNAL(triggered()), this, SLOT(comment()));
    uncommentAction = new QAction(TR("Uncomment"), this);
    uncommentAction->setShortcut(Qt::Key_T | Qt::CTRL);
    connect(uncommentAction, SIGNAL(triggered()), this, SLOT(uncomment()));
    gotoLineAction = new QAction(TR("&Go To Line ..."), this);
    gotoLineAction->setShortcut(Qt::Key_G | Qt::CTRL);
    connect(gotoLineAction, SIGNAL(triggered()), this, SLOT(gotoLine()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/run-file-start.svg"));
    runFileAction = new QAction(QIcon(fileNameIcon), TR("&Run file"), this);
    connect(runFileAction, SIGNAL(triggered()), this, SLOT(runFile()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/stop-interpreter.svg"));
    stopRunAction = new QAction(QIcon(fileNameIcon), TR("&Stop execution"), this);
    connect(stopRunAction, SIGNAL(triggered()), this, SLOT(stopRun()));
    stopRunAction->setEnabled(false);
    fileNameIcon = Nelson::wstringToQString(
                       Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory())
        + QString("/resources/help-icon.svg");
    helpOnSelectionAction = new QAction(QIcon(fileNameIcon), TR("Help on Selection"), this);
    connect(helpOnSelectionAction, SIGNAL(triggered()), this, SLOT(helpOnSelection()));
    smartIndentAction = new QAction(TR("Smart Indent"), this);
    smartIndentAction->setShortcut(Qt::Key_I | Qt::CTRL);
    connect(smartIndentAction, SIGNAL(triggered()), this, SLOT(smartIndent()));
    fixIssueAction = new QAction(TR("Fix Code Issue"), this);
    connect(fixIssueAction, SIGNAL(triggered()), this, SLOT(fixIssue()));
    fixAllTrivialIssuesAction = new QAction(TR("Fix All Trivial Code Issues"), this);
    connect(fixAllTrivialIssuesAction, SIGNAL(triggered()), this, SLOT(fixAllTrivialIssues()));
    suppressIssueOnLineAction = new QAction(TR("Suppress Code Issue on This Line"), this);
    connect(suppressIssueOnLineAction, SIGNAL(triggered()), this, SLOT(suppressIssueOnLine()));
    suppressIssueAboveLineAction = new QAction(TR("Suppress Code Issue Above This Line"), this);
    connect(
        suppressIssueAboveLineAction, SIGNAL(triggered()), this, SLOT(suppressIssueAboveLine()));
    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/document-print.svg"));
    printAction = new QAction(QIcon(fileNameIcon), TR("Print"), this);
    printAction->setShortcut(Qt::Key_P | Qt::CTRL);
    connect(printAction, SIGNAL(triggered()), this, SLOT(printDocument()));
    evaluateSelectionAction = new QAction(TR("Evaluate selection"), this);
    evaluateSelectionAction->setShortcut(Qt::Key_F9);
    evaluateSelectionAction->setToolTip(TR("Evaluate selection (F9)"));
    connect(evaluateSelectionAction, SIGNAL(triggered()), this, SLOT(evaluateSelection()));
    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/export-to.svg"));
    exportToAction = new QAction(QIcon(fileNameIcon), TR("Export to PDF ..."), this);
    connect(exportToAction, SIGNAL(triggered()), this, SLOT(onExportToAction()));

    fileNameIcon
        = Nelson::wstringToQString(textEditorRootPath + std::wstring(L"/resources/debug-step.svg"));
    dbStepAction = new QAction(QIcon(fileNameIcon), TR("Step"), this);
    dbStepAction->setShortcut(Qt::Key_F10);
    dbStepAction->setToolTip(TR("Step (F10)"));
    connect(dbStepAction, SIGNAL(triggered()), this, SLOT(dbStep()));
    dbStepAction->setEnabled(false);

    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/debug-step-in.svg"));
    dbStepInAction = new QAction(QIcon(fileNameIcon), TR("Step In"), this);
    dbStepInAction->setShortcut(Qt::Key_F11);
    dbStepInAction->setToolTip(TR("Step In (F11)"));
    connect(dbStepInAction, SIGNAL(triggered()), this, SLOT(dbStepIn()));
    dbStepInAction->setEnabled(false);

    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/debug-step-out.svg"));
    dbStepOutAction = new QAction(QIcon(fileNameIcon), TR("Step Out"), this);
    dbStepOutAction->setShortcut(Qt::Key_F11 | Qt::SHIFT);
    dbStepOutAction->setToolTip(TR("Step Out (Shift+F11)"));
    connect(dbStepOutAction, SIGNAL(triggered()), this, SLOT(dbStepOut()));
    dbStepOutAction->setEnabled(false);

    fileNameIcon = Nelson::wstringToQString(
        textEditorRootPath + std::wstring(L"/resources/debug-continue.svg"));
    dbContinueAction = new QAction(QIcon(fileNameIcon), TR("Continue"), this);
    dbContinueAction->setShortcut(Qt::Key_F5);
    dbContinueAction->setToolTip(TR("Continue (F5)"));
    connect(dbContinueAction, SIGNAL(triggered()), this, SLOT(dbContinue()));
    dbContinueAction->setEnabled(false);

    // F9 is already used by 'Evaluate selection', toggle breakpoint uses Ctrl+F9.
    toggleBreakpointAction = new QAction(TR("Toggle Breakpoint"), this);
    toggleBreakpointAction->setShortcut(QKeySequence(Qt::CTRL | Qt::Key_F9));
    toggleBreakpointAction->setToolTip(TR("Toggle Breakpoint (Ctrl+F9)"));
    connect(toggleBreakpointAction, SIGNAL(triggered()), this, SLOT(toggleBreakpoint()));

    findAction = new QAction(TR("Find..."), this);
    findAction->setShortcut(QKeySequence::Find);
    connect(findAction, SIGNAL(triggered()), this, SLOT(showFindDialog()));

    findNextAction = new QAction(TR("Find Next"), this);
    findNextAction->setShortcut(QKeySequence::FindNext);
    connect(findNextAction, SIGNAL(triggered()), this, SLOT(findNext()));
    findNextAction->setEnabled(!lastSearch.isEmpty());

    findPreviousAction = new QAction(TR("Find Previous"), this);
    findPreviousAction->setShortcut(QKeySequence::FindPrevious);
    connect(findPreviousAction, SIGNAL(triggered()), this, SLOT(findPrevious()));
    findPreviousAction->setEnabled(!lastSearch.isEmpty());

    replaceAction = new QAction(TR("Replace..."), this);
    replaceAction->setShortcut(QKeySequence::Replace);
    connect(replaceAction, &QAction::triggered, this, [this]() { showFindBar(true); });

    eolCrLfAction = new QAction(TR("Windows (CRLF)"), this);
    eolCrLfAction->setCheckable(true);
    connect(eolCrLfAction, &QAction::triggered, this, [this]() { changeEol(EOL_STYLE_CRLF); });
    eolLfAction = new QAction(TR("Unix (LF)"), this);
    eolLfAction->setCheckable(true);
    connect(eolLfAction, &QAction::triggered, this, [this]() { changeEol(EOL_STYLE_LF); });
    eolCrAction = new QAction(TR("Macintosh (CR)"), this);
    eolCrAction->setCheckable(true);
    connect(eolCrAction, &QAction::triggered, this, [this]() { changeEol(EOL_STYLE_CR); });

    showDebugStackAction = new QAction(TR("Show Call Stack"), this);
    showDebugStackAction->setCheckable(true);

    showBreakpointsPanelAction = new QAction(TR("Show Breakpoints"), this);
    showBreakpointsPanelAction->setCheckable(true);

    showCodeIssuesPanelAction = new QAction(TR("Show Code Issues"), this);
    showCodeIssuesPanelAction->setCheckable(true);

    enableCodeAnalyzerAction = new QAction(TR("Enable Code Analyzer"), this);
    enableCodeAnalyzerAction->setCheckable(true);
    enableCodeAnalyzerAction->setChecked(codeAnalyzerEnabled);
    connect(
        enableCodeAnalyzerAction, SIGNAL(toggled(bool)), this, SLOT(setCodeAnalyzerEnabled(bool)));

    fixIssueAction->setEnabled(codeAnalyzerEnabled);
    fixAllTrivialIssuesAction->setEnabled(codeAnalyzerEnabled);
    suppressIssueOnLineAction->setEnabled(codeAnalyzerEnabled);
    suppressIssueAboveLineAction->setEnabled(codeAnalyzerEnabled);
}
//=============================================================================
void
QtTextEditor::createMenus()
{
    fileMenu = menuBar()->addMenu(TR("&File"));
    fileMenu->addAction(newAction);
    fileMenu->addAction(openAction);
    fileMenu->addSeparator();
    fileMenu->addAction(saveAction);
    fileMenu->addAction(saveAsAction);
    fileMenu->addAction(saveAllAction);
    fileMenu->addSeparator();
    fileMenu->addAction(exportToAction);
    fileMenu->addAction(printAction);
    fileMenu->addSeparator();
    fileMenu->addAction(closeAction);
    fileMenu->addAction(closeAllAction);
    separatorAction = fileMenu->addSeparator();
    for (auto& recentFileAction : recentFileActions) {
        fileMenu->addAction(recentFileAction);
    }
    fileMenu->addSeparator();
    updateRecentFileActions();
    fileMenu->addAction(dockAction);
    fileMenu->addAction(quitAction);
    editMenu = menuBar()->addMenu(TR("&Edit"));
    editMenu->addAction(gotoLineAction);
    editMenu->addSeparator();
    editMenu->addAction(undoAction);
    editMenu->addAction(redoAction);
    editMenu->addSeparator();
    editMenu->addAction(copyAction);
    editMenu->addAction(pasteAction);
    editMenu->addAction(cutAction);
    editMenu->addSeparator();
    editMenu->addAction(commentAction);
    editMenu->addAction(uncommentAction);
    editMenu->addAction(smartIndentAction);
    editMenu->addSeparator();
    editMenu->addAction(fontAction);

    if (findAction != nullptr) {
        editMenu->addAction(findAction);
    }
    if (findNextAction != nullptr) {
        editMenu->addAction(findNextAction);
    }
    if (findPreviousAction != nullptr) {
        editMenu->addAction(findPreviousAction);
    }
    if (replaceAction != nullptr) {
        editMenu->addAction(replaceAction);
    }

    editMenu->addSeparator();
    eolMenu = editMenu->addMenu(TR("Line Endings"));
    eolMenu->addAction(eolCrLfAction);
    eolMenu->addAction(eolLfAction);
    eolMenu->addAction(eolCrAction);

    codeMenu = menuBar()->addMenu(TR("&Code"));
    codeMenu->addAction(evaluateSelectionAction);
    codeMenu->addSeparator();
    codeMenu->addAction(enableCodeAnalyzerAction);
    codeMenu->addAction(showCodeIssuesPanelAction);

    debugMenu = menuBar()->addMenu(TR("&Debug"));
    debugMenu->addAction(runFileAction);
    debugMenu->addAction(stopRunAction);
    debugMenu->addSeparator();
    debugMenu->addAction(toggleBreakpointAction);
    debugMenu->addSeparator();
    debugMenu->addAction(dbContinueAction);
    debugMenu->addAction(dbStepAction);
    debugMenu->addAction(dbStepInAction);
    debugMenu->addAction(dbStepOutAction);
    debugMenu->addSeparator();
    debugMenu->addAction(showDebugStackAction);
    debugMenu->addAction(showBreakpointsPanelAction);

    contextMenu = new QMenu();
    contextMenu->addAction(copyFullPathAction);
    contextMenu->addSeparator();
    contextMenu->addAction(evaluateSelectionAction);
    contextMenu->addAction(helpOnSelectionAction);
    contextMenu->addSeparator();

    if (findAction != nullptr) {
        contextMenu->addAction(findAction);
    }
    if (findNextAction != nullptr) {
        contextMenu->addAction(findNextAction);
    }
    if (findPreviousAction != nullptr) {
        contextMenu->addAction(findPreviousAction);
    }
    if (replaceAction != nullptr) {
        contextMenu->addAction(replaceAction);
    }

    contextMenu->addAction(copyAction);
    contextMenu->addAction(cutAction);
    contextMenu->addAction(pasteAction);
    contextMenu->addSeparator();
    contextMenu->addAction(undoAction);
    contextMenu->addAction(redoAction);
    contextMenu->addSeparator();
    contextMenu->addAction(commentAction);
    contextMenu->addAction(uncommentAction);
    contextMenu->addAction(smartIndentAction);
    contextMenu->addSeparator();
    contextMenu->addAction(fixIssueAction);
    contextMenu->addAction(fixAllTrivialIssuesAction);
    contextMenu->addAction(suppressIssueOnLineAction);
    contextMenu->addAction(suppressIssueAboveLineAction);
}
//=============================================================================
void
QtTextEditor::createToolBars()
{
    fileToolBar = addToolBar(TR("File"));
    fileToolBar->addAction(newAction);
    fileToolBar->addAction(openAction);
    fileToolBar->addAction(saveAction);
    fileToolBar->addAction(saveAsAction);
    fileToolBar->addAction(saveAllAction);
    editToolBar = addToolBar(TR("Edit"));
    editToolBar->addAction(printAction);
    editToolBar->addSeparator();
    editToolBar->addAction(copyAction);
    editToolBar->addAction(cutAction);
    editToolBar->addAction(pasteAction);
    editToolBar->addSeparator();
    editToolBar->addAction(undoAction);
    editToolBar->addAction(redoAction);
    editToolBar->addSeparator();
    editToolBar->addAction(runFileAction);
    editToolBar->addAction(stopRunAction);
    editToolBar->addSeparator();
    editToolBar->addAction(dbStepAction);
    editToolBar->addAction(dbStepInAction);
    editToolBar->addAction(dbStepOutAction);
    editToolBar->addSeparator();
    editToolBar->addAction(fontAction);
    editToolBar->addSeparator();
    editToolBar->addAction(dockAction);
}
//=============================================================================
void
QtTextEditor::createStatusBar()
{
    statusLabel = new QLabel(this);
    statusLabel->setText(TR("Ready"));
    statusLabel->setToolTip(TR("Click to change the line endings of the current file."));
    statusLabel->installEventFilter(this);
    statusBar()->addPermanentWidget(statusLabel);
    codeAnalyzerStatusLabel = new QLabel(this);
    codeAnalyzerStatusLabel->setText(TR("Code: pending"));
    codeAnalyzerStatusLabel->setToolTip(TR("Open the Code Issues panel for details."));
    statusBar()->addPermanentWidget(codeAnalyzerStatusLabel);
    statusBar()->showMessage(TR("Ready"));
}
//=============================================================================
// expands \1 ... \9 back references (regular expression mode only)
static QString
expandReplacement(
    const QString& replacement, const QRegularExpressionMatch& match, bool useBackReferences)
{
    if (!useBackReferences || !replacement.contains(QLatin1Char('\\'))) {
        return replacement;
    }
    QString result;
    result.reserve(replacement.size());
    for (int k = 0; k < replacement.size(); ++k) {
        const QChar character = replacement.at(k);
        if ((character == QLatin1Char('\\')) && (k + 1 < replacement.size())) {
            const QChar next = replacement.at(k + 1);
            if (next.isDigit()) {
                result += match.captured(next.digitValue());
                ++k;
                continue;
            }
            if (next == QLatin1Char('\\')) {
                result += QLatin1Char('\\');
                ++k;
                continue;
            }
        }
        result += character;
    }
    return result;
}
//=============================================================================
void
QtTextEditor::createFindBar()
{
    findBar = new QWidget(this);
    findBar->setVisible(false);
    QVBoxLayout* barLayout = new QVBoxLayout(findBar);
    barLayout->setContentsMargins(4, 2, 4, 2);
    barLayout->setSpacing(2);

    QHBoxLayout* findRowLayout = new QHBoxLayout();
    findRowLayout->setSpacing(4);
    QLabel* findLabel = new QLabel(TR("Find:"), findBar);
    findEdit = new QLineEdit(findBar);
    findEdit->setClearButtonEnabled(true);
    findEdit->installEventFilter(this);
    QPushButton* findPreviousButton = new QPushButton(TR("Previous"), findBar);
    findPreviousButton->setToolTip(TR("Find Previous (Shift+F3)"));
    QPushButton* findNextButton = new QPushButton(TR("Next"), findBar);
    findNextButton->setToolTip(TR("Find Next (F3)"));
    findCaseCheck = new QCheckBox(TR("Match case"), findBar);
    findWholeWordCheck = new QCheckBox(TR("Whole word"), findBar);
    findRegexCheck = new QCheckBox(TR("Regular expression"), findBar);
    findStatusLabel = new QLabel(findBar);
    findStatusLabel->setMinimumWidth(80);
    QPushButton* closeButton = new QPushButton(TR("Close"), findBar);
    closeButton->setToolTip(TR("Close (Esc)"));
    findRowLayout->addWidget(findLabel);
    findRowLayout->addWidget(findEdit, 1);
    findRowLayout->addWidget(findPreviousButton);
    findRowLayout->addWidget(findNextButton);
    findRowLayout->addWidget(findCaseCheck);
    findRowLayout->addWidget(findWholeWordCheck);
    findRowLayout->addWidget(findRegexCheck);
    findRowLayout->addWidget(findStatusLabel);
    findRowLayout->addWidget(closeButton);

    replaceRow = new QWidget(findBar);
    QHBoxLayout* replaceRowLayout = new QHBoxLayout(replaceRow);
    replaceRowLayout->setContentsMargins(0, 0, 0, 0);
    replaceRowLayout->setSpacing(4);
    QLabel* replaceLabel = new QLabel(TR("Replace:"), replaceRow);
    replaceLabel->setMinimumWidth(findLabel->sizeHint().width());
    replaceEdit = new QLineEdit(replaceRow);
    replaceEdit->setClearButtonEnabled(true);
    replaceEdit->installEventFilter(this);
    QPushButton* replaceButton = new QPushButton(TR("Replace"), replaceRow);
    QPushButton* replaceAllButton = new QPushButton(TR("Replace All"), replaceRow);
    replaceRowLayout->addWidget(replaceLabel);
    replaceRowLayout->addWidget(replaceEdit, 1);
    replaceRowLayout->addWidget(replaceButton);
    replaceRowLayout->addWidget(replaceAllButton);
    replaceRow->setVisible(false);

    barLayout->addLayout(findRowLayout);
    barLayout->addWidget(replaceRow);

    connect(findEdit, &QLineEdit::textChanged, this, [this](const QString&) {
        lastSearch = findEdit->text();
        if (findNextAction != nullptr) {
            findNextAction->setEnabled(!lastSearch.isEmpty());
        }
        if (findPreviousAction != nullptr) {
            findPreviousAction->setEnabled(!lastSearch.isEmpty());
        }
        updateFindHighlights();
    });
    connect(findEdit, &QLineEdit::returnPressed, this, [this]() { findFromBar(false); });
    connect(replaceEdit, &QLineEdit::returnPressed, this, [this]() { replaceCurrentMatch(); });
    connect(findNextButton, &QPushButton::clicked, this, [this]() { findFromBar(false); });
    connect(findPreviousButton, &QPushButton::clicked, this, [this]() { findFromBar(true); });
    connect(replaceButton, &QPushButton::clicked, this, [this]() { replaceCurrentMatch(); });
    connect(replaceAllButton, &QPushButton::clicked, this, [this]() { replaceAllMatches(); });
    connect(closeButton, &QPushButton::clicked, this, [this]() { hideFindBar(); });
    connect(findCaseCheck, &QCheckBox::toggled, this, [this](bool checked) {
        lastSearchCaseSensitive = checked;
        updateFindHighlights();
    });
    connect(
        findWholeWordCheck, &QCheckBox::toggled, this, [this](bool) { updateFindHighlights(); });
    connect(findRegexCheck, &QCheckBox::toggled, this, [this](bool) { updateFindHighlights(); });
}
//=============================================================================
void
QtTextEditor::showFindBar(bool withReplace)
{
    if ((findBar == nullptr) || (findEdit == nullptr)) {
        return;
    }
    QtTextEdit* editor = currentEditor();
    if (editor != nullptr) {
        QTextCursor cursor = editor->textCursor();
        if (cursor.hasSelection()) {
            const QString selected = selectedTextAsPlainText(cursor);
            if (!selected.isEmpty() && !selected.contains(QLatin1Char('\n'))) {
                findEdit->setText(selected);
            }
        }
    }
    if (replaceRow != nullptr) {
        replaceRow->setVisible(withReplace);
    }
    findBar->setVisible(true);
    updateFindHighlights();
    findEdit->setFocus();
    findEdit->selectAll();
}
//=============================================================================
void
QtTextEditor::hideFindBar()
{
    if (findBar == nullptr) {
        return;
    }
    findBar->setVisible(false);
    clearFindHighlights();
    if (findStatusLabel != nullptr) {
        findStatusLabel->clear();
    }
    QtTextEdit* editor = currentEditor();
    if (editor != nullptr) {
        editor->setFocus();
    }
}
//=============================================================================
QRegularExpression
QtTextEditor::currentFindExpression(bool& isValid) const
{
    isValid = false;
    QRegularExpression expression;
    if (findEdit == nullptr) {
        return expression;
    }
    QString pattern = findEdit->text();
    if (pattern.isEmpty()) {
        return expression;
    }
    if ((findRegexCheck == nullptr) || !findRegexCheck->isChecked()) {
        pattern = QRegularExpression::escape(pattern);
    }
    if ((findWholeWordCheck != nullptr) && findWholeWordCheck->isChecked()) {
        pattern = QString("\\b(?:%1)\\b").arg(pattern);
    }
    QRegularExpression::PatternOptions options = QRegularExpression::NoPatternOption;
    if ((findCaseCheck == nullptr) || !findCaseCheck->isChecked()) {
        options |= QRegularExpression::CaseInsensitiveOption;
    }
    expression.setPattern(pattern);
    expression.setPatternOptions(options);
    isValid = expression.isValid();
    return expression;
}
//=============================================================================
void
QtTextEditor::clearFindHighlights()
{
    for (int i = 0; i < tab->count(); ++i) {
        QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
        if ((pane != nullptr) && (pane->getEditor() != nullptr)) {
            pane->getEditor()->clearSearchHighlights();
        }
    }
}
//=============================================================================
void
QtTextEditor::updateFindHighlights()
{
    if ((findBar == nullptr) || (findEdit == nullptr)) {
        return;
    }
    clearFindHighlights();
    // isHidden() is used instead of isVisible(): the bar state must not depend
    // on the visibility of the editor window itself.
    if (findBar->isHidden()) {
        if (findStatusLabel != nullptr) {
            findStatusLabel->clear();
        }
        return;
    }
    QtEditPane* pane = qobject_cast<QtEditPane*>(tab->currentWidget());
    if ((pane == nullptr) || (pane->getEditor() == nullptr)) {
        return;
    }
    if (findEdit->text().isEmpty()) {
        if (findStatusLabel != nullptr) {
            findStatusLabel->clear();
        }
        return;
    }
    bool isValid = false;
    QRegularExpression expression = currentFindExpression(isValid);
    if (!isValid) {
        if (findStatusLabel != nullptr) {
            findStatusLabel->setText(wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:text_editor:invalidExpression", L"Invalid expression")));
        }
        return;
    }
    QtTextEdit* editor = pane->getEditor();
    const QString content = editor->toPlainText();
    const QTextCursor cursor = editor->textCursor();
    const int selectionStart = cursor.selectionStart();
    const int selectionLength = cursor.selectionEnd() - cursor.selectionStart();
    const int maximumHighlights = 5000;
    QList<QPair<int, int>> ranges;
    int currentIndex = -1;
    int count = 0;
    QRegularExpressionMatchIterator iterator = expression.globalMatch(content);
    while (iterator.hasNext()) {
        const QRegularExpressionMatch match = iterator.next();
        const int start = static_cast<int>(match.capturedStart());
        const int length = static_cast<int>(match.capturedLength());
        if (length <= 0) {
            continue;
        }
        if (count < maximumHighlights) {
            ranges.append(qMakePair(start, length));
        }
        if ((start == selectionStart) && (length == selectionLength)) {
            currentIndex = count;
        }
        ++count;
    }
    editor->setSearchHighlights(ranges, currentIndex);
    if (findStatusLabel == nullptr) {
        return;
    }
    if (count == 0) {
        findStatusLabel->setText(TR("No results"));
    } else if (currentIndex >= 0) {
        findStatusLabel->setText(QString("%1 / %2").arg(currentIndex + 1).arg(count));
    } else {
        findStatusLabel->setText(TR("%1 matches").arg(count));
    }
}
//=============================================================================
bool
QtTextEditor::findFromBar(bool backward)
{
    if (findEdit == nullptr) {
        return false;
    }
    lastSearch = findEdit->text();
    if (findNextAction != nullptr) {
        findNextAction->setEnabled(!lastSearch.isEmpty());
    }
    if (findPreviousAction != nullptr) {
        findPreviousAction->setEnabled(!lastSearch.isEmpty());
    }
    if (lastSearch.isEmpty()) {
        updateFindHighlights();
        return false;
    }
    bool isValid = false;
    QRegularExpression expression = currentFindExpression(isValid);
    if (!isValid) {
        if (findStatusLabel != nullptr) {
            findStatusLabel->setText(wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:text_editor:invalidExpression", L"Invalid expression")));
        }
        return false;
    }
    QtTextEdit* editor = currentEditor();
    if (editor == nullptr) {
        return false;
    }
    QTextDocument::FindFlags flags;
    if ((findCaseCheck != nullptr) && findCaseCheck->isChecked()) {
        flags |= QTextDocument::FindCaseSensitively;
    }
    if (backward) {
        flags |= QTextDocument::FindBackward;
    }
    const QTextCursor initialCursor = editor->textCursor();
    bool found = editor->find(expression, flags);
    if (!found) {
        QTextCursor restartCursor = initialCursor;
        restartCursor.movePosition(backward ? QTextCursor::End : QTextCursor::Start);
        editor->setTextCursor(restartCursor);
        found = editor->find(expression, flags);
        if (!found) {
            editor->setTextCursor(initialCursor);
            QApplication::beep();
        }
    }
    if (found) {
        editor->ensureCursorVisible();
    }
    updateFindHighlights();
    if (!found && (findStatusLabel != nullptr)) {
        findStatusLabel->setText(TR("No results"));
    }
    return found;
}
//=============================================================================
void
QtTextEditor::replaceCurrentMatch()
{
    if ((findEdit == nullptr) || (replaceEdit == nullptr)) {
        return;
    }
    bool isValid = false;
    QRegularExpression expression = currentFindExpression(isValid);
    if (!isValid) {
        if (findStatusLabel != nullptr) {
            findStatusLabel->setText(wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:text_editor:invalidExpression", L"Invalid expression")));
        }
        return;
    }
    QtTextEdit* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    QTextCursor cursor = editor->textCursor();
    if (cursor.hasSelection()) {
        const QString selected = selectedTextAsPlainText(cursor);
        const QRegularExpressionMatch match = expression.match(selected);
        const bool wholeSelectionMatches = match.hasMatch() && (match.capturedStart() == 0)
            && (match.capturedLength() == selected.size());
        if (wholeSelectionMatches) {
            const bool useBackReferences
                = (findRegexCheck != nullptr) && findRegexCheck->isChecked();
            cursor.insertText(expandReplacement(replaceEdit->text(), match, useBackReferences));
            editor->setTextCursor(cursor);
        }
    }
    findFromBar(false);
}
//=============================================================================
void
QtTextEditor::replaceAllMatches()
{
    if ((findEdit == nullptr) || (replaceEdit == nullptr)) {
        return;
    }
    bool isValid = false;
    QRegularExpression expression = currentFindExpression(isValid);
    if (!isValid) {
        if (findStatusLabel != nullptr) {
            findStatusLabel->setText(wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:text_editor:invalidExpression", L"Invalid expression")));
        }
        return;
    }
    QtTextEdit* editor = currentEditor();
    if (editor == nullptr) {
        return;
    }
    const bool useBackReferences = (findRegexCheck != nullptr) && findRegexCheck->isChecked();
    const QString content = editor->toPlainText();
    QList<int> starts;
    QList<int> lengths;
    QStringList replacements;
    QRegularExpressionMatchIterator iterator = expression.globalMatch(content);
    while (iterator.hasNext()) {
        const QRegularExpressionMatch match = iterator.next();
        if (match.capturedLength() <= 0) {
            continue;
        }
        starts.append(static_cast<int>(match.capturedStart()));
        lengths.append(static_cast<int>(match.capturedLength()));
        replacements.append(expandReplacement(replaceEdit->text(), match, useBackReferences));
    }
    if (starts.isEmpty()) {
        if (findStatusLabel != nullptr) {
            findStatusLabel->setText(TR("No results"));
        }
        statusBar()->showMessage(TR("No results"), DEFAULT_DELAY_MSG);
        return;
    }
    // replacements are applied backwards so that the collected positions stay
    // valid, and inside a single edit block so that a single undo reverts all.
    QTextCursor cursor(editor->document());
    cursor.beginEditBlock();
    for (int k = starts.size() - 1; k >= 0; --k) {
        cursor.setPosition(starts[k]);
        cursor.setPosition(starts[k] + lengths[k], QTextCursor::KeepAnchor);
        cursor.insertText(replacements[k]);
    }
    cursor.endEditBlock();
    const int replacementCount = static_cast<int>(starts.size());
    statusBar()->showMessage(
        TR("%1 replacement(s) done.").arg(replacementCount), DEFAULT_DELAY_MSG);
    if (findStatusLabel != nullptr) {
        findStatusLabel->setText(TR("%1 replaced").arg(replacementCount));
    }
    updateFindHighlights();
}
//=============================================================================
void
QtTextEditor::createCodeIssuesPanel()
{
    QWidget* panel = new QWidget(this);
    QVBoxLayout* panelLayout = new QVBoxLayout(panel);
    QHBoxLayout* filtersLayout = new QHBoxLayout();
    codeIssuesScopeCombo = new QComboBox(panel);
    codeIssuesScopeCombo->addItem(TR("Current file"));
    codeIssuesScopeCombo->addItem(TR("Open files"));
    codeIssuesSearchEdit = new QLineEdit(panel);
    codeIssuesSearchEdit->setPlaceholderText(TR("Filter"));
    codeIssuesErrorsCheck = new QCheckBox(TR("Errors"), panel);
    codeIssuesErrorsCheck->setChecked(true);
    codeIssuesWarningsCheck = new QCheckBox(TR("Warnings"), panel);
    codeIssuesWarningsCheck->setChecked(true);
    codeIssuesInfosCheck = new QCheckBox(TR("Infos"), panel);
    codeIssuesInfosCheck->setChecked(true);
    filtersLayout->addWidget(codeIssuesScopeCombo);
    filtersLayout->addWidget(codeIssuesSearchEdit);
    filtersLayout->addWidget(codeIssuesErrorsCheck);
    filtersLayout->addWidget(codeIssuesWarningsCheck);
    filtersLayout->addWidget(codeIssuesInfosCheck);
    codeIssuesTable = new QTableWidget(panel);
    codeIssuesTable->setColumnCount(6);
    codeIssuesTable->setHorizontalHeaderLabels(QStringList()
        << TR("Severity") << TR("Check") << TR("Line") << TR("Column") << TR("Message")
        << TR("File"));
    codeIssuesTable->setEditTriggers(QAbstractItemView::NoEditTriggers);
    codeIssuesTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    codeIssuesTable->setSelectionMode(QAbstractItemView::SingleSelection);
    codeIssuesTable->horizontalHeader()->setStretchLastSection(true);
    codeIssuesTable->horizontalHeader()->setSectionResizeMode(4, QHeaderView::Stretch);
    panelLayout->addLayout(filtersLayout);
    panelLayout->addWidget(codeIssuesTable);
    codeIssuesDock = new QDockWidget(TR("Code Issues"), this);
    codeIssuesDock->setWidget(panel);
    codeIssuesDock->setAllowedAreas(
        Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea | Qt::BottomDockWidgetArea);
    addDockWidget(Qt::BottomDockWidgetArea, codeIssuesDock);
    codeIssuesDock->hide();
    connect(codeIssuesTable, SIGNAL(cellDoubleClicked(int, int)), this,
        SLOT(onCodeIssueActivated(int, int)));
    connect(codeIssuesScopeCombo, QOverload<int>::of(&QComboBox::currentIndexChanged), this,
        [this](int) { refreshCodeIssuesPanel(); });
    connect(codeIssuesSearchEdit, &QLineEdit::textChanged, this,
        [this](const QString&) { refreshCodeIssuesPanel(); });
    connect(codeIssuesErrorsCheck, &QCheckBox::toggled, this,
        [this](bool) { refreshCodeIssuesPanel(); });
    connect(codeIssuesWarningsCheck, &QCheckBox::toggled, this,
        [this](bool) { refreshCodeIssuesPanel(); });
    connect(codeIssuesInfosCheck, &QCheckBox::toggled, this,
        [this](bool) { refreshCodeIssuesPanel(); });
}
//=============================================================================
void
QtTextEditor::scheduleCodeAnalysis()
{
    ++codeAnalyzerGeneration;
    if (!codeAnalyzerEnabled) {
        if (codeAnalyzerTimer) {
            codeAnalyzerTimer->stop();
        }
        return;
    }
    if (codeAnalyzerTimer) {
        codeAnalyzerTimer->start();
    }
}
//=============================================================================
void
QtTextEditor::clearCodeAnalyzerDiagnostics()
{
    for (int i = 0; i < tab->count(); ++i) {
        QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
        if (pane) {
            pane->setCodeAnalyzerDiagnostics({});
        }
    }
}
//=============================================================================
void
QtTextEditor::analyzeCurrentTab()
{
    if (!codeAnalyzerEnabled) {
        clearCodeAnalyzerDiagnostics();
        refreshCodeIssuesPanel();
        updateCodeAnalyzerStatus();
        return;
    }
    QtEditPane* pane = qobject_cast<QtEditPane*>(tab->currentWidget());
    analyzePane(pane);
}
//=============================================================================
void
QtTextEditor::analyzePane(QtEditPane* pane)
{
    if (!pane || !pane->getEditor()) {
        return;
    }
    if (!codeAnalyzerEnabled) {
        pane->setCodeAnalyzerDiagnostics({});
        refreshCodeIssuesPanel();
        updateCodeAnalyzerStatus();
        return;
    }
    QString filename = pane->getFileName();
    if (!filename.endsWith(".m", Qt::CaseInsensitive) && filename != DEFAULT_FILENAME) {
        pane->setCodeAnalyzerDiagnostics({});
        refreshCodeIssuesPanel();
        updateCodeAnalyzerStatus();
        return;
    }
    if (codeAnalyzerWatcher && codeAnalyzerWatcher->isRunning()) {
        codeAnalyzerPending = true;
        return;
    }
    CodeAnalyzerOptions options;
    options.configFile = QStringTowstring(codeAnalyzerConfigForFile(filename));
    QString displayName = filename.isEmpty() ? QString(DEFAULT_FILENAME) : filename;
    std::string source = wstring_to_utf8(QStringTowstring(pane->getEditor()->toPlainText()));
    codeAnalyzerTargetPane = pane;
    codeAnalyzerRunningGeneration = codeAnalyzerGeneration;
    codeAnalyzerWatcher->setFuture(QtConcurrent::run([displayName, source, options]() {
        return Nelson::analyzeCodeBuffer(QStringTowstring(displayName), source, options);
    }));
}
//=============================================================================
void
QtTextEditor::refreshCodeIssuesPanel()
{
    if (!codeIssuesTable) {
        return;
    }
    QSignalBlocker blocker(codeIssuesTable);
    codeIssuesTable->setRowCount(0);
    QString filter = codeIssuesSearchEdit ? codeIssuesSearchEdit->text().trimmed() : QString();
    int firstTab = (codeIssuesScopeCombo && codeIssuesScopeCombo->currentIndex() == 1)
        ? 0
        : tab->currentIndex();
    int lastTab = (codeIssuesScopeCombo && codeIssuesScopeCombo->currentIndex() == 1)
        ? tab->count() - 1
        : tab->currentIndex();
    for (int i = firstTab; i <= lastTab; ++i) {
        QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
        if (!pane) {
            continue;
        }
        for (const auto& diagnostic : pane->getCodeAnalyzerDiagnostics()) {
            QString severity = QString::fromStdString(diagnostic.severity);
            if ((severity == "error" && codeIssuesErrorsCheck
                    && !codeIssuesErrorsCheck->isChecked())
                || (severity == "warning" && codeIssuesWarningsCheck
                    && !codeIssuesWarningsCheck->isChecked())
                || (severity == "info" && codeIssuesInfosCheck
                    && !codeIssuesInfosCheck->isChecked())) {
                continue;
            }
            QString message = QString::fromStdString(diagnostic.message);
            QString checkId = QString::fromStdString(diagnostic.checkId);
            if (!filter.isEmpty() && !message.contains(filter, Qt::CaseInsensitive)
                && !checkId.contains(filter, Qt::CaseInsensitive)) {
                continue;
            }
            int row = codeIssuesTable->rowCount();
            codeIssuesTable->insertRow(row);
            QList<QTableWidgetItem*> items;
            items << new QTableWidgetItem(severity) << new QTableWidgetItem(checkId)
                  << new QTableWidgetItem(QString::number(diagnostic.lineStart))
                  << new QTableWidgetItem(QString::number(diagnostic.columnStart))
                  << new QTableWidgetItem(message) << new QTableWidgetItem(pane->getFileName());
            for (int column = 0; column < items.size(); ++column) {
                items[column]->setData(Qt::UserRole, i);
                items[column]->setData(Qt::UserRole + 1, diagnostic.lineStart);
                items[column]->setForeground(codeAnalyzerSeverityColor(diagnostic.severity));
                codeIssuesTable->setItem(row, column, items[column]);
            }
        }
    }
    codeIssuesTable->resizeColumnsToContents();
}
//=============================================================================
void
QtTextEditor::updateCodeAnalyzerStatus()
{
    if (!codeAnalyzerStatusLabel) {
        return;
    }
    if (!codeAnalyzerEnabled) {
        codeAnalyzerStatusLabel->setText(TR("Code: disabled"));
        return;
    }
    auto diagnostics = diagnosticsForCurrentPane();
    int errors = 0;
    int warnings = 0;
    int infos = 0;
    for (const auto& diagnostic : diagnostics) {
        if (diagnostic.severity == "error") {
            ++errors;
        } else if (diagnostic.severity == "warning") {
            ++warnings;
        } else {
            ++infos;
        }
    }
    codeAnalyzerStatusLabel->setText(
        QString("Code: %1 error(s), %2 warning(s), %3 info").arg(errors).arg(warnings).arg(infos));
}
//=============================================================================
void
QtTextEditor::gotoIssueLine(int line)
{
    if (line > 0 && gotoLineNumber(line)) {
        currentEditor()->setFocus();
    }
}
//=============================================================================
std::vector<Nelson::CodeAnalyzerDiagnostic>
QtTextEditor::diagnosticsForCurrentPane() const
{
    QtEditPane* pane = qobject_cast<QtEditPane*>(tab->currentWidget());
    if (!pane) {
        return {};
    }
    return pane->getCodeAnalyzerDiagnostics();
}
//=============================================================================
Nelson::CodeAnalyzerDiagnostic
QtTextEditor::currentLineDiagnostic() const
{
    Nelson::CodeAnalyzerDiagnostic best;
    QtTextEdit* editor = const_cast<QtTextEditor*>(this)->currentEditor();
    int line = editor ? editor->textCursor().blockNumber() + 1 : 0;
    int bestRank = -1;
    for (const auto& diagnostic : diagnosticsForCurrentPane()) {
        if (diagnostic.lineStart == line) {
            int rank = codeAnalyzerSeverityRank(diagnostic.severity);
            if (rank > bestRank) {
                best = diagnostic;
                bestRank = rank;
            }
        }
    }
    return best;
}
//=============================================================================
QString
QtTextEditor::codeAnalyzerConfigForFile(const QString& filename) const
{
    QString startPath = filename.isEmpty() || filename == DEFAULT_FILENAME
        ? Nelson::wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory())
        : QFileInfo(filename).absolutePath();
    QDir directory(startPath);
    while (true) {
        QString candidate = directory.absoluteFilePath("nelson-lint.json");
        if (QFileInfo::exists(candidate)) {
            return candidate;
        }
        if (!directory.cdUp()) {
            break;
        }
    }
    return QString();
}
//=============================================================================
void
QtTextEditor::applyTrivialFix(const Nelson::CodeAnalyzerDiagnostic& diagnostic)
{
    if (!codeAnalyzerIsTrivialFix(diagnostic) || !currentEditor()) {
        return;
    }
    QString text = currentEditor()->toPlainText();
    if (diagnostic.checkId == "NLS0011") {
        if (!text.endsWith('\n')) {
            text += '\n';
            currentEditor()->setPlainText(text);
        }
        scheduleCodeAnalysis();
        return;
    }
    QStringList lines = text.split('\n', Qt::KeepEmptyParts);
    int index = diagnostic.lineStart - 1;
    if (index >= 0 && index < lines.size()) {
        while (lines[index].endsWith(' ') || lines[index].endsWith('\t')) {
            lines[index].chop(1);
        }
        currentEditor()->setPlainText(lines.join('\n'));
    }
    scheduleCodeAnalysis();
}
//=============================================================================
void
QtTextEditor::suppressDiagnostic(
    const Nelson::CodeAnalyzerDiagnostic& diagnostic, bool previousLine)
{
    if (diagnostic.checkId.empty() || !currentEditor()) {
        return;
    }
    QString text = currentEditor()->toPlainText();
    QStringList lines = text.split('\n', Qt::KeepEmptyParts);
    int index = std::max(0, diagnostic.lineStart - 1);
    QString tag = QString("%#ok<%1>").arg(QString::fromStdString(diagnostic.checkId));
    if (previousLine) {
        lines.insert(index, tag);
    } else if (index < lines.size() && !lines[index].contains("%#ok")) {
        lines[index] += " " + tag;
    }
    currentEditor()->setPlainText(lines.join('\n'));
    scheduleCodeAnalysis();
}
//=============================================================================
void
QtTextEditor::updateRecentFileActions()
{
    int numRecentFiles = std::min((int)recentFilenames.size(), (int)MAX_RECENT_FILES);
    for (int i = 0; i < numRecentFiles; ++i) {
        QString text
            = TR("&%1 %2").arg(i + 1).arg(strippedName(wstringToQString(recentFilenames[i])));
        recentFileActions[i]->setText(text);
        recentFileActions[i]->setData(wstringToQString(recentFilenames[i]));
        recentFileActions[i]->setVisible(true);
    }
    for (int j = numRecentFiles; j < MAX_RECENT_FILES; ++j) {
        recentFileActions[j]->setVisible(false);
    }
    separatorAction->setVisible(numRecentFiles > 0);
}
//=============================================================================
bool
QtTextEditor::maybeSave()
{
    if (currentEditor()->document()->isModified()) {
        QMessageBox msgWarning(this);
        msgWarning.setText(
            TR("The document %1 has been modified.\nDo you want to save your changes ?")
                .arg(shownName()));
        msgWarning.setIcon(QMessageBox::Warning);
        msgWarning.setWindowTitle(TR("Nelson"));
        msgWarning.setStandardButtons(QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel);
        msgWarning.setDefaultButton(QMessageBox::Yes);
        msgWarning.setEscapeButton(QMessageBox::Cancel);
        int ret = msgWarning.exec();
        if (ret == QMessageBox::Yes) {
            return save();
        }
        if (ret == QMessageBox::Cancel) {
            return false;
        }
    }
    return true;
}
//=============================================================================
void
QtTextEditor::openRecentFile()
{
    QAction* action = qobject_cast<QAction*>(sender());
    QString fileName = action->data().toString();
    loadFile(fileName);
}
//=============================================================================
static QString
detectLineEndingStyle(const std::string& content, bool& isMixed)
{
    size_t countCrLf = 0;
    size_t countLf = 0;
    size_t countCr = 0;
    for (size_t k = 0; k < content.size(); ++k) {
        if (content[k] == '\r') {
            if ((k + 1 < content.size()) && (content[k + 1] == '\n')) {
                ++countCrLf;
                ++k;
            } else {
                ++countCr;
            }
        } else if (content[k] == '\n') {
            ++countLf;
        }
    }
    const int styles = static_cast<int>(countCrLf > 0) + static_cast<int>(countLf > 0)
        + static_cast<int>(countCr > 0);
    isMixed = (styles > 1);
    if (styles == 0) {
        return QtEditPane::defaultEol();
    }
    if ((countCrLf >= countLf) && (countCrLf >= countCr)) {
        return QString(EOL_STYLE_CRLF);
    }
    if (countLf >= countCr) {
        return QString(EOL_STYLE_LF);
    }
    return QString(EOL_STYLE_CR);
}
//=============================================================================
bool
QtTextEditor::saveFileWithEncoding(
    const QString& filename, const QString& data, const QString& sourceEncoding)
{
    // the buffer always holds '\n' in memory: the line endings of the file are
    // restored here. Both encoding paths share the same binary writer so that
    // they produce exactly the same bytes for the same content.
    QString normalizedData = data;
    normalizedData.replace(QLatin1String("\r\n"), QLatin1String("\n"));
    normalizedData.replace(QLatin1Char('\r'), QLatin1Char('\n'));
    const QString lineEnding = QtEditPane::eolSequence(currentEol());
    if (lineEnding != QLatin1String("\n")) {
        normalizedData.replace(QLatin1String("\n"), lineEnding);
    }
    std::string outData = wstring_to_utf8(QStringTowstring(normalizedData));
    if ((sourceEncoding != "UTF-8") && !sourceEncoding.isEmpty()) {
        std::string encoding = wstring_to_utf8(QStringTowstring(sourceEncoding));
        std::string convertedData;
        if (!utf8ToCharsetConverter(outData, convertedData, encoding)) {
            return false;
        }
        outData = convertedData;
    }
    std::ofstream datFile;
#ifdef _MSC_VER
    std::wstring unicodeFilename = QStringTowstring(filename);
    datFile.open(unicodeFilename, std::ofstream::binary);
#else
    std::string utf8Filename = wstring_to_utf8(QStringTowstring(filename));
    datFile.open(utf8Filename, std::ofstream::binary);
#endif
    if (!datFile.is_open()) {
        return false;
    }
    datFile.write(outData.c_str(), outData.length());
    datFile.close();
    return true;
}
//=============================================================================
bool
QtTextEditor::saveFile(const QString& filename)
{
    Nelson::setApplicationWaitCursor();
    bool res = saveFileWithEncoding(filename, currentEditor()->toPlainText(), currentEncoding());
    Nelson::restoreApplicationWaitCursor();
    if (res) {
        setCurrentFile(filename);
        statusBar()->showMessage(TR("File saved"), DEFAULT_DELAY_MSG);
    } else {
        QMessageBox::warning(this, TR("Nelson"),
            wstringToQString(Nelson::resolveMessageTemplate(
                                 L"Nelson:text_editor:cannotWriteFile", L"Cannot write file:\n%1."))
                .arg(filename));
        statusBar()->showMessage(TR("File not saved"), DEFAULT_DELAY_MSG);
    }
    return res;
}
//=============================================================================
bool
QtTextEditor::loadFileAsUtf8(const QString& filename, QString& data, QString& sourceEncoding)
{
    std::wstring wfilename = QStringTowstring(filename);
#ifdef _MSC_VER
    std::ifstream streamAsbin(wfilename, std::ios::binary);
#else
    std::ifstream streamAsbin(wstring_to_utf8(wfilename), std::ios::binary);
#endif
    if (!streamAsbin.is_open()) {
        return false;
    }
    std::ostringstream ss;
    ss << streamAsbin.rdbuf();
    streamAsbin.close();
    std::string lines = ss.str();
    ss.clear();
    stringVector encodings = detectEncodings(lines);
    if (encodings.size() > 0) {
        std::string encoding = encodings[0];
        sourceEncoding = wstringToQString(utf8_to_wstring(encoding));
        if (encoding != "UTF-8") {
            std::string asUtf8;
            if (charsetToUtf8Converter(lines, encoding, asUtf8)) {
                lines = asUtf8;
            }
        }
    }
    // line endings are detected on the decoded content, then normalized to '\n'
    // in memory; saveFileWithEncoding() restores the original style.
    bool isMixed = false;
    lastDetectedEol = detectLineEndingStyle(lines, isMixed);
    lastDetectedEolMixed = isMixed;
    std::wstring wdata = utf8_to_wstring(lines);
    data = wstringToQString(wdata);
    data.replace(QLatin1String("\r\n"), QLatin1String("\n"));
    data.replace(QLatin1Char('\r'), QLatin1Char('\n'));
    return true;
}
//=============================================================================
void
QtTextEditor::loadFile(const QString& filename)
{
    QFile file(filename);
    if (!file.open(QFile::ReadOnly | QFile::Text)) {
        QMessageBox::warning(this, TR("Nelson"),
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:text_editor:cannotReadFile",
                                 L"Cannot read file %1:\n%2."))
                .arg(filename)
                .arg(file.errorString()));
        return;
    }
    for (int i = 0; i < tab->count(); i++) {
        QWidget* widget = tab->widget(i);
        QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
        if (editPane) {
            if (editPane->getFileName() == filename) {
                tab->setCurrentIndex(i);
                return;
            }
        }
    }
    addTabUntitled();
    if (fileWatcher.files().empty()) {
        connect(&fileWatcher, SIGNAL(fileChanged(const QString)), this,
            SLOT(reloadFile(const QString)));
    }
    fileWatcher.addPath(filename);
    Nelson::setApplicationWaitCursor();

    QString content;
    QString encoding;
    lastDetectedEol = QtEditPane::defaultEol();
    lastDetectedEolMixed = false;
    loadFileAsUtf8(filename, content, encoding);
    currentEditor()->setPlainText(content);

    Nelson::restoreApplicationWaitCursor();
    setCurrentFile(filename);
    setCurrentEncoding(encoding);
    setCurrentEol(lastDetectedEol);
    updateTitles();
    if (lastDetectedEolMixed) {
        statusBar()->showMessage(
            TR("File loaded (mixed line endings, normalized to %1).").arg(lastDetectedEol),
            DEFAULT_DELAY_MSG);
    } else {
        statusBar()->showMessage(TR("File loaded"), DEFAULT_DELAY_MSG);
    }
    if (filename.endsWith(".m")) {
        runFileAction->setEnabled(true);
    } else {
        runFileAction->setEnabled(false);
    }
    currentEditor()->setFocus();
    updateCursorPosition();
}
//=============================================================================
void
QtTextEditor::setCurrentFile(const QString& filename)
{
    setCurrentFilename(filename);
    currentEditor()->document()->setModified(false);
    setWindowModified(false);
    updateTitles();
    std::wstring name = QStringTowstring(filename);
    recentFilenames.erase(
        std::remove(recentFilenames.begin(), recentFilenames.end(), name), recentFilenames.end());
    recentFilenames.insert(recentFilenames.begin(), name);
    while (recentFilenames.size() > MAX_RECENT_FILES) {
        recentFilenames.pop_back();
    }
    writeSettings();
    foreach (QWidget* widget, QApplication::topLevelWidgets()) {
        QtTextEditor* tEditor = qobject_cast<QtTextEditor*>(widget);
        if (tEditor) {
            tEditor->updateRecentFileActions();
        }
    }
}
//=============================================================================
QString
QtTextEditor::strippedName(const QString& fullfilename)
{
    return QFileInfo(fullfilename).fileName();
}
//=============================================================================
QtTextEdit*
QtTextEditor::currentEditor()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    return editPane->getEditor();
}
//=============================================================================
void
QtTextEditor::setCurrentFilename(const QString& filename)
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    editPane->setFileName(filename);
}
//=============================================================================
QString
QtTextEditor::currentFilename()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    return editPane->getFileName();
}
//=============================================================================
void
QtTextEditor::setCurrentEncoding(const QString& encoding)
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    editPane->setEncoding(encoding);
}
//=============================================================================
QString
QtTextEditor::currentEncoding()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    return editPane->getEncoding();
}
//=============================================================================
void
QtTextEditor::setCurrentEol(const QString& eol)
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    editPane->setEol(eol);
    updateEolActions();
}
//=============================================================================
QString
QtTextEditor::currentEol()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
        widget = tab->currentWidget();
        editPane = qobject_cast<QtEditPane*>(widget);
    }
    return editPane->getEol();
}
//=============================================================================
void
QtTextEditor::changeEol(const QString& eol)
{
    if (currentEol() == eol) {
        updateEolActions();
        return;
    }
    setCurrentEol(eol);
    // the buffer itself is unchanged (always stored with '\n'), the document is
    // flagged as modified so that the new line endings can be written on save.
    if (currentEditor() != nullptr) {
        currentEditor()->document()->setModified(true);
    }
    documentWasModified();
    updateCursorPosition();
    statusBar()->showMessage(TR("Line endings: %1").arg(eol), DEFAULT_DELAY_MSG);
}
//=============================================================================
void
QtTextEditor::updateEolActions()
{
    if ((eolCrLfAction == nullptr) || (eolLfAction == nullptr) || (eolCrAction == nullptr)) {
        return;
    }
    if (tab->count() == 0) {
        return;
    }
    QtEditPane* editPane = qobject_cast<QtEditPane*>(tab->currentWidget());
    if (editPane == nullptr) {
        return;
    }
    const QString eol = editPane->getEol();
    QSignalBlocker blockCrLf(eolCrLfAction);
    QSignalBlocker blockLf(eolLfAction);
    QSignalBlocker blockCr(eolCrAction);
    eolCrLfAction->setChecked(eol == EOL_STYLE_CRLF);
    eolLfAction->setChecked(eol == EOL_STYLE_LF);
    eolCrAction->setChecked(eol == EOL_STYLE_CR);
}
//=============================================================================
QString
QtTextEditor::shownName()
{
    QString sName;
    if (currentFilename().isEmpty()) {
        sName = DEFAULT_FILENAME;
    } else {
        sName = strippedName(currentFilename());
    }
    return sName;
}
//=============================================================================
void
QtTextEditor::updateTitles()
{
    tab->setTabText(tab->currentIndex(), shownName());
    tab->setTabToolTip(
        tab->currentIndex(), currentFilename().isEmpty() ? shownName() : currentFilename());
    if (currentFilename().isEmpty()) {
        setWindowTitle(TR("Nelson Editor"));
    } else {
        setWindowTitle(QString("%1[*]").arg(currentFilename()) + " - " + TR("Nelson Editor"));
    }
    documentWasModified();
    updateCursorPosition();
}
//=============================================================================
void
QtTextEditor::readSettings()
{
    QPoint pos;
    QSize size;
    TextEditorLoadPreferences(
        m_font, pos, size, recentFilenames, debugLineColor, codeAnalyzerEnabled);
    resize(size);
    move(pos);
    updateFont();
}
//=============================================================================
void
QtTextEditor::writeSettings()
{
    // while docked, pos()/size() are dock-relative: the last top-level
    // geometry (remembered at dock time) is persisted instead.
    QPoint windowPos = isDockedInDesktop() ? m_undockedPos : pos();
    QSize windowSize = isDockedInDesktop() ? m_undockedSize : size();
    TextEditorSavePreferences(
        m_font, windowPos, windowSize, recentFilenames, debugLineColor, codeAnalyzerEnabled);
}
//=============================================================================
bool
QtTextEditor::isDockedInDesktop() const
{
    return DesktopIsWindowDocked(this);
}
//=============================================================================
void
QtTextEditor::setDockedInDesktop(bool docked)
{
    if (docked == isDockedInDesktop()) {
        return;
    }
    if (docked) {
        if (!DesktopDockingAvailable()) {
            return;
        }
        m_undockedPos = pos();
        m_undockedSize = size();
        if (!DesktopDockWindow(this, "nelsonTextEditorDock")) {
            return;
        }
    } else {
        DesktopUndockWindow(this);
#ifdef _MSC_VER
        // reparenting recreated the native window: apply the dark title bar
        forceWindowsTitleBarToDark(this->winId());
#endif
    }
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_TEXT_EDITOR_DOCKED, docked);
    updateDockActionState();
}
//=============================================================================
void
QtTextEditor::restoreDockState()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    if (settings.value(SETTING_TEXT_EDITOR_DOCKED, false).toBool() && DesktopDockingAvailable()) {
        setDockedInDesktop(true);
    }
    updateDockActionState();
}
//=============================================================================
void
QtTextEditor::updateDockActionState()
{
    if (!dockAction) {
        return;
    }
    bool docked = isDockedInDesktop();
    dockAction->setVisible(docked || DesktopDockingAvailable());
    std::wstring rootPath = Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory();
    if (docked) {
        dockAction->setIcon(QIcon(
            Nelson::wstringToQString(rootPath + std::wstring(L"/resources/undock-window.svg"))));
        dockAction->setText(TR("&Undock"));
        dockAction->setToolTip(TR("Undock the editor from the Nelson desktop"));
    } else {
        dockAction->setIcon(QIcon(
            Nelson::wstringToQString(rootPath + std::wstring(L"/resources/dock-window.svg"))));
        dockAction->setText(TR("&Dock"));
        dockAction->setToolTip(TR("Dock the editor into the Nelson desktop"));
    }
}
//=============================================================================
void
QtTextEditor::updateFont()
{
    for (int i = 0; i < tab->count(); i++) {
        QWidget* p = tab->widget(i);
        QtEditPane* te = qobject_cast<QtEditPane*>(p);
        te->setFont(m_font);
    }
}
//=============================================================================
void
QtTextEditor::reloadPreferences()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    if (settings.contains(NELSON_EDITOR_SETTING_FONT_NAME)) {
        QFont f(settings.value(NELSON_EDITOR_SETTING_FONT_NAME).toString());
        f.setPointSize(
            settings.value(NELSON_EDITOR_SETTING_FONT_SIZE, getDefaultFontSize()).toInt());
        f.setFixedPitch(settings.value(NELSON_EDITOR_SETTING_FONT_FIXED_PITCH, true).toBool());
        m_font = f;
        updateFont();
    }
    int indent
        = settings.value(NELSON_EDITOR_SETTING_INDENT_SIZE, TextEditorGetIndentSize()).toInt();
    TextEditorSetIndentSize(indent);
    setCodeAnalyzerEnabled(
        settings.value(NELSON_EDITOR_SETTING_CODE_ANALYZER_ENABLED, true).toBool());
}
//=============================================================================
QColor
QtTextEditor::getDebugLineColor()
{
    return debugLineColor;
}
//=============================================================================
void
QtTextEditor::setDebugLineColor(const QColor& color)
{
    debugLineColor = color;
    for (int i = 0; i < tab->count(); i++) {
        QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
        if (pane && pane->getEditor()) {
            pane->getEditor()->setDebugLineColor(color);
        }
    }
}
//=============================================================================
bool
QtTextEditor::saveAll()
{
    int backupIndex = tab->currentIndex();
    for (int i = 0; i < tab->count(); i++) {
        tab->setCurrentIndex(i);
        if (currentEditor()->document()->isModified()) {
            save();
        }
    }
    tab->setCurrentIndex(backupIndex);
    return true;
}
//=============================================================================
bool
QtTextEditor::save()
{
    bool res = false;
    if (!currentFilename().isEmpty()) {
        lastFilenameSaved = currentFilename();
        res = saveFile(lastFilenameSaved);
    } else {
        res = saveAs();
    }
    return res;
}
//=============================================================================
bool
QtTextEditor::saveAs()
{
    bool res = false;
    QString fileName = QFileDialog::getSaveFileName(this, TR("Save File"),
        qtDialogInitialSavePath(L"desktop.save", shownName()), QString("Nelson (*.m)"));
    if (!fileName.isEmpty()) {
        qtRecordDialogSelection(L"desktop.save", QStringLiteral("save"), fileName);
        for (int i = 0; i < tab->count(); i++) {
            QWidget* w = tab->widget(i);
            QtEditPane* te = qobject_cast<QtEditPane*>(w);
            if (te) {
                if ((te->getFileName() == fileName) && (i != tab->currentIndex())) {
                    QMessageBox::critical(this, TR("Nelson"),
                        "Cannot save to filename\n " + fileName
                            + "\n as this file is open in another tab.\n  Please close the other "
                              "tab and\n then repeat the save operation.");
                    tab->setCurrentIndex(i);
                    return false;
                }
            }
        }
        res = saveFile(fileName);
    } else {
        res = false;
    }
    return res;
}
//=============================================================================
void
QtTextEditor::open()
{
    if (maybeSave()) {
        QString textFilesText = TR("Text files");
        QString markdowFilesText = TR("Markdown files");
        QString allFilesText = TR("All files");

        QString filesSupported = QString("Nelson (*.m);;") + textFilesText + QString("  (*.txt);;")
            + markdowFilesText + QString(" (*.md);;") + allFilesText + QString(" (*.*)");

        QStringList fileNames = QFileDialog::getOpenFileNames(
            this, TR("Open file ..."), qtDialogInitialDirectory(L"desktop.open"), filesSupported);
        if (!fileNames.isEmpty()) {
            qtRecordDialogSelection(L"desktop.open", QStringLiteral("open"), fileNames.first());
        }
        for (auto& fileName : fileNames) {
            loadFile(fileName);
        }
    }
}
//=============================================================================
QtTextEditor::~QtTextEditor()
{
    saveAll();
    if (contextMenu) {
        delete contextMenu;
        contextMenu = nullptr;
    }
}
//=============================================================================
void
QtTextEditor::font()
{
    bool bOk = false;
    QFont new_font = QFontDialog::getFont(&bOk, m_font, this);
    m_font = new_font;
    updateFont();
}
//=============================================================================
void
QtTextEditor::addTabUntitled()
{
    QWidget* widget = tab->currentWidget();
    QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
    if (!editPane) {
        addTab();
    } else {
        bool foundActive = false;
        for (int i = 0; i < tab->count(); i++) {
            if (tab->tabText(i) == DEFAULT_FILENAME) {
                foundActive = true;
                tab->setCurrentIndex(i);
                break;
            }
        }
        if (!foundActive) {
            addTab();
        }
    }
}
//=============================================================================
void
QtTextEditor::createTabUntitledWithText(const QString& text)
{
    addTabUntitled();
    currentEditor()->setPlainText(text);
}
//=============================================================================
void
QtTextEditor::addTab()
{
    QtEditPane* editPane = new QtEditPane(nlsEvaluator);
    editPane->setFileName(DEFAULT_FILENAME);
    connect(editPane->getBreakpointArea(), &QtBreakpointArea::breakpointToggled, this,
        [this](int, bool) {
            if (breakpointPanel) {
                breakpointPanel->refreshBreakpoints();
            }
            if (nlsEvaluator) {
                lastBreakpointCount = nlsEvaluator->getBreakpoints().size();
            }
            for (int i = 0; i < tab->count(); ++i) {
                QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
                if (pane) {
                    pane->refreshBreakpoints();
                }
            }
        });
    connect(editPane->getCodeAnalyzerMarkerArea(), &QtCodeAnalyzerMarkerArea::issueMarkerClicked,
        this, &QtTextEditor::gotoIssueLine);
    tab->addTab(editPane, DEFAULT_FILENAME);
    tab->setTabToolTip(tab->count() - 1, DEFAULT_FILENAME);
    tab->setCurrentIndex(tab->count() - 1);
    updateFont();
    scheduleCodeAnalysis();
}
//=============================================================================
bool
QtTextEditor::eventFilter(QObject* watched, QEvent* event)
{
    if (((watched == findEdit) || (watched == replaceEdit))
        && (event->type() == QEvent::KeyPress)) {
        QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
        if (keyEvent->key() == Qt::Key_Escape) {
            hideFindBar();
            return true;
        }
        if (((keyEvent->key() == Qt::Key_Return) || (keyEvent->key() == Qt::Key_Enter))
            && ((keyEvent->modifiers() & Qt::ShiftModifier) != 0)) {
            findFromBar(true);
            return true;
        }
    }
    if ((watched == statusLabel) && (event->type() == QEvent::MouseButtonRelease)
        && (eolMenu != nullptr)) {
        QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
        if (mouseEvent->button() == Qt::LeftButton) {
            updateEolActions();
            eolMenu->exec(mouseEvent->globalPosition().toPoint());
            return true;
        }
    }
    if (tab && (watched == tab->tabBar()) && (event->type() == QEvent::MouseButtonRelease)) {
        QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
        if (mouseEvent->button() == Qt::MiddleButton) {
            int indexTab = tab->tabBar()->tabAt(mouseEvent->position().toPoint());
            if (indexTab != -1) {
                closeTabAt(indexTab);
                return true;
            }
        }
    }
    return QMainWindow::eventFilter(watched, event);
}
//=============================================================================
bool
QtTextEditor::closeTabAt(int indexTab)
{
    if ((indexTab < 0) || (indexTab >= tab->count())) {
        return true;
    }
    if (tab->currentIndex() != indexTab) {
        tab->setCurrentIndex(indexTab);
    }
    QString filename = currentFilename();
    if (!maybeSave()) {
        return false;
    }
    QWidget* p = tab->widget(indexTab);
    tab->removeTab(indexTab);
    if (p) {
        p->deleteLater();
    }
    prevEdit = nullptr;
    if (!filename.isEmpty()) {
        fileWatcher.removePath(filename);
    }
    return true;
}
//=============================================================================
void
QtTextEditor::closeTab()
{
    closeTabAt(tab->currentIndex());
}
//=============================================================================
void
QtTextEditor::closeAllTabs()
{
    // bounded by the initial tab count: never trust the loop to make the
    // counter go down, closing a tab can create one back.
    int remaining = tab->count();
    while ((tab->count() > 0) && (remaining > 0)) {
        if (!closeTabAt(tab->count() - 1)) {
            return;
        }
        remaining--;
    }
    setWindowTitle(TR("Nelson Editor"));
    QStringList watchedFiles = fileWatcher.files();
    if (!watchedFiles.isEmpty()) {
        fileWatcher.removePaths(watchedFiles);
    }
}
//=============================================================================
void
QtTextEditor::closeTab(int indexTab)
{
    closeTabAt(indexTab);
}
//=============================================================================
void
QtTextEditor::tabChanged(int indexTab)
{
    // removing the last tab emits currentChanged(-1): nothing must be rebuilt
    // here, currentEditor() would create a brand new tab to honour the request
    // and the closing loops would never terminate.
    if ((indexTab < 0) || (tab->count() == 0)) {
        prevEdit = nullptr;
        return;
    }
    disconnect(cutAction, SIGNAL(triggered()), nullptr, nullptr);
    disconnect(pasteAction, SIGNAL(triggered()), nullptr, nullptr);
    disconnect(copyAction, SIGNAL(triggered()), nullptr, nullptr);
    connect(cutAction, SIGNAL(triggered()), currentEditor(), SLOT(cut()));
    connect(copyAction, SIGNAL(triggered()), currentEditor(), SLOT(copy()));
    connect(pasteAction, SIGNAL(triggered()), currentEditor(), SLOT(paste()));
    if (prevEdit) {
        disconnect(prevEdit->document(), SIGNAL(contentsChanged()), nullptr, nullptr);
    }
    connect(
        currentEditor()->document(), SIGNAL(contentsChanged()), this, SLOT(documentWasModified()));
    connect(currentEditor(), SIGNAL(cursorPositionChanged()), this, SLOT(updateCursorPosition()));
    connect(currentEditor()->document(), SIGNAL(undoAvailable(bool)), undoAction,
        SLOT(setEnabled(bool)));
    connect(currentEditor()->document(), SIGNAL(redoAvailable(bool)), redoAction,
        SLOT(setEnabled(bool)));
    undoAction->setEnabled(currentEditor()->document()->isUndoAvailable());
    redoAction->setEnabled(currentEditor()->document()->isRedoAvailable());
    updateTitles();
    prevEdit = currentEditor();
    QString filename = currentFilename();
    if (filename.endsWith(".m") || filename.isEmpty()) {
        runFileAction->setEnabled(true);
    } else {
        runFileAction->setEnabled(false);
    }
    refreshCodeIssuesPanel();
    updateCodeAnalyzerStatus();
    scheduleCodeAnalysis();
    updateEolActions();
    updateFindHighlights();
}
//=============================================================================
void
QtTextEditor::documentWasModified()
{
    setWindowModified(currentEditor()->document()->isModified());
    if (currentEditor()->document()->isModified()) {
        QString fileNameIcon = Nelson::wstringToQString(
            textEditorRootPath + std::wstring(L"/resources/document-modified.svg"));
        tab->setTabIcon(tab->currentIndex(), QIcon(fileNameIcon));
        tab->setTabText(tab->currentIndex(), shownName() + "*" + " (" + currentEncoding() + ")");
    } else {
        QString fileNameIcon = Nelson::wstringToQString(
            textEditorRootPath + std::wstring(L"/resources/document-new.svg"));
        tab->setTabIcon(tab->currentIndex(), QIcon(fileNameIcon));
        tab->setTabText(tab->currentIndex(), shownName());
    }
    scheduleCodeAnalysis();
    if ((findBar != nullptr) && !findBar->isHidden()) {
        // occurrences must follow the edits done while the bar is open
        updateFindHighlights();
    }
}
//=============================================================================
void
QtTextEditor::onCodeIssueActivated(int row, int column)
{
    Q_UNUSED(column);
    if (!codeIssuesTable || row < 0 || row >= codeIssuesTable->rowCount()) {
        return;
    }
    QTableWidgetItem* item = codeIssuesTable->item(row, 0);
    if (!item) {
        return;
    }
    int tabIndex = item->data(Qt::UserRole).toInt();
    int line = item->data(Qt::UserRole + 1).toInt();
    if (tabIndex >= 0 && tabIndex < tab->count()) {
        tab->setCurrentIndex(tabIndex);
    }
    gotoIssueLine(line);
}
//=============================================================================
void
QtTextEditor::setCodeAnalyzerEnabled(bool enabled)
{
    codeAnalyzerEnabled = enabled;
    if (enableCodeAnalyzerAction && enableCodeAnalyzerAction->isChecked() != enabled) {
        QSignalBlocker blocker(enableCodeAnalyzerAction);
        enableCodeAnalyzerAction->setChecked(enabled);
    }
    fixIssueAction->setEnabled(enabled);
    fixAllTrivialIssuesAction->setEnabled(enabled);
    suppressIssueOnLineAction->setEnabled(enabled);
    suppressIssueAboveLineAction->setEnabled(enabled);
    if (enabled) {
        scheduleCodeAnalysis();
    } else {
        if (codeAnalyzerTimer) {
            codeAnalyzerTimer->stop();
        }
        ++codeAnalyzerGeneration;
        codeAnalyzerPending = false;
        codeAnalyzerTargetPane.clear();
        clearCodeAnalyzerDiagnostics();
    }
    refreshCodeIssuesPanel();
    updateCodeAnalyzerStatus();
    writeSettings();
}
//=============================================================================
void
QtTextEditor::fixIssue()
{
    applyTrivialFix(currentLineDiagnostic());
}
//=============================================================================
void
QtTextEditor::fixAllTrivialIssues()
{
    if (!currentEditor()) {
        return;
    }
    QString text = currentEditor()->toPlainText();
    QStringList lines = text.split('\n', Qt::KeepEmptyParts);
    bool changed = false;
    for (const auto& diagnostic : diagnosticsForCurrentPane()) {
        if (diagnostic.checkId == "NLS0010") {
            int index = diagnostic.lineStart - 1;
            if (index >= 0 && index < lines.size()) {
                while (lines[index].endsWith(' ') || lines[index].endsWith('\t')) {
                    lines[index].chop(1);
                    changed = true;
                }
            }
        } else if (diagnostic.checkId == "NLS0011" && !text.endsWith('\n')) {
            text += '\n';
            changed = true;
        }
    }
    QString fixedText = lines.join('\n');
    if (text.endsWith('\n') && !fixedText.endsWith('\n')) {
        fixedText += '\n';
    }
    if (changed) {
        currentEditor()->setPlainText(fixedText);
        scheduleCodeAnalysis();
    }
}
//=============================================================================
void
QtTextEditor::suppressIssueOnLine()
{
    suppressDiagnostic(currentLineDiagnostic(), false);
}
//=============================================================================
void
QtTextEditor::suppressIssueAboveLine()
{
    suppressDiagnostic(currentLineDiagnostic(), true);
}
//=============================================================================
void
QtTextEditor::updateCursorPosition()
{
    if (!statusLabel) {
        return;
    }
    int line = getCurrentLineNumber();
    QTextCursor cursor = currentEditor()->textCursor();
    int col = cursor.positionInBlock() + 1;
    QString enc = currentEncoding();
    if (enc.isEmpty()) {
        enc = "UTF-8";
    }
    const QString eol = currentEol();
    statusLabel->setText(QString("Ln %1, Col %2 | %3 | %4").arg(line).arg(col).arg(enc).arg(eol));
    updateEolActions();
}
//=============================================================================
void
QtTextEditor::closeEvent(QCloseEvent* event)
{
    if (!isVisible()) {
        return;
    }
    writeSettings();
    int remaining = tab->count();
    while ((tab->count() > 0) && (remaining > 0)) {
        if (!closeTabAt(tab->count() - 1)) {
            event->ignore();
            return;
        }
        remaining--;
    }
    event->accept();
}
//=============================================================================
void
QtTextEditor::contextMenuEvent(QContextMenuEvent* event)
{
    // actions are kept visible (they also live in menus owning their shortcut),
    // only their enabled state depends on the current selection.
    QTextCursor cur = currentEditor()->textCursor();
    bool hasSelection = cur.hasSelection();

    if (helpOnSelectionAction) {
        helpOnSelectionAction->setEnabled(hasSelection);
    }
    if (evaluateSelectionAction) {
        evaluateSelectionAction->setEnabled(hasSelection);
    }

    if (cutAction) {
        cutAction->setEnabled(hasSelection);
    }
    if (copyAction) {
        copyAction->setEnabled(hasSelection);
    }

    QClipboard* Clipboard = QApplication::clipboard();
    QString clipboardText = Clipboard->text();
    if (pasteAction) {
        pasteAction->setEnabled(!clipboardText.isEmpty());
    }

    if (currentEditor()) {
        undoAction->setEnabled(currentEditor()->document()->isUndoAvailable());
        redoAction->setEnabled(currentEditor()->document()->isRedoAvailable());
    }

    if (findAction != nullptr) {
        findAction->setEnabled(currentEditor() != nullptr);
    }
    if (findNextAction != nullptr) {
        findNextAction->setEnabled(!lastSearch.isEmpty());
    }
    if (findPreviousAction != nullptr) {
        findPreviousAction->setEnabled(!lastSearch.isEmpty());
    }

    contextMenu->exec(event->globalPos());
}
//=============================================================================
void
QtTextEditor::copyFullPath()
{
    QClipboard* clipboard = QApplication::clipboard();
    if (!currentFilename().isEmpty()) {
        QString filename = currentFilename();
#ifdef _MSC_VER
        filename.replace("/", "\\");
#endif
        clipboard->setText(filename);
    } else {
        clipboard->setText(DEFAULT_FILENAME);
    }
}
//=============================================================================
void
QtTextEditor::undo()
{
    currentEditor()->undo();
}
//=============================================================================
void
QtTextEditor::redo()
{
    currentEditor()->redo();
}
//=============================================================================
void
QtTextEditor::comment()
{
    currentEditor()->comment();
}
//=============================================================================
void
QtTextEditor::uncomment()
{
    currentEditor()->uncomment();
}
//=============================================================================
void
QtTextEditor::onExportToAction()
{
    QString defaultFilePath
        = qtDialogInitialSavePath(L"desktop.save", QString(), QStringLiteral("document"));

    QString exportTypeMessage = TR("Export to PDF ...");
    QString filePath = QFileDialog::getSaveFileName(
        this, exportTypeMessage, defaultFilePath, TR("PDF Files (*.pdf)"));

    if (filePath.isEmpty()) {
        return;
    }
    qtRecordDialogSelection(L"desktop.save", QStringLiteral("save"), filePath);

    QString fileExtension = QFileInfo(filePath).suffix();
    exportToPdf(filePath);
}
//=============================================================================
void
QtTextEditor::exportToPdf(const QString& filename)
{
    QPrinter printer;
    printer.setOutputFormat(QPrinter::PdfFormat);
    printer.setOutputFileName(filename);
    printer.setPageSize(QPageSize(QPageSize::A4));
    printer.setPageOrientation(QPageLayout::Portrait);
    currentEditor()->print(&printer);
}
//=============================================================================
int
QtTextEditor::getCurrentLineNumber()
{
    return currentEditor()->textCursor().blockNumber() + 1;
}
//=============================================================================
bool
QtTextEditor::gotoLineNumber(int lineNumber)
{
    QTextCursor text_cursor(currentEditor()->document()->findBlockByLineNumber(lineNumber - 1));
    text_cursor.movePosition(QTextCursor::StartOfLine);
    currentEditor()->setTextCursor(text_cursor);
    currentEditor()->ensureCursorVisible();
    return true;
}
//=============================================================================
void
QtTextEditor::gotoLine()
{
    bool ok;
    int line_number
        = QInputDialog::getInt(this, TR("Go To Line ..."), TR("Enter a line number to go to: "), 1,
            1, currentEditor()->document()->blockCount(), 1, &ok);
    if (ok) {
        gotoLineNumber(line_number);
    }
}
//=============================================================================
void
QtTextEditor::runFile()
{
    if (nlsEvaluator->isBreakpointActive()) {
        dbContinue();
        return;
    }

    if (nlsEvaluator->getInterface()->isAtPrompt()) {
        if (currentEditor()->document()->isModified() || currentEditor()->document()->isEmpty()) {
            save();
        }
        std::wstring filename = QStringTowstring(currentFilename());
        postCommand(std::wstring(L"run('") + filename + std::wstring(L"')"));
        checkDebugState();
    } else {
        QMessageBox::warning(this,
            Nelson::resolveMessageTemplate("Nelson:text_editor:runFile", "Run file ...").c_str(),
            Nelson::resolveMessageTemplate(
                "Nelson:text_editor:interpreterCurrentlyRuns", "Interpreter currently runs.")
                .c_str());
    }
}
//=============================================================================
void
QtTextEditor::stopRun()
{
    if (nlsEvaluator->isBreakpointActive()) {
        nlsEvaluator->addCommandToQueue(L"dbquit", true);
    } else if (!nlsEvaluator->getInterface()->isAtPrompt()) {
        NelsonConfiguration::getInstance()->setInterruptPending(true, nlsEvaluator->getID());
    }
}
//=============================================================================
void
QtTextEditor::dbStep()
{
    if (nlsEvaluator->isBreakpointActive()) {
        nlsEvaluator->addCommandToQueue(L"dbstep", true);
    }
}
//=============================================================================
void
QtTextEditor::dbStepIn()
{
    if (nlsEvaluator->isBreakpointActive()) {
        nlsEvaluator->addCommandToQueue(L"dbstep in", true);
    }
}
//=============================================================================
void
QtTextEditor::dbStepOut()
{
    if (nlsEvaluator->isBreakpointActive()) {
        nlsEvaluator->addCommandToQueue(L"dbstep out", true);
    }
}
//=============================================================================
void
QtTextEditor::dbContinue()
{
    if (nlsEvaluator->isBreakpointActive()) {
        nlsEvaluator->addCommandToQueue(L"dbcont", true);
    }
}
//=============================================================================
void
QtTextEditor::toggleBreakpoint()
{
    QtEditPane* editPane = qobject_cast<QtEditPane*>(tab->currentWidget());
    if (!editPane) {
        return;
    }
    QtBreakpointArea* breakpointArea = editPane->getBreakpointArea();
    if (!breakpointArea) {
        return;
    }
    breakpointArea->toggleBreakpoint(getCurrentLineNumber());
}
//=============================================================================
void
QtTextEditor::checkDebugState()
{
    if (!nlsEvaluator) {
        return;
    }

    bool currentDebugActive = nlsEvaluator->isBreakpointActive();
    bool atPrompt = nlsEvaluator->getInterface()->isAtPrompt();
    std::wstring currentFilename;
    size_t currentLine = 0;

    if (currentDebugActive && nlsEvaluator->stepBreakpoint) {
        currentFilename = nlsEvaluator->stepBreakpoint.value().filename;
        currentLine = nlsEvaluator->stepBreakpoint.value().line;
    }

    size_t currentBreakpointCount = nlsEvaluator->getBreakpoints().size();
    bool breakpointsChanged = (currentBreakpointCount != lastBreakpointCount);
    if (breakpointsChanged) {
        lastBreakpointCount = currentBreakpointCount;
        breakpointPanel->refreshBreakpoints();
        for (int i = 0; i < tab->count(); ++i) {
            QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
            if (pane) {
                pane->refreshBreakpoints();
            }
        }
    }

    bool stateChanged = (currentDebugActive != lastDebugState)
        || (currentFilename != lastDebugFilename) || (currentLine != lastDebugLine);

    if (stateChanged) {
        lastDebugState = currentDebugActive;
        lastDebugFilename = currentFilename;
        lastDebugLine = currentLine;
        updateDebugLineHighlight();

        if (currentDebugActive) {
            debugStackPanel->refreshStack();
        } else {
            debugStackPanel->clear();
        }
    }

    stopRunAction->setEnabled(currentDebugActive || !atPrompt);

    dbStepAction->setEnabled(currentDebugActive && atPrompt);
    dbStepInAction->setEnabled(currentDebugActive && atPrompt);
    dbStepOutAction->setEnabled(currentDebugActive && atPrompt);
    dbContinueAction->setEnabled(currentDebugActive && atPrompt);

    // Run file button doubles as Continue when stopped in debugger
    if (currentDebugActive) {
        runFileAction->setText(TR("&Continue"));
        QString fileNameIcon = Nelson::wstringToQString(
            textEditorRootPath + std::wstring(L"/resources/debug-continue.svg"));
        runFileAction->setIcon(QIcon(fileNameIcon));
        runFileAction->setToolTip(TR("Continue execution (F5)"));
        runFileAction->setEnabled(true);
    } else if (atPrompt) {
        runFileAction->setText(TR("&Run file"));
        QString fileNameIcon = Nelson::wstringToQString(
            textEditorRootPath + std::wstring(L"/resources/run-file-start.svg"));
        runFileAction->setIcon(QIcon(fileNameIcon));
        runFileAction->setToolTip(TR("Run file"));
        runFileAction->setEnabled(true);
    } else {
        runFileAction->setEnabled(false);
    }
}
//=============================================================================
void
QtTextEditor::helpOnSelection()
{
    QString selectedText = selectedTextAsPlainText(currentEditor()->textCursor());
    selectedText = selectedText.trimmed();
    if (selectedText.startsWith('\'') && selectedText.endsWith('\'')) {
        selectedText.chop(1);
        selectedText.remove(0, 1);
    }
    if (!selectedText.isEmpty()) {
        std::wstring text = QStringTowstring(selectedText);
        StringHelpers::replace_all(text, L"'", L"\"");
        std::wstring cmd = L"doc('" + text + L"');";
        postCommand(cmd);
    }
}
//=============================================================================
void
QtTextEditor::smartIndent()
{
    const int indentSize = TextEditorGetIndentSize();
    QTextCursor cursor(currentEditor()->textCursor());
    QTextCursor cursorBackup(currentEditor()->textCursor());
    bool noTextSelected = false;
    if (cursor.selectedText().isEmpty()) {
        noTextSelected = true;
    }
    if (noTextSelected) {
        currentEditor()->selectAll();
        cursor = currentEditor()->textCursor();
    }
    try {
        ::smartIndent(currentEditor(), indentSize);
    } catch (const Nelson::NelsonException& e) {
        // the formatter refuses unbalanced code: report it instead of letting
        // the exception cross the Qt event loop.
        currentEditor()->setTextCursor(cursorBackup);
        currentEditor()->setFocus();
        statusBar()->showMessage(
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:text_editor:smartIndentFailed",
                                 L"Smart Indent failed: %1"))
                .arg(wstringToQString(e.what())),
            DEFAULT_DELAY_MSG);
        return;
    } catch (const std::exception& e) {
        currentEditor()->setTextCursor(cursorBackup);
        currentEditor()->setFocus();
        statusBar()->showMessage(
            wstringToQString(Nelson::resolveMessageTemplate(L"Nelson:text_editor:smartIndentFailed",
                                 L"Smart Indent failed: %1"))
                .arg(QString::fromUtf8(e.what())),
            DEFAULT_DELAY_MSG);
        return;
    } catch (...) {
        currentEditor()->setTextCursor(cursorBackup);
        currentEditor()->setFocus();
        statusBar()->showMessage(
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:text_editor:smartIndentFailedUnknown", L"Smart Indent failed.")),
            DEFAULT_DELAY_MSG);
        return;
    }
    if (noTextSelected) {
        currentEditor()->setTextCursor(cursorBackup);
        currentEditor()->setFocus();
    }
}
//=============================================================================
void
QtTextEditor::printDocument()
{
    QPrinter printer;
    printer.setPageSize(QPageSize(QPageSize::A4));
    printer.setPageOrientation(QPageLayout::Portrait);
    QPrintPreviewDialog* printPreview = nullptr;
    try {
        printPreview = new QPrintPreviewDialog(
            &printer, this, Qt::WindowCloseButtonHint | Qt::WindowMaximizeButtonHint);
    } catch (std::bad_alloc&) {
        printPreview = nullptr;
    }
    if (printPreview != nullptr) {
        connect(printPreview, SIGNAL(paintRequested(QPrinter*)), this, SLOT(print(QPrinter*)));
        printPreview->exec();
        delete printPreview;
    }
}
//=============================================================================
void
QtTextEditor::print(QPrinter* p)
{
    currentEditor()->print(p);
}
//=============================================================================
void
QtTextEditor::evaluateSelection()
{
    QString selectedText = selectedTextAsPlainText(currentEditor()->textCursor());
    selectedText = selectedText.trimmed();
    if (selectedText.startsWith('\'') && selectedText.endsWith('\'')) {
        selectedText.chop(1);
        selectedText.remove(0, 1);
    }
    if (!selectedText.isEmpty()) {
        std::wstring text = QStringTowstring(selectedText);
        postCommand(text);
    }
}
//=============================================================================
void
QtTextEditor::dragEnterEvent(QDragEnterEvent* event)
{
    if (event->mimeData()->hasUrls() || event->mimeData()->hasText()) {
        event->acceptProposedAction();
    } else {
        event->ignore();
    }
}
//=============================================================================
void
QtTextEditor::dropEvent(QDropEvent* event)
{
    if (event->mimeData()->hasUrls()) {
        QList<QUrl> urls = event->mimeData()->urls();
        for (const QUrl& url : urls) {
            if (!url.isLocalFile()) {
                continue;
            }
            QString local = url.toLocalFile();
            if (!local.isEmpty()) {
                loadOrCreateFile(local);
            }
        }
        event->acceptProposedAction();
        return;
    }

    if (event->mimeData()->hasText()) {
        QString text = event->mimeData()->text().trimmed();
        if (!text.isEmpty()) {
            QString normalized = text;
            normalized.replace("\r\n", "\n");
            normalized.replace('\r', '\n');
            const QStringList parts = normalized.split('\n', Qt::SkipEmptyParts);
            for (const QString& part : parts) {
                QString candidate = part.trimmed();
                if (candidate.startsWith("file://")) {
                    QUrl u(candidate);
                    candidate = u.toLocalFile();
                }
                if (!candidate.isEmpty()) {
                    loadOrCreateFile(candidate);
                }
            }
            event->acceptProposedAction();
            return;
        }
    }

    event->ignore();
}
//=============================================================================
void
QtTextEditor::reloadFile(const QString filenameModified)
{
    for (int i = 0; i < tab->count(); i++) {
        QWidget* widget = tab->widget(i);
        QtEditPane* editPane = qobject_cast<QtEditPane*>(widget);
        if (editPane) {
            if (editPane->getFileName() == filenameModified) {
                if (lastFilenameSaved == filenameModified) {
                    filesModifiedMessageDisplayedList.removeAll(filenameModified);
                    return;
                }
                if (filesModifiedMessageDisplayedList.contains(filenameModified)) {
                    return;
                }
                filesModifiedMessageDisplayedList.append(filenameModified);
                if (QMessageBox::question(this, TR("Nelson"),
                        TR("File %1 was modified by an external software.\nDo you want to reopen "
                           "it?")
                            .arg(filenameModified),
                        QMessageBox::Yes | QMessageBox::Default, QMessageBox::No)
                    == QMessageBox::No) {
                    filesModifiedMessageDisplayedList.removeAll(filenameModified);
                    return;
                }
                filesModifiedMessageDisplayedList.removeAll(filenameModified);
                tab->setCurrentIndex(i);
                closeTab();
                loadFile(filenameModified);
                return;
            }
        }
    }
}
//=============================================================================
void
QtTextEditor::showFindDialog()
{
    showFindBar(false);
}
//=============================================================================
void
QtTextEditor::findPrevious()
{
    if ((findEdit != nullptr) && findEdit->text().isEmpty() && !lastSearch.isEmpty()) {
        findEdit->setText(lastSearch);
    }
    if ((findEdit == nullptr) || findEdit->text().isEmpty()) {
        showFindBar(false);
        return;
    }
    findFromBar(true);
}
//=============================================================================
void
QtTextEditor::findNext()
{
    if ((findEdit != nullptr) && findEdit->text().isEmpty() && !lastSearch.isEmpty()) {
        findEdit->setText(lastSearch);
    }
    if ((findEdit == nullptr) || findEdit->text().isEmpty()) {
        showFindBar(false);
        return;
    }
    findFromBar(false);
}
//=============================================================================
void
QtTextEditor::updateDebugLineHighlight()
{
    for (int i = 0; i < tab->count(); ++i) {
        QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
        if (pane) {
            pane->clearDebugLine();
            pane->clearExecutionLine();
        }
    }

    if (nlsEvaluator && nlsEvaluator->isBreakpointActive() && nlsEvaluator->stepBreakpoint) {
        const Breakpoint& bp = nlsEvaluator->stepBreakpoint.value();
        QString debugFilename = wstringToQString(bp.filename);
        int debugLine = static_cast<int>(bp.line);

        QFileInfo debugFileInfo(debugFilename);
        QString debugAbsolutePath = debugFileInfo.absoluteFilePath();

        bool found = false;
        for (int i = 0; i < tab->count(); ++i) {
            QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
            if (pane) {
                QFileInfo openFileInfo(pane->getFileName());
                QString openAbsolutePath = openFileInfo.absoluteFilePath();

                if (openAbsolutePath == debugAbsolutePath) {
                    pane->setDebugLine(debugLine);
                    pane->setExecutionLine(debugLine);
                    tab->setCurrentIndex(i);
                    if (pane->getEditor()) {
                        QTextBlock block
                            = pane->getEditor()->document()->findBlockByLineNumber(debugLine - 1);
                        if (block.isValid()) {
                            QTextCursor cursor(block);
                            pane->getEditor()->setTextCursor(cursor);
                            pane->getEditor()->ensureCursorVisible();
                        }
                    }
                    found = true;
                    break;
                }
            }
        }

        if (!found && !debugFilename.isEmpty() && QFile::exists(debugAbsolutePath)) {
            loadFile(debugAbsolutePath);
            for (int i = 0; i < tab->count(); ++i) {
                QtEditPane* pane = qobject_cast<QtEditPane*>(tab->widget(i));
                if (pane) {
                    QFileInfo openFileInfo(pane->getFileName());
                    QString openAbsolutePath = openFileInfo.absoluteFilePath();

                    if (openAbsolutePath == debugAbsolutePath) {
                        pane->setDebugLine(debugLine);
                        pane->setExecutionLine(debugLine);
                        tab->setCurrentIndex(i);
                        if (pane->getEditor()) {
                            QTextBlock block = pane->getEditor()->document()->findBlockByLineNumber(
                                debugLine - 1);
                            if (block.isValid()) {
                                QTextCursor cursor(block);
                                pane->getEditor()->setTextCursor(cursor);
                                pane->getEditor()->ensureCursorVisible();
                            }
                        }
                        break;
                    }
                }
            }
        }
    }
}
//=============================================================================
