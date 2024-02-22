//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QMimeData>
#include <QtCore/QSettings>
#include <QtCore/QtGlobal>
#include <QtGui/QClipboard>
#include <QtGui/QCloseEvent>
#include <QtGui/QDesktopServices>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QVBoxLayout>
#include "QtMainWindow.h"
#include "PostCommand.hpp"
#include "Nelson_VERSION.h"
#include "QStringConverter.hpp"
#include "QtTerminal.h"
#include "QtTranslation.hpp"
#include "UiGetDirectory.hpp"
#include "characters_encoding.hpp"
#include "NelsonPalette.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "Evaluator.hpp"
#include "QtWorkspaceBrowser.h"
#include "QtHistoryBrowser.h"
#include "QtFileBrowser.h"
#include "HistoryBrowser.hpp"
#include "FileBrowser.hpp"
#include "WorkspaceBrowser.hpp"
#include "nlsBuildConfig.h"
//=============================================================================
using namespace Nelson;
//=============================================================================
const QString SETTING_MAIN_WINDOW_GEOMETRY = "mw_Geometry";
const QString SETTING_MAIN_WINDOW_Window_State = "mw_WindowState";
//=============================================================================
QtMainWindow::~QtMainWindow()
{
    if (runAction) {
        runAction->deleteLater();
        runAction = nullptr;
    }
    if (loadWorkspaceAction) {
        loadWorkspaceAction->deleteLater();
        loadWorkspaceAction = nullptr;
    }
    if (saveWorkspaceAction) {
        saveWorkspaceAction->deleteLater();
        saveWorkspaceAction = nullptr;
    }
    if (pwdAction) {
        delete pwdAction;
        pwdAction = nullptr;
    }
    if (exitAction) {
        delete exitAction;
        exitAction = nullptr;
    }
    if (aboutAction) {
        aboutAction->deleteLater();
        aboutAction = nullptr;
    }
    if (helpAction) {
        helpAction->deleteLater();
        helpAction = nullptr;
    }
    if (cutAction) {
        cutAction->deleteLater();
        cutAction = nullptr;
    }
    if (copyAction) {
        copyAction->deleteLater();
        copyAction = nullptr;
    }
    if (pasteAction) {
        pasteAction->deleteLater();
        pasteAction = nullptr;
    }
    if (selectAllAction) {
        selectAllAction->deleteLater();
        selectAllAction = nullptr;
    }
    if (emptyClipboardAction) {
        emptyClipboardAction->deleteLater();
        emptyClipboardAction = nullptr;
    }
    if (clearConsoleAction) {
        clearConsoleAction->deleteLater();
        clearConsoleAction = nullptr;
    }
    if (editMenu) {
        editMenu->deleteLater();
        editMenu = nullptr;
    }
    if (fileMenu) {
        fileMenu->deleteLater();
        fileMenu = nullptr;
    }
    if (helpMenu) {
        helpMenu->deleteLater();
        helpMenu = nullptr;
    }
    if (mainMenuBar) {
        mainMenuBar->deleteLater();
        mainMenuBar = nullptr;
    }
    if (windowsMenu) {
        windowsMenu->deleteLater();
        windowsMenu = nullptr;
    }
    if (qtTerminal) {
        qtTerminal = nullptr;
    }
}
//=============================================================================
QtMainWindow::QtMainWindow(bool minimized)
    : nelsonPath(
        Nelson::wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()))
{
    QWidget* widget = new QWidget;
    setCentralWidget(widget);
    QWidget* topFiller = new QWidget;
    topFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QWidget* bottomFiller = new QWidget;
    bottomFiller->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    QVBoxLayout* layout = new QVBoxLayout;
    layout->setContentsMargins(5, 5, 5, 5);
    layout->addWidget(topFiller);
    layout->addWidget(bottomFiller);
    layout->setMenuBar(this->menuBar());
    widget->setLayout(layout);
    qtTerminal = new QtTerminal(this);
    setFocusProxy(qtTerminal);
    setCentralWidget(qtTerminal);
    statusBar()->showMessage("");
    setPalette(getNelsonPalette());
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    createMenus();
    createToolbars();
    setWindowTitle(TR("Nelson"));
    setMinimumSize(640, 480);
    resize(840, 600);
    // https://bugreports.qt.io/browse/QTBUG-76354
    if (minimized) {
        showMinimized();
    } else {
        show();
    }
    qtTerminal->show();
    qApp->processEvents(QEventLoop::WaitForMoreEvents);
    bClosed = false;
    QString fileNameIcon = nelsonPath + "/resources/fibonacci.ico";
    QIcon icon(fileNameIcon);
    setWindowIcon(icon);
    setAcceptDrops(true);
}
//=============================================================================
void
QtMainWindow::runFile()
{
    if (qtTerminal) {
        if (!qtTerminal->isAtPrompt()) {
            QMessageBox::warning(
                this, _("Execute...").c_str(), _("Interpreter currently runs.").c_str());
        } else {
            QString qfileName = QFileDialog::getOpenFileName(
                this, TR("Execute..."), QDir::currentPath(), QString("Nelson (*.m)"));
            if (!qfileName.isEmpty()) {
                std::wstring filename = Nelson::QStringTowstring(qfileName);
                qtTerminal->outputMessage(L"\n");
                qtTerminal->sendReturnKey();
                std::wstring cmd = L"run('" + filename + L"')";
                postCommand(cmd);
            }
        }
    }
}
//=============================================================================
void
QtMainWindow::loadWorkspace()
{
    if (qtTerminal) {
        QString qfileName = QFileDialog::getOpenFileName(
            this, TR("Load workspace..."), QDir::currentPath(), TR("Nelson (*.nh5 *.mat)"));
        if (!qfileName.isEmpty()) {
            std::wstring filename = Nelson::QStringTowstring(qfileName);
            qtTerminal->outputMessage(L"\n");
            qtTerminal->sendReturnKey();
            std::wstring cmd = L"load('" + filename + L"')";
            postCommand(cmd);
        }
    }
}
//=============================================================================
void
QtMainWindow::saveWorkspace()
{
    if (qtTerminal) {
        QString qfileName = QFileDialog::getSaveFileName(this, TR("Save workspace..."),
            QDir::currentPath(), TR("Nelson (*.nh5);;MAT files (*.mat)"));
        if (!qfileName.isEmpty()) {
            std::wstring filename = Nelson::QStringTowstring(qfileName);
            qtTerminal->outputMessage(L"\n");
            qtTerminal->sendReturnKey();
            std::wstring cmd = L"save('" + filename + L"')";
            postCommand(cmd);
        }
    }
}
//=============================================================================
void
QtMainWindow::about()
{
    std::string version
        = std::string(NELSON_PRODUCT_NAME) + " " + std::string(NELSON_VERSION_STRING);
    std::string aboutText = version + "\n" + "Copyright 2016-present Allan CORNET";
    try {
        QMessageBox* msgBox = new QMessageBox(this);
#ifdef _MSC_VER
        forceWindowsTitleBarToDark(msgBox->winId());
#endif
        msgBox->about(this, _("About Nelson...").c_str(), aboutText.c_str());
        delete msgBox;
    } catch (std::bad_alloc&) {
    }
}
//=============================================================================
void
QtMainWindow::website()
{
    QString link = wstringToQString(NelsonConfiguration::getInstance()->getWebsiteUrl());
    QDesktopServices::openUrl(QUrl(link));
}
//=============================================================================
void
QtMainWindow::bugAndRequest()
{
    QString link = wstringToQString(NelsonConfiguration::getInstance()->getBugTrackerUrl());
    QDesktopServices::openUrl(QUrl(link));
}
//=============================================================================
void
QtMainWindow::checkUpdate()
{
    postCommand(L"checkupdate()");
}
//=============================================================================
void
QtMainWindow::help()
{
    postCommand(L"doc");
}
//=============================================================================
void
QtMainWindow::cutText()
{
    if (qtTerminal) {
        qtTerminal->cut();
    }
}
//=============================================================================
void
QtMainWindow::copyText()
{
    if (qtTerminal) {
        qtTerminal->copy();
    }
}
//=============================================================================
void
QtMainWindow::pasteText()
{
    if (qtTerminal) {
        qtTerminal->paste();
    }
}
//=============================================================================
void
QtMainWindow::selectAllText()
{
    if (qtTerminal) {
        qtTerminal->selectAll();
    }
}
//=============================================================================
void
QtMainWindow::emptyClipboard()
{
    QClipboard* Clipboard = QApplication::clipboard();
    Clipboard->clear(QClipboard::Clipboard);
}
//=============================================================================
void
QtMainWindow::clearConsole()
{
    if (qtTerminal) {
        qtTerminal->clearTerminal();
    }
}
//=============================================================================
void
QtMainWindow::pwdDisplay()
{
    postCommand(L"disp(pwd())");
}
//=============================================================================
void
QtMainWindow::changeDir()
{
    std::wstring pathSelected;
    bool bCanceled = UiGetDirectory(L"", L"", pathSelected);
    if (!bCanceled) {
        QDir::setCurrent(wstringToQString(pathSelected));
    }
}
//=============================================================================
void
QtMainWindow::textEditor()
{
    postCommand(L"edit()");
}
//=============================================================================
void
QtMainWindow::historyBrowserToggle()
{
    postCommand(L"commandhistory('visible', 'toggle')");
}
//=============================================================================
void
QtMainWindow::fileBrowserToggle()
{
    postCommand(L"filebrowser('visible', 'toggle')");
}
//=============================================================================
void
QtMainWindow::workspaceBrowserToggle()
{
    postCommand(L"workspace('visible', 'toggle')");
}
//=============================================================================
void
QtMainWindow::createDockWigdets(Context* context)
{
    FileBrowser::createFileBrowser();
    HistoryBrowser::createHistoryBrowser();
    WorkspaceBrowser::createWorkspaceBrowser(context);

    QtWorkspaceBrowser* qtWorkspaceBrowser
        = (QtWorkspaceBrowser*)WorkspaceBrowser::getWorkspaceBrowser();
    QtHistoryBrowser* qtHistoryBrowser = (QtHistoryBrowser*)HistoryBrowser::getHistoryBrowser();
    QtFileBrowser* qtFileBrowser = (QtFileBrowser*)FileBrowser::getFileBrowser();

    addDockWidget(Qt::LeftDockWidgetArea, (QDockWidget*)qtFileBrowser);
    addDockWidget(Qt::RightDockWidgetArea, (QDockWidget*)qtWorkspaceBrowser);
    addDockWidget(Qt::RightDockWidgetArea, (QDockWidget*)qtHistoryBrowser);

    connect(
        qtWorkspaceBrowser, SIGNAL(closeWorkspaceBrowser()), this, SLOT(onCloseWorkspaceBrowser()));
    connect(qtFileBrowser, SIGNAL(closeFileBrowser()), this, SLOT(onCloseFileBrowser()));
    connect(qtHistoryBrowser, SIGNAL(closeHistoryBrowser()), this, SLOT(onCloseHistoryBrowser()));

    restoreDockWidgetPositions();

    qtWorkspaceBrowser->restoreVisibility();
    qtHistoryBrowser->restoreVisibility();
    qtFileBrowser->restoreVisibility();

    workspaceBrowserAction->setChecked(WorkspaceBrowser::isWorkspaceBrowserVisible());
    historyBrowserAction->setChecked(HistoryBrowser::isHistoryBrowserVisible());
    fileBrowserAction->setChecked(FileBrowser::isFileBrowserVisible());
}
//=============================================================================
void
QtMainWindow::destroyDockWigdets()
{
    saveDockWidgetPositions();
    FileBrowser::destroyFileBrowser();
    HistoryBrowser::destroyHistoryBrowser();
    WorkspaceBrowser::destroyWorkspaceBrowser();
}
//=============================================================================
void
QtMainWindow::saveDockWidgetPositions()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    settings.setValue(SETTING_MAIN_WINDOW_GEOMETRY, saveGeometry());
    settings.setValue(SETTING_MAIN_WINDOW_Window_State, saveState());
}
//=============================================================================
void
QtMainWindow::restoreDockWidgetPositions()
{
    QSettings settings(NELSON_PRODUCT_NAME, NELSON_SEMANTIC_VERSION_STRING);
    restoreGeometry(settings.value(SETTING_MAIN_WINDOW_GEOMETRY).toByteArray());
    restoreState(settings.value(SETTING_MAIN_WINDOW_Window_State).toByteArray());
}
//=============================================================================
void
QtMainWindow::createToolbars()
{
    toolBar = addToolBar(TR("Text editor"));
    toolBar->addAction(editorAction);
    toolBar->addAction(runAction);
    toolBar->addAction(chdirAction);
    toolBar->addSeparator();
    toolBar->addAction(cutAction);
    toolBar->addAction(copyAction);
    toolBar->addAction(pasteAction);
    toolBar->addSeparator();
    toolBar->addAction(helpAction);
    toolBar->addSeparator();
}
//=============================================================================
void
QtMainWindow::createMenus()
{
    QString fileNameIcon;
    mainMenuBar = this->menuBar();
    mainMenuBar->setNativeMenuBar(false);
    fileMenu = mainMenuBar->addMenu(TR("&File"));
    // File menu
    fileNameIcon = nelsonPath + QString("/resources/file-run.svg");
    runAction = new QAction(QIcon(fileNameIcon), TR("&Execute..."), this);
    runAction->setShortcut(QKeySequence("Ctrl+E"));
    runAction->setStatusTip(TR("Execute a .m file"));
    connect(runAction, SIGNAL(triggered()), this, SLOT(runFile()));
    fileMenu->addAction(runAction);
    // separator
    fileMenu->addSeparator();
    // load workspace
    fileNameIcon = nelsonPath + QString("/resources/load-workspace.svg");
    loadWorkspaceAction = new QAction(QIcon(fileNameIcon), TR("&Load Workspace..."), this);
    loadWorkspaceAction->setShortcut(QKeySequence("Ctrl+L"));
    loadWorkspaceAction->setStatusTip(TR("Load a .nh5 file"));
    connect(loadWorkspaceAction, SIGNAL(triggered()), this, SLOT(loadWorkspace()));
    fileMenu->addAction(loadWorkspaceAction);
    // save workspace
    fileNameIcon = nelsonPath + QString("/resources/save-workspace.svg");
    saveWorkspaceAction = new QAction(QIcon(fileNameIcon), TR("&Save Workspace..."), this);
    saveWorkspaceAction->setShortcut(QKeySequence("Ctrl+S"));
    saveWorkspaceAction->setStatusTip(TR("Save a .nh5 file"));
    connect(saveWorkspaceAction, SIGNAL(triggered()), this, SLOT(saveWorkspace()));
    fileMenu->addAction(saveWorkspaceAction);
    // separator
    fileMenu->addSeparator();
    // chdir
    fileNameIcon = nelsonPath + QString("/resources/folder-open.svg");
    chdirAction = new QAction(QIcon(fileNameIcon), TR("&Change current directory"), this);
    chdirAction->setStatusTip(TR("Change current directory"));
    connect(chdirAction, SIGNAL(triggered()), this, SLOT(changeDir()));
    fileMenu->addAction(chdirAction);
    // pwd
    pwdAction = new QAction(TR("&Display current directory"), this);
    pwdAction->setStatusTip(TR("Display current directory"));
    connect(pwdAction, SIGNAL(triggered()), this, SLOT(pwdDisplay()));
    fileMenu->addAction(pwdAction);
    // separator
    fileMenu->addSeparator();
    // exit
    fileNameIcon = nelsonPath + QString("/resources/exit.svg");
    exitAction = new QAction(QIcon(fileNameIcon), TR("E&xit"), this);
    exitAction->setShortcuts(QKeySequence::Quit);
    exitAction->setStatusTip(TR("Exit the application"));
    connect(exitAction, SIGNAL(triggered()), this, SLOT(close()));
    fileMenu->addAction(exitAction);
    //
    // Edit menu
    editMenu = mainMenuBar->addMenu(TR("&Edit"));
    // cut
    fileNameIcon = nelsonPath + QString("/resources/edit-cut.svg");
    cutAction = new QAction(QIcon(fileNameIcon), TR("C&ut"), this);
    cutAction->setShortcuts(QKeySequence::Cut);
    cutAction->setStatusTip(TR("Cut"));
    connect(cutAction, SIGNAL(triggered()), this, SLOT(cutText()));
    editMenu->addAction(cutAction);
    // copy
    fileNameIcon = nelsonPath + QString("/resources/edit-copy.svg");
    copyAction = new QAction(QIcon(fileNameIcon), TR("&Copy"), this);
    copyAction->setShortcuts(QKeySequence::Copy);
    copyAction->setStatusTip(TR("Copy"));
    connect(copyAction, SIGNAL(triggered()), this, SLOT(copyText()));
    editMenu->addAction(copyAction);
    // paste
    fileNameIcon = nelsonPath + QString("/resources/edit-paste.svg");
    pasteAction = new QAction(QIcon(fileNameIcon), TR("&Paste"), this);
    pasteAction->setShortcuts(QKeySequence::Paste);
    pasteAction->setStatusTip(TR("Paste"));
    connect(pasteAction, SIGNAL(triggered()), this, SLOT(pasteText()));
    editMenu->addAction(pasteAction);
    // separator
    editMenu->addSeparator();
    // select all
    fileNameIcon = nelsonPath + QString("/resources/edit-select-all.svg");
    selectAllAction = new QAction(QIcon(fileNameIcon), TR("&Select all"), this);
    selectAllAction->setStatusTip(TR("Select all"));
    connect(selectAllAction, SIGNAL(triggered()), this, SLOT(selectAllText()));
    editMenu->addAction(selectAllAction);
    // separator
    editMenu->addSeparator();
    // empty clipboard
    fileNameIcon = nelsonPath + QString("/resources/clipboard-empty.svg");
    emptyClipboardAction = new QAction(QIcon(fileNameIcon), TR("&Empty clipboard"), this);
    emptyClipboardAction->setStatusTip(TR("Empty clipboard"));
    connect(emptyClipboardAction, SIGNAL(triggered()), this, SLOT(emptyClipboard()));
    editMenu->addAction(emptyClipboardAction);
    // separator
    editMenu->addSeparator();
    // clear console
    fileNameIcon = nelsonPath + QString("/resources/console-clear.svg");
    clearConsoleAction = new QAction(QIcon(fileNameIcon), TR("Clear c&onsole"), this);
    clearConsoleAction->setStatusTip(TR("Clear console"));
    connect(clearConsoleAction, SIGNAL(triggered()), this, SLOT(clearConsole()));
    editMenu->addAction(clearConsoleAction);

    // editor
    fileNameIcon = nelsonPath + QString("/resources/textedit-icon.svg");
    editorAction = new QAction(QIcon(fileNameIcon), TR("&Text editor"), this);
    editorAction->setStatusTip(TR("Text editor"));
    connect(editorAction, SIGNAL(triggered()), this, SLOT(textEditor()));

    // Windows Menu
    windowsMenu = mainMenuBar->addMenu(TR("&Windows"));
    fileNameIcon = nelsonPath + QString("/resources/layout-default.png");
    layoutMenu = windowsMenu->addMenu(QIcon(fileNameIcon), TR("&Layout"));

    fileNameIcon = nelsonPath + QString("/resources/layout-default.png");
    layoutDefaultAction = new QAction(QIcon(fileNameIcon), TR("Default"), this);
    connect(layoutDefaultAction, SIGNAL(triggered()), this, SLOT(onLayoutDefaultAction()));
    layoutMenu->addAction(layoutDefaultAction);

    fileNameIcon = nelsonPath + QString("/resources/layout-terminal-only.png");
    layoutTerminalOnlyAction = new QAction(QIcon(fileNameIcon), TR("&Command line only"), this);
    connect(layoutTerminalOnlyAction, SIGNAL(triggered()), this, SLOT(onLayoutTerminalOnly()));
    layoutMenu->addAction(layoutTerminalOnlyAction);

    fileNameIcon = nelsonPath + QString("/resources/layout-two-columns.png");
    layoutTwoColumnsAction = new QAction(QIcon(fileNameIcon), TR("&Two columns"), this);
    connect(layoutTwoColumnsAction, SIGNAL(triggered()), this, SLOT(onLayoutTwoColumnsAction()));
    layoutMenu->addAction(layoutTwoColumnsAction);

    fileNameIcon = nelsonPath + QString("/resources/appointment-new.svg");
    historyBrowserAction = new QAction(QIcon(fileNameIcon), TR("&History browser"), this);
    historyBrowserAction->setStatusTip(TR("History browser"));
    historyBrowserAction->setCheckable(true);
    connect((QDockWidget*)(HistoryBrowser::getHistoryBrowser()), &QDockWidget::visibilityChanged,
        this, [=](bool visible) { historyBrowserAction->setChecked(visible); });
    connect(historyBrowserAction, SIGNAL(triggered()), this, SLOT(historyBrowserToggle()));
    windowsMenu->addAction(historyBrowserAction);

    fileNameIcon = nelsonPath + QString("/resources/folder.svg");
    fileBrowserAction = new QAction(QIcon(fileNameIcon), TR("&File browser"), this);
    fileBrowserAction->setStatusTip(TR("File browser"));
    fileBrowserAction->setCheckable(true);
    connect((QDockWidget*)(FileBrowser::getFileBrowser()), &QDockWidget::visibilityChanged, this,
        [=](bool visible) { fileBrowserAction->setChecked(visible); });
    connect(fileBrowserAction, SIGNAL(triggered()), this, SLOT(fileBrowserToggle()));
    windowsMenu->addAction(fileBrowserAction);

    fileNameIcon = nelsonPath + QString("/resources/spreadsheet.svg");
    workspaceBrowserAction = new QAction(QIcon(fileNameIcon), TR("&Workspace browser"), this);
    workspaceBrowserAction->setStatusTip(TR("Workspace browser"));
    workspaceBrowserAction->setCheckable(true);
    connect((QDockWidget*)(WorkspaceBrowser::getWorkspaceBrowser()),
        &QDockWidget::visibilityChanged, this,
        [=](bool visible) { workspaceBrowserAction->setChecked(visible); });
    connect(workspaceBrowserAction, SIGNAL(triggered()), this, SLOT(workspaceBrowserToggle()));
    windowsMenu->addAction(workspaceBrowserAction);

    windowsMenu->addSeparator();
    windowsMenu->addAction(editorAction);

    // Help menu
    helpMenu = mainMenuBar->addMenu(TR("&Help"));
    // documentation
    fileNameIcon = nelsonPath + QString("/resources/help-icon.svg");
    helpAction = new QAction(QIcon(fileNameIcon), TR("&Documentation"), this);
    helpAction->setShortcuts(QKeySequence::HelpContents);
    helpAction->setStatusTip(TR("Documentation"));
    connect(helpAction, SIGNAL(triggered()), this, SLOT(help()));
    helpMenu->addAction(helpAction);
    // website
    fileNameIcon = nelsonPath + QString("/resources/fibonacci.png");
    webAction = new QAction(QIcon(fileNameIcon), TR("Nelson &website"), this);
    webAction->setStatusTip(TR("Nelson website"));
    connect(webAction, SIGNAL(triggered()), this, SLOT(website()));
    helpMenu->addAction(webAction);
    // bugs and requests
    fileNameIcon = nelsonPath + QString("/resources/bug-icon.svg");
    bugAction = new QAction(QIcon(fileNameIcon), TR("B&ugs and Requests"), this);
    bugAction->setStatusTip(TR("Bugs and Requests"));
    connect(bugAction, SIGNAL(triggered()), this, SLOT(bugAndRequest()));
    helpMenu->addAction(bugAction);

#ifdef WITH_WEBTOOLS_MODULE
    // check update
    fileNameIcon = nelsonPath + QString("/resources/system-software-update.svg");
    checkUpdateAction = new QAction(QIcon(fileNameIcon), TR("&Check for updates"), this);
    checkUpdateAction->setStatusTip(TR("Check for updates"));
    connect(checkUpdateAction, SIGNAL(triggered()), this, SLOT(checkUpdate()));
    helpMenu->addAction(checkUpdateAction);
#endif

    // about
    fileNameIcon = nelsonPath + QString("/resources/information-icon.svg");
    aboutAction = new QAction(QIcon(fileNameIcon), TR("&About"), this);
    aboutAction->setStatusTip(TR("About"));
    connect(aboutAction, SIGNAL(triggered()), this, SLOT(about()));
    helpMenu->addAction(aboutAction);
}
//=============================================================================
void
QtMainWindow::closeEvent(QCloseEvent* event)
{
    if (!qtTerminal->isAtPrompt()) {
        if (!bClosed) {
            QMessageBox::StandardButton reply
                = QMessageBox::question(this, _("Close confirmation").c_str(),
                    _("Are you sure to quit?").c_str(), QMessageBox::Yes | QMessageBox::No);
            if (reply == QMessageBox::No) {
                event->ignore();
                return;
            }
        }
    }
    event->ignore();
    postCommand(L"exit");
}
//=============================================================================
QtTerminal*
QtMainWindow::getQtTerminal()
{
    return qtTerminal;
}
//=============================================================================
void
QtMainWindow::dragEnterEvent(QDragEnterEvent* event)
{
    event->mimeData()->hasFormat("text/uri-list") ? event->accept() : event->ignore();
}
//=============================================================================
void
QtMainWindow::dropEvent(QDropEvent* event)
{
    if (event->mimeData()->hasFormat("text/uri-list")) {
        QList<QUrl> urls = event->mimeData()->urls();
        for (int k = 0; k < urls.size(); k++) {
            QFileInfo qmake(QString(urls[k].toLocalFile()));
            if (!urls.isEmpty()) {
                if (qmake.suffix() == "nls") {
                    if (!qtTerminal->isAtPrompt()) {
                        QMessageBox::warning(this, _("Execute...").c_str(),
                            _("Interpreter currently runs.").c_str());
                    } else {
                        std::wstring filename = Nelson::QStringTowstring(urls[k].toLocalFile());
                        qtTerminal->outputMessage(L"\n");
                        qtTerminal->sendReturnKey();
                        std::wstring cmd = L"run('" + filename + L"')";
                        postCommand(cmd);
                    }
                } else if (qmake.suffix() == "nlf") {
                    postCommand(
                        L"edit('" + Nelson::QStringTowstring(urls[k].toLocalFile()) + L"')");
                }
            }
        }
        event->accept();
    } else {
        event->ignore();
    }
}
//=============================================================================
void
QtMainWindow::declareAsClosed()
{
    bClosed = true;
    destroyDockWigdets();
}
//=============================================================================
void
QtMainWindow::onCloseWorkspaceBrowser()
{
    workspaceBrowserAction->setChecked(false);
}
//=============================================================================
void
QtMainWindow::onCloseHistoryBrowser()
{
    historyBrowserAction->setChecked(false);
}
//=============================================================================
void
QtMainWindow::onCloseFileBrowser()
{
    fileBrowserAction->setChecked(false);
}
//=============================================================================
void
QtMainWindow::onLayoutTerminalOnly()
{
    QtWorkspaceBrowser* qtWorkspaceBrowser
        = (QtWorkspaceBrowser*)WorkspaceBrowser::getWorkspaceBrowser();
    QtHistoryBrowser* qtHistoryBrowser = (QtHistoryBrowser*)HistoryBrowser::getHistoryBrowser();
    QtFileBrowser* qtFileBrowser = (QtFileBrowser*)FileBrowser::getFileBrowser();

    qtWorkspaceBrowser->hide();
    qtWorkspaceBrowser->setFloating(false);
    qtFileBrowser->hide();
    qtFileBrowser->setFloating(false);
    qtHistoryBrowser->hide();
    qtHistoryBrowser->setFloating(false);

    historyBrowserAction->setChecked(false);
    fileBrowserAction->setChecked(false);
    workspaceBrowserAction->setChecked(false);

    saveDockWidgetPositions();
}
//=============================================================================
void
QtMainWindow::onLayoutDefaultAction()
{
    QtWorkspaceBrowser* qtWorkspaceBrowser
        = (QtWorkspaceBrowser*)WorkspaceBrowser::getWorkspaceBrowser();
    QtHistoryBrowser* qtHistoryBrowser = (QtHistoryBrowser*)HistoryBrowser::getHistoryBrowser();
    QtFileBrowser* qtFileBrowser = (QtFileBrowser*)FileBrowser::getFileBrowser();

    qtWorkspaceBrowser->hide();
    qtWorkspaceBrowser->setFloating(false);
    qtFileBrowser->hide();
    qtFileBrowser->setFloating(false);
    qtHistoryBrowser->hide();
    qtHistoryBrowser->setFloating(false);

    addDockWidget(Qt::LeftDockWidgetArea, (QDockWidget*)qtFileBrowser);
    addDockWidget(Qt::RightDockWidgetArea, (QDockWidget*)qtWorkspaceBrowser);
    addDockWidget(Qt::RightDockWidgetArea, (QDockWidget*)qtHistoryBrowser);

    qtWorkspaceBrowser->show();
    qtFileBrowser->show();
    qtHistoryBrowser->show();

    historyBrowserAction->setChecked(true);
    fileBrowserAction->setChecked(true);
    workspaceBrowserAction->setChecked(true);

    saveDockWidgetPositions();
}
//=============================================================================
void
QtMainWindow::onLayoutTwoColumnsAction()
{
    QtWorkspaceBrowser* qtWorkspaceBrowser
        = (QtWorkspaceBrowser*)WorkspaceBrowser::getWorkspaceBrowser();
    QtHistoryBrowser* qtHistoryBrowser = (QtHistoryBrowser*)HistoryBrowser::getHistoryBrowser();
    QtFileBrowser* qtFileBrowser = (QtFileBrowser*)FileBrowser::getFileBrowser();

    qtWorkspaceBrowser->hide();
    qtWorkspaceBrowser->setFloating(false);
    qtFileBrowser->hide();
    qtFileBrowser->setFloating(false);
    qtHistoryBrowser->hide();
    qtHistoryBrowser->setFloating(false);

    addDockWidget(Qt::LeftDockWidgetArea, (QDockWidget*)qtFileBrowser);
    addDockWidget(Qt::LeftDockWidgetArea, (QDockWidget*)qtWorkspaceBrowser);
    addDockWidget(Qt::LeftDockWidgetArea, (QDockWidget*)qtHistoryBrowser);

    qtWorkspaceBrowser->show();
    qtFileBrowser->show();
    qtHistoryBrowser->show();

    historyBrowserAction->setChecked(true);
    fileBrowserAction->setChecked(true);
    workspaceBrowserAction->setChecked(true);

    saveDockWidgetPositions();
}
//=============================================================================
