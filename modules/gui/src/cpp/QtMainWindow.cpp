//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QDir>
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QMimeData>
#include <QtCore/QPointer>
#include <QtCore/QSettings>
#include <QtCore/QStringList>
#include <QtCore/QSysInfo>
#include <QtCore/QtGlobal>
#include <QtGui/QClipboard>
#include <QtGui/QCloseEvent>
#include <QtGui/QDesktopServices>
#include <QtGui/QDragLeaveEvent>
#include <QtGui/QGuiApplication>
#include <QtGui/QScreen>
#include <QtGui/QAction>
#include <QtGui/QActionGroup>
#include <QtGui/QShortcut>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QInputDialog>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMessageBox>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QStatusBar>
#include "CurrentDirectoryWidget.h"
#include "DesktopCommandPalette.h"
#include "DesktopPanelRail.h"
#include "DesktopUiStyle.h"
#include "QtMainWindow.h"
#include "PostCommand.hpp"
#include "RecentDirectories.hpp"
#include "QtFileDialogHistory.hpp"
#include "Nelson_VERSION.h"
#include "NelsonSettingsScope.hpp"
#include "QStringConverter.hpp"
#include "QtTerminal.h"
#include "QtPreferencesDialog.h"
#include "QtTranslation.hpp"
#include "UiGetDirectory.hpp"
#include "characters_encoding.hpp"
#include "NelsonPalette.hpp"
#include "NelsonConfiguration.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "ModulesManager.hpp"
#include "i18n.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "Evaluator.hpp"
#include "QtWorkspaceBrowser.h"
#include "QtHistoryBrowser.h"
#include "QtFileBrowser.h"
#include "QtVariablesEditor.h"
#include "QtProfilerBrowser.h"
#include "QtNewsfeedBrowser.h"
#include "QtDockTitleBar.h"
#include "DesktopDocking.hpp"
#include "DesktopActionBadge.hpp"
#include "PackageUpdatesCount.hpp"
#include "HistoryBrowser.hpp"
#include "FileBrowser.hpp"
#include "WorkspaceBrowser.hpp"
#include "VariablesEditor.hpp"
#include "nlsBuildConfig.h"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
const QString SETTING_MAIN_WINDOW_GEOMETRY = "mw_Geometry";
const QString SETTING_MAIN_WINDOW_Window_State = "mw_WindowState";
const QString SETTING_DESKTOP_SIDEBAR_VISIBLE = "desktop_sidebar_visible";
const QString SETTING_DESKTOP_STATUS_BAR_VISIBLE = "desktop_status_bar_visible";
//=============================================================================
static void
setActionTips(QAction* action, const QString& tip);
// user saved layouts: one key per layout name, value = QMainWindow::saveState()
const QString SETTING_MAIN_WINDOW_USER_LAYOUTS_GROUP = "mw_UserLayouts";
//=============================================================================
QtMainWindow::~QtMainWindow()
{
    // Sever focusChanged before the base QMainWindow destructor deletes the dock
    // widgets: otherwise clearing a child's focus during teardown re-enters
    // updateActivePanel(), which walks a half-destroyed widget tree and crashes.
    shuttingDown = true;
    disconnect(focusChangedConnection);
    if (statusBarTimer) {
        statusBarTimer->stop();
        statusBarTimer = nullptr;
    }
    if (stopAction) {
        stopAction->deleteLater();
        stopAction = nullptr;
    }
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
    if (preferencesAction) {
        delete preferencesAction;
        preferencesAction = nullptr;
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
    if (terminalAction) {
        terminalAction->deleteLater();
        terminalAction = nullptr;
    }
    if (profilerBrowserAction) {
        profilerBrowserAction->deleteLater();
        profilerBrowserAction = nullptr;
    }
    if (newsfeedBrowserAction) {
        newsfeedBrowserAction->deleteLater();
        newsfeedBrowserAction = nullptr;
    }
    if (nflowAction) {
        nflowAction->deleteLater();
        nflowAction = nullptr;
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
    , preferencesAction(nullptr)
    , profilerBrowserDock(nullptr)
    , newsfeedBrowserDock(nullptr)
    , interpreterStateLabel(nullptr)
    , statusBarTimer(nullptr)
    , lastAtPrompt(true)
    , dropIndicator(nullptr)
    , bClosed(false)
{
    qtTerminal = new QtTerminal(this);
    connect(qtTerminal, &QtTerminal::promptDisplayed, this, &QtMainWindow::recordCurrentDirectory);
    setFocusProxy(qtTerminal);
    // The command window is hosted in a dock so it can be collapsed and floated
    // like the browsers. A QMainWindow central widget cannot be collapsed, so we
    // give it an empty zero-size central widget and let the docks tile the whole
    // window (numerical computing desktop layout).
    commandWindowDock = new QDockWidget(TR("Command Window"), this);
    commandWindowDock->setObjectName("commandWindowDock");
    commandWindowDock->setWidget(qtTerminal);
    // no close button: the command window must not be closeable, only collapsed
    // or floated.
    commandWindowDock->setFeatures(
        QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetFloatable);
    commandWindowDock->setTitleBarWidget(new QtDockTitleBar(commandWindowDock, "cw"));
    QWidget* emptyCenter = new QWidget(this);
    emptyCenter->setMaximumSize(0, 0);
    setCentralWidget(emptyCenter);
    setDockNestingEnabled(true);
    // the left and right dock columns own the corners, so a side panel spans the
    // full height instead of being clipped by the top/bottom areas: this is what
    // makes clean left/right columns possible (default and two columns layouts).
    setCorner(Qt::TopLeftCorner, Qt::LeftDockWidgetArea);
    setCorner(Qt::BottomLeftCorner, Qt::LeftDockWidgetArea);
    setCorner(Qt::TopRightCorner, Qt::RightDockWidgetArea);
    setCorner(Qt::BottomRightCorner, Qt::RightDockWidgetArea);
    addDockWidget(Qt::TopDockWidgetArea, commandWindowDock);
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
    forceWindowsUseAppWindow(this->winId());
#endif
    createMenus();
    commandPalette = new DesktopCommandPalette(this);
    registerCommandPaletteActions();
    registerUserLayoutPaletteActions();
    createToolbars();
    createPanelsSidebar();
    createStatusBar();
    QShortcut* nextAreaShortcut = new QShortcut(desktopCommandShortcut(Qt::Key_F6), this);
    connect(
        nextAreaShortcut, &QShortcut::activated, this, [this]() { focusNextDesktopArea(false); });
    QShortcut* previousAreaShortcut = new QShortcut(desktopCommandShortcut(Qt::Key_F6, true), this);
    connect(previousAreaShortcut, &QShortcut::activated, this,
        [this]() { focusNextDesktopArea(true); });
    setObjectName("NelsonMainWindow");
    refreshDesktopUi();
    setAccessibleName(TR("Nelson main window"));
    updateWindowTitle();
    applyDesktopWindowChrome(this, toolBar);
#define WIDTH_MIN 800
#define HEIGHT_MIN 600
    setMinimumSize(WIDTH_MIN, HEIGHT_MIN);
    QScreen* primaryScreen = QGuiApplication::primaryScreen();
    QRect desktopRect
        = primaryScreen ? primaryScreen->availableGeometry() : QRect(0, 0, WIDTH_MIN, HEIGHT_MIN);
    QPoint center = desktopRect.center();
    int widthCentered = desktopRect.width() * .66;
    int heightCentered = desktopRect.height() * .66;
    if (widthCentered < WIDTH_MIN) {
        widthCentered = WIDTH_MIN;
    }
    if (heightCentered < HEIGHT_MIN) {
        heightCentered = HEIGHT_MIN;
    }
    setGeometry(0, 0, widthCentered, heightCentered);
    move(center.x() - width() / 2, center.y() - height() / 2);

    // https://bugreports.qt.io/browse/QTBUG-76354
    if (QGuiApplication::primaryScreen()) {
        if (minimized) {
            showMinimized();
        } else {
            show();
        }
        qtTerminal->show();
        qApp->processEvents(QEventLoop::WaitForMoreEvents);
    }
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
            QMessageBox::warning(this,
                Nelson::resolveMessageTemplate("Nelson:gui:execute", "Execute...").c_str(),
                Nelson::resolveMessageTemplate(
                    "Nelson:gui:interpreterCurrentlyRuns", "Interpreter currently runs.")
                    .c_str());
        } else {
            QString qfileName = QFileDialog::getOpenFileName(this, TR("Execute..."),
                qtDialogInitialDirectory(L"desktop.open"), QString("Nelson (*.m)"));
            if (!qfileName.isEmpty()) {
                qtRecordDialogSelection(L"desktop.open", QStringLiteral("open"), qfileName);
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
        QString qfileName = QFileDialog::getOpenFileName(this, TR("Load workspace..."),
            qtDialogInitialDirectory(L"desktop.open"), TR("Nelson (*.nh5 *.mat)"));
        if (!qfileName.isEmpty()) {
            qtRecordDialogSelection(L"desktop.open", QStringLiteral("open"), qfileName);
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
            qtDialogInitialSavePath(L"desktop.save", QString()),
            TR("Nelson (*.nh5);;MAT files (*.mat)"));
        if (!qfileName.isEmpty()) {
            qtRecordDialogSelection(L"desktop.save", QStringLiteral("save"), qfileName);
            std::wstring filename = Nelson::QStringTowstring(qfileName);
            qtTerminal->outputMessage(L"\n");
            qtTerminal->sendReturnKey();
            std::wstring cmd = L"save('" + filename + L"')";
            postCommand(cmd);
        }
    }
}
//=============================================================================
QString
QtMainWindow::systemInformation() const
{
    QStringList lines;
    lines << QString(NELSON_PRODUCT_NAME) + QString(" ") + QString(NELSON_VERSION_STRING)
            + QString(" (") + QString(NELSON_RELEASE_NAME) + QString(")");
#ifdef NELSON_VERSION_COMMIT_HASH
    lines << TR("Git revision:") + QString(" ") + QString(NELSON_VERSION_COMMIT_HASH);
#endif
    lines << TR("Qt version:") + QString(" ") + QString(qVersion()) + QString(" (")
            + TR("built with") + QString(" ") + QString(QT_VERSION_STR) + QString(")");
    lines << TR("Operating system:") + QString(" ") + QSysInfo::prettyProductName() + QString(" (")
            + QSysInfo::currentCpuArchitecture() + QString(")");
    lines << TR("Kernel:") + QString(" ") + QSysInfo::kernelType() + QString(" ")
            + QSysInfo::kernelVersion();
    return lines.join(QString("\n"));
}
//=============================================================================
void
QtMainWindow::about()
{
    QString aboutText = QString(NELSON_PRODUCT_NAME) + " " + QString(NELSON_VERSION_STRING) + "\n"
        + "Copyright 2016-present Allan CORNET";
    QMessageBox msgBox(this);
    msgBox.setWindowTitle(TR("About Nelson."));
    msgBox.setIconPixmap(QIcon(nelsonPath + "/resources/fibonacci.png").pixmap(QSize(64, 64)));
    msgBox.setText(aboutText);
    msgBox.setInformativeText(systemInformation());
    msgBox.setTextInteractionFlags(Qt::TextSelectableByMouse);
    QPushButton* copyButton
        = msgBox.addButton(TR("Copy system information"), QMessageBox::ActionRole);
    copyButton->setToolTip(TR("Copy system information to the clipboard (useful for bug reports)"));
    msgBox.addButton(QMessageBox::Ok);
    msgBox.setDefaultButton(QMessageBox::Ok);
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(msgBox.winId());
#endif
    msgBox.exec();
    if (msgBox.clickedButton() == copyButton) {
        QClipboard* clipboard = QApplication::clipboard();
        if (clipboard) {
            clipboard->setText(systemInformation());
        }
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
QtMainWindow::demo()
{
    postCommand(L"demo()");
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
        setCurrentDirectory(wstringToQString(pathSelected));
    }
}
//=============================================================================
void
QtMainWindow::setCurrentDirectory(const QString& directory)
{
    if (!CurrentDirectoryWidget::isValidDirectory(directory)) {
        statusBar()->showMessage(
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:directoryDoesExist", L"Directory does not exist")),
            3000);
        updateStatusBar();
        return;
    }
    if (!QDir::setCurrent(directory)) {
        statusBar()->showMessage(
            wstringToQString(Nelson::resolveMessageTemplate(
                L"Nelson:gui:unableChangeCurrentDirectory", L"Unable to change current directory")),
            3000);
        updateStatusBar();
        return;
    }
    // Keep the function index in sync: functions defined in the new current
    // directory must be resolvable, exactly as after a 'cd' at the prompt.
    PathFunctionIndexerManager::getInstance()->setCurrentUserPath(
        Nelson::QStringTowstring(QDir::currentPath()));
    recordCurrentDirectory();
    updateStatusBar();
    FileBrowser::updateFileBrowser();
}
//=============================================================================
void
QtMainWindow::recordCurrentDirectory()
{
    if (bClosed) {
        return;
    }
    const QString directory = QDir::currentPath();
    if (directory == lastRecordedDirectory) {
        return;
    }
    Nelson::addRecentDirectory(Nelson::QStringTowstring(directory));
    lastRecordedDirectory = directory;
    refreshCurrentDirectoryHistory();
}
//=============================================================================
void
QtMainWindow::refreshCurrentDirectoryHistory()
{
    if (!currentDirectoryWidget) {
        return;
    }
    QStringList directories;
    for (const std::wstring& directory : Nelson::getRecentDirectories()) {
        directories.append(Nelson::wstringToQString(directory));
    }
    currentDirectoryWidget->setDirectories(directories);
}
//=============================================================================
void
QtMainWindow::showPreferences()
{
    QtPreferencesDialog dialog(qtTerminal, this);
    dialog.exec();
}
//=============================================================================
void
QtMainWindow::textEditor()
{
    postCommand(L"edit()");
}
//=============================================================================
void
QtMainWindow::nflow()
{
    postCommand(L"nflow()");
}
//=============================================================================
void
QtMainWindow::packageManager()
{
    postCommand(L"nmm_gui()");
}
//=============================================================================
void
QtMainWindow::toggleDockWidget(QDockWidget* dock)
{
    // The visibility is toggled directly on the GUI side: going through the
    // interpreter (postCommand) does nothing while a computation is running.
    if (!dock) {
        return;
    }
    if (dock->isVisible()) {
        dock->hide();
    } else {
        dock->show();
        dock->raise();
        if (dock->widget()) {
            dock->widget()->setFocus(Qt::ShortcutFocusReason);
        }
    }
}
//=============================================================================
void
QtMainWindow::activatePanelFromRail(QAction* action)
{
    QDockWidget* dock = nullptr;
    if (action == commandWindowAction) {
        dock = commandWindowDock;
    } else if (action == fileBrowserAction) {
        dock = (QDockWidget*)FileBrowser::getFileBrowser();
    } else if (action == workspaceBrowserAction) {
        dock = (QDockWidget*)WorkspaceBrowser::getWorkspaceBrowser();
    } else if (action == historyBrowserAction) {
        dock = (QDockWidget*)HistoryBrowser::getHistoryBrowser();
    } else if (action == variablesEditorAction) {
        dock = (QDockWidget*)VariablesEditor::getVariablesEditor();
    } else if (action == profilerBrowserAction) {
        dock = profilerBrowserDock;
    } else if (action == newsfeedBrowserAction) {
        dock = newsfeedBrowserDock;
    }
    if (!dock) {
        return;
    }
    QWidget* focus = QApplication::focusWidget();
    const bool active = focus && (focus == dock || dock->isAncestorOf(focus));
    if (dock->isVisible() && active) {
        dock->hide();
        return;
    }
    dock->show();
    dock->raise();
    if (dock->widget()) {
        dock->widget()->setFocus(Qt::ShortcutFocusReason);
    }
}
//=============================================================================
void
QtMainWindow::updateActivePanel(QWidget* focusWidget)
{
    if (shuttingDown || !panelsSidebar) {
        return;
    }
    QDockWidget* activeDock = nullptr;
    const QList<QDockWidget*> docks = findChildren<QDockWidget*>();
    for (QDockWidget* dock : docks) {
        if (focusWidget && (focusWidget == dock || dock->isAncestorOf(focusWidget))) {
            activeDock = dock;
            break;
        }
    }
    panelsSidebar->setActiveDock(activeDock);
}
//=============================================================================
void
QtMainWindow::commandWindowToggle()
{
    toggleDockWidget(commandWindowDock);
}
//=============================================================================
void
QtMainWindow::historyBrowserToggle()
{
    toggleDockWidget((QDockWidget*)HistoryBrowser::getHistoryBrowser());
}
//=============================================================================
void
QtMainWindow::fileBrowserToggle()
{
    toggleDockWidget((QDockWidget*)FileBrowser::getFileBrowser());
}
//=============================================================================
void
QtMainWindow::workspaceBrowserToggle()
{
    toggleDockWidget((QDockWidget*)WorkspaceBrowser::getWorkspaceBrowser());
}
//=============================================================================
void
QtMainWindow::variablesEditorToggle()
{
    toggleDockWidget((QDockWidget*)VariablesEditor::getVariablesEditor());
}
//=============================================================================
void
QtMainWindow::profilerBrowserToggle()
{
    toggleDockWidget(profilerBrowserDock);
}
//=============================================================================
void
QtMainWindow::newsfeedBrowserToggle()
{
    toggleDockWidget(newsfeedBrowserDock);
}
//=============================================================================
void
QtMainWindow::stopRun()
{
    if (qtTerminal) {
        qtTerminal->stopRun();
    }
}
//=============================================================================
void
QtMainWindow::updateWindowTitle()
{
    // Keep the taskbar entry stable; the current directory is editable in the
    // main toolbar.
    setWindowTitle(TR("Nelson") + QString(" ") + QString(NELSON_SEMANTIC_VERSION_STRING));
}
//=============================================================================
void
QtMainWindow::updateDockedWindowsMenuSection()
{
    for (QAction* action : dockedWindowsMenuActions) {
        windowsMenu->removeAction(action);
        action->deleteLater();
    }
    dockedWindowsMenuActions.clear();
    QList<QDockWidget*> docks = DesktopDockedClientWindows();
    if (docks.isEmpty()) {
        return;
    }
    dockedWindowsMenuActions.append(windowsMenu->addSeparator());
    for (QDockWidget* dock : docks) {
        QIcon icon = dock->widget() ? dock->widget()->windowIcon() : QIcon();
        QAction* action = new QAction(icon, dock->windowTitle(), this);
        QPointer<QDockWidget> dockPointer(dock);
        connect(action, &QAction::triggered, this, [dockPointer]() {
            if (dockPointer) {
                if (dockPointer->widget()) {
                    dockPointer->widget()->show();
                }
                dockPointer->show();
                dockPointer->raise();
                if (dockPointer->widget()) {
                    dockPointer->widget()->setFocus();
                }
            }
        });
        windowsMenu->addAction(action);
        dockedWindowsMenuActions.append(action);
    }
    // tile / tab only make sense with at least two windows actually open (not
    // floating, not hidden): match the availability to what the operation acts
    // on, otherwise the entries appear for docked-but-hidden figures and no-op.
    if (DesktopOpenClientWindowCount() >= 2) {
        QAction* tileAction
            = new QAction(QIcon(nelsonPath + QString("/resources/layout-two-columns.svg")),
                TR("&Tile docked windows"), this);
        setActionTips(tileAction, TR("Arrange the docked windows side by side"));
        connect(tileAction, &QAction::triggered, this, []() { DesktopTileClientWindows(); });
        windowsMenu->addAction(tileAction);
        dockedWindowsMenuActions.append(tileAction);

        QAction* tabAction
            = new QAction(QIcon(nelsonPath + QString("/resources/layout-default.svg")),
                TR("Ta&b docked windows"), this);
        setActionTips(tabAction, TR("Group the docked windows as tabs"));
        connect(tabAction, &QAction::triggered, this, []() { DesktopTabClientWindows(); });
        windowsMenu->addAction(tabAction);
        dockedWindowsMenuActions.append(tabAction);
    }
}
//=============================================================================
void
QtMainWindow::createStatusBar()
{
    QStatusBar* bar = statusBar();
    bar->setObjectName("NelsonStatusBar");
    bar->setAccessibleName(TR("Status bar"));

    interpreterStateLabel = new QLabel(bar);
    interpreterStateLabel->setObjectName("NelsonInterpreterStateLabel");
    interpreterStateLabel->setAccessibleName(TR("Interpreter state"));
    interpreterStateLabel->setToolTip(TR("Interpreter state"));
    bar->addPermanentWidget(interpreterStateLabel);

    lastCurrentDirectory.clear();
    lastAtPrompt = true;
    updateStatusBar();

    // QtTerminal does not expose a prompt state signal: a light polling timer is
    // used to keep the Busy/Ready indicator and the Stop action in sync.
    statusBarTimer = new QTimer(this);
    statusBarTimer->setInterval(300);
    connect(statusBarTimer, SIGNAL(timeout()), this, SLOT(updateStatusBar()));
    statusBarTimer->start();

    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    const bool visible = settings.value(SETTING_DESKTOP_STATUS_BAR_VISIBLE, true).toBool();
    bar->setVisible(visible);
    statusBarVisibilityAction->setChecked(visible);
}
//=============================================================================
void
QtMainWindow::updateStatusBar()
{
    // the timer must never touch widgets while the window is being torn down
    if (bClosed) {
        return;
    }
    QString currentDirectory = QDir::currentPath();
    if (currentDirectory != lastCurrentDirectory) {
        lastCurrentDirectory = currentDirectory;
        if (currentDirectoryWidget) {
            currentDirectoryWidget->setDirectory(currentDirectory);
        }
    }
    bool atPrompt = qtTerminal ? qtTerminal->isAtPrompt() : true;
    bool stateNotYetDisplayed
        = (interpreterStateLabel != nullptr) && interpreterStateLabel->text().isEmpty();
    if ((atPrompt != lastAtPrompt) || stateNotYetDisplayed) {
        lastAtPrompt = atPrompt;
        if (interpreterStateLabel) {
            interpreterStateLabel->setText(atPrompt ? TR("Ready") : TR("Busy"));
        }
        if (stopAction) {
            stopAction->setEnabled(!atPrompt);
        }
    }
    if (packageManagerBadge) {
        packageManagerBadge->setCount(Nelson::getPackageUpdatesCount());
    }
}
//=============================================================================
bool
QtMainWindow::eventFilter(QObject* watched, QEvent* event)
{
    return QMainWindow::eventFilter(watched, event);
}
//=============================================================================
void
QtMainWindow::changeEvent(QEvent* event)
{
    QMainWindow::changeEvent(event);
    if (event->type() == QEvent::PaletteChange || event->type() == QEvent::ApplicationPaletteChange
        || event->type() == QEvent::StyleChange || event->type() == QEvent::ScreenChangeInternal
#if QT_VERSION >= QT_VERSION_CHECK(6, 6, 0)
        || event->type() == QEvent::DevicePixelRatioChange
#endif
    ) {
        refreshDesktopUi();
    }
}
//=============================================================================
void
QtMainWindow::refreshDesktopUi()
{
    const QString shellStyle = desktopShellStyleSheet(qApp->palette());
    if (styleSheet() != shellStyle) {
        setStyleSheet(shellStyle);
    }
    const QString resources = nelsonPath + "/resources/";
    const QPalette palette = qApp->palette();

    struct ActionIcon
    {
        QAction* action;
        const char* file;
    };
    const ActionIcon icons[] = { { editorAction, "action-editor.svg" },
        { examplesAction, "action-examples.svg" }, { nflowAction, "action-nflow.svg" },
        { packageManagerAction, "action-package.svg" }, { runAction, "action-run.svg" },
        { stopAction, "action-stop.svg" }, { commandWindowAction, "panel-command.svg" },
        { fileBrowserAction, "panel-files.svg" }, { workspaceBrowserAction, "panel-workspace.svg" },
        { historyBrowserAction, "panel-history.svg" },
        { variablesEditorAction, "panel-variables.svg" },
        { profilerBrowserAction, "panel-profiler.svg" }, { terminalAction, "panel-terminal.svg" },
        { newsfeedBrowserAction, "panel-newsfeed.svg" } };
    for (const ActionIcon& icon : icons) {
        if (icon.action) {
            icon.action->setIcon(desktopTintedIcon(resources + icon.file, palette));
        }
    }
    if (packageManagerBadge) {
        packageManagerBadge->setBaseIcon(packageManagerAction->icon());
    }
}
//=============================================================================
void
QtMainWindow::terminal()
{
    postCommand(L"terminal()");
}
//=============================================================================
void
QtMainWindow::createDockWidgets(Evaluator* eval)
{
    FileBrowser::createFileBrowser();
    HistoryBrowser::createHistoryBrowser();
    WorkspaceBrowser::createWorkspaceBrowser(eval->getContext());
    VariablesEditor::createVariablesEditor(eval);
    profilerBrowserDock = new QtProfilerBrowser(this);
    newsfeedBrowserDock = new QtNewsfeedBrowser(this);

    QtWorkspaceBrowser* qtWorkspaceBrowser
        = (QtWorkspaceBrowser*)WorkspaceBrowser::getWorkspaceBrowser();
    QtHistoryBrowser* qtHistoryBrowser = (QtHistoryBrowser*)HistoryBrowser::getHistoryBrowser();
    QtFileBrowser* qtFileBrowser = (QtFileBrowser*)FileBrowser::getFileBrowser();
    QtVariablesEditor* qtVariablesEditor
        = (QtVariablesEditor*)VariablesEditor::getVariablesEditor();
    QtProfilerBrowser* qtProfilerBrowser = (QtProfilerBrowser*)profilerBrowserDock;
    QtNewsfeedBrowser* qtNewsfeedBrowser = (QtNewsfeedBrowser*)newsfeedBrowserDock;

    qtFileBrowser->setMinimumWidth(260);
    qtWorkspaceBrowser->setMinimumWidth(260);
    qtHistoryBrowser->setMinimumWidth(260);
    qtVariablesEditor->setMinimumWidth(260);
    qtProfilerBrowser->setMinimumWidth(260);
    qtNewsfeedBrowser->setMinimumWidth(260);

    // Initial arrangement: the two-column desktop layout. A saved state,
    // if any, overrides it in restoreDockWidgetPositions() below.
    buildDefaultLayout();

    connect(
        qtWorkspaceBrowser, SIGNAL(closeWorkspaceBrowser()), this, SLOT(onCloseWorkspaceBrowser()));
    connect(qtFileBrowser, SIGNAL(closeFileBrowser()), this, SLOT(onCloseFileBrowser()));
    connect(qtHistoryBrowser, SIGNAL(closeHistoryBrowser()), this, SLOT(onCloseHistoryBrowser()));
    connect(
        qtVariablesEditor, SIGNAL(closeVariablesEditor()), this, SLOT(onCloseVariablesEditor()));
    connect(
        qtProfilerBrowser, SIGNAL(closeProfilerBrowser()), this, SLOT(onCloseProfilerBrowser()));
    connect(
        qtNewsfeedBrowser, SIGNAL(closeNewsfeedBrowser()), this, SLOT(onCloseNewsfeedBrowser()));

    // these connections require the dock widgets to exist: they cannot be done in
    // createMenus() which runs before createDockWidgets().
    connect((QDockWidget*)qtHistoryBrowser, &QDockWidget::visibilityChanged, this,
        [=](bool visible) { historyBrowserAction->setChecked(visible); });
    connect((QDockWidget*)qtFileBrowser, &QDockWidget::visibilityChanged, this,
        [=](bool visible) { fileBrowserAction->setChecked(visible); });
    connect((QDockWidget*)qtWorkspaceBrowser, &QDockWidget::visibilityChanged, this,
        [=](bool visible) { workspaceBrowserAction->setChecked(visible); });
    connect((QDockWidget*)qtVariablesEditor, &QDockWidget::visibilityChanged, this,
        [=](bool visible) { variablesEditorAction->setChecked(visible); });
    connect(profilerBrowserDock, &QDockWidget::visibilityChanged, this,
        [=](bool visible) { profilerBrowserAction->setChecked(visible); });
    connect(newsfeedBrowserDock, &QDockWidget::visibilityChanged, this,
        [=](bool visible) { newsfeedBrowserAction->setChecked(visible); });

    const bool restoredDockState = restoreDockWidgetPositions();

    if (!restoredDockState) {
        // The deterministic default layout is authoritative when the global
        // dock state is missing or corrupt: old per-browser visibility keys
        // must not override the first-launch two-column arrangement.
        qtFileBrowser->show();
        qtWorkspaceBrowser->show();
        qtHistoryBrowser->hide();
        qtVariablesEditor->hide();
        profilerBrowserDock->hide();
        newsfeedBrowserDock->hide();
    }

    workspaceBrowserAction->setChecked(WorkspaceBrowser::isWorkspaceBrowserVisible());
    historyBrowserAction->setChecked(HistoryBrowser::isHistoryBrowserVisible());
    fileBrowserAction->setChecked(FileBrowser::isFileBrowserVisible());
    variablesEditorAction->setChecked(VariablesEditor::isVariablesEditorVisible());
    profilerBrowserAction->setChecked(profilerBrowserDock->isVisible());
    newsfeedBrowserAction->setChecked(newsfeedBrowserDock->isVisible());

    // collapsed states are restored last: QMainWindow::restoreState() above
    // would otherwise override the collapsed dock heights.
    QDockWidget* docks[] = { (QDockWidget*)qtFileBrowser, (QDockWidget*)qtWorkspaceBrowser,
        (QDockWidget*)qtHistoryBrowser, (QDockWidget*)qtVariablesEditor, profilerBrowserDock,
        newsfeedBrowserDock, commandWindowDock };
    for (QDockWidget* dock : docks) {
        QtDockTitleBar* titleBar = qobject_cast<QtDockTitleBar*>(dock->titleBarWidget());
        if (titleBar) {
            titleBar->restoreState();
        }
    }
    bindPanelRailDocks();
}
//=============================================================================
void
QtMainWindow::bindPanelRailDocks()
{
    if (!panelsSidebar) {
        return;
    }
    panelsSidebar->bindDock(commandWindowAction, commandWindowDock);
    panelsSidebar->bindDock(fileBrowserAction, (QDockWidget*)FileBrowser::getFileBrowser());
    panelsSidebar->bindDock(
        workspaceBrowserAction, (QDockWidget*)WorkspaceBrowser::getWorkspaceBrowser());
    panelsSidebar->bindDock(
        historyBrowserAction, (QDockWidget*)HistoryBrowser::getHistoryBrowser());
    panelsSidebar->bindDock(
        variablesEditorAction, (QDockWidget*)VariablesEditor::getVariablesEditor());
    panelsSidebar->bindDock(profilerBrowserAction, profilerBrowserDock);
    panelsSidebar->bindDock(newsfeedBrowserAction, newsfeedBrowserDock);
    focusChangedConnection = connect(qApp, &QApplication::focusChanged, this,
        [this](QWidget*, QWidget* focus) { updateActivePanel(focus); });
    updateActivePanel(QApplication::focusWidget());
}
//=============================================================================
void
QtMainWindow::focusNextDesktopArea(bool backwards)
{
    QList<QWidget*> areas;
    if (toolBar && toolBar->isVisible()) {
        areas << toolBar;
    }
    const QList<QDockWidget*> docks = { (QDockWidget*)FileBrowser::getFileBrowser(),
        (QDockWidget*)WorkspaceBrowser::getWorkspaceBrowser(),
        (QDockWidget*)HistoryBrowser::getHistoryBrowser(),
        (QDockWidget*)VariablesEditor::getVariablesEditor(), profilerBrowserDock,
        newsfeedBrowserDock, commandWindowDock };
    for (QDockWidget* dock : docks) {
        if (dock && dock->isVisible()) {
            areas << (dock->widget() ? dock->widget() : dock);
        }
    }
    if (areas.isEmpty()) {
        return;
    }
    QWidget* focus = QApplication::focusWidget();
    int current = -1;
    for (int index = 0; index < areas.size(); ++index) {
        if (focus && (focus == areas[index] || areas[index]->isAncestorOf(focus))) {
            current = index;
            break;
        }
    }
    const int delta = backwards ? -1 : 1;
    const int next = (current + delta + areas.size()) % areas.size();
    areas[next]->setFocus(Qt::ShortcutFocusReason);
}
//=============================================================================
void
QtMainWindow::destroyDockWidgets()
{
    saveDockWidgetPositions();
    FileBrowser::destroyFileBrowser();
    HistoryBrowser::destroyHistoryBrowser();
    WorkspaceBrowser::destroyWorkspaceBrowser();
    VariablesEditor::destroyVariablesEditor();
    if (profilerBrowserDock) {
        profilerBrowserDock->deleteLater();
        profilerBrowserDock = nullptr;
    }
    if (newsfeedBrowserDock) {
        newsfeedBrowserDock->deleteLater();
        newsfeedBrowserDock = nullptr;
    }
}
//=============================================================================
void
QtMainWindow::saveDockWidgetPositions()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.setValue(SETTING_MAIN_WINDOW_GEOMETRY, saveGeometry());
    settings.setValue(SETTING_MAIN_WINDOW_Window_State, saveState());
}
//=============================================================================
// true when the window rectangle is visible on at least one connected screen.
static bool
isRectOnAnyScreen(const QRect& frame)
{
    if (frame.isEmpty()) {
        return false;
    }
    const QList<QScreen*> screens = QGuiApplication::screens();
    for (QScreen* screen : screens) {
        // require a real overlap, not just a shared edge, so a window nudged
        // one pixel onto a former screen still counts as off screen.
        QRect visible = screen->availableGeometry().intersected(frame);
        if (visible.width() > 40 && visible.height() > 40) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
QtMainWindow::restoreDockWidgetPositions()
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    QByteArray geometry = settings.value(SETTING_MAIN_WINDOW_GEOMETRY).toByteArray();
    QByteArray state = settings.value(SETTING_MAIN_WINDOW_Window_State).toByteArray();

    if (!geometry.isEmpty()) {
        restoreGeometry(geometry);
        // a geometry saved on a monitor that is gone would leave the window off
        // every screen: recenter it on the primary screen in that case.
        if (!isRectOnAnyScreen(frameGeometry())) {
            QScreen* primary = QGuiApplication::primaryScreen();
            if (primary) {
                QRect available = primary->availableGeometry();
                resize(qMin(width(), available.width()), qMin(height(), available.height()));
                move(available.center() - rect().center());
            }
        }
    }

    // an empty or corrupted state (e.g. saved by an older version with a
    // different set of docks) must not leave a broken layout: fall back to the
    // deterministic default arrangement.
    if (state.isEmpty() || !restoreState(state)) {
        buildDefaultLayout();
        return false;
    }
    return true;
}
//=============================================================================
void
QtMainWindow::createToolbars()
{
    toolBar = addToolBar(TR("Main toolbar"));
    // an objectName is mandatory, otherwise QMainWindow::saveState() warns and
    // cannot restore the toolbar position/visibility.
    toolBar->setObjectName("NelsonMainToolBar");
    toolBar->setAccessibleName(TR("Main toolbar"));
    toolBar->setMovable(false);
    toolBar->setFloatable(false);
    toolBar->setAllowedAreas(Qt::TopToolBarArea);
#ifdef Q_OS_MACOS
    toolBar->setIconSize(QSize(20, 20));
#else
    toolBar->setIconSize(QSize(18, 18));
#endif
    toolBar->addAction(nflowAction);
#if WITH_WEBVIEW_MODULE
    if (packageManagerAction) {
        toolBar->addAction(packageManagerAction);
    }
#endif
    toolBar->addAction(editorAction);
    toolBar->addAction(runAction);
    toolBar->addAction(stopAction);
    toolBar->addSeparator();

    currentDirectoryWidget = new CurrentDirectoryWidget(toolBar);
    currentDirectoryWidget->setDirectory(QDir::currentPath());
    refreshCurrentDirectoryHistory();
    connect(currentDirectoryWidget, &CurrentDirectoryWidget::browseRequested, this,
        &QtMainWindow::changeDir);
    connect(currentDirectoryWidget, &CurrentDirectoryWidget::directoryChangeRequested, this,
        &QtMainWindow::setCurrentDirectory);
    toolBar->addWidget(currentDirectoryWidget);

    QToolButton* moreButton = new QToolButton(toolBar);
    moreButton->setObjectName("NelsonToolbarMoreButton");
    moreButton->setText(QString::fromUtf8("⋯"));
    moreButton->setToolTip(TR("More actions"));
    moreButton->setAccessibleName(TR("More actions"));
    moreButton->setPopupMode(QToolButton::InstantPopup);
    QMenu* moreMenu = new QMenu(moreButton);
    moreMenu->addAction(loadWorkspaceAction);
    moreMenu->addAction(saveWorkspaceAction);
    moreButton->setMenu(moreMenu);
    toolBar->addWidget(moreButton);
    applyDesktopToolbarMetrics(toolBar);

    connect(toolBar, &QToolBar::visibilityChanged, toolbarVisibilityAction, &QAction::setChecked);
}
//=============================================================================
void
QtMainWindow::createPanelsSidebar()
{
    panelsSidebar = new DesktopPanelRail(this);
    panelsSidebar->addPanelAction(commandWindowAction);
    panelsSidebar->addRailSeparator();
    panelsSidebar->addPanelAction(fileBrowserAction);
    panelsSidebar->addPanelAction(workspaceBrowserAction);
    panelsSidebar->addPanelAction(historyBrowserAction);
    panelsSidebar->addPanelAction(variablesEditorAction);
    panelsSidebar->addPanelAction(profilerBrowserAction);
    panelsSidebar->addPanelAction(newsfeedBrowserAction);
    panelsSidebar->addRailSeparator();
    panelsSidebar->addCommandAction(terminalAction);
#if WITH_WEBVIEW_MODULE
    panelsSidebar->addCommandAction(examplesAction);
    if (packageManagerAction) {
        panelsSidebar->addCommandAction(packageManagerAction);
    }
#endif
    panelsSidebar->complete();
    connect(panelsSidebar, &DesktopPanelRail::panelRequested, this,
        &QtMainWindow::activatePanelFromRail);
    addToolBar(Qt::LeftToolBarArea, panelsSidebar);

    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    const bool visible = settings.value(SETTING_DESKTOP_SIDEBAR_VISIBLE, true).toBool();
    panelsSidebar->setVisible(visible);
    sidebarVisibilityAction->setChecked(visible);
    connect(
        panelsSidebar, &QToolBar::visibilityChanged, sidebarVisibilityAction, &QAction::setChecked);
}
//=============================================================================
static void
setActionTips(QAction* action, const QString& tip)
{
    if (!action) {
        return;
    }
    action->setStatusTip(tip);
    // without an explicit tooltip, Qt builds it from the menu text and displays
    // the '&' mnemonic in the toolbar tooltips.
    QString toolTip = tip;
    if (!action->shortcut().isEmpty()) {
        toolTip += QString(" (%1)").arg(action->shortcut().toString(QKeySequence::NativeText));
    }
    action->setToolTip(toolTip);
}
//=============================================================================
void
QtMainWindow::registerCommandPaletteActions()
{
    const QList<QAction*> fileActions = { runAction, stopAction, loadWorkspaceAction,
        saveWorkspaceAction, chdirAction, pwdAction, preferencesAction, exitAction };
    for (QAction* action : fileActions) {
        commandPalette->addAction(action, TR("File"));
    }
    const QList<QAction*> editActions = { cutAction, copyAction, pasteAction, selectAllAction,
        emptyClipboardAction, clearConsoleAction, editorAction, nflowAction };
    for (QAction* action : editActions) {
        commandPalette->addAction(action, TR("Edit"));
    }
    const QList<QAction*> panelActions
        = { commandWindowAction, fileBrowserAction, workspaceBrowserAction, historyBrowserAction,
              variablesEditorAction, profilerBrowserAction, newsfeedBrowserAction, terminalAction,
              toolbarVisibilityAction, sidebarVisibilityAction, statusBarVisibilityAction };
    for (QAction* action : panelActions) {
        commandPalette->addAction(action, TR("View"));
    }
    const QList<QAction*> layoutActions = { layoutDefaultAction, layoutThreeColumnsAction,
        layoutTerminalOnlyAction, layoutRestoreDefaultAction, layoutSaveAction };
    for (QAction* action : layoutActions) {
        commandPalette->addAction(action, TR("Layout"));
    }
#if WITH_WEBVIEW_MODULE
    if (packageManagerAction) {
        commandPalette->addAction(packageManagerAction, TR("Window"));
    }
#endif
    QList<QAction*> helpActions
        = { helpAction, webAction, bugAction, checkUpdateAction, aboutAction };
#if WITH_WEBVIEW_MODULE
    helpActions.prepend(examplesAction);
#endif
    for (QAction* action : helpActions) {
        commandPalette->addAction(action, TR("Help"));
    }
}
//=============================================================================
void
QtMainWindow::registerUserLayoutPaletteActions()
{
    for (QAction* action : userLayoutPaletteActions) {
        commandPalette->removeAction(action);
        delete action;
    }
    userLayoutPaletteActions.clear();
    for (const QString& name : userLayoutNames()) {
        QAction* action
            = new QAction(QString("%1: %2").arg(TR("Apply layout"), name), commandPalette);
        connect(action, &QAction::triggered, this, [this, name]() { applyUserLayout(name); });
        commandPalette->addAction(action, TR("Layout"));
        userLayoutPaletteActions.append(action);
    }
}
//=============================================================================
void
QtMainWindow::showCommandPalette()
{
    commandPalette->openWithQuery();
}
//=============================================================================
void
QtMainWindow::createMenus()
{
    QString fileNameIcon;
    mainMenuBar = this->menuBar();
#ifdef Q_OS_MACOS
    mainMenuBar->setNativeMenuBar(true);
#else
    mainMenuBar->setNativeMenuBar(false);
#endif
    mainMenuBar->setObjectName("NelsonMenuBar");
    mainMenuBar->setAccessibleName(TR("Main menu"));
    fileMenu = mainMenuBar->addMenu(TR("&File"));
    fileNameIcon = nelsonPath + QString("/resources/file-run.svg");
    runAction = new QAction(QIcon(fileNameIcon), TR("&Execute..."), this);
    runAction->setShortcut(desktopCommandShortcut(Qt::Key_E));
    setActionTips(runAction, TR("Execute a .m file"));
    connect(runAction, SIGNAL(triggered()), this, SLOT(runFile()));
    fileMenu->addAction(runAction);
    fileNameIcon = nelsonPath + QString("/resources/stop-interpreter.svg");
    stopAction = new QAction(QIcon(fileNameIcon), TR("S&top execution"), this);
    setActionTips(stopAction, TR("Stop the current execution"));
    stopAction->setEnabled(false);
    connect(stopAction, SIGNAL(triggered()), this, SLOT(stopRun()));
    fileMenu->addAction(stopAction);
    fileMenu->addSeparator();
    // Ctrl+L is reserved for 'clear console' (terminal habit): the workspace
    // shortcuts are moved to Ctrl+Shift+L / Ctrl+Shift+S.
    fileNameIcon = nelsonPath + QString("/resources/load-workspace.svg");
    loadWorkspaceAction = new QAction(QIcon(fileNameIcon), TR("&Load Workspace..."), this);
    loadWorkspaceAction->setShortcut(desktopCommandShortcut(Qt::Key_L, true));
    setActionTips(loadWorkspaceAction, TR("Load a .nh5 file"));
    connect(loadWorkspaceAction, SIGNAL(triggered()), this, SLOT(loadWorkspace()));
    fileMenu->addAction(loadWorkspaceAction);
    fileNameIcon = nelsonPath + QString("/resources/save-workspace.svg");
    saveWorkspaceAction = new QAction(QIcon(fileNameIcon), TR("&Save Workspace..."), this);
    saveWorkspaceAction->setShortcut(desktopCommandShortcut(Qt::Key_S, true));
    setActionTips(saveWorkspaceAction, TR("Save a .nh5 file"));
    connect(saveWorkspaceAction, SIGNAL(triggered()), this, SLOT(saveWorkspace()));
    fileMenu->addAction(saveWorkspaceAction);
    fileMenu->addSeparator();
    fileNameIcon = nelsonPath + QString("/resources/folder-open.svg");
    chdirAction = new QAction(QIcon(fileNameIcon), TR("&Change current directory"), this);
    setActionTips(chdirAction, TR("Change current directory"));
    connect(chdirAction, SIGNAL(triggered()), this, SLOT(changeDir()));
    fileMenu->addAction(chdirAction);
    pwdAction = new QAction(TR("&Display current directory"), this);
    setActionTips(pwdAction, TR("Display current directory"));
    connect(pwdAction, SIGNAL(triggered()), this, SLOT(pwdDisplay()));
    fileMenu->addAction(pwdAction);
    fileMenu->addSeparator();
    preferencesAction = new QAction(TR("&Preferences..."), this);
    preferencesAction->setShortcut(QKeySequence::Preferences);
    preferencesAction->setMenuRole(QAction::PreferencesRole);
    setActionTips(preferencesAction, TR("Edit the application preferences"));
    connect(preferencesAction, SIGNAL(triggered()), this, SLOT(showPreferences()));
    fileMenu->addAction(preferencesAction);
    fileMenu->addSeparator();
    fileNameIcon = nelsonPath + QString("/resources/exit.svg");
    exitAction = new QAction(QIcon(fileNameIcon), TR("E&xit"), this);
    exitAction->setMenuRole(QAction::QuitRole);
    // QKeySequence::Quit is empty on Windows: Ctrl+Q is added outside macOS.
    {
        QList<QKeySequence> quitShortcuts = QKeySequence::keyBindings(QKeySequence::Quit);
#ifndef Q_OS_MACOS
        QKeySequence explicitQuitShortcut = desktopCommandShortcut(Qt::Key_Q);
        if (!quitShortcuts.contains(explicitQuitShortcut)) {
            quitShortcuts.append(explicitQuitShortcut);
        }
#endif
        exitAction->setShortcuts(quitShortcuts);
    }
    setActionTips(exitAction, TR("Exit the application"));
    connect(exitAction, SIGNAL(triggered()), this, SLOT(close()));
    fileMenu->addAction(exitAction);
    editMenu = mainMenuBar->addMenu(TR("&Edit"));
    fileNameIcon = nelsonPath + QString("/resources/edit-cut.svg");
    cutAction = new QAction(QIcon(fileNameIcon), TR("C&ut"), this);
    cutAction->setShortcuts(QKeySequence::Cut);
    setActionTips(cutAction, TR("Cut"));
    connect(cutAction, SIGNAL(triggered()), this, SLOT(cutText()));
    editMenu->addAction(cutAction);
    fileNameIcon = nelsonPath + QString("/resources/edit-copy.svg");
    copyAction = new QAction(QIcon(fileNameIcon), TR("&Copy"), this);
    copyAction->setShortcuts(QKeySequence::Copy);
    setActionTips(copyAction, TR("Copy"));
    connect(copyAction, SIGNAL(triggered()), this, SLOT(copyText()));
    editMenu->addAction(copyAction);
    fileNameIcon = nelsonPath + QString("/resources/edit-paste.svg");
    pasteAction = new QAction(QIcon(fileNameIcon), TR("&Paste"), this);
    pasteAction->setShortcuts(QKeySequence::Paste);
    setActionTips(pasteAction, TR("Paste"));
    connect(pasteAction, SIGNAL(triggered()), this, SLOT(pasteText()));
    editMenu->addAction(pasteAction);
    editMenu->addSeparator();
    fileNameIcon = nelsonPath + QString("/resources/edit-select-all.svg");
    selectAllAction = new QAction(QIcon(fileNameIcon), TR("&Select all"), this);
    selectAllAction->setShortcuts(QKeySequence::SelectAll);
    setActionTips(selectAllAction, TR("Select all"));
    connect(selectAllAction, SIGNAL(triggered()), this, SLOT(selectAllText()));
    editMenu->addAction(selectAllAction);
    editMenu->addSeparator();
    fileNameIcon = nelsonPath + QString("/resources/clipboard-empty.svg");
    emptyClipboardAction = new QAction(QIcon(fileNameIcon), TR("&Empty clipboard"), this);
    setActionTips(emptyClipboardAction, TR("Empty clipboard"));
    connect(emptyClipboardAction, SIGNAL(triggered()), this, SLOT(emptyClipboard()));
    editMenu->addAction(emptyClipboardAction);
    editMenu->addSeparator();
    fileNameIcon = nelsonPath + QString("/resources/console-clear.svg");
    clearConsoleAction = new QAction(QIcon(fileNameIcon), TR("Clear c&onsole"), this);
    clearConsoleAction->setShortcut(desktopCommandShortcut(Qt::Key_L));
    setActionTips(clearConsoleAction, TR("Clear console"));
    connect(clearConsoleAction, SIGNAL(triggered()), this, SLOT(clearConsole()));
    editMenu->addAction(clearConsoleAction);

    editMenu->addSeparator();
    commandPaletteAction = new QAction(TR("Search &Commands..."), this);
    commandPaletteAction->setShortcut(desktopCommandShortcut(Qt::Key_P, true));
    setActionTips(commandPaletteAction, TR("Search desktop commands"));
    connect(commandPaletteAction, &QAction::triggered, this, &QtMainWindow::showCommandPalette);
    editMenu->addAction(commandPaletteAction);

    fileNameIcon = nelsonPath + QString("/resources/textedit-icon.svg");
    editorAction = new QAction(QIcon(fileNameIcon), TR("&Text editor"), this);
    setActionTips(editorAction, TR("Text editor"));
    connect(editorAction, SIGNAL(triggered()), this, SLOT(textEditor()));

    fileNameIcon = nelsonPath + QString("/resources/action-nflow.svg");
    nflowAction = new QAction(QIcon(fileNameIcon), TR("&NFlow"), this);
    setActionTips(nflowAction, TR("NFlow block-diagram editor"));
    nflowAction->setVisible(false);
    if (IsConfiguredModuleName(L"nflow_gui")) {
        nflowAction->setVisible(true);
    }
    connect(nflowAction, SIGNAL(triggered()), this, SLOT(nflow()));

    viewMenu = mainMenuBar->addMenu(TR("&View"));
    fileNameIcon = nelsonPath + QString("/resources/layout-default.svg");
    layoutMenu = viewMenu->addMenu(QIcon(fileNameIcon), TR("&Layout"));

    layoutActionGroup = new QActionGroup(this);
    layoutActionGroup->setExclusive(true);

    fileNameIcon = nelsonPath + QString("/resources/layout-default.svg");
    layoutDefaultAction = new QAction(QIcon(fileNameIcon), TR("Two columns (Default)"), this);
    layoutDefaultAction->setCheckable(true);
    layoutDefaultAction->setChecked(true);
    setActionTips(layoutDefaultAction, TR("Default layout"));
    layoutActionGroup->addAction(layoutDefaultAction);
    connect(layoutDefaultAction, SIGNAL(triggered()), this, SLOT(onLayoutDefaultAction()));
    layoutMenu->addAction(layoutDefaultAction);

    fileNameIcon = nelsonPath + QString("/resources/layout-terminal-only.svg");
    layoutTerminalOnlyAction = new QAction(QIcon(fileNameIcon), TR("&Command Window only"), this);
    layoutTerminalOnlyAction->setCheckable(true);
    setActionTips(layoutTerminalOnlyAction, TR("Command Window only layout"));
    layoutActionGroup->addAction(layoutTerminalOnlyAction);
    connect(layoutTerminalOnlyAction, SIGNAL(triggered()), this, SLOT(onLayoutTerminalOnly()));
    layoutMenu->addAction(layoutTerminalOnlyAction);

    fileNameIcon = nelsonPath + QString("/resources/layout-two-columns.svg");
    layoutThreeColumnsAction = new QAction(QIcon(fileNameIcon), TR("&Three columns"), this);
    layoutThreeColumnsAction->setCheckable(true);
    setActionTips(layoutThreeColumnsAction, TR("Three columns layout"));
    layoutActionGroup->addAction(layoutThreeColumnsAction);
    connect(
        layoutThreeColumnsAction, SIGNAL(triggered()), this, SLOT(onLayoutThreeColumnsAction()));
    layoutMenu->addAction(layoutThreeColumnsAction);

    layoutMenu->addSeparator();
    layoutRestoreDefaultAction
        = new QAction(QIcon(nelsonPath + QString("/resources/layout-default.svg")),
            TR("&Restore default layout"), this);
    setActionTips(layoutRestoreDefaultAction, TR("Restore the default layout"));
    connect(layoutRestoreDefaultAction, SIGNAL(triggered()), this, SLOT(onLayoutDefaultAction()));
    layoutMenu->addAction(layoutRestoreDefaultAction);

    fileNameIcon = nelsonPath + QString("/resources/save-workspace.svg");
    layoutSaveAction = new QAction(QIcon(fileNameIcon), TR("&Save current layout..."), this);
    setActionTips(layoutSaveAction, TR("Save the current layout under a name"));
    connect(layoutSaveAction, SIGNAL(triggered()), this, SLOT(onSaveCurrentLayout()));
    layoutMenu->addAction(layoutSaveAction);
    connect(layoutMenu, &QMenu::aboutToShow, this, [this]() { updateUserLayoutsMenuSection(); });

    viewMenu->addSeparator();

    fileNameIcon = nelsonPath + QString("/resources/console.svg");
    commandWindowAction = new QAction(QIcon(fileNameIcon), TR("&Command Window"), this);
    setActionTips(commandWindowAction, TR("Command Window"));
    commandWindowAction->setCheckable(true);
    commandWindowAction->setChecked(true);
    connect(commandWindowAction, SIGNAL(triggered()), this, SLOT(commandWindowToggle()));
    // keep the checkbox in sync when the dock is collapsed / floated / hidden
    connect(commandWindowDock, &QDockWidget::visibilityChanged, this,
        [this](bool visible) { commandWindowAction->setChecked(visible); });
    viewMenu->addAction(commandWindowAction);

    fileNameIcon = nelsonPath + QString("/resources/appointment-new.svg");
    historyBrowserAction = new QAction(QIcon(fileNameIcon), TR("&History browser"), this);
    historyBrowserAction->setShortcut(desktopCommandShortcut(Qt::Key_1));
    setActionTips(historyBrowserAction, TR("History browser"));
    historyBrowserAction->setCheckable(true);
    connect(historyBrowserAction, SIGNAL(triggered()), this, SLOT(historyBrowserToggle()));
    viewMenu->addAction(historyBrowserAction);

    fileNameIcon = nelsonPath + QString("/resources/folder.svg");
    fileBrowserAction = new QAction(QIcon(fileNameIcon), TR("&File browser"), this);
    fileBrowserAction->setShortcut(desktopCommandShortcut(Qt::Key_2));
    setActionTips(fileBrowserAction, TR("File browser"));
    fileBrowserAction->setCheckable(true);
    connect(fileBrowserAction, SIGNAL(triggered()), this, SLOT(fileBrowserToggle()));
    viewMenu->addAction(fileBrowserAction);

    fileNameIcon = nelsonPath + QString("/resources/spreadsheet.svg");
    workspaceBrowserAction = new QAction(QIcon(fileNameIcon), TR("&Workspace browser"), this);
    workspaceBrowserAction->setShortcut(desktopCommandShortcut(Qt::Key_3));
    setActionTips(workspaceBrowserAction, TR("Workspace browser"));
    workspaceBrowserAction->setCheckable(true);
    connect(workspaceBrowserAction, SIGNAL(triggered()), this, SLOT(workspaceBrowserToggle()));
    viewMenu->addAction(workspaceBrowserAction);

    // a distinct icon: the workspace browser already uses spreadsheet.svg and
    // both actions sit side by side in the panels sidebar.
    fileNameIcon = nelsonPath + QString("/resources/create-variable.svg");
    variablesEditorAction = new QAction(QIcon(fileNameIcon), TR("&Variables editor"), this);
    // Ctrl+0 is deliberately left to the console: it is the universal 'reset
    // zoom' shortcut, and a window level QAction would swallow it before the
    // terminal ever sees the key event.
    variablesEditorAction->setShortcut(desktopCommandShortcut(Qt::Key_4));
    setActionTips(variablesEditorAction, TR("Variables editor"));
    variablesEditorAction->setCheckable(true);
    connect(variablesEditorAction, SIGNAL(triggered()), this, SLOT(variablesEditorToggle()));
    viewMenu->addAction(variablesEditorAction);

    fileNameIcon = nelsonPath + QString("/resources/profiler.svg");
    profilerBrowserAction = new QAction(QIcon(fileNameIcon), TR("&Profiler"), this);
    profilerBrowserAction->setShortcut(desktopCommandShortcut(Qt::Key_5));
    setActionTips(profilerBrowserAction, TR("Profiler"));
    profilerBrowserAction->setCheckable(true);
    connect(profilerBrowserAction, SIGNAL(triggered()), this, SLOT(profilerBrowserToggle()));
    viewMenu->addAction(profilerBrowserAction);

    fileNameIcon = nelsonPath + QString("/resources/panel-newsfeed.svg");
    newsfeedBrowserAction = new QAction(QIcon(fileNameIcon), TR("&News feed"), this);
    newsfeedBrowserAction->setShortcut(desktopCommandShortcut(Qt::Key_6));
    setActionTips(newsfeedBrowserAction, TR("News feed"));
    newsfeedBrowserAction->setCheckable(true);
    connect(newsfeedBrowserAction, SIGNAL(triggered()), this, SLOT(newsfeedBrowserToggle()));
    viewMenu->addAction(newsfeedBrowserAction);

    viewMenu->addSeparator();
    toolbarVisibilityAction = new QAction(TR("Main toolbar"), this);
    toolbarVisibilityAction->setCheckable(true);
    toolbarVisibilityAction->setChecked(true);
    connect(toolbarVisibilityAction, &QAction::toggled, this, [this](bool visible) {
        if (toolBar) {
            toolBar->setVisible(visible);
        }
    });
    viewMenu->addAction(toolbarVisibilityAction);

    sidebarVisibilityAction = new QAction(TR("Panels sidebar"), this);
    sidebarVisibilityAction->setCheckable(true);
    connect(sidebarVisibilityAction, &QAction::toggled, this, [this](bool visible) {
        if (panelsSidebar) {
            panelsSidebar->setVisible(visible);
        }
        QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
        settings.setValue(SETTING_DESKTOP_SIDEBAR_VISIBLE, visible);
    });
    viewMenu->addAction(sidebarVisibilityAction);

    statusBarVisibilityAction = new QAction(TR("Status bar"), this);
    statusBarVisibilityAction->setCheckable(true);
    connect(statusBarVisibilityAction, &QAction::toggled, this, [this](bool visible) {
        statusBar()->setVisible(visible);
        QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
        settings.setValue(SETTING_DESKTOP_STATUS_BAR_VISIBLE, visible);
    });
    viewMenu->addAction(statusBarVisibilityAction);

    windowsMenu = mainMenuBar->addMenu(TR("&Window"));
    fileNameIcon = nelsonPath + QString("/resources/console.svg");
    terminalAction = new QAction(QIcon(fileNameIcon), TR("&Terminal"), this);
    setActionTips(terminalAction, TR("Terminal"));
    connect(terminalAction, SIGNAL(triggered()), this, SLOT(terminal()));
    windowsMenu->addAction(terminalAction);
    windowsMenu->addAction(editorAction);
    windowsMenu->addAction(nflowAction);
#if WITH_WEBVIEW_MODULE
    // Package manager (nmm_gui) is a WebUI window, so keep it with the other
    // window-opening actions.
    fileNameIcon = nelsonPath + QString("/resources/action-package.svg");
    packageManagerAction = new QAction(QIcon(fileNameIcon), TR("&Package Manager"), this);
    setActionTips(packageManagerAction, TR("Install, update and remove Nelson packages"));
    connect(packageManagerAction, SIGNAL(triggered()), this, SLOT(packageManager()));
    windowsMenu->addAction(packageManagerAction);
    packageManagerBadge = new DesktopActionBadge(packageManagerAction);
#endif
    connect(windowsMenu, &QMenu::aboutToShow, this, [this]() { updateDockedWindowsMenuSection(); });

    helpMenu = mainMenuBar->addMenu(TR("&Help"));
    fileNameIcon = nelsonPath + QString("/resources/help-icon.svg");
    helpAction = new QAction(QIcon(fileNameIcon), TR("&Documentation"), this);
    helpAction->setShortcuts(QKeySequence::HelpContents);
    setActionTips(helpAction, TR("Documentation"));
    connect(helpAction, SIGNAL(triggered()), this, SLOT(help()));
    helpMenu->addAction(helpAction);
#if WITH_WEBVIEW_MODULE
    fileNameIcon = nelsonPath + QString("/resources/action-examples.svg");
    examplesAction = new QAction(QIcon(fileNameIcon), TR("&Examples"), this);
    setActionTips(examplesAction, TR("Browse Nelson examples"));
    connect(examplesAction, SIGNAL(triggered()), this, SLOT(demo()));
    helpMenu->addAction(examplesAction);
#endif
    fileNameIcon = nelsonPath + QString("/resources/fibonacci.png");
    webAction = new QAction(QIcon(fileNameIcon), TR("Nelson &website"), this);
    setActionTips(webAction, TR("Nelson website"));
    connect(webAction, SIGNAL(triggered()), this, SLOT(website()));
    helpMenu->addAction(webAction);
    fileNameIcon = nelsonPath + QString("/resources/bug-icon.svg");
    bugAction = new QAction(QIcon(fileNameIcon), TR("B&ugs and Requests"), this);
    setActionTips(bugAction, TR("Bugs and Requests"));
    connect(bugAction, SIGNAL(triggered()), this, SLOT(bugAndRequest()));
    helpMenu->addAction(bugAction);

#ifdef WITH_WEBTOOLS_MODULE
    fileNameIcon = nelsonPath + QString("/resources/system-software-update.svg");
    checkUpdateAction = new QAction(QIcon(fileNameIcon), TR("&Check for updates"), this);
    setActionTips(checkUpdateAction, TR("Check for updates"));
    connect(checkUpdateAction, SIGNAL(triggered()), this, SLOT(checkUpdate()));
    helpMenu->addAction(checkUpdateAction);
#endif

    fileNameIcon = nelsonPath + QString("/resources/information-icon.svg");
    aboutAction = new QAction(QIcon(fileNameIcon), TR("&About"), this);
    aboutAction->setMenuRole(QAction::AboutRole);
    setActionTips(aboutAction, TR("About"));
    connect(aboutAction, SIGNAL(triggered()), this, SLOT(about()));
    helpMenu->addAction(aboutAction);
}
//=============================================================================
void
QtMainWindow::closeEvent(QCloseEvent* event)
{
    if (!NelsonConfiguration::getInstance()->isClosing() && !qtTerminal->isAtPrompt()) {
        QMessageBox::StandardButton reply
            = QMessageBox::question(this, _("Close confirmation").c_str(),
                _("Are you sure to quit?").c_str(), QMessageBox::Yes | QMessageBox::No);
        if (reply == QMessageBox::No) {
            NelsonConfiguration::getInstance()->setAsClosing(false);
            event->ignore();
            return;
        }
    }
    declareAsClosed();
    postCommand(L"exit");
    event->accept();
}
//=============================================================================
QtTerminal*
QtMainWindow::getQtTerminal()
{
    return qtTerminal;
}
//=============================================================================
static std::wstring
quotePathForNelson(const QString& path)
{
    QString quoted(path);
    quoted.replace("'", "''");
    return Nelson::QStringTowstring(quoted);
}
//=============================================================================
// A .m file may hold either a script or a function definition: only a script can
// be executed with run(), a function file is opened in the text editor.
static bool
isFunctionFile(const QString& localFile)
{
    QFile file(localFile);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
        return false;
    }
    int checkedLines = 0;
    while (!file.atEnd() && checkedLines < 200) {
        QString line = QString::fromUtf8(file.readLine()).trimmed();
        checkedLines++;
        if (line.isEmpty() || line.startsWith("%") || line.startsWith("//")
            || line.startsWith("#")) {
            continue;
        }
        return line.startsWith("function ") || line.startsWith("function\t") || line == "function";
    }
    return false;
}
//=============================================================================
void
QtMainWindow::handleDroppedFile(const QString& localFile, QStringList& unhandled)
{
    QFileInfo fileInfo(localFile);
    if (!fileInfo.exists()) {
        unhandled << localFile;
        return;
    }
    if (fileInfo.isDir()) {
        setCurrentDirectory(fileInfo.absoluteFilePath());
        return;
    }
    const QString suffix = fileInfo.suffix().toLower();
    const std::wstring quotedFilename = quotePathForNelson(fileInfo.absoluteFilePath());

    if (suffix == "m" || suffix == "nls") {
        if (isFunctionFile(localFile)) {
            postCommand(L"edit('" + quotedFilename + L"')");
            return;
        }
        if (qtTerminal && !qtTerminal->isAtPrompt()) {
            QMessageBox::warning(this,
                Nelson::resolveMessageTemplate("Nelson:gui:execute", "Execute...").c_str(),
                Nelson::resolveMessageTemplate(
                    "Nelson:gui:interpreterCurrentlyRuns", "Interpreter currently runs.")
                    .c_str());
            return;
        }
        if (qtTerminal) {
            qtTerminal->outputMessage(L"\n");
            qtTerminal->sendReturnKey();
        }
        postCommand(L"run('" + quotedFilename + L"')");
        return;
    }
    if (suffix == "mat" || suffix == "nh5") {
        if (qtTerminal && !qtTerminal->isAtPrompt()) {
            QMessageBox::warning(this,
                Nelson::resolveMessageTemplate("Nelson:gui:loadWorkspace", "Load workspace...")
                    .c_str(),
                Nelson::resolveMessageTemplate(
                    "Nelson:gui:interpreterCurrentlyRuns", "Interpreter currently runs.")
                    .c_str());
            return;
        }
        if (qtTerminal) {
            qtTerminal->outputMessage(L"\n");
            qtTerminal->sendReturnKey();
        }
        postCommand(L"load('" + quotedFilename + L"')");
        return;
    }
    static const QStringList editableSuffixes = { "txt", "nlf", "md", "json", "xml", "c", "cpp",
        "h", "hpp", "py", "js", "csv", "log", "qss", "ini", "cmake", "yml", "yaml" };
    if (editableSuffixes.contains(suffix)) {
        postCommand(L"edit('" + quotedFilename + L"')");
        return;
    }
    unhandled << fileInfo.fileName();
}
//=============================================================================
void
QtMainWindow::dragEnterEvent(QDragEnterEvent* event)
{
    if (event->mimeData()->hasFormat("text/uri-list")) {
        if (!dropIndicator) {
            dropIndicator = new QRubberBand(QRubberBand::Rectangle, this);
        }
        QWidget* target = centralWidget() ? centralWidget() : this;
        dropIndicator->setGeometry(QRect(target->mapTo(this, QPoint(0, 0)), target->size()));
        dropIndicator->show();
        event->acceptProposedAction();
    } else {
        event->ignore();
    }
}
//=============================================================================
void
QtMainWindow::dragLeaveEvent(QDragLeaveEvent* event)
{
    if (dropIndicator) {
        dropIndicator->hide();
    }
    QMainWindow::dragLeaveEvent(event);
}
//=============================================================================
void
QtMainWindow::dropEvent(QDropEvent* event)
{
    if (dropIndicator) {
        dropIndicator->hide();
    }
    if (!event->mimeData()->hasFormat("text/uri-list")) {
        event->ignore();
        return;
    }
    QStringList unhandled;
    const QList<QUrl> urls = event->mimeData()->urls();
    for (const QUrl& url : urls) {
        QString localFile = url.toLocalFile();
        if (localFile.isEmpty()) {
            unhandled << url.toString();
            continue;
        }
        handleDroppedFile(localFile, unhandled);
    }
    if (!unhandled.isEmpty()) {
        QMessageBox::information(this, TR("Drag and drop"),
            TR("Unsupported file type:") + QString("\n") + unhandled.join(QString("\n")));
    }
    event->acceptProposedAction();
}
//=============================================================================
void
QtMainWindow::declareAsClosed()
{
    bClosed = true;
    // Stop reacting to focus changes before destroying the dock widgets:
    // destroyDockWidgets() clears child focus, which would otherwise re-enter
    // updateActivePanel() and walk a half-destroyed widget tree (crash on close).
    shuttingDown = true;
    disconnect(focusChangedConnection);
    if (statusBarTimer) {
        statusBarTimer->stop();
    }
    destroyDockWidgets();
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
QtMainWindow::onCloseVariablesEditor()
{
    if (variablesEditorAction) {
        variablesEditorAction->setChecked(false);
    }
}
//=============================================================================
void
QtMainWindow::onCloseProfilerBrowser()
{
    if (profilerBrowserAction) {
        profilerBrowserAction->setChecked(false);
    }
}
//=============================================================================
void
QtMainWindow::onCloseNewsfeedBrowser()
{
    if (newsfeedBrowserAction) {
        newsfeedBrowserAction->setChecked(false);
    }
}
//=============================================================================
void
QtMainWindow::buildDefaultLayout()
{
    QDockWidget* qtFileBrowser = (QDockWidget*)FileBrowser::getFileBrowser();
    QDockWidget* qtWorkspaceBrowser = (QDockWidget*)WorkspaceBrowser::getWorkspaceBrowser();
    QDockWidget* qtHistoryBrowser = (QDockWidget*)HistoryBrowser::getHistoryBrowser();
    QDockWidget* qtVariablesEditor = (QDockWidget*)VariablesEditor::getVariablesEditor();
    QDockWidget* qtProfilerBrowser = profilerBrowserDock;
    QDockWidget* qtNewsfeedBrowser = newsfeedBrowserDock;

    QDockWidget* all[] = { commandWindowDock, qtFileBrowser, qtWorkspaceBrowser, qtHistoryBrowser,
        qtVariablesEditor, qtProfilerBrowser, qtNewsfeedBrowser };
    for (QDockWidget* dock : all) {
        if (!dock) {
            continue;
        }
        dock->setFloating(false);
        QtDockTitleBar* titleBar = qobject_cast<QtDockTitleBar*>(dock->titleBarWidget());
        if (titleBar) {
            titleBar->setCollapsed(false);
        }
    }

    for (QDockWidget* dock : all) {
        if (dock) {
            removeDockWidget(dock);
        }
    }

    if (qtFileBrowser) {
        addDockWidget(Qt::LeftDockWidgetArea, qtFileBrowser);
        if (qtWorkspaceBrowser) {
            splitDockWidget(qtFileBrowser, qtWorkspaceBrowser, Qt::Vertical);
        }
        splitDockWidget(qtFileBrowser, commandWindowDock, Qt::Horizontal);
    } else {
        addDockWidget(Qt::RightDockWidgetArea, commandWindowDock);
    }
    if (qtWorkspaceBrowser) {
        if (qtHistoryBrowser) {
            tabifyDockWidget(qtWorkspaceBrowser, qtHistoryBrowser);
        }
        if (qtVariablesEditor) {
            tabifyDockWidget(qtWorkspaceBrowser, qtVariablesEditor);
        }
        if (qtProfilerBrowser) {
            tabifyDockWidget(qtWorkspaceBrowser, qtProfilerBrowser);
        }
        if (qtNewsfeedBrowser) {
            tabifyDockWidget(qtWorkspaceBrowser, qtNewsfeedBrowser);
        }
    }

    commandWindowDock->show();
    if (qtFileBrowser) {
        qtFileBrowser->show();
    }
    if (qtWorkspaceBrowser) {
        qtWorkspaceBrowser->show();
    }
    if (qtHistoryBrowser) {
        qtHistoryBrowser->hide();
    }
    if (qtVariablesEditor) {
        qtVariablesEditor->hide();
    }
    if (qtProfilerBrowser) {
        qtProfilerBrowser->hide();
    }
    if (qtNewsfeedBrowser) {
        qtNewsfeedBrowser->hide();
    }

    if (qtFileBrowser) {
        resizeDocks({ qtFileBrowser, commandWindowDock }, { 320, 960 }, Qt::Horizontal);
    }
    if (qtFileBrowser && qtWorkspaceBrowser) {
        resizeDocks({ qtFileBrowser, qtWorkspaceBrowser }, { 520, 360 }, Qt::Vertical);
    }
    if (qtTerminal) {
        qtTerminal->setFocus();
    }
}
//=============================================================================
void
QtMainWindow::applyLayout(Qt::DockWidgetArea fileBrowserArea,
    Qt::DockWidgetArea workspaceBrowserArea, Qt::DockWidgetArea historyBrowserArea, bool visible)
{
    QDockWidget* qtWorkspaceBrowser = (QDockWidget*)WorkspaceBrowser::getWorkspaceBrowser();
    QDockWidget* qtHistoryBrowser = (QDockWidget*)HistoryBrowser::getHistoryBrowser();
    QDockWidget* qtFileBrowser = (QDockWidget*)FileBrowser::getFileBrowser();
    QDockWidget* qtVariablesEditor = (QDockWidget*)VariablesEditor::getVariablesEditor();
    QDockWidget* qtProfilerBrowser = profilerBrowserDock;
    QDockWidget* qtNewsfeedBrowser = newsfeedBrowserDock;

    QDockWidget* docks[] = { qtFileBrowser, qtWorkspaceBrowser, qtHistoryBrowser, qtVariablesEditor,
        qtProfilerBrowser, qtNewsfeedBrowser };
    for (QDockWidget* dock : docks) {
        if (dock) {
            dock->hide();
            dock->setFloating(false);
            // layout presets promise a predictable result: expanded panels
            QtDockTitleBar* titleBar = qobject_cast<QtDockTitleBar*>(dock->titleBarWidget());
            if (titleBar) {
                titleBar->setCollapsed(false);
            }
        }
    }

    if (visible) {
        if (qtFileBrowser) {
            addDockWidget(fileBrowserArea, qtFileBrowser);
            qtFileBrowser->show();
        }
        if (qtWorkspaceBrowser) {
            addDockWidget(workspaceBrowserArea, qtWorkspaceBrowser);
            qtWorkspaceBrowser->show();
        }
        if (qtHistoryBrowser) {
            addDockWidget(historyBrowserArea, qtHistoryBrowser);
            qtHistoryBrowser->show();
        }
    }

    // a preset must yield a complete, predictable state: the command window is
    // always brought back visible, docked and expanded, whatever state the user
    // left it in (floated, collapsed or hidden).
    ensureCommandWindowRestored();

    historyBrowserAction->setChecked(visible);
    fileBrowserAction->setChecked(visible);
    workspaceBrowserAction->setChecked(visible);
    variablesEditorAction->setChecked(false);
    profilerBrowserAction->setChecked(false);
    newsfeedBrowserAction->setChecked(false);

    saveDockWidgetPositions();
}
//=============================================================================
void
QtMainWindow::ensureCommandWindowRestored()
{
    if (!commandWindowDock) {
        return;
    }
    if (commandWindowDock->isFloating()) {
        commandWindowDock->setFloating(false);
    }
    QtDockTitleBar* titleBar = qobject_cast<QtDockTitleBar*>(commandWindowDock->titleBarWidget());
    if (titleBar) {
        titleBar->setCollapsed(false);
    }
    if (!commandWindowDock->isVisible()) {
        // if it was hidden, put it back where the default layout expects it
        addDockWidget(Qt::TopDockWidgetArea, commandWindowDock);
    }
    commandWindowDock->show();
    commandWindowDock->raise();
    if (qtTerminal) {
        qtTerminal->setFocus();
    }
}
//=============================================================================
void
QtMainWindow::onLayoutTerminalOnly()
{
    applyLayout(Qt::LeftDockWidgetArea, Qt::RightDockWidgetArea, Qt::RightDockWidgetArea, false);
}
//=============================================================================
void
QtMainWindow::onLayoutDefaultAction()
{
    buildDefaultLayout();
    layoutDefaultAction->setChecked(true);
    fileBrowserAction->setChecked(true);
    workspaceBrowserAction->setChecked(true);
    historyBrowserAction->setChecked(false);
    variablesEditorAction->setChecked(false);
    profilerBrowserAction->setChecked(false);
    newsfeedBrowserAction->setChecked(false);
    saveDockWidgetPositions();
}
//=============================================================================
void
QtMainWindow::buildThreeColumnsLayout()
{
    QDockWidget* qtFileBrowser = (QDockWidget*)FileBrowser::getFileBrowser();
    QDockWidget* qtWorkspaceBrowser = (QDockWidget*)WorkspaceBrowser::getWorkspaceBrowser();
    QDockWidget* qtHistoryBrowser = (QDockWidget*)HistoryBrowser::getHistoryBrowser();
    QDockWidget* qtVariablesEditor = (QDockWidget*)VariablesEditor::getVariablesEditor();
    QDockWidget* qtProfilerBrowser = profilerBrowserDock;
    QDockWidget* qtNewsfeedBrowser = newsfeedBrowserDock;

    QDockWidget* all[] = { commandWindowDock, qtFileBrowser, qtWorkspaceBrowser, qtHistoryBrowser,
        qtVariablesEditor, qtProfilerBrowser, qtNewsfeedBrowser };
    for (QDockWidget* dock : all) {
        if (!dock) {
            continue;
        }
        dock->setFloating(false);
        QtDockTitleBar* titleBar = qobject_cast<QtDockTitleBar*>(dock->titleBarWidget());
        if (titleBar) {
            titleBar->setCollapsed(false);
        }
        removeDockWidget(dock);
    }

    if (!threeColumnLayoutState.isEmpty()) {
        restoreState(threeColumnLayoutState);
    } else {
        if (qtFileBrowser) {
            addDockWidget(Qt::LeftDockWidgetArea, qtFileBrowser);
            splitDockWidget(qtFileBrowser, commandWindowDock, Qt::Horizontal);
        } else {
            addDockWidget(Qt::LeftDockWidgetArea, commandWindowDock);
        }
        if (qtWorkspaceBrowser) {
            splitDockWidget(commandWindowDock, qtWorkspaceBrowser, Qt::Horizontal);
            if (qtHistoryBrowser) {
                splitDockWidget(qtWorkspaceBrowser, qtHistoryBrowser, Qt::Vertical);
            }
            if (qtVariablesEditor) {
                tabifyDockWidget(qtWorkspaceBrowser, qtVariablesEditor);
            }
            if (qtProfilerBrowser) {
                tabifyDockWidget(qtWorkspaceBrowser, qtProfilerBrowser);
            }
            if (qtNewsfeedBrowser) {
                tabifyDockWidget(qtWorkspaceBrowser, qtNewsfeedBrowser);
            }
        }
    }

    if (qtFileBrowser) {
        qtFileBrowser->show();
    }
    if (qtWorkspaceBrowser) {
        qtWorkspaceBrowser->show();
    }
    if (qtHistoryBrowser) {
        qtHistoryBrowser->show();
    }
    if (qtVariablesEditor) {
        qtVariablesEditor->hide();
    }
    if (qtProfilerBrowser) {
        qtProfilerBrowser->hide();
    }
    if (qtNewsfeedBrowser) {
        qtNewsfeedBrowser->hide();
    }
    commandWindowDock->show();
    commandWindowDock->raise();

    QList<QDockWidget*> columns;
    QList<int> widths;
    if (qtFileBrowser) {
        columns << qtFileBrowser;
        widths << 260;
    }
    columns << commandWindowDock;
    widths << 760;
    if (qtWorkspaceBrowser) {
        columns << qtWorkspaceBrowser;
        widths << 320;
    }
    if (columns.size() > 1) {
        resizeDocks(columns, widths, Qt::Horizontal);
    }
    if (qtWorkspaceBrowser && qtHistoryBrowser) {
        resizeDocks({ qtWorkspaceBrowser, qtHistoryBrowser }, { 520, 360 }, Qt::Vertical);
    }
    if (threeColumnLayoutState.isEmpty()) {
        threeColumnLayoutState = saveState();
    }
    if (qtTerminal) {
        qtTerminal->setFocus();
    }
}
//=============================================================================
void
QtMainWindow::onLayoutThreeColumnsAction()
{
    buildThreeColumnsLayout();
    fileBrowserAction->setChecked(true);
    workspaceBrowserAction->setChecked(true);
    historyBrowserAction->setChecked(true);
    variablesEditorAction->setChecked(false);
    profilerBrowserAction->setChecked(false);
    newsfeedBrowserAction->setChecked(false);
    saveDockWidgetPositions();
}
//=============================================================================
QStringList
QtMainWindow::userLayoutNames() const
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.beginGroup(SETTING_MAIN_WINDOW_USER_LAYOUTS_GROUP);
    QStringList names = settings.childKeys();
    settings.endGroup();
    names.sort(Qt::CaseInsensitive);
    return names;
}
//=============================================================================
void
QtMainWindow::onSaveCurrentLayout()
{
    bool ok = false;
    QString name = QInputDialog::getText(
        this, TR("Save current layout"), TR("Layout name:"), QLineEdit::Normal, QString(), &ok);
    if (!ok) {
        return;
    }
    name = name.trimmed();
    // '/' and '\' are QSettings group separators
    name.replace('/', '-');
    name.replace('\\', '-');
    if (name.isEmpty()) {
        return;
    }
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.beginGroup(SETTING_MAIN_WINDOW_USER_LAYOUTS_GROUP);
    settings.setValue(name, saveState());
    settings.endGroup();
    registerUserLayoutPaletteActions();
}
//=============================================================================
void
QtMainWindow::applyUserLayout(const QString& name)
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.beginGroup(SETTING_MAIN_WINDOW_USER_LAYOUTS_GROUP);
    QByteArray state = settings.value(name).toByteArray();
    settings.endGroup();
    if (state.isEmpty()) {
        return;
    }
    restoreState(state);
    // a user layout replaces any preset: none of them is current anymore
    desktopClearExclusiveActionSelection(layoutActionGroup,
        { layoutDefaultAction, layoutTerminalOnlyAction, layoutThreeColumnsAction });
    saveDockWidgetPositions();
}
//=============================================================================
void
QtMainWindow::removeUserLayout(const QString& name)
{
    QSettings settings(NELSON_SETTINGS_ORGANIZATION, NELSON_SETTINGS_APPLICATION);
    settings.beginGroup(SETTING_MAIN_WINDOW_USER_LAYOUTS_GROUP);
    settings.remove(name);
    settings.endGroup();
    registerUserLayoutPaletteActions();
}
//=============================================================================
void
QtMainWindow::updateUserLayoutsMenuSection()
{
    for (QAction* action : userLayoutMenuActions) {
        layoutMenu->removeAction(action);
        action->deleteLater();
    }
    userLayoutMenuActions.clear();
    QStringList names = userLayoutNames();
    if (names.isEmpty()) {
        return;
    }
    userLayoutMenuActions.append(layoutMenu->addSeparator());
    for (const QString& name : names) {
        QAction* action = new QAction(name, this);
        connect(action, &QAction::triggered, this, [this, name]() { applyUserLayout(name); });
        layoutMenu->addAction(action);
        userLayoutMenuActions.append(action);
    }
    QMenu* removeMenu = new QMenu(TR("&Remove layout"), layoutMenu);
    for (const QString& name : names) {
        QAction* action = new QAction(name, removeMenu);
        connect(action, &QAction::triggered, this, [this, name]() { removeUserLayout(name); });
        removeMenu->addAction(action);
    }
    QAction* removeMenuAction = layoutMenu->addMenu(removeMenu);
    // the menu is not owned by its action: it must follow it when the section
    // is rebuilt, otherwise every menu opening leaks one QMenu.
    connect(removeMenuAction, &QObject::destroyed, removeMenu, &QObject::deleteLater);
    userLayoutMenuActions.append(removeMenuAction);
}
//=============================================================================
