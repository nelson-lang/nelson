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
#include <QtGui/QActionGroup>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QLabel>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QRubberBand>
#include <QtWidgets/QDockWidget>
#include <QtCore/QStringList>
#include <QtCore/QTimer>
#include <QtCore/QVariant>
//=============================================================================
class CurrentDirectoryWidget;
class DesktopActionBadge;
class DesktopCommandPalette;
class DesktopPanelRail;
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
    createDockWidgets(Evaluator* eval);

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
    demo();
    void
    packageManager();
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
    showPreferences();

    void
    textEditor();
    void
    nflow();

    void
    commandWindowToggle();
    void
    historyBrowserToggle();
    void
    fileBrowserToggle();
    void
    workspaceBrowserToggle();
    void
    variablesEditorToggle();
    void
    profilerBrowserToggle();
    void
    newsfeedBrowserToggle();
    void
    terminal();

    void
    stopRun();

    void
    updateStatusBar();

    void
    onLayoutTerminalOnly();
    void
    onLayoutDefaultAction();
    void
    onLayoutThreeColumnsAction();
    void
    onSaveCurrentLayout();
    void
    showCommandPalette();

private:
    void
    createMenus();
    void
    createToolbars();
    void
    createPanelsSidebar();
    void
    createStatusBar();
    void
    registerCommandPaletteActions();
    void
    registerUserLayoutPaletteActions();
    void
    bindPanelRailDocks();
    void
    activatePanelFromRail(QAction* action);
    void
    updateActivePanel(QWidget* focusWidget);
    void
    refreshDesktopUi();
    void
    setCurrentDirectory(const QString& directory);
    void
    recordCurrentDirectory();
    void
    refreshCurrentDirectoryHistory();
    void
    focusNextDesktopArea(bool backwards);

    void
    applyLayout(Qt::DockWidgetArea fileBrowserArea, Qt::DockWidgetArea workspaceBrowserArea,
        Qt::DockWidgetArea historyBrowserArea, bool visible);

    // builds the default two column arrangement:
    // [ file browser / workspace | command window ].
    void
    buildDefaultLayout();

    // optional three column preset:
    // [ file browser | command window | workspace / history ].
    void
    buildThreeColumnsLayout();

    // brings the command window back to a visible, docked, expanded, focused
    // state so every layout preset yields a complete result.
    void
    ensureCommandWindowRestored();

    // baseline saved the first time the three-column layout is built (window already
    // shown, so splitDockWidget is reliable). Runtime resets restore this state
    // instead of rebuilding with splits, whose result depends on the current
    // nesting and would otherwise collapse to a single column.
    QByteArray threeColumnLayoutState;

    void
    toggleDockWidget(QDockWidget* dock);

    void
    updateWindowTitle();

    void
    handleDroppedFile(const QString& localFile, QStringList& unhandled);

    QString
    systemInformation() const;

    void
    destroyDockWidgets();

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
    QAction* preferencesAction;
    QAction* exitAction;

    QMenu* editMenu;
    QAction* cutAction;
    QAction* copyAction;
    QAction* pasteAction;
    QAction* selectAllAction;
    QAction* emptyClipboardAction;
    QAction* clearConsoleAction;

    QAction* stopAction;

    QMenu* viewMenu = nullptr;
    QMenu* windowsMenu;
    QAction* commandWindowAction;
    QAction* historyBrowserAction;
    QAction* fileBrowserAction;
    QAction* workspaceBrowserAction;
    QAction* variablesEditorAction;
    QAction* profilerBrowserAction;
    QAction* newsfeedBrowserAction;
    QAction* terminalAction;
    QAction* commandPaletteAction = nullptr;
    QAction* toolbarVisibilityAction = nullptr;
    QAction* sidebarVisibilityAction = nullptr;
    QAction* statusBarVisibilityAction = nullptr;
    // dynamic entries listing the docked client windows (figures, editor),
    // rebuilt each time the Windows menu opens
    QList<QAction*> dockedWindowsMenuActions;

    void
    updateDockedWindowsMenuSection();

    QMenu* layoutMenu;
    QActionGroup* layoutActionGroup;
    QAction* layoutTerminalOnlyAction;
    QAction* layoutDefaultAction;
    QAction* layoutThreeColumnsAction = nullptr;
    QAction* layoutRestoreDefaultAction = nullptr;
    QAction* layoutSaveAction;
    // dynamic entries listing the user saved layouts, rebuilt each time the
    // Layout menu opens
    QList<QAction*> userLayoutMenuActions;
    QList<QAction*> userLayoutPaletteActions;

    QStringList
    userLayoutNames() const;
    void
    updateUserLayoutsMenuSection();
    void
    applyUserLayout(const QString& name);
    void
    removeUserLayout(const QString& name);

    QMenu* helpMenu;
    QAction* helpAction;
    QAction* examplesAction = nullptr;
    QAction* webAction;
    QAction* bugAction;
    QAction* checkUpdateAction = nullptr;

    QAction* aboutAction;

    QAction* editorAction;
    QAction* nflowAction = nullptr;
    QAction* packageManagerAction = nullptr;
    DesktopActionBadge* packageManagerBadge = nullptr;

    QtTerminal* qtTerminal;
    // The command window lives in a dock so it can be collapsed / floated like
    // the browsers instead of being the fixed central widget.
    QDockWidget* commandWindowDock;
    QDockWidget* profilerBrowserDock;
    QDockWidget* newsfeedBrowserDock;

    QToolBar* toolBar = nullptr;
    DesktopPanelRail* panelsSidebar = nullptr;
    // focusChanged -> updateActivePanel: severed on teardown so the slot cannot
    // walk a half-destroyed dock-widget tree while children are being deleted.
    QMetaObject::Connection focusChangedConnection;
    bool shuttingDown = false;
    CurrentDirectoryWidget* currentDirectoryWidget = nullptr;
    DesktopCommandPalette* commandPalette = nullptr;

    QLabel* interpreterStateLabel;
    QTimer* statusBarTimer;
    QString lastCurrentDirectory;
    QString lastRecordedDirectory;
    bool lastAtPrompt;

    // drag and drop feedback
    QRubberBand* dropIndicator;

    bool bClosed;

    bool
    eventFilter(QObject* watched, QEvent* event) override;
    void
    changeEvent(QEvent* event) override;

    void
    dragEnterEvent(QDragEnterEvent* event) override;
    void
    dragLeaveEvent(QDragLeaveEvent* event) override;
    void
    dropEvent(QDropEvent* event) override;

    void
    saveDockWidgetPositions();
    bool
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
    void
    onCloseProfilerBrowser();
    void
    onCloseNewsfeedBrowser();
};
//=============================================================================
