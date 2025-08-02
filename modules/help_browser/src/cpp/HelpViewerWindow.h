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
#include <QtWidgets/QMainWindow>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QMessageBox>
#include <QtHelp/QHelpEngine>
#include <QtHelp/QHelpContentWidget>
#include <QtHelp/QHelpIndexWidget>
#include <QtHelp/QHelpSearchQueryWidget>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QSplitter>
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QApplication>
#include <QtGui/QKeyEvent>
#include <string>
#include "HelpViewer.h"
//=============================================================================
class TextEdit;
//=============================================================================
class HelpViewerWindow : public QMainWindow
{
    Q_OBJECT

public:
    HelpViewerWindow(const std::wstring& qchFilename, const std::wstring& openUrl);
    ~HelpViewerWindow();
    void
    setSource(const std::wstring& url);
    void
    search(const std::wstring& name);

public slots:
    void
    search();

private:
    HelpViewer* helpViewer;
    QHelpEngine* helpEngine;
    QHelpSearchEngine* searchHelpEngine;
    QHelpSearchQueryWidget* queryWidget;
    QHelpSearchResultWidget* resultWidget;
    QHelpContentWidget* contentWidget;
    QHelpIndexWidget* indexWidget;
    QWidget* searchWidget;
    QTabWidget* tabWidget;

    QToolBar* toolbar;
    QAction* printAction;
    QAction* homeAction;
    QAction* previousAction;
    QAction* nextAction;
    QAction* zoomInAction;
    QAction* zoomOutAction;
    QAction* zoomDefaultAction;

    QString _qchFilename;
    QString _openUrl;
    void
    createConnections();
    void
    createHelpWindow();
    void
    createToolbar();
};
//=============================================================================
