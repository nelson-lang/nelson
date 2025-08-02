//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtHelp/QHelpContentWidget>
#include <QtHelp/QHelpIndexWidget>
#include <QtHelp/QHelpSearchEngine>
#include <QtHelp/QHelpSearchQueryWidget>
#include <QtHelp/QHelpSearchResultWidget>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QToolBar>
//=============================================================================
#include "HelpViewerWindow.h"
#include "HelpViewer.h"
#include "HelpCollection.hpp"
#include "QStringConverter.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
HelpViewerWindow::HelpViewerWindow(const std::wstring& qchFilename, const std::wstring& openUrl)
    : _qchFilename(Nelson::wstringToQString(qchFilename))
    , _openUrl(Nelson::wstringToQString(openUrl))
{
    createHelpWindow();
    setWindowTitle(tr("Nelson's Help"));
    resize(600, 400);
}
//=============================================================================
HelpViewerWindow::~HelpViewerWindow()
{
    _qchFilename.clear();
    _openUrl.clear();

    queryWidget = nullptr;
    resultWidget = nullptr;
    contentWidget = nullptr;
    indexWidget = nullptr;

    if (helpEngine) {
        delete helpEngine;
        helpEngine = nullptr;
    }
    if (helpViewer) {
        delete helpViewer;
        helpViewer = nullptr;
    }

    if (printAction) {
        delete printAction;
        printAction = nullptr;
    }
    if (homeAction) {
        delete homeAction;
        homeAction = nullptr;
    }
    if (previousAction) {
        delete previousAction;
        previousAction = nullptr;
    }
    if (nextAction) {
        delete nextAction;
        nextAction = nullptr;
    }
    if (zoomInAction) {
        delete zoomInAction;
        zoomInAction = nullptr;
    }
    if (zoomOutAction) {
        delete zoomOutAction;
        zoomOutAction = nullptr;
    }
    if (zoomDefaultAction) {
        delete zoomDefaultAction;
        zoomDefaultAction = nullptr;
    }

    if (toolbar) {
        delete toolbar;
        toolbar = nullptr;
    }
}
//=============================================================================
void
HelpViewerWindow::createConnections()
{
    connect(contentWidget, SIGNAL(linkActivated(const QUrl&)), helpViewer,
        SLOT(setUrlSource(const QUrl&)));

    connect(indexWidget, SIGNAL(documentActivated(QHelpLink, QString)), helpViewer,
        SLOT(indexActivated(QHelpLink, QString)));

    connect(indexWidget, SIGNAL(documentsActivated(QList<QHelpLink>, QString)), helpViewer,
        SLOT(indexesActivated(QList<QHelpLink>, QString)));

    connect(queryWidget, SIGNAL(search()), this, SLOT(search()));

    connect(resultWidget, SIGNAL(requestShowLink(const QUrl&)), helpViewer,
        SLOT(setUrlSource(const QUrl&)));
}
//=============================================================================
void
HelpViewerWindow::setSource(const std::wstring& url)
{
    QUrl qUrl(Nelson::wstringToQString(url));
    if (helpViewer) {
        helpViewer->setSource(qUrl);
    }
}
//=============================================================================
void
HelpViewerWindow::createToolbar()
{
    toolbar = addToolBar(tr("Help's toolbar"));
    QString nelsonPath(Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()));

    QString fileNameIcon;

    fileNameIcon = nelsonPath + QString("/resources/document-print.svg");
    printAction = new QAction(QIcon(fileNameIcon), tr("P&rint"), this);
    printAction->setStatusTip(tr("Print"));
    toolbar->addAction(printAction);

    toolbar->addSeparator();

    fileNameIcon = nelsonPath + QString("/resources/go-home.svg");
    homeAction = new QAction(QIcon(fileNameIcon), tr("&Homepage"), this);
    homeAction->setStatusTip(tr("Homepage"));
    toolbar->addAction(homeAction);

    toolbar->addSeparator();

    fileNameIcon = nelsonPath + QString("/resources/go-previous.svg");
    previousAction = new QAction(QIcon(fileNameIcon), tr("&Previous"), this);
    previousAction->setStatusTip(tr("Previous"));
    toolbar->addAction(previousAction);
    previousAction->setEnabled(false);

    fileNameIcon = nelsonPath + QString("/resources/go-next.svg");
    nextAction = new QAction(QIcon(fileNameIcon), tr("&Next"), this);
    nextAction->setStatusTip(tr("Next"));
    toolbar->addAction(nextAction);
    nextAction->setEnabled(false);

    toolbar->addSeparator();

    fileNameIcon = nelsonPath + QString("/resources/view-zoom-in.svg");
    zoomInAction = new QAction(QIcon(fileNameIcon), tr("Zoom &In"), this);
    zoomInAction->setStatusTip(tr("Zoom In"));
    toolbar->addAction(zoomInAction);

    fileNameIcon = nelsonPath + QString("/resources/view-zoom-out.svg");
    zoomOutAction = new QAction(QIcon(fileNameIcon), tr("Zoom &Out"), this);
    zoomOutAction->setStatusTip(tr("Zoom Out"));
    toolbar->addAction(zoomOutAction);

    fileNameIcon = nelsonPath + QString("/resources/view-zoom-0.svg");
    zoomDefaultAction = new QAction(QIcon(fileNameIcon), tr("Zoom &Default"), this);
    zoomDefaultAction->setStatusTip(tr("Zoom Default"));
    toolbar->addAction(zoomDefaultAction);

    connect(printAction, SIGNAL(triggered()), helpViewer, SLOT(printCurrent()));
    connect(homeAction, SIGNAL(triggered()), helpViewer, SLOT(goHomePage()));
    connect(previousAction, SIGNAL(triggered()), helpViewer, SLOT(backward()));
    connect(nextAction, SIGNAL(triggered()), helpViewer, SLOT(forward()));

    connect(zoomInAction, SIGNAL(triggered()), helpViewer, SLOT(setZoomIn()));
    connect(zoomOutAction, SIGNAL(triggered()), helpViewer, SLOT(setZoomOut()));
    connect(zoomDefaultAction, SIGNAL(triggered()), helpViewer, SLOT(setZoomDefault()));

    connect(helpViewer, SIGNAL(backwardAvailable(bool)), previousAction, SLOT(setEnabled(bool)));
    connect(helpViewer, SIGNAL(forwardAvailable(bool)), nextAction, SLOT(setEnabled(bool)));
}
//=============================================================================
void
HelpViewerWindow::createHelpWindow()
{
    QString nelsonPath(Nelson::wstringToQString(
        Nelson::NelsonConfiguration::getInstance()->getNelsonRootDirectory()));
    QString fileNameIcon = nelsonPath + "/resources/fibonacci.ico";
    QIcon qIcon(fileNameIcon);
    this->setWindowIcon(qIcon);

    QUrl qUrl(_openUrl);

    helpEngine = new QHelpEngine(_qchFilename);
    if (!helpEngine->setupData()) {
        QMessageBox::warning(this, tr("Nelson Documentation"),
            tr("Could not setup the data required for the\n"
               "documentation viewer. Only help texts in\n"
               "the Console Widget will be available."));
    }

    searchHelpEngine = helpEngine->searchEngine();

    tabWidget = new QTabWidget;
    searchWidget = new QWidget(this);
    QVBoxLayout* vLayout = new QVBoxLayout(searchWidget);

    queryWidget = searchHelpEngine->queryWidget();
    resultWidget = searchHelpEngine->resultWidget();
    contentWidget = helpEngine->contentWidget();
    indexWidget = helpEngine->indexWidget();

    vLayout->addWidget(queryWidget);
    vLayout->addWidget(resultWidget);
    searchWidget->setLayout(vLayout);
    searchWidget->setFocusProxy(queryWidget);

    searchHelpEngine->reindexDocumentation();

    tabWidget->addTab(contentWidget, tr("Contents"));
    tabWidget->addTab(indexWidget, tr("Index"));
    tabWidget->addTab(searchWidget, tr("Search"));

    helpViewer = new HelpViewer(helpEngine, qUrl);

    createToolbar();

    createConnections();

    QSplitter* horizSplitter = new QSplitter(Qt::Horizontal);
    horizSplitter->insertWidget(0, tabWidget);
    horizSplitter->insertWidget(1, helpViewer);
    horizSplitter->setStretchFactor(1, 1);

    QSplitter* verticalSplitter = new QSplitter(Qt::Vertical);
    verticalSplitter->insertWidget(0, toolbar);
    verticalSplitter->insertWidget(1, horizSplitter);
    this->setCentralWidget(verticalSplitter);
}
//=============================================================================
void
HelpViewerWindow::search()
{
    if (helpEngine == nullptr || searchHelpEngine == nullptr) {
        return;
    }
    QString searchInput = searchHelpEngine->queryWidget()->searchInput();
    searchHelpEngine->search(searchInput);
}
//=============================================================================
void
HelpViewerWindow::search(const std::wstring& name)
{
    if (helpEngine == nullptr || searchHelpEngine == nullptr) {
        return;
    }
    QString searchInput = Nelson::wstringToQString(name);
    if (tabWidget) {
        tabWidget->setCurrentWidget(searchWidget);
        searchHelpEngine->queryWidget()->setSearchInput(searchInput);
        searchHelpEngine->search(searchInput);
    }
}
//=============================================================================
