//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include <QtWidgets/QWidget>
#include <QtWidgets/QMenuBar>
#include <QtGui/QIcon>
#include <QtGui/QMouseEvent>
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "QtTranslation.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOWindow.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOFiguresManager.hpp"
#include "GOStringProperty.hpp"
#include "BaseFigureQt.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
#include "Types.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define TITLE_WITH_NAME_FORMAT L"Figure %d: %s"
#define TITLE_DEFAULT_FORMAT L"Figure %d"
//=============================================================================
GOWindow::GOWindow(int64 ahandle) : QMainWindow()
{
    initialized = false;
    QString fileNameIcon
        = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/modules/graphics/resources/icon-mathematical-plot.svg");
    setWindowIcon(QPixmap(fileNameIcon));
    handle = ahandle;
    int figureId = handle + 1;
    goFig = new GOFigure(this, figureId);
    setWindowTitle(wstringToQString(fmt::sprintf(TITLE_DEFAULT_FORMAT, figureId)));
    setFocusPolicy(Qt::ClickFocus);
    qtchild = new BaseFigureQt(NULL, goFig);
    setCentralWidget(qtchild);
    resize(600, 400);
    createActions();
    createMenuBar();
    initialized = true;
}
//=============================================================================
void
GOWindow::closeEvent(QCloseEvent* e)
{
    notifyFigureClosed(handle);
}
//=============================================================================
GOFigure*
GOWindow::getGOFigure()
{
    return goFig;
}
//=============================================================================
int64
GOWindow::getHandle()
{
    return handle;
}
//=============================================================================
void
GOWindow::updateState()
{
    if (!initialized) {
        return;
    }
    if (goFig->hasChanged(GO_NAME_PROPERTY_NAME_STR)) {
        GOStringProperty* name = (GOStringProperty*)goFig->findProperty(GO_NAME_PROPERTY_NAME_STR);
        int figureId = handle + 1;
        if (name->data().empty()) {
            setWindowTitle(wstringToQString(fmt::sprintf(TITLE_DEFAULT_FORMAT, figureId)));
        } else {
            setWindowTitle(
                wstringToQString(fmt::sprintf(TITLE_WITH_NAME_FORMAT, figureId, name->data())));
        }
    }
    GOFourVectorProperty* hfv
        = (GOFourVectorProperty*)goFig->findProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (hfv->isModified()) {
        int ws;
        int hs;
        ((BaseFigureQt*)(qtchild))->currentScreenResolution(ws, hs);
        int w = (int)(hfv->data()[2]);
        int h = (int)(hfv->data()[3]);

        int x = (int)(hfv->data()[0]);
        int y = (int)(hfv->data()[1]);
        move(x, y);
        resize(w, h);
    }
}
//=============================================================================
void
GOWindow::moveEvent(QMoveEvent* e)
{
    QWidget::moveEvent(e);
    goFig->refreshPositionProperty();
}
//=============================================================================
void
GOWindow::focusInEvent(QFocusEvent* event)
{
    QWidget::focusInEvent(event);
    goFig->setFocus();
}
//=============================================================================
void
GOWindow::getClickPosition(int& x, int& y)
{
    QApplication::setOverrideCursor(Qt::CrossCursor);
    m_loop.exec();
    x = click_x;
    y = click_y;
    QApplication::restoreOverrideCursor();
}
//=============================================================================
void
GOWindow::mousePressEvent(QMouseEvent* e)
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)

    click_x = e->position().x();
    click_y = e->position().y();
#else
    click_x = e->x();
    click_y = e->y();
#endif
    m_loop.exit();
}
//=============================================================================
void
GOWindow::createActions()
{
    QString fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/exit.svg");
    this->closeAction = new QAction(QIcon(fileNameIcon), TR("&Close"), this);
    connect(closeAction, SIGNAL(triggered()), this, SLOT(close()));
}
//=============================================================================
void
GOWindow::createMenuBar()
{
    fileMenu = this->menuBar()->addMenu("&File");
    fileMenu->addAction(closeAction);
}
//=============================================================================
}
//=============================================================================
