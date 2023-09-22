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
#define FMT_HEADER_ONLY
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
#include "GOHelpers.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "NelsonPalette.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define TITLE_WITH_FIGURE_NUMBER_FORMAT L"Figure %d"
#define TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT L"Figure %d: %s"
#define TITLE_WITH_NAME_ONLY_FORMAT L"%s"
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
    setWindowTitle(wstringToQString(fmt::sprintf(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId)));
    setFocusPolicy(Qt::ClickFocus);
    qtchild = new BaseFigureQt(NULL, goFig);
    setCentralWidget(qtchild);
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
    resize(600, 400);
    createActions();
    createMenuBar();
    initialized = true;
}
//=============================================================================
void
GOWindow::closeEvent(QCloseEvent* e)
{
    closeFigure(handle, false);
    e->accept();
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
    if (goFig->hasChanged(GO_VISIBLE_PROPERTY_NAME_STR)) {
        if (goFig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
            show();
        } else {
            hide();
        }
    }
    if (goFig->hasChanged(GO_NAME_PROPERTY_NAME_STR)
        || goFig->hasChanged(GO_NUMBER_PROPERTY_NAME_STR)
        || goFig->hasChanged(GO_NUMBER_TITLE_PROPERTY_NAME_STR)) {
        bool withNumberTitle
            = (goFig->stringCheck(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
        int figureId = handle + 1;
        GOStringProperty* name = (GOStringProperty*)goFig->findProperty(GO_NAME_PROPERTY_NAME_STR);
        if (withNumberTitle) {
            if (name->data().empty()) {
                setWindowTitle(
                    wstringToQString(fmt::sprintf(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId)));
            } else {
                setWindowTitle(wstringToQString(fmt::sprintf(
                    TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT, figureId, name->data())));
            }
        } else {
            setWindowTitle(
                wstringToQString(fmt::sprintf(TITLE_WITH_NAME_ONLY_FORMAT, name->data())));
        }
    }

    GOFourVectorProperty* hfv
        = (GOFourVectorProperty*)goFig->findProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (hfv->isModified()) {
        int ws = 0;
        int hs = 0;
        ((BaseFigureQt*)(qtchild))->currentScreenResolution(ws, hs);
        int w = (int)(hfv->data()[2]);
        int h = (int)(hfv->data()[3]);

        int x = (int)(hfv->data()[0]);
        int y = (int)(hfv->data()[1]);

        int transformedY = hs - h - y;
        move(x, transformedY);
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
