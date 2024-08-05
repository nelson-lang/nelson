//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QWindow>
#include <QtGui/QGuiApplication>
#include <QtWidgets/QApplication>
#include "BaseFigureQt.hpp"
#include "RenderQt.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
BaseFigureQt::resizeEvent(QResizeEvent* e)
{
    hfig->resizeGL(width(), height());
    QWidget::resizeEvent(e);
}
//=============================================================================
void
BaseFigureQt::paintEvent(QPaintEvent* e)
{
    QWidget::paintEvent(e);
    QPainter painter(this);
    RenderQt gc(&painter, 0, 0, width(), height(), L"GL");
    hfig->paintMe(gc);
}
//=============================================================================
BaseFigureQt::BaseFigureQt(QWidget* parent, GOFigure* fig) : QWidget(parent) { hfig = fig; }
//=============================================================================
QScreen*
BaseFigureQt::getActiveScreen()
{
    QScreen* screen = nullptr;
    QScreen* primaryScreen = QGuiApplication::primaryScreen();
    QWidget* widget = QApplication::activeWindow();
    if (widget) {
        QPoint widgetPos = widget->pos();
        screen = QGuiApplication::screenAt(widgetPos);
    }
    if (!screen) {
        screen = primaryScreen;
    }
    return screen;
}
//=============================================================================
bool
BaseFigureQt::currentScreenResolution(int& w, int& h)
{
    QScreen* qScreen = getActiveScreen();
    if (qScreen) {
        QRect screenSize = qScreen->geometry();
        w = screenSize.width();
        h = screenSize.height();
        return true;
    } else {
        w = -1;
        h = -1;
    }
    return false;
}
//=============================================================================
int
BaseFigureQt::getCurrentScreenHeight()
{
    int w = 0;
    int h = 0;
    currentScreenResolution(w, h);
    return h;
}
//=============================================================================
int
BaseFigureQt::getCurrentScreenWidth()
{
    int w = 0;
    int h = 0;
    currentScreenResolution(w, h);
    return w;
}
//=============================================================================
}
//=============================================================================
