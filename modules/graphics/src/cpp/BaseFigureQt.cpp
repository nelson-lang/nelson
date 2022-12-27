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
#include <QtGui/QScreen>
#include "BaseFigureQt.hpp"
#include "RenderQt.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
BaseFigureQt::resizeEvent(QResizeEvent* e)
{
    QWidget::resizeEvent(e);
    hfig->resizeGL(width(), height());
}
//=============================================================================
void
BaseFigureQt::paintEvent(QPaintEvent* e)
{
    QWidget::paintEvent(e);
    QPainter painter(this);
    RenderQt gc(&painter, 0, 0, width(), height());
    hfig->paintMe(gc);
}
//=============================================================================
BaseFigureQt::BaseFigureQt(QWidget* parent, GOFigure* fig) : QWidget(parent) { hfig = fig; }
//=============================================================================
QScreen*
BaseFigureQt::getActiveScreen()
{
    QScreen* pActive = nullptr;
    QWidget* pWidget = this;
    while (pWidget) {
        auto w = pWidget->windowHandle();
        if (w != nullptr) {
            pActive = w->screen();
            break;
        } else
            pWidget = pWidget->parentWidget();
    }

    return pActive;
}
//=============================================================================
void
BaseFigureQt::currentScreenResolution(int& w, int& h)
{
    QScreen* qScreen = getActiveScreen();
    if (qScreen) {
        QRect screenSize = qScreen->availableGeometry();
        w = screenSize.width();
        h = screenSize.height();
    }
}
//=============================================================================
}
//=============================================================================
