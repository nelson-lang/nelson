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
#include <QtGui/QPaintEvent>
#include <QtWidgets/QApplication>
#include "BaseFigureQt.hpp"
#include "RenderQt.hpp"
#include "GOWindow.hpp"
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
QImage
BaseFigureQt::getFrame()
{
    if (isVisible()) {
        return backStore.toImage().convertToFormat(QImage::Format_ARGB32);
    }
    QPixmap pxmap(grab());
    return pxmap.toImage().convertToFormat(QImage::Format_ARGB32);
}
//=============================================================================
void
BaseFigureQt::paintEvent(QPaintEvent* e)
{
    // Call base class paint event first
    QWidget::paintEvent(e);

    // Get the device pixel ratio once
    qreal dpr = devicePixelRatio();

    // Check if buffer needs to be recreated (e.g., on resize or DPI change)
    QSize scaledSize = size() * dpr;
    if (backStore.isNull() || backStore.size() != scaledSize
        || !qFuzzyCompare(backStore.devicePixelRatio(), dpr)) {
        backStore = QPixmap(scaledSize);
        backStore.fill(Qt::transparent); // Or your background color
        backStore.setDevicePixelRatio(dpr);
        hfig->setRenderingStateInvalid(true);
    }

    // Only redraw if the state is dirty
    if (hfig->isRenderingStateInvalid()) {
        // Create painter for the back buffer
        QPainter bufferPainter(&backStore);

        // Enable antialiasing for smoother rendering
        bufferPainter.setRenderHint(QPainter::Antialiasing);
        bufferPainter.setRenderHint(QPainter::TextAntialiasing);
        bufferPainter.setRenderHint(QPainter::LosslessImageRendering);

        // Clear the buffer with transparency
        bufferPainter.setCompositionMode(QPainter::CompositionMode_Clear);
        bufferPainter.fillRect(QRect(QPoint(0, 0), scaledSize), Qt::transparent);
        bufferPainter.setCompositionMode(QPainter::CompositionMode_SourceOver);

        // Create rendering context and perform drawing
        RenderQt gc(&bufferPainter, 0, 0, width(), height(), L"GL");
        hfig->paintMe(gc);

        // Explicitly end painting on the buffer
        bufferPainter.end();
    }

    // Draw the back buffer to the widget using the clipped region
    QPainter widgetPainter(this);
    widgetPainter.setClipRegion(e->region());
    widgetPainter.drawPixmap(0, 0, backStore);
}
//=============================================================================
BaseFigureQt::BaseFigureQt(QWidget* parent, GOFigure* fig) : QWidget(parent)
{
    setAttribute(Qt::WA_OpaquePaintEvent, false);
    setAttribute(Qt::WA_NoSystemBackground, true);
    hfig = fig;
}
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
