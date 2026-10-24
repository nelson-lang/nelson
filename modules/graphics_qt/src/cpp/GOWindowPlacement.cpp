//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QGuiApplication>
#include <QtGui/QMoveEvent>
#include <QtGui/QScreen>
#include <QtWidgets/QLayout>
#include "GOWindow.hpp"
#include "BaseFigureQt.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOWindow::adjustGeometryToDrawingArea(int x, int y, int width, int height)
{
    QWidget* drawingArea = centralWidget();
    if (!drawingArea) {
        return;
    }
    if (layout()) {
        layout()->activate();
    }
    int deltaWidth = width - drawingArea->width();
    int deltaHeight = height - drawingArea->height();
    if (deltaWidth == 0 && deltaHeight == 0) {
        return;
    }
    QRect currentGeometry = geometry();
    setGeometry(x, y, currentGeometry.width() + deltaWidth, currentGeometry.height() + deltaHeight);
}
//=============================================================================
void
GOWindow::setFigureGeometryFromInnerPosition(int x, int y, int width, int height, int screenHeight)
{
    // Position is the drawing area: the window geometry (which already excludes the
    // title bar and borders) starts above it by the menu/tool bars.
    // Chrome (frame borders, menu/tool bars) is measured in the current, already laid-out
    // state and added to the requested drawing size. Measuring after the resize reads a
    // drawing area whose QResizeEvent is still pending (headless figures have no event
    // loop) and gave a window of (2 * requested - previous).
    int chromeWidth = 0;
    int chromeHeight = figureWindowExtraHeight();
    int chromeTop = chromeHeight;
    if (QWidget* drawingArea = centralWidget()) {
        if (layout()) {
            layout()->activate();
        }
        if (drawingArea->width() > 0 && drawingArea->height() > 0) {
            int measuredWidth = geometry().width() - drawingArea->width();
            int measuredHeight = geometry().height() - drawingArea->height();
            chromeWidth = measuredWidth > 0 ? measuredWidth : 0;
            chromeHeight = measuredHeight > 0 ? measuredHeight : 0;
            chromeTop = drawingArea->y();
        }
    }
    int windowTop = screenHeight - height - y - chromeTop;
    if (windowTop < 0) {
        windowTop = 0;
    }
    setGeometry(x, windowTop, width + chromeWidth, height + chromeHeight);
    requestedInnerWidth = width;
    requestedInnerHeight = height;
    drawingAreaRefitDone = false;
    // the drawing area reported the size during setGeometry on most platforms: when it has
    // not (Cocoa resizes later), the size is applied now and the report must still run the
    // SizeChangedFcn
    if (goFig && (goFig->getWidth() != width || goFig->getHeight() != height)) {
        goFig->setRenderingSize(width, height);
        goFig->expectSizeReport();
    }
}
//=============================================================================
void
GOWindow::refitDrawingAreaToRequest()
{
    if (drawingAreaRefitDone || requestedInnerWidth <= 0 || requestedInnerHeight <= 0
        || isDockedInDesktop()) {
        return;
    }
    QWidget* drawingArea = centralWidget();
    if (!drawingArea) {
        return;
    }
    if (layout()) {
        layout()->activate();
    }
    int deltaWidth = requestedInnerWidth - drawingArea->width();
    int deltaHeight = requestedInnerHeight - drawingArea->height();
    if (deltaWidth == 0 && deltaHeight == 0) {
        return;
    }
    drawingAreaRefitDone = true;
    QRect currentGeometry = geometry();
    int top = currentGeometry.y() - deltaHeight;
    if (top < 0) {
        top = 0;
    }
    adjustGeometryToDrawingArea(
        currentGeometry.x(), top, requestedInnerWidth, requestedInnerHeight);
}
//=============================================================================
void
GOWindow::presentNativeWindow()
{
    if (!nativeWindowPresentationAvailable()) {
        return;
    }
    if (isVisible()) {
        raise();
        activateWindow();
        setFocus(Qt::ActiveWindowFocusReason);
        return;
    }
    // Paint the first frame into the framebuffer while the window is hidden
    // (prerenderFirstFrame), so it maps with the figure background and content instead of
    // flashing black. Show it directly at its target geometry: parking it off-screen and
    // moving it after show() failed with a live event loop, because the native window was
    // realized at the parked spot.
    const QRect targetGeometry = geometry();
    // The target geometry is transient bookkeeping until show() applies it: it
    // must never be read back into the figure Position (see hostFrameGeometry /
    // moveEvent) while the window is still being realized.
    presentationTargetGeometry = targetGeometry;
    try {
        if (BaseFigureQt* baseFigure = dynamic_cast<BaseFigureQt*>(qtchild)) {
            baseFigure->prerenderFirstFrame();
        }
        setGeometry(targetGeometry);
        show();
        // Re-apply after show() so the window lands where Position asks even if
        // the platform realized it at a default placement first.
        setGeometry(targetGeometry);
        // The realized window knows its real chrome: fit the drawing area to the
        // requested Position before the geometry is read back.
        refitDrawingAreaToRequest();
        if (qtchild) {
            qtchild->repaint();
        }
        repaint();
        presentationTargetGeometry = QRect();
        if (initialized && goFig && !isDockedInDesktop()) {
            goFig->refreshPositionProperties(false);
        }
        raise();
        activateWindow();
        setFocus(Qt::ActiveWindowFocusReason);
    } catch (...) {
        // Realizing the native window failed. On macOS a process without window-server
        // access throws an Objective-C exception from AppKit while the style mask is applied,
        // and unwinding it through Qt aborts the interpreter. Swallow it, latch native
        // presentation off for the session and tear the half-built window down. The figure
        // keeps its off-screen render surface, so getframe/saveas still work.
        presentationTargetGeometry = QRect();
        disableNativeWindowPresentation();
        try {
            QWidget::destroy();
        } catch (...) {
        }
    }
}
//=============================================================================
bool
GOWindow::isTransientPlacement()
{
    return presentationTargetGeometry.isValid() || isMinimized();
}
//=============================================================================
void
GOWindow::moveEvent(QMoveEvent* e)
{
    QWidget::moveEvent(e);
    // while docked, moves are dock-layout internals: they must not feed the
    // figure Position property (coordinates are desktop-relative). Parking the
    // window off-screen or minimizing it are not user placements either.
    if (initialized && !isDockedInDesktop() && !isTransientPlacement()) {
        goFig->refreshPositionProperties(false);
    }
}
//=============================================================================
bool
GOWindow::hostFrameGeometry(double& x, double& y, double& width, double& height, double& frameTop)
{
    if (m_headless) {
        return false;
    }
    // A minimized window sits at the platform's iconic coordinates and a window
    // being presented is parked off-screen: report the placement the window
    // has (or will have) on screen instead.
    QRect g = geometry();
    if (presentationTargetGeometry.isValid()) {
        g = presentationTargetGeometry;
    } else if (isMinimized() && normalGeometry().isValid()) {
        g = normalGeometry();
    }
    QPoint drawingOffset = centralWidget() ? centralWidget()->pos() : QPoint();
    x = g.x() + drawingOffset.x();
    y = g.y() + drawingOffset.y();
    width = g.width();
    height = g.height();
    frameTop = 0;
    return true;
}
//=============================================================================
bool
GOWindow::hostDrawingAreaScreenRect(double& x, double& y, double& width, double& height)
{
    // Measured from the native window (not from the placement bookkeeping), so
    // a constant offset shared by placement and read-back shows up here.
    QWidget* drawingArea = centralWidget();
    QScreen* screen = QGuiApplication::primaryScreen();
    if (m_headless || !drawingArea || !screen || !isVisible() || isMinimized()) {
        return false;
    }
    const QRect screenGeometry = screen->geometry();
    const QPoint topLeft = drawingArea->mapToGlobal(QPoint(0, 0));
    x = topLeft.x() - screenGeometry.x();
    y = screenGeometry.y() + screenGeometry.height() - (topLeft.y() + drawingArea->height());
    width = drawingArea->width();
    height = drawingArea->height();
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
