//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOWindow.hpp"
#include "FigureWorkspaceQt.hpp"
#include "GOAxis.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOFigureMouseEvents.hpp"
#include <QtGui/QMouseEvent>
#include <QtGui/QWheelEvent>
//=============================================================================
namespace Nelson {
void
GOWindow::forceCurrentAxes(QMouseEvent* e)
{
    mousePositionOrigin = e->pos();
    int x, y;
    qtGetPosition(e, x, y);
    GOAxis* h = findContainingAxis(goFig, remapX(x), remapY(y));
    if (h) {
        int64 handle = findGraphicsObjectHandle(h);
        if (handle != -1) {
            goFig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, handle);
        }
    }
}
//=============================================================================
void
GOWindow::mousePressEvent(QMouseEvent* e)
{
    e->accept();
    updateCurrentPoint(e->scenePosition());
    setFocus(Qt::MouseFocusReason);
    if (waitKeyOrMousePressedMode == WAIT_PRESS_MODE::WAIT) {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::MOUSE;
        return;
    }
    const int button = e->button() == Qt::RightButton ? 2 : e->button() == Qt::MiddleButton ? 1 : 0;
    deliverButtonDownCallbacks(e,
        resolveSelectionType(button, e->modifiers().testFlag(Qt::ShiftModifier),
            e->modifiers().testFlag(Qt::ControlModifier), false));
    if (workspace) {
        workspace->press(e);
    }
}
//=============================================================================
void
GOWindow::mouseDoubleClickEvent(QMouseEvent* e)
{
    e->accept();
    updateCurrentPoint(e->scenePosition());
    setFocus(Qt::MouseFocusReason);
    if (waitKeyOrMousePressedMode == WAIT_PRESS_MODE::WAIT) {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::MOUSE;
        return;
    }
    deliverButtonDownCallbacks(e, GO_PROPERTY_VALUE_OPEN_STR);
    if (workspace) {
        workspace->doubleClick(e);
    }
}
//=============================================================================
void
GOWindow::mouseMoveEvent(QMouseEvent* e)
{
    e->accept();
    if (workspace) {
        workspace->move(e);
    }
}
//=============================================================================
void
GOWindow::mouseReleaseEvent(QMouseEvent* e)
{
    e->accept();
    updateCurrentPoint(e->scenePosition());
    handleMouseEvent(e, GO_WINDOW_BUTTON_UP_FCN_PROPERTY_NAME_STR, L"WindowMouseRelease");
    if (workspace) {
        workspace->move(e);
        workspace->release();
    }
}
//=============================================================================
GOAxis*
GOWindow::findContainingAxis(GOFigure* fig, int x, int y)
{
    if (!workspace || !qtchild || fig != goFig) {
        return nullptr;
    }
    const double ratio = qtchild->devicePixelRatioF();
    const auto origin = qtchild->geometry().topLeft();
    return workspace->axisAt(
        QPointF(origin.x() + x / ratio, origin.y() + qtchild->height() - y / ratio));
}
//=============================================================================
void
GOWindow::qtGetPosition(QMouseEvent* e, int& x, int& y)
{
    x = (int)e->position().x();
    y = (int)e->position().y();
}
//=============================================================================
void
GOWindow::wheelEvent(QWheelEvent* event)
{
    event->accept();
    // A figure that defines WindowScrollWheelFcn owns the wheel: its callback
    // replaces the built-in axes zoom instead of running on top of it.
    auto* callback = goFig
        ? goFig->findProperty(GO_WINDOW_SCROLL_WHEEL_FCN_PROPERTY_NAME_STR, false)
        : nullptr;
    const bool figureOwnsWheel = callback && !callback->get().isEmpty();
    if (workspace && !figureOwnsWheel) {
        workspace->wheel(event);
    }
    handleMouseEvent(event, GO_WINDOW_SCROLL_WHEEL_FCN_PROPERTY_NAME_STR, L"WindowScrollWheel");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
