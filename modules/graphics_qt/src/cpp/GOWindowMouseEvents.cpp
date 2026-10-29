//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QMouseEvent>
#include <QtGui/QWindow>
#include "GOWindow.hpp"
#include "GOCallbackProperty.hpp"
#include "GOFigureMouseEvents.hpp"
#include "GOPropertyNames.hpp"
#include "FigureWorkspaceQt.hpp"
//=============================================================================
// Exported by Qt GUI for its test library (declared in qtestmouse.h).
Q_GUI_EXPORT void
qt_handleMouseEvent(QWindow* window, const QPointF& local, const QPointF& global,
    Qt::MouseButtons state, Qt::MouseButton button, QEvent::Type type, Qt::KeyboardModifiers mods,
    int timestamp);
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOWindow::updateCurrentPoint(const QPointF& scenePosition, quint64 eventTimestamp)
{
    if (!qtchild) {
        return;
    }
    // An injected event carries its figure point: use it directly instead of reconstructing one from scenePosition, which depends on a window placement a headless X server may not honor. Drop it and any older unconsumed injection (events are delivered in timestamp order).
    auto injected = injectedFigurePoints.find(eventTimestamp);
    if (injected != injectedFigurePoints.end()) {
        lastFigurePixelX = injected->second.x();
        lastFigurePixelY = injected->second.y();
        injectedFigurePoints.erase(injectedFigurePoints.begin(), injected);
        injectedFigurePoints.erase(injected);
        updateFigureCurrentPoint(goFig, lastFigurePixelX, lastFigurePixelY);
        return;
    }
    // Logical pixels (Qt already divides by the device pixel ratio), origin
    // moved to the bottom-left corner of the drawing area. The event
    // scenePosition() is used: position() is rounded to whole pixels when the
    // event propagates up from a child widget. A docked window is not the
    // top-level one.
    QPointF windowPosition(mapFrom(window(), scenePosition));
    QRect plotRegion(qtchild->geometry());
    lastFigurePixelX = windowPosition.x() - plotRegion.x();
    lastFigurePixelY = plotRegion.height() - (windowPosition.y() - plotRegion.y());
    updateFigureCurrentPoint(goFig, lastFigurePixelX, lastFigurePixelY);
}
//=============================================================================
// Motion anywhere over the window, whatever the widget under the pointer: an
// empty drawing area, the axes canvas or a uicontrol (none of them tracks the
// mouse, so Qt would not pass a button-less motion up to this window).
void
GOWindow::installWindowMouseMotionFilter()
{
    windowMouseMotionFilter
        = std::make_unique<WindowMouseMotionFilter>(
            this, [this](const QPointF& scenePosition, quint64 eventTimestamp) {
              if (workspace) {
                  workspace->hover(mapFrom(window(), scenePosition));
              }
              if (!figureListensToMotion(goFig)) {
                  return;
              }
              updateCurrentPoint(scenePosition, eventTimestamp);
              handleMouseEvent(
                  goFig, GO_WINDOW_BUTTON_MOTION_FCN_PROPERTY_NAME_STR, L"WindowMouseMotion");
          });
}
//=============================================================================
bool
GOWindow::injectHostMouseEvent(const std::wstring& action, double x, double y, int button,
    bool shiftDown, bool ctrlDown, bool doubleClick)
{
    QWindow* window = windowHandle();
    if (!qtchild || !window) {
        return false;
    }
    const Qt::MouseButton qtButton
        = (button == 2) ? Qt::RightButton : (button == 1 ? Qt::MiddleButton : Qt::LeftButton);
    Qt::KeyboardModifiers modifiers = Qt::NoModifier;
    if (shiftDown) {
        modifiers |= Qt::ShiftModifier;
    }
    if (ctrlDown) {
        modifiers |= Qt::ControlModifier;
    }
    // Queued like an event of the window system, the system cursor does not
    // move: Qt then picks the widget under the point and applies its mouse
    // tracking and propagation rules, as for the real pointer. Timestamps are
    // spaced so that two unrelated presses never make a double click.
    static int timestamp = 0;
    timestamp += 1000;
    QRect plotRegion(qtchild->geometry());
    QPointF windowPosition(plotRegion.x() + x, plotRegion.y() + plotRegion.height() - y);
    QPointF globalPosition(mapToGlobal(windowPosition));
    auto post = [&](Qt::MouseButton eventButton, QEvent::Type type, int ts) {
        // the delivery reads this back by timestamp instead of reconstructing the point from scenePosition
        injectedFigurePoints[static_cast<quint64>(ts)] = QPointF(x, y);
        qt_handleMouseEvent(window, windowPosition, globalPosition, injectedMouseButtons,
            eventButton, type, modifiers, ts);
    };
    if (action == L"press" && doubleClick) {
        // A double click is two presses within the double-click interval: the
        // window system turns the second press into a MouseButtonDblClick, so
        // the two are posted a few milliseconds apart.
        injectedMouseButtons = qtButton;
        post(qtButton, QEvent::MouseButtonPress, timestamp);
        injectedMouseButtons = Qt::NoButton;
        post(qtButton, QEvent::MouseButtonRelease, timestamp + 5);
        injectedMouseButtons = qtButton;
        post(qtButton, QEvent::MouseButtonPress, timestamp + 10);
        return true;
    }
    if (action == L"press") {
        injectedMouseButtons = qtButton;
        post(qtButton, QEvent::MouseButtonPress, timestamp);
    } else if (action == L"motion") {
        post(Qt::NoButton, QEvent::MouseMove, timestamp);
    } else if (action == L"release") {
        injectedMouseButtons = Qt::NoButton;
        post(qtButton, QEvent::MouseButtonRelease, timestamp);
    } else {
        return false;
    }
    return true;
}
//=============================================================================
bool
GOWindow::handleMouseEvent(
    QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback = (GOCallbackProperty*)goFig->findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }
    goCallback->pushEvent(goFig, L"MouseData", eventType);
    return true;
}
//=============================================================================
bool
GOWindow::handleMouseEvent(
    GraphicsObject* object, const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    if (!object) {
        return true;
    }
    GOCallbackProperty* goCallback
        = static_cast<GOCallbackProperty*>(object->findProperty(callbackPropertyStr, false));
    if (!goCallback) {
        return true;
    }
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }
    goCallback->pushEvent(object, L"MouseData", eventType);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
