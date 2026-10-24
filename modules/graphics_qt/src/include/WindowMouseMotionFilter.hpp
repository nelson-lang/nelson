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
#include <functional>
#include <utility>
#include <QtCore/QCoreApplication>
#include <QtCore/QEvent>
#include <QtCore/QObject>
#include <QtCore/QPointer>
#include <QtGui/QMouseEvent>
#include <QtWidgets/QWidget>
//=============================================================================
namespace Nelson {
//=============================================================================
// Reports every pointer motion over a window, whatever widget is under the
// pointer. Qt drops a button-less motion on a widget without mouse tracking
// before it reaches that widget or its parents; application event filters
// still see it, so the window is told from here.
class WindowMouseMotionFilter : public QObject
{
public:
    // scenePosition: unrounded pointer position in the top-level window.
    using MotionHandler = std::function<void(const QPointF& scenePosition)>;

    WindowMouseMotionFilter(QWidget* window, MotionHandler motionHandler)
        : targetWindow(window), handler(std::move(motionHandler))
    {
        if (QCoreApplication::instance()) {
            QCoreApplication::instance()->installEventFilter(this);
        }
    }

    ~WindowMouseMotionFilter() override
    {
        if (QCoreApplication::instance()) {
            QCoreApplication::instance()->removeEventFilter(this);
        }
    }

protected:
    bool
    eventFilter(QObject* watched, QEvent* event) override
    {
        if (event->type() != QEvent::MouseMove) {
            return false;
        }
        // One motion is seen by the native QWindow first, then by the widget
        // under the pointer, then by each ancestor it propagates to while
        // ignored (as a copy mapped to that ancestor). Only the first widget
        // delivery counts, and only when that widget belongs to this window.
        QWidget* receiver = qobject_cast<QWidget*>(watched);
        if (!receiver) {
            return false;
        }
        QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
        if (isPropagation(*mouseEvent, receiver)) {
            return false;
        }
        lastDelivery = { mouseEvent->timestamp(), mouseEvent->globalPosition(),
            mouseEvent->buttons(), receiver };

        if (!targetWindow || (receiver != targetWindow && !targetWindow->isAncestorOf(receiver))) {
            return false;
        }
        if (handler) {
            handler(mouseEvent->scenePosition());
        }
        return false;
    }

private:
    struct Delivery
    {
        quint64 timestamp = 0;
        QPointF globalPosition;
        Qt::MouseButtons buttons;
        QPointer<QWidget> receiver;
    };

    bool
    isPropagation(const QMouseEvent& mouseEvent, const QWidget* receiver) const
    {
        const QWidget* firstReceiver = lastDelivery.receiver.data();
        return lastDelivery.timestamp == mouseEvent.timestamp()
            && lastDelivery.globalPosition == mouseEvent.globalPosition()
            && lastDelivery.buttons == mouseEvent.buttons() && firstReceiver
            && receiver != firstReceiver && receiver->isAncestorOf(firstReceiver);
    }

    QPointer<QWidget> targetWindow;
    MotionHandler handler;
    Delivery lastDelivery;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
