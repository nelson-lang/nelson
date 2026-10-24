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
#include <QtGui/QKeyEvent>
#include <QtWidgets/QWidget>
//=============================================================================
namespace Nelson {
//=============================================================================
class WindowKeyEventFilter : public QObject
{
public:
    using KeyHandler = std::function<void(QKeyEvent&)>;

    WindowKeyEventFilter(QWidget* window, KeyHandler keyPressHandler, KeyHandler keyReleaseHandler)
        : targetWindow(window)
        , pressHandler(std::move(keyPressHandler))
        , releaseHandler(std::move(keyReleaseHandler))
    {
        if (QCoreApplication::instance()) {
            QCoreApplication::instance()->installEventFilter(this);
        }
    }

    ~WindowKeyEventFilter() override
    {
        if (QCoreApplication::instance()) {
            QCoreApplication::instance()->removeEventFilter(this);
        }
    }

protected:
    bool
    eventFilter(QObject* watched, QEvent* event) override
    {
        if (event->type() != QEvent::KeyPress && event->type() != QEvent::KeyRelease) {
            return false;
        }
        // The application filter sees one key event once per delivery: the
        // native QWindow first, then the receiving widget, then each ancestor
        // the event propagates to while ignored. Only the first widget
        // delivery counts, and only when that widget belongs to this window.
        QWidget* receiver = qobject_cast<QWidget*>(watched);
        if (!receiver) {
            return false;
        }
        QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
        if (isPropagation(*keyEvent, receiver)) {
            return false;
        }
        lastDelivery
            = { keyEvent, keyEvent->type(), keyEvent->timestamp(), keyEvent->key(), receiver };

        if (!targetWindow || (receiver != targetWindow && !targetWindow->isAncestorOf(receiver))) {
            return false;
        }

        if (event->type() == QEvent::KeyPress) {
            if (pressHandler) {
                pressHandler(*keyEvent);
            }
        } else {
            if (!keyEvent->isAutoRepeat() && releaseHandler) {
                releaseHandler(*keyEvent);
            }
        }
        return false;
    }

private:
    struct Delivery
    {
        const QKeyEvent* event = nullptr;
        QEvent::Type type = QEvent::None;
        quint64 timestamp = 0;
        int key = 0;
        QPointer<QWidget> receiver;
    };

    bool
    isPropagation(const QKeyEvent& keyEvent, const QWidget* receiver) const
    {
        const QWidget* firstReceiver = lastDelivery.receiver.data();
        return lastDelivery.event == &keyEvent && lastDelivery.type == keyEvent.type()
            && lastDelivery.timestamp == keyEvent.timestamp() && lastDelivery.key == keyEvent.key()
            && firstReceiver && receiver != firstReceiver && receiver->isAncestorOf(firstReceiver);
    }

    QWidget* targetWindow;
    KeyHandler pressHandler;
    KeyHandler releaseHandler;
    Delivery lastDelivery;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
