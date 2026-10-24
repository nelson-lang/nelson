//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include <iostream>
#include <memory>
#include <QtCore/QCoreApplication>
#include <QtGui/QKeyEvent>
#include <QtGui/QWindow>
#include <QtWidgets/QApplication>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMainWindow>
#include "KeyReleaseEventState.hpp"
#include "WindowKeyEventFilter.hpp"
//=============================================================================
namespace {
//=============================================================================
class NativeEventReceiver : public QWidget
{
protected:
    bool
    event(QEvent* event) override
    {
        if (event->type() == QEvent::KeyPress || event->type() == QEvent::KeyRelease) {
            return true;
        }
        return QWidget::event(event);
    }
};
//=============================================================================
void
require(bool condition, const char* message)
{
    if (!condition) {
        std::cerr << message << std::endl;
        std::exit(EXIT_FAILURE);
    }
}
//=============================================================================
void
testDeferredReleaseState()
{
    Nelson::KeyReleaseEventState state;

    state.press();
    require(!state.takePendingRelease(), "timeout before key release must be empty");

    QKeyEvent immediateRelease(QEvent::KeyRelease, Qt::Key_W, Qt::NoModifier, QStringLiteral("w"));
    require(!state.deferRelease(immediateRelease), "release after timeout must not be deferred");

    state.press();
    QKeyEvent deferredRelease(
        QEvent::KeyRelease, Qt::Key_A, Qt::ShiftModifier, QStringLiteral("A"));
    require(state.deferRelease(deferredRelease), "release during debounce must be deferred");

    std::unique_ptr<QKeyEvent> pending = state.takePendingRelease();
    require(pending != nullptr, "deferred release must survive until timeout");
    require(pending->key() == Qt::Key_A, "deferred key must be preserved");
    require(pending->text() == QStringLiteral("A"), "deferred text must be preserved");
    require(pending->modifiers() == Qt::ShiftModifier, "deferred modifiers must be preserved");
    require(!state.takePendingRelease(), "pending release must be consumed once");
}
//=============================================================================
void
testWindowCallbacksFollowChildFocus()
{
    QMainWindow window;
    QLineEdit editor;
    window.setCentralWidget(&editor);

    int pressCount = 0;
    int releaseCount = 0;
    int lastKey = Qt::Key_unknown;
    Nelson::WindowKeyEventFilter filter(
        &window,
        [&](QKeyEvent& event) {
            ++pressCount;
            lastKey = event.key();
        },
        [&](QKeyEvent& event) {
            ++releaseCount;
            lastKey = event.key();
        });

    window.show();
    window.activateWindow();
    editor.setFocus(Qt::OtherFocusReason);
    QCoreApplication::processEvents();
    require(QApplication::focusWidget() == &editor, "child widget must own keyboard focus");

    NativeEventReceiver nativeEventReceiver;
    nativeEventReceiver.setParent(&editor);
    QCoreApplication::postEvent(&nativeEventReceiver,
        new QKeyEvent(QEvent::KeyPress, Qt::Key_W, Qt::NoModifier, QStringLiteral("w")));
    QCoreApplication::postEvent(&nativeEventReceiver,
        new QKeyEvent(QEvent::KeyRelease, Qt::Key_W, Qt::NoModifier, QStringLiteral("w")));
    QCoreApplication::processEvents();

    require(pressCount == 1, "window press callback must observe a focused child event");
    require(releaseCount == 1, "window release callback must observe a focused child event");
    require(lastKey == Qt::Key_W, "window callback must preserve the key");

    QCoreApplication::postEvent(&editor,
        new QKeyEvent(QEvent::KeyRelease, Qt::Key_W, Qt::NoModifier, QStringLiteral("w"), true));
    QCoreApplication::processEvents();
    require(releaseCount == 1, "auto-repeat release must not end a held key");

    editor.setParent(nullptr);
}
//=============================================================================
void
testWindowCallbacksFireOncePerEvent()
{
    // The key event reaches the application filter once per delivery: the
    // native window first, then the focus widget, then every ancestor that
    // ignores it. The window callback must still fire once per key event.
    QMainWindow window;
    QWidget central;
    QWidget focused(&central);
    focused.setFocusPolicy(Qt::StrongFocus);
    window.setCentralWidget(&central);

    int pressCount = 0;
    int releaseCount = 0;
    Nelson::WindowKeyEventFilter filter(
        &window, [&](QKeyEvent&) { ++pressCount; }, [&](QKeyEvent&) { ++releaseCount; });

    window.show();
    window.activateWindow();
    focused.setFocus(Qt::OtherFocusReason);
    QCoreApplication::processEvents();
    require(QApplication::focusWidget() == &focused, "inner widget must own keyboard focus");
    require(window.windowHandle() != nullptr, "window must have a native handle");

    for (int k = 0; k < 2; ++k) {
        QKeyEvent press(QEvent::KeyPress, Qt::Key_Q, Qt::NoModifier, QStringLiteral("q"));
        QCoreApplication::sendEvent(window.windowHandle(), &press);
        QKeyEvent release(QEvent::KeyRelease, Qt::Key_Q, Qt::NoModifier, QStringLiteral("q"));
        QCoreApplication::sendEvent(window.windowHandle(), &release);
    }
    require(pressCount == 2, "window press callback must fire once per key press");
    require(releaseCount == 2, "window release callback must fire once per key release");

    QKeyEvent direct(QEvent::KeyPress, Qt::Key_Shift, Qt::ShiftModifier);
    QCoreApplication::sendEvent(&focused, &direct);
    require(pressCount == 3, "ignored key must not fire again while it propagates to parents");
}
//=============================================================================
void
testWindowCallbacksFollowReceivingWindow()
{
    // Each figure installs its own application filter: a key event delivered
    // to one figure must fire only that figure's callback, even when the
    // keyboard focus sits in another figure.
    QMainWindow first;
    QMainWindow second;
    QWidget secondCentral;
    second.setCentralWidget(&secondCentral);
    int firstCount = 0;
    int secondCount = 0;
    Nelson::WindowKeyEventFilter firstFilter(
        &first, [&](QKeyEvent&) { ++firstCount; }, [](QKeyEvent&) {});
    Nelson::WindowKeyEventFilter secondFilter(
        &second, [&](QKeyEvent&) { ++secondCount; }, [](QKeyEvent&) {});

    first.show();
    second.show();
    QCoreApplication::processEvents();
    require(second.windowHandle() != nullptr, "second window must have a native handle");

    QKeyEvent press(QEvent::KeyPress, Qt::Key_Q, Qt::NoModifier, QStringLiteral("q"));
    QCoreApplication::sendEvent(second.windowHandle(), &press);
    require(secondCount == 1, "receiving window callback must fire once");
    require(firstCount == 0, "other window callback must not fire");
}
//=============================================================================
} // namespace
//=============================================================================
int
main(int argc, char* argv[])
{
    QApplication application(argc, argv);
    {
        QMainWindow window;
        int presses = 0;
        int releases = 0;
        Nelson::WindowKeyEventFilter filter(
            &window, [&](QKeyEvent&) { ++presses; }, [&](QKeyEvent&) { ++releases; });
        QKeyEvent press(QEvent::KeyPress, Qt::Key_F, Qt::NoModifier, QStringLiteral("f"));
        QKeyEvent release(QEvent::KeyRelease, Qt::Key_F, Qt::NoModifier, QStringLiteral("f"));
        QCoreApplication::sendEvent(&window, &press);
        QCoreApplication::sendEvent(&window, &release);
        require(presses == 1 && releases == 1,
            "root figure events must reach window callbacks exactly once");
    }
    testDeferredReleaseState();
    testWindowCallbacksFollowChildFocus();
    testWindowCallbacksFireOncePerEvent();
    testWindowCallbacksFollowReceivingWindow();
    return EXIT_SUCCESS;
}
//=============================================================================
