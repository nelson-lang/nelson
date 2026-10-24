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
#include "KeyReleaseEventState.hpp"
//=============================================================================
namespace {
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
} // namespace
//=============================================================================
int
main()
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

    return EXIT_SUCCESS;
}
//=============================================================================
