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
#include <memory>
#include <QtGui/QKeyEvent>
//=============================================================================
namespace Nelson {
//=============================================================================
class KeyReleaseEventState
{
public:
    void
    press()
    {
        keyPressed = true;
    }

    bool
    deferRelease(const QKeyEvent& event)
    {
        if (!keyPressed) {
            return false;
        }
        pendingRelease.reset(event.clone());
        return true;
    }

    std::unique_ptr<QKeyEvent>
    takePendingRelease()
    {
        keyPressed = false;
        return std::move(pendingRelease);
    }

private:
    bool keyPressed = false;
    std::unique_ptr<QKeyEvent> pendingRelease;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
