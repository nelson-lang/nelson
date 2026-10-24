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
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOWindow::installWindowKeyEventFilter()
{
    windowKeyEventFilter = std::make_unique<WindowKeyEventFilter>(
        this,
        [this](QKeyEvent& event) {
            handleKeyEvent(&event, GO_WINDOW_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
        },
        [this](QKeyEvent& event) {
            handleKeyEvent(&event, GO_WINDOW_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
        });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
