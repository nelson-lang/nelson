//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// WebUiHelpers.cpp links the webui runtime for window icon handling; the
// helper tests only exercise pure path helpers, so a null window stands in.
#include <cstddef>
//=============================================================================
extern "C" void*
webui_get_hwnd(size_t window)
{
    (void)window;
    return nullptr;
}
//=============================================================================
