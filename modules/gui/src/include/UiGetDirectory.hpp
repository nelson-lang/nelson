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
#include <string>
#include "nlsGui_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Returns true when the selection was cancelled (pathSelected is then unchanged),
// false when a directory was chosen. Served through the dialog provider.
NLSGUI_IMPEXP bool
UiGetDirectory(
    const std::wstring& pathOrigin, const std::wstring& title, std::wstring& pathSelected);
//=============================================================================
} // namespace Nelson
//=============================================================================
