//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
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
#include "nlsError_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// wraps a label in the OSC 8 hyperlink the desktop consoles turn into an editor open at that line
NLSERROR_MANAGER_IMPEXP std::wstring
openToLineHyperlink(const std::wstring& label, const std::wstring& filename, int line);
//=============================================================================
} // namespace Nelson
//=============================================================================
