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
#include "nlsGui_exports.h"
#include <string>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGUI_IMPEXP wstringVector
GetLookAndFeelAvailable();
//=============================================================================
NLSGUI_IMPEXP std::wstring
GetCurrentLookAndFeel();
//=============================================================================
NLSGUI_IMPEXP bool
SetCurrentLookAndFeel(const std::wstring& lf);
//=============================================================================
NLSGUI_IMPEXP void
SetCurrentStyleSheet(const std::wstring& styleSheet);
//=============================================================================
NLSGUI_IMPEXP std::wstring
GetCurrentStyleSheet();
//=============================================================================
} // namespace Nelson
//=============================================================================
