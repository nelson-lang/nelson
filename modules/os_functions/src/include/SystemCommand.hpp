//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "nlsOs_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP std::wstring
SystemCommand(const std::wstring& command, int& ierr, bool withLoopEvents);
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP wstringVector
ParallelSystemCommand(const wstringVector& commands, std::vector<int>& ierrs, bool withEventsLoop);
//=============================================================================
} // namespace Nelson
//=============================================================================
