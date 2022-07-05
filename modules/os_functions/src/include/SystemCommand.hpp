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
#include <utility>
#include "ArrayOf.hpp"
#include "nlsOs_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP std::pair<int, std::wstring>
SystemCommand(const std::wstring& command, bool withLoopEvents, size_t evaluatorID);
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP std::vector<std::pair<int, std::wstring>>
ParallelSystemCommand(const wstringVector& commands, bool withEventsLoop, size_t evaluatorID);
//=============================================================================
} // namespace Nelson
//=============================================================================
