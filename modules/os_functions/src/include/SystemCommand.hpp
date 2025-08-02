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
#include <tuple>
#include <vector>
#include <utility>
#include "ArrayOf.hpp"
#include "nlsOs_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP std::tuple<int, std::wstring, uint64>
SystemCommand(const std::wstring& command, uint64 timeout, bool withLoopEvents, size_t evaluatorID);
//=============================================================================
NLSOS_FUNCTIONS_IMPEXP std::vector<std::tuple<int, std::wstring, uint64>>
ParallelSystemCommand(const wstringVector& commands, const std::vector<uint64>& timeouts,
    bool withEventsLoop, size_t evaluatorID);
//=============================================================================
} // namespace Nelson
//=============================================================================
