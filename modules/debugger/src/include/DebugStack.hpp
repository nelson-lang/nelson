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
#include <vector>
#include "nlsDebugger_exports.h"
#include "PositionScript.hpp"
#include "CallStack.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using stackTrace = std::vector<PositionScript>;
//=============================================================================
NLSDEBUGGER_IMPEXP void
DebugStack(const CallStack& callstack, int nbOmitLines, stackTrace& stackPositions);
//=============================================================================
} // namespace Nelson
//=============================================================================
