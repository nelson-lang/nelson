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
#include "Evaluator.hpp"
#include "nlsFunctions_manager_exports.h"
//=============================================================================
namespace Nelson {
NLSFUNCTIONS_MANAGER_IMPEXP bool
ClearBuiltin(const std::wstring& builtinName);
NLSFUNCTIONS_MANAGER_IMPEXP bool
ClearAllBuiltin();
NLSFUNCTIONS_MANAGER_IMPEXP bool
ClearMacroCache(Evaluator* eval);
} // namespace Nelson
//=============================================================================
