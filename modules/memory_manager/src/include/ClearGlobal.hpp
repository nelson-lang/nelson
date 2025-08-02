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
#include "nlsMemory_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSMEMORY_MANAGER_IMPEXP bool
ClearGlobalVariable(Evaluator* eval, const std::wstring& variable);
NLSMEMORY_MANAGER_IMPEXP bool
ClearGlobalVariable(Evaluator* eval, const std::string& variable);
NLSMEMORY_MANAGER_IMPEXP bool
ClearAllGlobalVariables(Evaluator* eval);
NLSMEMORY_MANAGER_IMPEXP bool
ClearAllPersistentVariables(Evaluator* eval);
NLSMEMORY_MANAGER_IMPEXP bool
ClearPersistentVariable(Evaluator* eval, const std::wstring& variable);
NLSMEMORY_MANAGER_IMPEXP bool
ClearPersistentVariable(Evaluator* eval, const std::string& variable);
} // namespace Nelson
//=============================================================================
