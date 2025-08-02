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
#include "Scope.hpp"
#include "nlsMemory_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSMEMORY_MANAGER_IMPEXP bool
LockVariable(const std::wstring& variable, Scope* scope);
NLSMEMORY_MANAGER_IMPEXP bool
LockVariable(const std::string& variable, Scope* scope);
NLSMEMORY_MANAGER_IMPEXP bool
UnlockVariable(const std::wstring& variable, Scope* scope);
NLSMEMORY_MANAGER_IMPEXP bool
UnlockVariable(const std::string& variable, Scope* scope);
NLSMEMORY_MANAGER_IMPEXP bool
IsLockedVariable(const std::wstring& variable, Scope* scope);
NLSMEMORY_MANAGER_IMPEXP bool
IsLockedVariable(const std::string& variable, Scope* scope);
NLSMEMORY_MANAGER_IMPEXP stringVector
GetLockedVariables(const std::string& variable, Scope* scope);

} // namespace Nelson
//=============================================================================
