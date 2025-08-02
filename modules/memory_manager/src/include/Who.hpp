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
#include "Evaluator.hpp"
#include "nlsMemory_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSMEMORY_MANAGER_IMPEXP stringVector
Who(Evaluator* eval, SCOPE_LEVEL scopeLevel, bool withPersistent);
//=============================================================================
NLSMEMORY_MANAGER_IMPEXP stringVector
Who(Evaluator* eval, Scope* scope, bool withPersistent);
//=============================================================================
NLSMEMORY_MANAGER_IMPEXP stringVector
Who(Evaluator* eval, bool withPersistent);
//=============================================================================
} // namespace Nelson
//=============================================================================
