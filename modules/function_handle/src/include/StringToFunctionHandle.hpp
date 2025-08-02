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
#include "Types.hpp"
#include "nlsFunction_handle_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
NLSFUNCTION_HANDLE_IMPEXP function_handle
StringToFunctionHandle(Evaluator* eval, const std::string& functionName);
}
//=============================================================================
