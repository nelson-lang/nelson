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
#include "Evaluator.hpp"
#include "nlsOverload_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSOVERLOAD_IMPEXP ArrayOfVector
OverloadFunction(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, bool& bSuccess);
//=============================================================================
NLSOVERLOAD_IMPEXP ArrayOfVector
OverloadFunction(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn, const std::string& functionName);
//=============================================================================
} // namespace Nelson
//=============================================================================
