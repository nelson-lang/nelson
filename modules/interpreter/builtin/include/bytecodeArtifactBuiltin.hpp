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
#include "nlsInterpreter_builtin_exports.h"
#include "Evaluator.hpp"
#include "NelsonGateway.hpp"
//=============================================================================
namespace Nelson::InterpreterGateway {
NLSINTERPRETER_BUILTIN_IMPEXP ArrayOfVector
bytecodeArtifactBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson::InterpreterGateway
//=============================================================================
