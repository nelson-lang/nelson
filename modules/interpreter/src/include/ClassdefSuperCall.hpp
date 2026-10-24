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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Native superclass call: obj@Super(args) / method@Super(args). This is the
// engine operation the SUPER_CALL opcode dispatches to.
// argIn layout:
//   constructor: [obj, 'Super', ctorArgs...]  -> returns the updated object.
//   method:      ['Super', 'method', args...] -> returns nLhs outputs.
NLSINTERPRETER_IMPEXP ArrayOfVector
evaluateClassdefSuperCall(
    Evaluator* eval, bool isConstructor, const ArrayOfVector& argIn, int nLhs);
//=============================================================================
} // namespace Nelson
//=============================================================================
