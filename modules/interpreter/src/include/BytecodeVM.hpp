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
#include "BytecodeChunk.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
class NLSINTERPRETER_IMPEXP BytecodeVM
{
public:
    static ArrayOfVector
    executeFunction(
        Evaluator* eval, const BytecodeChunk& chunk, const ArrayOfVector& argIn, int nLhs);

    static void
    executeScript(Evaluator* eval, const BytecodeChunk& chunk);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
