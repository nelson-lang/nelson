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
#include "ArrayOf.hpp"
#include "ScalarMathBuiltin.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class FunctionDef;
//=============================================================================
struct ScalarBuiltinFastPath
{
    ScalarMathBuiltinKind kind = ScalarMathBuiltinKind::None;
    uint16_t inputCount = 0;
};
//=============================================================================
ScalarBuiltinFastPath
scalarBuiltinFastPathForName(const std::string& name, uint16_t inputCount);
//=============================================================================
bool
isScalarBuiltinFastPathCandidate(const ScalarBuiltinFastPath& fastPath);
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPathArg(
    const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef, const ArrayOf& arg, ArrayOf& result);
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPathArgs(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOf& lhsArg, const ArrayOf& rhsArg, ArrayOf& result);
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPathArgs(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOf& lhsArg, const ArrayOf& rhsArg, double& result);
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPath(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOfVector& args, ArrayOf& result);
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPath(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOfVector& args, double& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
