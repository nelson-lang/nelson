//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ScalarBuiltinFastPath.hpp"
#include "FunctionDef.hpp"
#include "Profiler.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isResolvedBuiltinFastPath(FunctionDef* fdef)
{
    return !Profiler::getInstance()->isOn() && fdef != nullptr
        && fdef->type() == NLS_BUILT_IN_FUNCTION && !fdef->isOverload();
}
//=============================================================================
static bool
isScalarDouble(const ArrayOf& value)
{
    return value.isDoubleType(true) && value.elementCount() == 1;
}
//=============================================================================
static double
scalarDoubleValue(const ArrayOf& value)
{
    return *static_cast<const double*>(value.dataPointer());
}
//=============================================================================
ScalarBuiltinFastPath
scalarBuiltinFastPathForName(const std::string& name, uint16_t inputCount)
{
    return { scalarMathBuiltinKind(name, inputCount), inputCount };
}
//=============================================================================
bool
isScalarBuiltinFastPathCandidate(const ScalarBuiltinFastPath& fastPath)
{
    return isScalarMathBuiltinCandidate(fastPath.kind);
}
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPathArg(
    const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef, const ArrayOf& arg, ArrayOf& result)
{
    if (fastPath.inputCount != 1 || !isResolvedBuiltinFastPath(fdef) || !isScalarDouble(arg)) {
        return false;
    }
    double scalarResult = 0.;
    if (!evaluateUnaryScalarMathBuiltin(fastPath.kind, scalarDoubleValue(arg), scalarResult)) {
        return false;
    }
    result = ArrayOf::doubleScalar(scalarResult);
    return true;
}
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPathArgs(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOf& lhsArg, const ArrayOf& rhsArg, ArrayOf& result)
{
    if (fastPath.inputCount != 2 || !isResolvedBuiltinFastPath(fdef) || !isScalarDouble(lhsArg)
        || !isScalarDouble(rhsArg)) {
        return false;
    }
    double lhs = scalarDoubleValue(lhsArg);
    double rhs = scalarDoubleValue(rhsArg);
    double scalarResult = 0.;
    if (!evaluateBinaryScalarMathBuiltin(fastPath.kind, lhs, rhs, scalarResult)) {
        return false;
    }
    result = ArrayOf::doubleScalar(scalarResult);
    return true;
}
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPathArgs(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOf& lhsArg, const ArrayOf& rhsArg, double& result)
{
    if (fastPath.inputCount != 2 || !isResolvedBuiltinFastPath(fdef) || !isScalarDouble(lhsArg)
        || !isScalarDouble(rhsArg)) {
        return false;
    }
    return evaluateBinaryScalarMathBuiltin(
        fastPath.kind, scalarDoubleValue(lhsArg), scalarDoubleValue(rhsArg), result);
}
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPath(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOfVector& args, ArrayOf& result)
{
    if (fastPath.inputCount != args.size()) {
        return false;
    }
    if (args.size() == 1) {
        return tryEvaluateScalarBuiltinFastPathArg(fastPath, fdef, args[0], result);
    }
    if (args.size() == 2) {
        return tryEvaluateScalarBuiltinFastPathArgs(fastPath, fdef, args[0], args[1], result);
    }
    return false;
}
//=============================================================================
bool
tryEvaluateScalarBuiltinFastPath(const ScalarBuiltinFastPath& fastPath, FunctionDef* fdef,
    const ArrayOfVector& args, double& result)
{
    if (fastPath.inputCount != args.size() || args.size() != 2) {
        return false;
    }
    return tryEvaluateScalarBuiltinFastPathArgs(fastPath, fdef, args[0], args[1], result);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
