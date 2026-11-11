//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
#include "LeftDivide.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "Operators.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadName.hpp"
#include "ClassName.hpp"
#include "RightDivide.hpp"
#include "ComplexTranspose.hpp"
#include "FunctionsInMemory.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Evaluator::leftDivideOperator(const ArrayOfVector& args)
{
    const std::string functionName = MLDIVIDE_OPERATOR_STR;
    BinaryOperatorCommonType commonType;

    ArrayOf res;
    if (tryCallBinaryOperatorOverload(this, args, functionName, commonType, res)) {
        return res;
    }

    bool needToOverload = false;
    ArrayOf A(args[0]);
    ArrayOf B(args[1]);
    prepareBinaryNumericOperands(commonType, A, B);
    res = LeftDivide(A, B, needToOverload);

    if (needToOverload) {
        return callBinaryOperatorOverloadOrRequire(this, args, functionName, commonType);
    }

    return res;
}
//=============================================================================
//=============================================================================
// True when "@className/name" exists and so must see the operation.
static bool
builtinOverloadDefined(Evaluator* eval, const std::string& className, const std::string& name)
{
    if (NelsonConfiguration::getInstance()->getOverloadLevelCompatibility()
        != NLS_OVERLOAD_ALL_TYPES) {
        return false;
    }
    const std::string overloadName = getOverloadFunctionName(className, name);
    if (FunctionsInMemory::getInstance()->isNotExistingFunction(overloadName)) {
        return false;
    }
    FunctionDefPtr funcDef = nullptr;
    return lookupOverloadFunctionCached(eval->getContext(), overloadName, funcDef)
        && funcDef != nullptr;
}
//=============================================================================
// Dense square double or single A and B of the same precision, with B rows matching A.
static bool
isTransposedSolveOperand(const ArrayOf& A, const ArrayOf& B, bool conjugate)
{
    if (A.isSparse() || B.isSparse() || !(A.isDoubleType() || A.isSingleType())
        || A.isDoubleType() != B.isDoubleType() || A.isSingleType() != B.isSingleType()) {
        return false;
    }
    if (!conjugate && A.isComplex()) {
        return false;
    }
    const Dimensions& dimsA = A.dimensions();
    const Dimensions& dimsB = B.dimensions();
    return dimsA.is2D() && dimsB.is2D() && dimsA.rows() == dimsA.columns() && dimsA.rows() > 1
        && dimsB.rows() == dimsA.rows() && dimsB.columns() > 0;
}
//=============================================================================
ArrayOf
Evaluator::transposedLeftDivideOperator(const ArrayOf& A, const ArrayOf& B, bool conjugate)
{
    const std::string transposeName = conjugate ? CTRANSPOSE_OPERATOR_STR : TRANSPOSE_OPERATOR_STR;
    if (isTransposedSolveOperand(A, B, conjugate)) {
        const std::string className = ClassName(A);
        if (!builtinOverloadDefined(this, className, transposeName)
            && !builtinOverloadDefined(this, className, MLDIVIDE_OPERATOR_STR)) {
            // A' \ B = (B' / A)': every step is exact and A is factored as given
            bool needToOverload = false;
            ArrayOf numerator = ComplexTranspose(B, needToOverload);
            ArrayOf quotient;
            if (!needToOverload) {
                quotient = RightDivide(numerator, A, needToOverload);
            }
            if (!needToOverload) {
                ArrayOf res = ComplexTranspose(quotient, needToOverload);
                if (!needToOverload) {
                    return res;
                }
            }
        }
    }
    ArrayOfVector args;
    args.push_back(conjugate ? complexTransposeOperator(A) : transposeOperator(A));
    args.push_back(B);
    return leftDivideOperator(args);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
