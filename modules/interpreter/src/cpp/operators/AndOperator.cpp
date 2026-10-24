//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "And.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
#include "BinaryOperatorHelpers.hpp"
#include "OverloadRequired.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::andOperator(const ArrayOfVector& args)
{
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    ArrayOf res;
    if (FindCommonType(args, commonType, isSparse, isComplex, commonTypeName) && !isSparse) {
        bool overloadWasFound = false;
        res = callOperatorOverload(
            this, args, AND_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    if (isComplex) {
        Error("Nelson:interpreter:operandsMustBeReal", "Operands must be real.");
    }

    if (isSparse
        && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_LOGICAL)) {
        Error("Nelson:UnableToConvert", "Attempt to convert to unimplemented sparse type");
    }

    bool neeDToOverload = false;
    res = And(args[0], args[1], commonType, neeDToOverload);
    if (!neeDToOverload) {
        return res;
    }
    bool overloadWasFound = false;
    res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, args, AND_OPERATOR_STR,
        commonTypeName, commonType, overloadWasFound);
    if (overloadWasFound) {
        return res;
    }
    OverloadRequired(AND_OPERATOR_STR);
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
