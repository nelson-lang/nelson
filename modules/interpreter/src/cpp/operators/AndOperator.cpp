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
#include "OverloadRequired.hpp"
#include "PredefinedErrorMessages.hpp"
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
    if (FindCommonType(args, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(this,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), args,
            AND_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    if (isComplex) {
        raiseError(L"Nelson:interpreter:ERROR_OPERANDS_MUST_BE_REAL", ERROR_OPERANDS_MUST_BE_REAL);
    }

    if (isSparse
        && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_LOGICAL)) {
        raiseError(L"Nelson:interpreter:UnableToConvert",
            ERROR_ATTEMPT_TO_CONVERT_TO_UNIMPLEMENTED_SPARSE_TYPE);
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
ArrayOf
Evaluator::andOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOfVector args;
    args.reserve(2);
    args.push_back(expression(t->down));
    args.push_back(expression(t->down->right));
    ArrayOf retval = andOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
