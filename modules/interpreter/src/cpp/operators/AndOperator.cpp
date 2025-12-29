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
        Error(_("Operands must be real."));
    }

    if (isSparse
        && (commonType != NLS_DOUBLE && commonType != NLS_DCOMPLEX && commonType != NLS_LOGICAL)) {
        Error(_("Attempt to convert to unimplemented sparse type"), "Nelson:UnableToConvert");
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
