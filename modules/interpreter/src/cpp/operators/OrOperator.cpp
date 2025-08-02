//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Or.hpp"
#include "Evaluator.hpp"
#include "Operators.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::orOperator(const ArrayOfVector& args)
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
            OR_OPERATOR_STR, commonTypeName, commonType, overloadWasFound);
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
    res = Or(args[0], args[1], commonType, neeDToOverload);
    if (!neeDToOverload) {
        return res;
    }
    bool overloadWasFound = false;
    res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, args, OR_OPERATOR_STR,
        commonTypeName, commonType, overloadWasFound);
    if (overloadWasFound) {
        return res;
    }
    OverloadRequired(OR_OPERATOR_STR);
    return {};
}
//=============================================================================
ArrayOf
Evaluator::orOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID((size_t)t->getContext());
    ArrayOfVector args;
    args << expression(t->down);
    args << expression(t->down->right);
    ArrayOf retval = orOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
