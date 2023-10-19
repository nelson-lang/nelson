//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Evaluator.hpp"
#include "ElementWiseMultiplication.hpp"
#include "Operators.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "FindCommonType.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::timesOperator(AbstractSyntaxTreePtr t)
{
    callstack.pushID(t->getContext());
    ArrayOfVector args;
    args << expression(t->down);
    args << expression(t->down->right);
    ArrayOf retval = this->timesOperator(args);
    callstack.popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::timesOperator(const ArrayOfVector& args)
{
    const std::string functionName = TIMES_OPERATOR_STR;
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    ArrayOf res;
    if (FindCommonType(args, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(this,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), args, functionName,
            commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }

    bool needToOverload = false;
    ArrayOf A(args[0]);
    ArrayOf B(args[1]);

    if (A.getDataClass() != B.getDataClass()) {
        if (A.isIntegerType()) {
            bool isCompatible = (B.getDataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
        } else if (B.isIntegerType()) {
            bool isCompatible = (A.getDataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
        } else if (commonType <= NLS_CHAR) {
            NelsonType _commonType = commonType;
            if (_commonType == NLS_CHAR) {
                _commonType = NLS_DOUBLE;
            }
            if (_commonType == NLS_DOUBLE && isComplex) {
                _commonType = NLS_DCOMPLEX;
            }
            if (_commonType == NLS_SINGLE && isComplex) {
                _commonType = NLS_SCOMPLEX;
            }
            A.promoteType(_commonType);
            B.promoteType(_commonType);
        }
    }
    res = elementWiseMultiplication(A, B, needToOverload);

    if (needToOverload) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(this, NLS_OVERLOAD_ALL_TYPES, args, functionName,
            commonTypeName, commonType, overloadWasFound);
        if (!overloadWasFound) {
            OverloadRequired(functionName);
        }
    }

    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
