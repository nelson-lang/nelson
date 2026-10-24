//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "hypotBuiltin.hpp"
#include "Error.hpp"
#include "Hypotenuse.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::hypotBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{

    const std::string functionName = "hypot";
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    nargincheck(argIn, 2, 2);
    ArrayOf res;
    if (FindCommonType(argIn, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn,
            functionName, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }
    bool needToOverload = false;
    ArrayOf A(argIn[0]);
    ArrayOf B(argIn[1]);
    if (A.dataClass() != B.dataClass()) {
        if (A.isIntegerType()) {
            bool isCompatible = (B.dataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(L"Nelson:hypot:integerCombination",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            A.promoteType(commonType);
            B.promoteType(commonType);
        } else if (B.isIntegerType()) {
            bool isCompatible = (A.dataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(L"Nelson:hypot:integerCombination",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            A.promoteType(commonType);
            B.promoteType(commonType);
        } else {
            if (commonType <= NLS_CHAR) {
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
    }
    res = Hypotenuse(A, B, needToOverload);

    if (needToOverload) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, argIn, functionName,
            commonTypeName, commonType, overloadWasFound);
        if (!overloadWasFound) {
            OverloadRequired(functionName);
        }
    }
    return res;
}
//=============================================================================
