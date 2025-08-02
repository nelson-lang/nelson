//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcdBuiltin.hpp"
#include "GCD.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FindCommonType.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::gcdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    const std::string functionName = "gcd";
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 3);

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
    if (A.getDataClass() != B.getDataClass()) {
        if (A.isIntegerType()) {
            bool isCompatible = (B.getDataClass() == NLS_DOUBLE) && B.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
            }
            A.promoteType(commonType);
            B.promoteType(commonType);
        } else if (B.isIntegerType()) {
            bool isCompatible = (A.getDataClass() == NLS_DOUBLE) && A.isScalar();
            if (!isCompatible) {
                Error(_W("Integers can only be combined with integers of the same class, or scalar "
                         "doubles."));
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
    res = GCD(A, B, needToOverload);

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
