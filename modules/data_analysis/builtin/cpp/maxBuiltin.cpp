//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "maxBuiltin.hpp"
#include "Error.hpp"
#include "Maximum.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadHelpers.hpp"
#include "OverloadRequired.hpp"
#include "FindCommonType.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
unaryMaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
static ArrayOfVector
binaryMaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::maxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    switch (argIn.size()) {
    case 1: {
        // M = max(A);
        // [M, i] = max(A);
        return unaryMaxBuiltin(eval, nLhs, argIn);
    } break;
    case 2: {
        // C = max(A, B)
        return binaryMaxBuiltin(eval, nLhs, argIn);
    } break;
    case 3: {
        // [M, I] = max(A, [], dim)
        // [M, I] = max(A, [], nanflag)
        // M = max(A, B, nanflag)
        // M = max(A, [], 'all')
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf param3 = argIn[2];
        Dimensions dimsA = param1.getDimensions();
        Dimensions dimsB = param2.getDimensions();
        if (dimsA.equals(dimsB)) {
            if (param3.isRowVectorCharacterArray()) {
                return binaryMaxBuiltin(eval, nLhs, argIn);
            }
            Error(_("Invalid third argument."));
        }
        return unaryMaxBuiltin(eval, nLhs, argIn);

    } break;
    case 4: {
        // [M, I] = max(A, [], dim, nanflag)
        // M = max(A, [], 'all', nanflag)
        return unaryMaxBuiltin(eval, nLhs, argIn);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return {};
}
//=============================================================================
ArrayOfVector
unaryMaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    const std::string functionName = "max";
    bool wasFound = false;
    ArrayOf res = callOverloadedFunction(eval,
        NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn, functionName,
        ClassName(argIn[0]), argIn[0].getDataClass(), wasFound);
    if (wasFound) {
        return res;
    }
    ArrayOfVector retval;
    bool omitNaN = true;
    bool needToOverload = false;
    switch (argIn.size()) {
    case 1: {
        // M = max(A);
        // [M, i] = max(A);
        retval = Maximum(omitNaN, argIn[0], nLhs, needToOverload);
    } break;
    case 3: {
        if (!argIn[1].isEmpty()) {
            Error(_("Invalid second argument."));
        }
        indexType dim = 0;
        bool isAll = false;
        if (argIn[2].isRowVectorCharacterArray()
            || (argIn[2].isStringArray() && argIn[2].isScalar())) {
            std::wstring s = argIn[2].getContentAsWideString();
            if (s == L"omitnan") {
                omitNaN = true;
            } else if (s == L"includenan") {
                omitNaN = false;
            } else if (s == L"all") {
                isAll = true;
            } else {
                Error(_("Invalid third argument."));
            }
        } else {
            dim = argIn[2].getContentAsScalarIndex(false);
        }
        if (isAll) {
            nargoutcheck(nLhs, 0, 1);
            retval << MaximumAll(omitNaN, argIn[0], needToOverload);
        } else if (dim == 0) {
            retval = Maximum(omitNaN, argIn[0], nLhs, needToOverload);
        } else {
            retval = Maximum(omitNaN, argIn[0], dim, nLhs, needToOverload);
        }
    } break;
    case 4: {
        ArrayOf param4 = argIn[3];
        if (argIn[3].isRowVectorCharacterArray()
            || (argIn[3].isStringArray() && argIn[3].isScalar())) {
            std::wstring s = argIn[3].getContentAsWideString();
            if (s == L"omitnan") {
                omitNaN = true;
            } else if (s == L"includenan") {
                omitNaN = false;
            } else {
                Error(_("Invalid 4th argument."));
            }
        }
        if (!argIn[1].isEmpty()) {
            Error(_("Invalid second argument."));
        }
        if (argIn[2].isRowVectorCharacterArray()
            || (argIn[2].isStringArray() && argIn[2].isScalar())) {
            std::wstring s = argIn[2].getContentAsWideString();
            if (s != L"all") {
                Error(_("Invalid third argument."));
            }
            nargoutcheck(nLhs, 0, 1);
            retval = MaximumAll(omitNaN, argIn[0], needToOverload);
        } else {
            indexType dim = argIn[2].getContentAsScalarIndex(false);
            retval = Maximum(omitNaN, argIn[0], dim, nLhs, needToOverload);
        }
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }

    if (needToOverload) {
        bool overloadWasFound = false;
        NelsonType commonType = argIn[0].getDataClass();
        std::string overloadTypeName = ClassName(argIn[0]);
        retval = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, argIn, functionName,
            overloadTypeName, commonType, overloadWasFound);
        if (!overloadWasFound) {
            OverloadRequired(functionName);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
binaryMaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    const std::string functionName = "max";
    std::string commonTypeName = NLS_UNKNOWN_STR;
    NelsonType commonType = NLS_UNKNOWN;
    bool isSparse = false;
    bool isComplex = false;

    ArrayOfVector args;
    args << argIn[0];
    args << argIn[1];
    ArrayOf res;
    if (FindCommonType(args, commonType, isSparse, isComplex, commonTypeName)) {
        bool overloadWasFound = false;
        res = callOverloadedFunction(eval,
            NelsonConfiguration::getInstance()->getOverloadLevelCompatibility(), argIn,
            functionName, commonTypeName, commonType, overloadWasFound);
        if (overloadWasFound) {
            return res;
        }
    }

    ArrayOfVector retval;
    bool omitNaN = true;
    bool needToOverload = false;

    if (commonType <= NLS_CHAR && !isSparse) {
        NelsonType _commonType = commonType;
        if (commonType == NLS_DOUBLE && isComplex) {
            _commonType = NLS_DCOMPLEX;
        }
        if (commonType == NLS_SINGLE && isComplex) {
            _commonType = NLS_SCOMPLEX;
        }

        ArrayOf A(argIn[0]);
        ArrayOf B(argIn[1]);
        A.promoteType(_commonType);
        B.promoteType(_commonType);

        switch (argIn.size()) {
        case 2: {
            // C = max(A, B)
            retval = Maximum(omitNaN, A, B, needToOverload);
        } break;
        case 3: {
            nargoutcheck(nLhs, 0, 1);
            if (argIn[2].isRowVectorCharacterArray()
                || (argIn[2].isStringArray() && argIn[2].isScalar())) {
                std::wstring s = argIn[2].getContentAsWideString();
                if (s == L"omitnan") {
                    omitNaN = true;
                } else if (s == L"includenan") {
                    omitNaN = false;
                } else {
                    Error(_("Invalid third argument."));
                }

                retval << Maximum(omitNaN, A, B, needToOverload);
            } else {
                Error(_("Invalid third argument."));
            }
        } break;
        default: {
            Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        } break;
        };

        if (!needToOverload) {
            return retval;
        }
    }

    bool overloadWasFound = false;
    retval = callOverloadedFunction(eval, NLS_OVERLOAD_ALL_TYPES, argIn, functionName,
        commonTypeName, commonType, overloadWasFound);

    if (!overloadWasFound) {
        OverloadRequired(functionName);
    }
    return retval;
}
//=============================================================================
