//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "colonBuiltin.hpp"
#include "Error.hpp"
#include "Colon.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassToString.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOf
colon(const ArrayOf& A, const ArrayOf& B);
//=============================================================================
static ArrayOf
colon(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C);
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::colonBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 2) {
        return colon(argIn[0], argIn[1]);
    }
    if (argIn.size() == 3) {
        return colon(argIn[0], argIn[1], argIn[2]);
    }
    return {};
}
//=============================================================================
static NelsonType
getCommonType(NelsonType typeA, NelsonType typeB)
{
    if (typeA == typeB) {
        return typeA;
    }
    if (typeA == NLS_DOUBLE || typeA == NLS_DCOMPLEX) {
        return typeB;
    }
    if (typeB == NLS_DOUBLE || typeA == NLS_DCOMPLEX) {
        return typeA;
    }
    return NLS_UNKNOWN;
}
//=============================================================================
static NelsonType
getCommonType(const ArrayOf& A, const ArrayOf& B)
{
    NelsonType commonType = NLS_UNKNOWN;
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();

    bool isObjectA = classA > NLS_CHAR;
    bool isObjectB = classB > NLS_CHAR;
    if (isObjectA || isObjectB) {
        return NLS_UNKNOWN;
    }

    if ((classA == NLS_CHAR) && (classB != NLS_CHAR)) {
        Error(_W("For colon operator with char operands, first and last operands must be char."),
            L"Nelson:colon:mixedCharOperands");
    }
    bool isIntegerA = (classA >= NLS_INT8 && classA <= NLS_UINT64);
    bool isIntegerB = (classB >= NLS_INT8 && classB <= NLS_UINT64);
    bool isDoubleA = A.isDoubleClass() && A.isIntegerValue();
    bool isDoubleB = B.isDoubleClass() && B.isIntegerValue();

    bool isSupportedMixedInteger = (isIntegerA && isIntegerB && (classA == classB))
        || (isIntegerA && isDoubleB) || (isDoubleA && isIntegerB);

    if (!isSupportedMixedInteger && (isIntegerA || isIntegerB)) {
        Error(_W("Colon operands must be all the same type, or mixed with real double scalar."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    return getCommonType(classA, classB);
}
//=============================================================================
static NelsonType
getCommonType(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C)
{
    NelsonType commonType = NLS_UNKNOWN;
    NelsonType classA = A.getDataClass();
    NelsonType classB = B.getDataClass();
    NelsonType classC = C.getDataClass();

    bool isObjectA = classA > NLS_CHAR;
    bool isObjectB = classB > NLS_CHAR;
    bool isObjectC = classC > NLS_CHAR;

    if (isObjectA || isObjectB || isObjectC) {
        return NLS_UNKNOWN;
    }
    if (classA == NLS_CHAR) {
        if ((classB != NLS_DOUBLE) && (classB != NLS_DCOMPLEX) && (classB != NLS_CHAR)) {
            Error(_W("For colon operator with char operands, second operand must be char or real "
                     "scalar double."),
                L"Nelson:colon:mixedCharOperand");
        }
        if (classC != NLS_CHAR) {
            Error(
                _W("For colon operator with char operands, first and last operands must be char."),
                L"Nelson:colon:mixedCharOperand");
        }
    }
    bool isIntegerA = (classA >= NLS_INT8 && classA <= NLS_UINT64);
    bool isIntegerB = (classB >= NLS_INT8 && classB <= NLS_UINT64);
    bool isIntegerC = (classC >= NLS_INT8 && classC <= NLS_UINT64);
    bool isDoubleA = A.isDoubleClass() && A.isIntegerValue();
    bool isDoubleB = B.isDoubleClass() && B.isIntegerValue();
    bool isDoubleC = C.isDoubleClass() && C.isIntegerValue();

    bool isSupportedMixedInteger = false;

    if (isIntegerA && isIntegerB && isIntegerC) {
        isSupportedMixedInteger = (classA == classB) && (classA == classC);
    } else if (isIntegerA && isDoubleB && isIntegerC) {
        isSupportedMixedInteger = (classA == classC);
    } else if (isIntegerA && isIntegerB && isDoubleC) {
        isSupportedMixedInteger = (classA == classB);
    } else if (isIntegerA && isDoubleB && isDoubleC) {
        isSupportedMixedInteger = true;
    } else if (isDoubleA && isIntegerB && isDoubleC) {
        isSupportedMixedInteger = true;
    } else if (isDoubleA && isDoubleB && isDoubleC) {
        isSupportedMixedInteger = true;
    } else if (isDoubleA && isDoubleB && isIntegerC) {
        isSupportedMixedInteger = true;
    }

    if (!isSupportedMixedInteger && (isIntegerA || isIntegerB || isIntegerC)) {
        Error(_W("Colon operands must be all the same type, or mixed with real double scalar."),
            L"Nelson:colon:mixedNonDoubleOperands");
    }
    return getCommonType(getCommonType(classA, classB), classC);
}
//=============================================================================
ArrayOf
colon(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res;
    NelsonType commonType = getCommonType(A, B);
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(commonType);
    _B.promoteType(commonType);
    switch (commonType) {
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = Colon(_A, _B);
    } break;
    default: {
        OverloadRequired(getOperatorName(COLON_OP));
    } break;
    }
    return res;
}
//=============================================================================
ArrayOf
colon(const ArrayOf& A, const ArrayOf& B, const ArrayOf& C)
{
    ArrayOf res;
    NelsonType commonType = getCommonType(A, B, C);
    ArrayOf _A(A);
    ArrayOf _B(B);
    ArrayOf _C(C);
    _A.promoteType(commonType);
    _B.promoteType(commonType);
    _C.promoteType(commonType);
    switch (commonType) {
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_DCOMPLEX:
    case NLS_SCOMPLEX:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_LOGICAL:
    case NLS_CHAR: {
        res = Colon(_A, _B, _C);
    } break;
    default: {
        OverloadRequired(getOperatorName(COLON_OP));
    } break;
    }
    return res;
}
//=============================================================================
