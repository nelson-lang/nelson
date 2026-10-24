//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BitwiseOperators.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "BitwiseInteger.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSupportedAssumedType(const std::wstring& assumedType);
//=============================================================================
static ArrayOf
BitwiseOperatorDoubleDouble(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B);
//=============================================================================
static ArrayOf
BitwiseOperatorIntegerInteger(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B,
    const std::wstring& assumedType);
//=============================================================================
ArrayOf
BitwiseOperator(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B,
    const std::wstring& assumedType, bool withAssumedType)
{
    ArrayOf res = {};
    if (A.isDoubleType()) {
        if (!A.isIntegerValue() && !A.isEmpty()) {
            Error(L"Nelson:operators:finiteIntegerExpected", L"finite integer values expected.");
        }
        if (!A.isPositive()) {
            Error(L"Nelson:operators:finitePositiveIntegerExpected",
                L"finite positive integer values expected.");
        }
    }
    if (B.isDoubleType()) {
        if (!B.isIntegerValue() && !B.isEmpty()) {
            Error(L"Nelson:operators:finiteIntegerExpected", L"finite integer values expected.");
        }
        if (!B.isPositive()) {
            Error(L"Nelson:operators:finitePositiveIntegerExpected",
                L"finite positive integer values expected.");
        }
    }
    if (withAssumedType && !isSupportedAssumedType(assumedType)) {
        Error(L"Nelson:operators:invalidAssumedType", L"ASSUMEDTYPE must be an integer type name.");
    }
    bool isSupportedTypeA = (A.isDoubleType(true) || A.isIntegerType() || A.isLogical());
    bool isSupportedTypeB = (B.isDoubleType(true) || B.isIntegerType() || B.isLogical());
    bool isSupportedType = isSupportedTypeA && isSupportedTypeB;
    if (!isSupportedType) {
        Error(L"Nelson:operators:operandsNotNumeric", L"Operands to BIT Ops must be numeric.");
    }
    if (A.isSparse() || B.isSparse()) {
        Error(L"Nelson:operators:sparseNotSupported", L"Inputs must be full.");
    }
    if (A.isLogical() && B.isLogical()) {
        // assumedType is ignored
        return integer_bitwise<logical>(bitwiseOperator, NLS_LOGICAL, A, B);
    } else if (A.isDoubleType(true) && B.isDoubleType(true)) {
        // assumedType is ignored
        return BitwiseOperatorDoubleDouble(bitwiseOperator, A, B);
    } else if (A.isIntegerType() && B.isIntegerType()) {
        if (A.dataClass() != B.dataClass()) {
            Error(L"Nelson:operators:mixedClasses",
                L"Input arguments must have the same class or scalar doubles.");
        }
        std::wstring _assumedType = assumedType;
        if (!withAssumedType) {
            ClassName(A, _assumedType);
        }
        return BitwiseOperatorIntegerInteger(bitwiseOperator, A, B, _assumedType);
    }

    NelsonType destinationType;
    if (A.isDoubleType()) {
        if (B.dataClass() == NLS_LOGICAL) {
            destinationType = NLS_DOUBLE;
        } else {
            destinationType = B.dataClass();
        }
    } else {
        if (A.dataClass() == NLS_LOGICAL) {
            destinationType = NLS_DOUBLE;
        } else {
            destinationType = A.dataClass();
        }
    }
    ArrayOf _A(A);
    ArrayOf _B(B);

    _A.promoteType(destinationType);
    _B.promoteType(destinationType);

    if (destinationType == NLS_LOGICAL) {
        return integer_bitwise<logical>(bitwiseOperator, NLS_LOGICAL, _A, _B);
    }
    if (destinationType == NLS_DOUBLE) {
        return BitwiseOperatorDoubleDouble(bitwiseOperator, _A, _B);
    }
    if (IS_INTEGER_TYPE(destinationType)) {
        std::wstring _assumedType = assumedType;
        if (!withAssumedType) {
            ClassName(_A, _assumedType);
        }
        return BitwiseOperatorIntegerInteger(bitwiseOperator, _A, _B, _assumedType);
    }
    return res;
}
//=============================================================================
bool
isSupportedAssumedType(const std::wstring& assumedType)
{
    wstringVector supportedType
        = { L"uint64", L"uint32", L"uint16", L"uint8", L"int64", L"int32", L"int16", L"int8" };
    return std::find(supportedType.begin(), supportedType.end(), assumedType)
        != supportedType.end();
}
//=============================================================================
ArrayOf
BitwiseOperatorDoubleDouble(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res = {};
    // assumedType is ignored
    ArrayOf _A(A);
    ArrayOf _B(B);
    _A.promoteType(NLS_UINT64);
    _B.promoteType(NLS_UINT64);
    res = integer_bitwise<uint64>(bitwiseOperator, NLS_UINT64, _A, _B);
    res.promoteType(NLS_DOUBLE);
    return res;
}
//=============================================================================
ArrayOf
BitwiseOperatorIntegerInteger(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B,
    const std::wstring& assumedType)
{
    ArrayOf res = {};
    if (assumedType == L"uint64") {
        return integer_bitwise<uint64>(bitwiseOperator, NLS_UINT64, A, B);
    }
    if (assumedType == L"uint32") {
        return integer_bitwise<uint32>(bitwiseOperator, NLS_UINT32, A, B);
    }
    if (assumedType == L"uint16") {
        return integer_bitwise<uint16>(bitwiseOperator, NLS_UINT16, A, B);
    }
    if (assumedType == L"uint8") {
        return integer_bitwise<uint8>(bitwiseOperator, NLS_UINT8, A, B);
    }
    if (assumedType == L"int64") {
        return integer_bitwise<int64>(bitwiseOperator, NLS_INT64, A, B);
    }
    if (assumedType == L"int32") {
        return integer_bitwise<int32>(bitwiseOperator, NLS_INT32, A, B);
    }
    if (assumedType == L"int16") {
        return integer_bitwise<int16>(bitwiseOperator, NLS_INT16, A, B);
    }
    if (assumedType == L"int8") {
        return integer_bitwise<int8>(bitwiseOperator, NLS_INT8, A, B);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
