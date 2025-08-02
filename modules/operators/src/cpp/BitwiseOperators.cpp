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
#include "i18n.hpp"
#include "BitwiseInteger.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSupportedAssumedType(const std::wstring& assumedType);
//=============================================================================
static std::wstring
getIdPrefix(BITWISE_OPERATOR bitwiseOperator);
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
            Error(_W("finite integer values expected."),
                getIdPrefix(bitwiseOperator) + L"outOfRange");
        }
        if (!A.isPositive()) {
            Error(_W("finite positive integer values expected."),
                getIdPrefix(bitwiseOperator) + L"outOfRange");
        }
    }
    if (B.isDoubleType()) {
        if (!B.isIntegerValue() && !B.isEmpty()) {
            Error(_W("finite integer values expected."),
                getIdPrefix(bitwiseOperator) + L"outOfRange");
        }
        if (!B.isPositive()) {
            Error(_W("finite positive integer values expected."),
                getIdPrefix(bitwiseOperator) + L"outOfRange");
        }
    }
    if (withAssumedType && !isSupportedAssumedType(assumedType)) {
        Error(_W("ASSUMEDTYPE must be an integer type name."),
            getIdPrefix(bitwiseOperator) + L"unknownTypeString");
    }
    bool isSupportedTypeA = (A.isDoubleType(true) || A.isIntegerType() || A.isLogical());
    bool isSupportedTypeB = (B.isDoubleType(true) || B.isIntegerType() || B.isLogical());
    bool isSupportedType = isSupportedTypeA && isSupportedTypeB;
    if (!isSupportedType) {
        Error(_W("Operands to BIT Ops must be numeric."),
            getIdPrefix(bitwiseOperator) + L"operandsNotNumeric");
    }
    if (A.isSparse() || B.isSparse()) {
        Error(_W("Inputs must be full."), getIdPrefix(bitwiseOperator) + L"sparseArgs");
    }
    if (A.isLogical() && B.isLogical()) {
        // assumedType not considered for compatibility
        return integer_bitwise<logical>(bitwiseOperator, NLS_LOGICAL, A, B);
    } else if (A.isDoubleType(true) && B.isDoubleType(true)) {
        // assumedType not considered for compatibility
        return BitwiseOperatorDoubleDouble(bitwiseOperator, A, B);
    } else if (A.isIntegerType() && B.isIntegerType()) {
        if (A.getDataClass() != B.getDataClass()) {
            Error(_W("Input arguments must have the same class or scalar doubles."),
                getIdPrefix(bitwiseOperator) + L"mixedClasses");
        }
        std::wstring _assumedType = assumedType;
        if (!withAssumedType) {
            ClassName(A, _assumedType);
        }
        return BitwiseOperatorIntegerInteger(bitwiseOperator, A, B, _assumedType);
    }

    NelsonType destinationType;
    if (A.isDoubleType()) {
        if (B.getDataClass() == NLS_LOGICAL) {
            destinationType = NLS_DOUBLE;
        } else {
            destinationType = B.getDataClass();
        }
    } else {
        if (A.getDataClass() == NLS_LOGICAL) {
            destinationType = NLS_DOUBLE;
        } else {
            destinationType = A.getDataClass();
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
std::wstring
getIdPrefix(BITWISE_OPERATOR bitwiseOperator)
{
    std::wstring id;
    switch (bitwiseOperator) {
    case BITWISE_OPERATOR::BIT_AND: {
        id = L"Nelson:Bitand:";
    } break;
    case BITWISE_OPERATOR::BIT_OR: {
        id = L"Nelson:Bitor:";
    } break;
    case BITWISE_OPERATOR::BIT_XOR: {
        id = L"Nelson:Bitxor:";
    } break;
    }
    return id;
}
//=============================================================================
ArrayOf
BitwiseOperatorDoubleDouble(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf res = {};
    // assumedType not considered for compatibility
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
