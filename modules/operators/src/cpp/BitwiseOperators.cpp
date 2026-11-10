//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "BitwiseOperators.hpp"
#include "BitwiseRange.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "BitwiseInteger.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
doubleBitwiseDispatch(
    BITWISE_OPERATOR bitwiseOperator, NelsonType workClass, const ArrayOf& A, const ArrayOf& B)
{
    switch (workClass) {
    case NLS_INT8:
        return double_bitwise<int8>(bitwiseOperator, A, B);
    case NLS_UINT8:
        return double_bitwise<uint8>(bitwiseOperator, A, B);
    case NLS_INT16:
        return double_bitwise<int16>(bitwiseOperator, A, B);
    case NLS_UINT16:
        return double_bitwise<uint16>(bitwiseOperator, A, B);
    case NLS_INT32:
        return double_bitwise<int32>(bitwiseOperator, A, B);
    case NLS_UINT32:
        return double_bitwise<uint32>(bitwiseOperator, A, B);
    case NLS_INT64:
        return double_bitwise<int64>(bitwiseOperator, A, B);
    case NLS_UINT64:
    default:
        return double_bitwise<uint64>(bitwiseOperator, A, B);
    }
}
//=============================================================================
static ArrayOf
integerBitwiseDispatch(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B)
{
    NelsonType cls = A.dataClass();
    switch (cls) {
    case NLS_INT8:
        return integer_bitwise<int8>(bitwiseOperator, cls, A, B);
    case NLS_UINT8:
        return integer_bitwise<uint8>(bitwiseOperator, cls, A, B);
    case NLS_INT16:
        return integer_bitwise<int16>(bitwiseOperator, cls, A, B);
    case NLS_UINT16:
        return integer_bitwise<uint16>(bitwiseOperator, cls, A, B);
    case NLS_INT32:
        return integer_bitwise<int32>(bitwiseOperator, cls, A, B);
    case NLS_UINT32:
        return integer_bitwise<uint32>(bitwiseOperator, cls, A, B);
    case NLS_INT64:
        return integer_bitwise<int64>(bitwiseOperator, cls, A, B);
    case NLS_UINT64:
    default:
        return integer_bitwise<uint64>(bitwiseOperator, cls, A, B);
    }
}
//=============================================================================
static void
checkBitwiseOperand(const ArrayOf& X)
{
    NelsonType cls = X.dataClass();
    if (cls == NLS_DCOMPLEX || cls == NLS_SCOMPLEX) {
        Error(L"Nelson:operators:invalidBitInput:inputArgumentReal",
            L"Input argument must be real.");
    }
    if (cls == NLS_SINGLE) {
        Error(L"Nelson:operators:invalidBitshiftInput",
            L"Invalid data type. First and second arguments must be integer or double.");
    }
    if (cls != NLS_DOUBLE && cls != NLS_LOGICAL && !IS_INTEGER_TYPE(cls)) {
        Error(L"Nelson:operators:operandsNotNumeric", L"Operands to BIT Ops must be numeric.");
    }
}
//=============================================================================
static void
checkBitwiseSizes(const ArrayOf& A, const ArrayOf& B)
{
    const Dimensions& dA = A.dimensions();
    const Dimensions& dB = B.dimensions();
    if (A.isScalar() || B.isScalar() || dA.equals(dB)) {
        return;
    }
    indexType n = std::max(dA.length(), dB.length());
    for (indexType k = 0; k < n; ++k) {
        indexType a = dA.dimensionLength(k);
        indexType b = dB.dimensionLength(k);
        if (a != b && a != 1 && b != 1) {
            Error(L"Nelson:dimagree:inputsSizeX", L"Inputs must have the same size.");
        }
    }
}
//=============================================================================
ArrayOf
BitwiseOperator(BITWISE_OPERATOR bitwiseOperator, const ArrayOf& A, const ArrayOf& B,
    const std::wstring& assumedType, bool withAssumedType)
{
    if (A.isSparse() || B.isSparse()) {
        Error(L"Nelson:operators:sparseNotSupported", L"Inputs must be full.");
    }
    checkBitwiseOperand(A);
    checkBitwiseOperand(B);
    NelsonType assumedClass = NLS_UNKNOWN;
    if (withAssumedType) {
        assumedClass = bitwiseAssumedClass(assumedType);
        if (assumedClass == NLS_UNKNOWN) {
            Error(L"Nelson:operators:invalidAssumedType",
                L"ASSUMEDTYPE must be an integer type name.");
        }
    }
    checkBitwiseSizes(A, B);
    NelsonType ca = A.dataClass();
    NelsonType cb = B.dataClass();
    bool intA = IS_INTEGER_TYPE(ca);
    bool intB = IS_INTEGER_TYPE(cb);
    if (!intA && !intB) {
        if (ca == NLS_LOGICAL && cb == NLS_LOGICAL && !withAssumedType) {
            return integer_bitwise<logical>(bitwiseOperator, NLS_LOGICAL, A, B);
        }
        // double or logical values are worked in the assumed integer class
        NelsonType workClass = withAssumedType ? assumedClass : NLS_UINT64;
        ArrayOf a(A);
        ArrayOf b(B);
        a.promoteType(NLS_DOUBLE);
        b.promoteType(NLS_DOUBLE);
        checkDoubleInBitRange(a, workClass, withAssumedType);
        checkDoubleInBitRange(b, workClass, withAssumedType);
        return doubleBitwiseDispatch(bitwiseOperator, workClass, a, b);
    }
    NelsonType intClass = intA ? ca : cb;
    if (withAssumedType && assumedClass != intClass) {
        Error(L"Nelson:operators:integerAssumedType",
            L"Integer inputs cannot be assumed as any other types than their own types.");
    }
    if (intA && intB) {
        if (ca != cb) {
            Error(L"Nelson:operators:integerClassMismatch",
                L"Integers can only be combined with integers of the same class, or scalar "
                L"doubles.");
        }
        return integerBitwiseDispatch(bitwiseOperator, A, B);
    }
    const ArrayOf& other = intA ? B : A;
    if (other.dataClass() != NLS_DOUBLE || !other.isScalar()) {
        Error(L"Nelson:operators:integerClassMismatch",
            L"Integers can only be combined with integers of the same class, or scalar doubles.");
    }
    if (!isDoubleInBitRange(static_cast<const double*>(other.dataPointer())[0], intClass)) {
        Error(L"Nelson:operators:bitMixedValueOutOfRange",
            L"A double combined with an integer must be a whole number that fits the integer "
            L"class.");
    }
    ArrayOf converted(other);
    converted.promoteType(intClass);
    return intA ? integerBitwiseDispatch(bitwiseOperator, A, converted)
                : integerBitwiseDispatch(bitwiseOperator, converted, B);
}
//=============================================================================
template <class T>
static ArrayOf
integerComplement(const ArrayOf& A)
{
    indexType len = A.elementCount();
    T* out = static_cast<T*>(ArrayOf::allocateArrayOf(A.dataClass(), len));
    ArrayOf res(A.dataClass(), A.dimensions(), out, false);
    const T* p = static_cast<const T*>(A.dataPointer());
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
        out[k] = static_cast<T>(~p[k]);
    }
    return res;
}
//=============================================================================
template <class T>
static ArrayOf
doubleComplement(const ArrayOf& A)
{
    indexType len = A.elementCount();
    double* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, len));
    ArrayOf res(NLS_DOUBLE, A.dimensions(), out, false);
    const double* p = static_cast<const double*>(A.dataPointer());
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
        out[k] = static_cast<double>(static_cast<T>(~static_cast<T>(p[k])));
    }
    return res;
}
//=============================================================================
ArrayOf
Bitcmp(const ArrayOf& A, const std::wstring& assumedType, bool withAssumedType)
{
    NelsonType cls = A.dataClass();
    if (cls == NLS_DCOMPLEX || cls == NLS_SCOMPLEX) {
        Error(L"Nelson:operators:invalidBitInput:inputArgumentReal",
            L"Input argument must be real.");
    }
    bool isDouble = cls == NLS_DOUBLE;
    if ((!isDouble && !IS_INTEGER_TYPE(cls)) || A.isSparse()) {
        Error(L"Nelson:operators:invalidBitInput:firstInputIntegerDoubleArray",
            L"First input must be an integer or double array.");
    }
    NelsonType workClass = isDouble ? NLS_UINT64 : cls;
    if (withAssumedType) {
        workClass = bitwiseAssumedClass(assumedType);
        if (workClass == NLS_UNKNOWN) {
            Error(L"Nelson:operators:invalidAssumedType",
                L"ASSUMEDTYPE must be an integer type name.");
        }
        if (!isDouble && workClass != cls) {
            Error(L"Nelson:operators:integerAssumedType",
                L"Integer inputs cannot be assumed as any other types than their own types.");
        }
    }
    if (isDouble) {
        checkDoubleInBitRange(A, workClass, withAssumedType);
    }
    switch (workClass) {
    case NLS_INT8:
        return isDouble ? doubleComplement<int8>(A) : integerComplement<int8>(A);
    case NLS_UINT8:
        return isDouble ? doubleComplement<uint8>(A) : integerComplement<uint8>(A);
    case NLS_INT16:
        return isDouble ? doubleComplement<int16>(A) : integerComplement<int16>(A);
    case NLS_UINT16:
        return isDouble ? doubleComplement<uint16>(A) : integerComplement<uint16>(A);
    case NLS_INT32:
        return isDouble ? doubleComplement<int32>(A) : integerComplement<int32>(A);
    case NLS_UINT32:
        return isDouble ? doubleComplement<uint32>(A) : integerComplement<uint32>(A);
    case NLS_INT64:
        return isDouble ? doubleComplement<int64>(A) : integerComplement<int64>(A);
    case NLS_UINT64:
    default:
        return isDouble ? doubleComplement<uint64>(A) : integerComplement<uint64>(A);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
