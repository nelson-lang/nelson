//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <type_traits>
#include "BitwiseBits.hpp"
#include "BitwiseRange.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Calls f with a value of the C++ type of an integer class.
template <class F>
static ArrayOf
withIntegerClass(NelsonType cls, F&& f)
{
    switch (cls) {
    case NLS_INT8:
        return f(int8());
    case NLS_UINT8:
        return f(uint8());
    case NLS_INT16:
        return f(int16());
    case NLS_UINT16:
        return f(uint16());
    case NLS_INT32:
        return f(int32());
    case NLS_UINT32:
        return f(uint32());
    case NLS_INT64:
        return f(int64());
    case NLS_UINT64:
    default:
        return f(uint64());
    }
}
//=============================================================================
static int
classWidth(NelsonType cls)
{
    switch (cls) {
    case NLS_INT8:
    case NLS_UINT8:
        return 8;
    case NLS_INT16:
    case NLS_UINT16:
        return 16;
    case NLS_INT32:
    case NLS_UINT32:
        return 32;
    default:
        return 64;
    }
}
//=============================================================================
static NelsonType
workClassFor(const ArrayOf& A, const std::wstring& assumedType, bool withAssumedType)
{
    bool isDouble = A.dataClass() == NLS_DOUBLE;
    if (!withAssumedType) {
        return isDouble ? NLS_UINT64 : A.dataClass();
    }
    NelsonType cls = bitwiseAssumedClass(assumedType);
    if (cls == NLS_UNKNOWN) {
        Error(L"Nelson:operators:invalidAssumedType", L"ASSUMEDTYPE must be an integer type name.");
    }
    if (!isDouble && cls != A.dataClass()) {
        Error(L"Nelson:operators:integerAssumedType",
            L"Integer inputs cannot be assumed as any other types than their own types.");
    }
    return cls;
}
//=============================================================================
// Returns the bit positions as int64 after checking they are whole and in [1, width].
static ArrayOf
bitPositions(const ArrayOf& bit, int width)
{
    NelsonType cls = bit.dataClass();
    if (cls == NLS_DOUBLE || cls == NLS_SINGLE) {
        // one pass: check and convert floating positions straight to int64
        indexType len = bit.elementCount();
        int64* out = static_cast<int64*>(ArrayOf::allocateArrayOf(NLS_INT64, len));
        ArrayOf positions(NLS_INT64, bit.dimensions(), out, false);
        bool isSingle = cls == NLS_SINGLE;
        const double* pd = static_cast<const double*>(bit.dataPointer());
        const single* ps = static_cast<const single*>(bit.dataPointer());
        int bad = 0;
#if WITH_OPENMP
#pragma omp parallel for reduction(                                                                \
        | : bad) if (NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled()          \
        && (uint64_t)len                                                                           \
            > NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold())
#endif
        for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
            double v = isSingle ? static_cast<double>(ps[k]) : pd[k];
            bad |= !(v >= 1 && v <= width && v == std::floor(v));
            out[k] = bad ? 1 : static_cast<int64>(v);
        }
        if (bad) {
            Error(L"Nelson:operators:bitOutOfRange", L"Bit position out of range.");
        }
        return positions;
    }
    ArrayOf positions(bit);
    positions.promoteType(NLS_INT64);
    const int64* p = static_cast<const int64*>(positions.dataPointer());
    indexType len = positions.elementCount();
    for (indexType k = 0; k < len; ++k) {
        if (p[k] < 1 || p[k] > width) {
            Error(L"Nelson:operators:bitOutOfRange", L"Bit position out of range.");
        }
    }
    return positions;
}
//=============================================================================
// Non-scalar operands must all have one size; returns that size.
static Dimensions
commonDimensions(const ArrayOf* operands, size_t count)
{
    const ArrayOf* first = nullptr;
    for (size_t k = 0; k < count; ++k) {
        if (operands[k].isScalar()) {
            continue;
        }
        if (first == nullptr) {
            first = &operands[k];
        } else if (!first->dimensions().equals(operands[k].dimensions())) {
            Error(L"Nelson:dimagree:inputsSizeX", L"Inputs must have the same size.");
        }
    }
    return first ? first->dimensions() : operands[0].dimensions();
}
//=============================================================================
template <class T, class S, class O>
static ArrayOf
bitgetKernel(
    const ArrayOf& A, const ArrayOf& positions, NelsonType outClass, const Dimensions& dims)
{
    using U = typename std::make_unsigned<T>::type;
    indexType len = dims.elementCount();
    O* out = static_cast<O*>(ArrayOf::allocateArrayOf(outClass, len));
    ArrayOf res(outClass, dims, out, false);
    const S* pa = static_cast<const S*>(A.dataPointer());
    const int64* pp = static_cast<const int64*>(positions.dataPointer());
    bool aScalar = A.isScalar();
    if (positions.isScalar()) {
        const int shift = static_cast<int>(pp[0] - 1);
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
            U v = static_cast<U>(static_cast<T>(pa[aScalar ? 0 : k]));
            out[k] = static_cast<O>((v >> shift) & static_cast<U>(1));
        }
        return res;
    }
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
        U v = static_cast<U>(static_cast<T>(pa[aScalar ? 0 : k]));
        out[k] = static_cast<O>((v >> (pp[k] - 1)) & static_cast<U>(1));
    }
    return res;
}
//=============================================================================
ArrayOf
Bitget(const ArrayOf& A, const ArrayOf& bit, const std::wstring& assumedType, bool withAssumedType)
{
    if (A.isSparse() || bit.isSparse()) {
        Error(L"Nelson:operators:sparseNotSupported", L"Inputs must be full.");
    }
    if (A.isComplex()) {
        Error(L"Nelson:operators:invalidBitInput", L"First input must be real.");
    }
    bool isDouble = A.dataClass() == NLS_DOUBLE;
    if (!isDouble && !A.isIntegerType()) {
        Error(L"Nelson:operators:invalidBitInput:firstInputIntegerDoubleArray",
            L"First input must be an integer or double array.");
    }
    NelsonType workClass = workClassFor(A, assumedType, withAssumedType);
    if (isDouble) {
        checkDoubleInBitRange(A, workClass, withAssumedType);
        if (bit.dataClass() == NLS_SINGLE) {
            Error(L"Nelson:operators:invalidBitshiftInput",
                L"Invalid data type. First and second arguments must be integer or double.");
        }
    }
    if (!bit.isNumeric() || bit.isComplex()) {
        Error(L"Nelson:operators:invalidBitPosition",
            L"Bit position must be a real positive integer.");
    }
    const ArrayOf operands[2] = { A, bit };
    Dimensions dims = commonDimensions(operands, 2);
    ArrayOf positions = bitPositions(bit, classWidth(workClass));
    return withIntegerClass(workClass, [&](auto tag) {
        using T = decltype(tag);
        if (isDouble) {
            return bitgetKernel<T, double, double>(A, positions, NLS_DOUBLE, dims);
        }
        return bitgetKernel<T, T, T>(A, positions, workClass, dims);
    });
}
//=============================================================================
template <class T, class S>
static ArrayOf
bitsetKernel(const ArrayOf& A, const ArrayOf& positions, const ArrayOf& values, NelsonType outClass,
    const Dimensions& dims)
{
    using U = typename std::make_unsigned<T>::type;
    indexType len = dims.elementCount();
    S* out = static_cast<S*>(ArrayOf::allocateArrayOf(outClass, len));
    ArrayOf res(outClass, dims, out, false);
    const S* pa = static_cast<const S*>(A.dataPointer());
    const int64* pp = static_cast<const int64*>(positions.dataPointer());
    const double* pv = static_cast<const double*>(values.dataPointer());
    bool aScalar = A.isScalar();
    bool pScalar = positions.isScalar();
    bool vScalar = values.isScalar();
    if (pScalar && vScalar) {
        const U mask = static_cast<U>(static_cast<U>(1) << (pp[0] - 1));
        const bool setBit = pv[0] != 0;
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
            U u = static_cast<U>(static_cast<T>(pa[aScalar ? 0 : k]));
            u = setBit ? static_cast<U>(u | mask) : static_cast<U>(u & static_cast<U>(~mask));
            out[k] = static_cast<S>(static_cast<T>(u));
        }
        return res;
    }
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
        U u = static_cast<U>(static_cast<T>(pa[aScalar ? 0 : k]));
        U mask = static_cast<U>(static_cast<U>(1) << (pp[pScalar ? 0 : k] - 1));
        u = (pv[vScalar ? 0 : k] != 0) ? static_cast<U>(u | mask)
                                       : static_cast<U>(u & static_cast<U>(~mask));
        out[k] = static_cast<S>(static_cast<T>(u));
    }
    return res;
}
//=============================================================================
ArrayOf
Bitset(const ArrayOf& A, const ArrayOf& bit, const ArrayOf& V, const std::wstring& assumedType,
    bool withAssumedType)
{
    if (A.isComplex() || bit.isComplex() || V.isComplex()) {
        Error(
            L"Nelson:operators:invalidBitInput:inputArgumentReal", L"Input argument must be real.");
    }
    bool isDouble = A.dataClass() == NLS_DOUBLE;
    if (A.dataClass() == NLS_SINGLE || (bit.dataClass() == NLS_SINGLE && !A.isIntegerType())) {
        Error(L"Nelson:operators:invalidBitshiftInput",
            L"Invalid data type. First and second arguments must be integer or double.");
    }
    if ((!isDouble && !A.isIntegerType()) || A.isSparse()) {
        Error(L"Nelson:operators:invalidBitInput:firstInputIntegerDoubleArray",
            L"First input must be an integer or double array.");
    }
    NelsonType bitClass = bit.dataClass();
    if ((bitClass != NLS_DOUBLE && bitClass != NLS_SINGLE && !IS_INTEGER_TYPE(bitClass))
        || bit.isSparse()) {
        Error(L"Nelson:operators:invalidBitPosition",
            L"Bit position must be a real positive integer.");
    }
    NelsonType workClass = workClassFor(A, assumedType, withAssumedType);
    const ArrayOf operands[3] = { A, bit, V };
    Dimensions dims = commonDimensions(operands, 3);
    ArrayOf positions = bitPositions(bit, classWidth(workClass));
    if (isDouble) {
        checkDoubleInBitRange(A, workClass, withAssumedType);
    }
    ArrayOf values(V);
    values.promoteType(NLS_DOUBLE);
    return withIntegerClass(workClass, [&](auto tag) {
        using T = decltype(tag);
        if (isDouble) {
            return bitsetKernel<T, double>(A, positions, values, NLS_DOUBLE, dims);
        }
        return bitsetKernel<T, T>(A, positions, values, workClass, dims);
    });
}
//=============================================================================
} // namespace Nelson
//=============================================================================
