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
#include "BitwiseShift.hpp"
#include "BitwiseRange.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
bitshiftInteger(NelsonType destinationClass, const ArrayOf& A, const ArrayOf& N);
//=============================================================================
template <class T>
static T
shiftOne(T value, int64 n)
{
    const int width = (int)(sizeof(T) * 8);
    if (n == 0) {
        return value;
    }
    if (n > 0) {
        if (n >= width) {
            return (T)0;
        }
        // shift in the unsigned domain to avoid signed overflow, then narrow
        using U = typename std::make_unsigned<T>::type;
        return (T)((U)value << (int)n);
    }
    int s = (int)(-n);
    if (s >= width) {
        // right shift past the width: sign fill for signed, 0 for unsigned
        return (T)(value < 0 ? (T)(-1) : (T)0);
    }
    return (T)(value >> s); // arithmetic for signed, logical for unsigned
}
//=============================================================================
// S is the stored element type of A and of the result, T the integer class used to shift
template <class T, class S = T>
static ArrayOf
bitshiftTyped(NelsonType destinationClass, const ArrayOf& A, const ArrayOf& N)
{
    ArrayOf shifts(N);
    shifts.promoteType(NLS_INT64);
    indexType lenA = A.elementCount();
    indexType lenN = shifts.elementCount();
    bool aScalar = (lenA == 1);
    bool nScalar = (lenN == 1);
    if (!aScalar && !nScalar && !A.dimensions().equals(shifts.dimensions())) {
        Error(L"Nelson:dimagree:bitshiftArraysSizeOneScalar",
            L"bitshift: arrays must be the same size or one must be scalar.");
    }
    Dimensions dims = aScalar ? shifts.dimensions() : A.dimensions();
    indexType len = dims.elementCount();
    S* out = (S*)ArrayOf::allocateArrayOf(destinationClass, len);
    ArrayOf res(destinationClass, dims, out, false);
    const S* pa = (const S*)A.dataPointer();
    const int64* pn = (const int64*)shifts.dataPointer();
    const int64 width = static_cast<int64>(sizeof(T) * 8);
    if (nScalar && !aScalar && pn[0] != 0 && pn[0] < width && pn[0] > -width) {
        // one shift for every element: no per-element branch, the loop vectorizes
        using U = typename std::make_unsigned<T>::type;
        const int s = static_cast<int>(pn[0] > 0 ? pn[0] : -pn[0]);
        if (pn[0] > 0) {
            OMP_PARALLEL_FOR_LOOP(len)
            for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
                out[k] = static_cast<S>(static_cast<T>(static_cast<U>(static_cast<T>(pa[k])) << s));
            }
        } else {
            OMP_PARALLEL_FOR_LOOP(len)
            for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
                out[k] = static_cast<S>(static_cast<T>(static_cast<T>(pa[k]) >> s));
            }
        }
        return res;
    }
    if (nScalar) {
        const int64 n = pn[0];
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
            out[k] = static_cast<S>(shiftOne<T>(static_cast<T>(pa[aScalar ? 0 : k]), n));
        }
        return res;
    }
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
        T a = static_cast<T>(pa[aScalar ? 0 : k]);
        int64 n = pn[k];
        out[k] = static_cast<S>(shiftOne<T>(a, n));
    }
    return res;
}
//=============================================================================
static ArrayOf
bitshiftDouble(NelsonType workClass, const ArrayOf& A, const ArrayOf& N)
{
    switch (workClass) {
    case NLS_INT8:
        return bitshiftTyped<int8, double>(NLS_DOUBLE, A, N);
    case NLS_UINT8:
        return bitshiftTyped<uint8, double>(NLS_DOUBLE, A, N);
    case NLS_INT16:
        return bitshiftTyped<int16, double>(NLS_DOUBLE, A, N);
    case NLS_UINT16:
        return bitshiftTyped<uint16, double>(NLS_DOUBLE, A, N);
    case NLS_INT32:
        return bitshiftTyped<int32, double>(NLS_DOUBLE, A, N);
    case NLS_UINT32:
        return bitshiftTyped<uint32, double>(NLS_DOUBLE, A, N);
    case NLS_INT64:
        return bitshiftTyped<int64, double>(NLS_DOUBLE, A, N);
    case NLS_UINT64:
    default:
        return bitshiftTyped<uint64, double>(NLS_DOUBLE, A, N);
    }
}
//=============================================================================
static void
checkShiftOperand(const ArrayOf& X)
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
    if (cls != NLS_DOUBLE && !IS_INTEGER_TYPE(cls)) {
        Error(L"Nelson:operators:operandsNotNumeric", L"Operands to BIT Ops must be numeric.");
    }
}
//=============================================================================
static void
checkShiftAmounts(const ArrayOf& N)
{
    bool isSingle = N.dataClass() == NLS_SINGLE;
    if (N.dataClass() != NLS_DOUBLE && !isSingle) {
        return;
    }
    indexType len = N.elementCount();
    for (indexType k = 0; k < len; ++k) {
        double v = isSingle ? static_cast<const single*>(N.dataPointer())[k]
                            : static_cast<const double*>(N.dataPointer())[k];
        if (!std::isfinite(v) || v != std::floor(v)) {
            Error(L"Nelson:operators:invalidBitInput:bitshiftShiftAmountARealInteger",
                L"bitshift: the shift amount must be a real integer.");
        }
    }
}
//=============================================================================
ArrayOf
Bitshift(const ArrayOf& A, const ArrayOf& N, const std::wstring& assumedType, bool withAssumedType)
{
    if (A.isSparse() || N.isSparse()) {
        Error(L"Nelson:operators:sparseNotSupported", L"Inputs must be full.");
    }
    checkShiftOperand(A);
    // a single shift amount is accepted with an integer value only
    if (!(N.dataClass() == NLS_SINGLE && IS_INTEGER_TYPE(A.dataClass()))) {
        checkShiftOperand(N);
    }
    NelsonType assumedClass = NLS_UINT64;
    if (withAssumedType) {
        assumedClass = bitwiseAssumedClass(assumedType);
        if (assumedClass == NLS_UNKNOWN) {
            Error(L"Nelson:operators:invalidAssumedType",
                L"ASSUMEDTYPE must be an integer type name.");
        }
    }
    if (!A.isScalar() && !N.isScalar() && !A.dimensions().equals(N.dimensions())) {
        Error(L"Nelson:dimagree:bitshiftArraysSizeOneScalar",
            L"bitshift: arrays must be the same size or one must be scalar.");
    }
    checkShiftAmounts(N);
    if (IS_INTEGER_TYPE(A.dataClass())) {
        if (withAssumedType && A.dataClass() != assumedClass) {
            Error(L"Nelson:operators:integerAssumedType",
                L"Integer inputs cannot be assumed as any other types than their own types.");
        }
        return bitshiftInteger(A.dataClass(), A, N);
    }
    // double values are shifted in the assumed integer class, then restored to double
    checkDoubleInBitRange(A, assumedClass, withAssumedType);
    return bitshiftDouble(assumedClass, A, N);
}
//=============================================================================
ArrayOf
bitshiftInteger(NelsonType destinationClass, const ArrayOf& A, const ArrayOf& N)
{
    switch (destinationClass) {
    case NLS_INT8:
        return bitshiftTyped<int8>(NLS_INT8, A, N);
    case NLS_UINT8:
        return bitshiftTyped<uint8>(NLS_UINT8, A, N);
    case NLS_INT16:
        return bitshiftTyped<int16>(NLS_INT16, A, N);
    case NLS_UINT16:
        return bitshiftTyped<uint16>(NLS_UINT16, A, N);
    case NLS_INT32:
        return bitshiftTyped<int32>(NLS_INT32, A, N);
    case NLS_UINT32:
        return bitshiftTyped<uint32>(NLS_UINT32, A, N);
    case NLS_INT64:
        return bitshiftTyped<int64>(NLS_INT64, A, N);
    case NLS_UINT64:
        return bitshiftTyped<uint64>(NLS_UINT64, A, N);
    default:
        Error(L"Nelson:operators:invalidBitInput:bitshiftIntegerRealInputExpected",
            L"bitshift: integer or real input expected.");
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
