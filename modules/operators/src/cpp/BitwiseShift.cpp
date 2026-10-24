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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static NelsonType
integerTypeFromName(const std::wstring& name);
//=============================================================================
static void
validateDoubleInAssumedRange(const ArrayOf& A, NelsonType assumedClass);
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
template <class T>
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
    T* out = (T*)ArrayOf::allocateArrayOf(destinationClass, len);
    ArrayOf res(destinationClass, dims, out, false);
    const T* pa = (const T*)A.dataPointer();
    const int64* pn = (const int64*)shifts.dataPointer();
    OMP_PARALLEL_FOR_LOOP(len)
    for (ompIndexType k = 0; k < (ompIndexType)len; ++k) {
        T a = pa[aScalar ? 0 : k];
        int64 n = pn[nScalar ? 0 : k];
        out[k] = shiftOne<T>(a, n);
    }
    return res;
}
//=============================================================================
ArrayOf
Bitshift(const ArrayOf& A, const ArrayOf& N, const std::wstring& assumedType, bool withAssumedType)
{
    if (A.isSparse() || N.isSparse()) {
        Error(L"Nelson:operators:sparseNotSupported", L"Inputs must be full.");
    }
    if (!N.isNumeric() || N.isComplex()) {
        Error(L"Nelson:operators:invalidBitInput:bitshiftShiftAmountARealInteger",
            L"bitshift: the shift amount must be a real integer.");
    }
    if (withAssumedType) {
        NelsonType assumedClass = integerTypeFromName(assumedType);
        if (assumedClass == NLS_UNKNOWN) {
            Error(L"Nelson:operators:invalidAssumedType",
                L"ASSUMEDTYPE must be an integer type name.");
        }
        if (A.isIntegerType()) {
            if (A.dataClass() != assumedClass) {
                Error(L"Nelson:operators:integerAssumedType",
                    L"Integer inputs cannot be assumed as any other types than their own types.");
            }
            // assumedType matches A's class: redundant, fall through to the
            // regular typed path below.
        } else if (A.dataClass() == NLS_DOUBLE) {
            validateDoubleInAssumedRange(A, assumedClass);
            // reinterpret the double values as the assumed integer type, shift
            // bounded by that type's width, then restore the double class.
            ArrayOf wide(A);
            wide.promoteType(assumedClass);
            ArrayOf res = bitshiftInteger(assumedClass, wide, N);
            res.promoteType(NLS_DOUBLE);
            return res;
        } else {
            Error(L"Nelson:operators:invalidBitshiftInput",
                L"Invalid data type. First and second arguments must be integer or double.");
        }
    }
    switch (A.dataClass()) {
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64:
        return bitshiftInteger(A.dataClass(), A, N);
    case NLS_LOGICAL:
    case NLS_DOUBLE:
    case NLS_SINGLE: {
        if (A.isComplex()) {
            Error(L"Nelson:operators:invalidBitInput:bitshiftValueReal",
                L"bitshift: the value must be real.");
        }
        if (!A.isIntegerValue() && !A.isEmpty()) {
            Error(L"Nelson:operators:invalidBitInput:bitshiftFiniteIntegerValuesExpected",
                L"bitshift: finite integer values expected.");
        }
        if (!A.isPositive()) {
            Error(L"Nelson:operators:invalidBitInput:bitshiftFinitePositiveIntegerValuesExpected",
                L"bitshift: finite positive integer values expected.");
        }
        // double/single/logical shift in an unsigned 64-bit domain, back to double
        ArrayOf wide(A);
        wide.promoteType(NLS_UINT64);
        ArrayOf res = bitshiftTyped<uint64>(NLS_UINT64, wide, N);
        res.promoteType(NLS_DOUBLE);
        return res;
    }
    default:
        Error(L"Nelson:operators:invalidBitInput:bitshiftIntegerRealInputExpected",
            L"bitshift: integer or real input expected.");
    }
    return {};
}
//=============================================================================
NelsonType
integerTypeFromName(const std::wstring& name)
{
    if (name == L"int8") {
        return NLS_INT8;
    }
    if (name == L"uint8") {
        return NLS_UINT8;
    }
    if (name == L"int16") {
        return NLS_INT16;
    }
    if (name == L"uint16") {
        return NLS_UINT16;
    }
    if (name == L"int32") {
        return NLS_INT32;
    }
    if (name == L"uint32") {
        return NLS_UINT32;
    }
    if (name == L"int64") {
        return NLS_INT64;
    }
    if (name == L"uint64") {
        return NLS_UINT64;
    }
    return NLS_UNKNOWN;
}
//=============================================================================
static void
assumedTypeRange(NelsonType assumedClass, double& minValue, double& maxValue)
{
    switch (assumedClass) {
    case NLS_INT8:
        minValue = -128.0;
        maxValue = 127.0;
        break;
    case NLS_UINT8:
        minValue = 0.0;
        maxValue = 255.0;
        break;
    case NLS_INT16:
        minValue = -32768.0;
        maxValue = 32767.0;
        break;
    case NLS_UINT16:
        minValue = 0.0;
        maxValue = 65535.0;
        break;
    case NLS_INT32:
        minValue = -2147483648.0;
        maxValue = 2147483647.0;
        break;
    case NLS_UINT32:
        minValue = 0.0;
        maxValue = 4294967295.0;
        break;
    case NLS_INT64:
        minValue = -9223372036854775808.0;
        maxValue = 9223372036854775807.0;
        break;
    case NLS_UINT64:
    default:
        minValue = 0.0;
        maxValue = 18446744073709551615.0;
        break;
    }
}
//=============================================================================
void
validateDoubleInAssumedRange(const ArrayOf& A, NelsonType assumedClass)
{
    if (A.isComplex()) {
        Error(L"Nelson:operators:invalidBitInput:doubleInputsIntegerValuesRangeASSUMEDTYPE",
            L"Double inputs must have integer values in the range of ASSUMEDTYPE.");
    }
    double minValue;
    double maxValue;
    assumedTypeRange(assumedClass, minValue, maxValue);
    const double* ptr = (const double*)A.dataPointer();
    indexType len = A.elementCount();
    for (indexType k = 0; k < len; ++k) {
        double v = ptr[k];
        if (!std::isfinite(v) || v != std::floor(v) || v < minValue || v > maxValue) {
            Error(L"Nelson:operators:invalidBitInput:doubleInputsIntegerValuesRangeASSUMEDTYPE",
                L"Double inputs must have integer values in the range of ASSUMEDTYPE.");
        }
    }
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
