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
#include <limits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "roundBuiltin.hpp"
#include "OverloadRequired.hpp"
#include "TruncateFunctions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// round(X, N) scales by 10^N, rounds the scaled value and scales back.
// A scaled value whose fractional part is within one ulp of 0.5 is treated as
// a tie and rounded away from zero (so round(1.005, 2) gives 1.01).
static inline double
roundScaledValue(double t)
{
    double truncated = std::trunc(t);
    double magnitude = std::fabs(t);
    double ulp = std::nextafter(magnitude, std::numeric_limits<double>::infinity()) - magnitude;
    if (std::fabs(std::fabs(t - truncated) - 0.5) <= ulp) {
        return truncated + std::copysign(1.0, t);
    }
    return std::round(t);
}
//=============================================================================
template <class T>
static void
roundDigitsData(const T* src, T* dst, indexType n, double factor, bool multiply)
{
    if (!std::isfinite(factor)) {
        // 10^N overflows: the value is already exact (N > 0) or rounds to zero (N < 0)
        for (indexType i = 0; i < n; ++i) {
            dst[i] = multiply ? src[i] : static_cast<T>(0);
        }
        return;
    }
    OMP_PARALLEL_FOR_LOOP(n)
    for (ompIndexType i = 0; i < (ompIndexType)n; ++i) {
        double v = static_cast<double>(src[i]);
        double t = multiply ? v * factor : v / factor;
        if (!std::isfinite(t)) {
            // NaN, Inf or an overflowed scaling leave the value unchanged
            dst[i] = src[i];
            continue;
        }
        double r = roundScaledValue(t);
        dst[i] = static_cast<T>(multiply ? r / factor : r * factor);
    }
}
//=============================================================================
template <class T>
static void
roundSignificantData(const T* src, T* dst, indexType n, int digits)
{
    for (indexType i = 0; i < n; ++i) {
        double v = static_cast<double>(src[i]);
        if (v == 0.0 || !std::isfinite(v)) {
            dst[i] = static_cast<T>(v);
            continue;
        }
        double magnitude = std::floor(std::log10(std::fabs(v)));
        double factor = std::pow(10.0, static_cast<double>(digits - 1) - magnitude);
        dst[i] = static_cast<T>(std::round(v * factor) / factor);
    }
}
//=============================================================================
static ArrayOf
roundSignificant(const ArrayOf& A, int digits)
{
    NelsonType cl = A.dataClass();
    Dimensions dims = A.dimensions();
    indexType n = dims.elementCount();
    switch (cl) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(cl, n);
        roundSignificantData<double>((const double*)A.dataPointer(), ptr, n, digits);
        return ArrayOf(cl, dims, ptr);
    }
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(cl, n);
        roundSignificantData<single>((const single*)A.dataPointer(), ptr, n, digits);
        return ArrayOf(cl, dims, ptr);
    }
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(cl, n);
        roundSignificantData<double>((const double*)A.dataPointer(), ptr, n * 2, digits);
        return ArrayOf(cl, dims, ptr);
    }
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(cl, n);
        roundSignificantData<single>((const single*)A.dataPointer(), ptr, n * 2, digits);
        return ArrayOf(cl, dims, ptr);
    }
    default: {
        Error(L"Nelson:elementary_functions:firstArgumentSingleDoubleSecond",
            L"First argument must be single or double when a second argument is used.");
    }
    }
    return {};
}
//=============================================================================
static ArrayOf
roundDigits(const ArrayOf& A, double digits)
{
    NelsonType cl = A.dataClass();
    Dimensions dims = A.dimensions();
    indexType n = dims.elementCount();
    bool multiply = (digits > 0);
    double factor = std::pow(10.0, std::fabs(digits));
    switch (cl) {
    case NLS_DOUBLE: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(cl, n);
        roundDigitsData<double>((const double*)A.dataPointer(), ptr, n, factor, multiply);
        return ArrayOf(cl, dims, ptr);
    }
    case NLS_SINGLE: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(cl, n);
        roundDigitsData<single>((const single*)A.dataPointer(), ptr, n, factor, multiply);
        return ArrayOf(cl, dims, ptr);
    }
    case NLS_DCOMPLEX: {
        double* ptr = (double*)ArrayOf::allocateArrayOf(cl, n);
        roundDigitsData<double>((const double*)A.dataPointer(), ptr, n * 2, factor, multiply);
        return ArrayOf(cl, dims, ptr);
    }
    case NLS_SCOMPLEX: {
        single* ptr = (single*)ArrayOf::allocateArrayOf(cl, n);
        roundDigitsData<single>((const single*)A.dataPointer(), ptr, n * 2, factor, multiply);
        return ArrayOf(cl, dims, ptr);
    }
    default: {
        Error(L"Nelson:elementary_functions:firstArgumentSingleDoubleSecond",
            L"First argument must be single or double when a second argument is used.");
    }
    }
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::roundBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct() || argIn[0].isClassType()) {
        OverloadRequired("round");
    }
    if (argIn.size() >= 2) {
        // round(X, N): round to N digits (N can be negative)
        // round(X, N, 'decimals'): same as round(X, N)
        // round(X, N, 'significant'): round to N significant digits (N > 0)
        ArrayOf A = argIn[0];
        ArrayOf param2 = argIn[1];
        if (!param2.isNumeric() || !param2.isScalar() || param2.isComplex() || param2.isSparse()) {
            Error(L"Nelson:elementary_functions:secondArgumentRealIntegerScalar",
                L"Second argument must be a real integer scalar.");
        }
        double nd = param2.asDoubleScalar();
        if (!std::isfinite(nd) || std::floor(nd) != nd) {
            Error(L"Nelson:elementary_functions:secondArgumentRealIntegerScalar",
                L"Second argument must be a real integer scalar.");
        }
        bool significant = false;
        if (argIn.size() == 3) {
            ArrayOf param3 = argIn[2];
            if (!param3.isRowVectorCharacterArray() && !param3.isScalarStringArray()) {
                Error(L"Nelson:elementary_functions:thirdArgumentDecimalsSignificant",
                    L"Third argument must be 'decimals' or 'significant'.");
            }
            std::wstring flag = param3.asWideString();
            if (flag == L"significant") {
                significant = true;
            } else if (flag != L"decimals") {
                Error(L"Nelson:elementary_functions:thirdArgumentDecimalsSignificant",
                    L"Third argument must be 'decimals' or 'significant'.");
            }
        }
        // Sparse inputs are rounded densely and re-sparsified (round preserves
        // zeros, so the result stays sparse).
        bool wasSparse = A.isSparse();
        if (wasSparse) {
            A.makeDense();
        }
        NelsonType cl = A.dataClass();
        bool supported
            = (cl == NLS_DOUBLE || cl == NLS_SINGLE || cl == NLS_DCOMPLEX || cl == NLS_SCOMPLEX);
        if (!supported) {
            Error(L"Nelson:elementary_functions:firstArgumentSingleDoubleSecond",
                L"First argument must be single or double when a second argument is used.");
        }
        ArrayOf result;
        if (significant) {
            if (nd < 1) {
                Error(L"Nelson:elementary_functions:flagSignificantSecondInputPositive",
                    L"With flag 'significant', the second input must be positive.");
            }
            result = roundSignificant(A, static_cast<int>(nd));
        } else if (nd == 0) {
            result = Round(A);
        } else {
            result = roundDigits(A, nd);
        }
        if (wasSparse) {
            result.makeSparse();
        }
        retval << result;
        return retval;
    }
    retval << Round(argIn[0]);
    return retval;
}
//=============================================================================
