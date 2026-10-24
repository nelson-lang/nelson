//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cmath>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
constexpr double NLS_WRAP_PI = 3.14159265358979323846;
//=============================================================================
inline double
wrapModPositive(double x, double period)
{
    return x - std::floor(x / period) * period;
}
//=============================================================================
inline double
wrapTo360Scalar(double x)
{
    double m = wrapModPositive(x, 360.0);
    if (m == 0.0 && x > 0.0) {
        m = 360.0;
    }
    return m;
}
//=============================================================================
inline double
wrapTo180Scalar(double x)
{
    if (x < -180.0 || x > 180.0) {
        return wrapTo360Scalar(x + 180.0) - 180.0;
    }
    return x;
}
//=============================================================================
inline double
wrapTo2PiScalar(double x)
{
    double period = 2.0 * NLS_WRAP_PI;
    double m = wrapModPositive(x, period);
    if (m == 0.0 && x > 0.0) {
        m = period;
    }
    return m;
}
//=============================================================================
inline double
wrapToPiScalar(double x)
{
    if (x < -NLS_WRAP_PI || x > NLS_WRAP_PI) {
        return wrapTo2PiScalar(x + NLS_WRAP_PI) - NLS_WRAP_PI;
    }
    return x;
}
//=============================================================================
inline ArrayOf
applyWrap(const ArrayOf& A, double (*fn)(double))
{
    NelsonType cl = A.dataClass();
    Dimensions dims = A.dimensions();
    indexType n = dims.elementCount();
    if (cl == NLS_DOUBLE) {
        double* dst = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, n);
        const double* src = (const double*)A.dataPointer();
        for (indexType i = 0; i < n; ++i) {
            dst[i] = fn(src[i]);
        }
        return ArrayOf(NLS_DOUBLE, dims, dst);
    }
    if (cl == NLS_SINGLE) {
        single* dst = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, n);
        const single* src = (const single*)A.dataPointer();
        for (indexType i = 0; i < n; ++i) {
            dst[i] = static_cast<single>(fn(static_cast<double>(src[i])));
        }
        return ArrayOf(NLS_SINGLE, dims, dst);
    }
    if (A.isNumeric() && !A.isComplex()) {
        ArrayOf promoted(A);
        promoted.promoteType(NLS_DOUBLE);
        return applyWrap(promoted, fn);
    }
    Error(L"Nelson:trigonometric_functions:expectedRealNumericArray",
        L"Input must be a real numeric array.");
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
