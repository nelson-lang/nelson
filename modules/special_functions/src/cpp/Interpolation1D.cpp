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
#include <cstring>
#include <vector>
#include "Interpolation1D.hpp"
#include "Interpolation1DKernels.hpp"
#include "LinearInterpolation1D.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static indexType
floatElementSize(NelsonType type);
static bool
isDenseFloat(const ArrayOf& value);
static bool
isDenseRealDouble(const ArrayOf& value);
static ArrayOf
columnVector(
    NelsonType type, indexType length, const void* source, indexType stride, bool reversed);
static ArrayOf
samplePoints(const ArrayOf* X, const ArrayOf& V);
static int
monotonicDirection(const ArrayOf& x);
static bool
hasNaN(const ArrayOf& value);
static ArrayOf
interpolateColumn(const ArrayOf& x, const ArrayOf& y, const ArrayOf& q, const std::wstring& method,
    const std::wstring& extrapolationMode, const ArrayOf& extrapolationValue,
    const Interpolation1DSpline& spline);
static Dimensions
resultDimensions(const ArrayOf& V, const ArrayOf& XQ);
//=============================================================================
bool
Interpolation1D(const ArrayOf* X, const ArrayOf& V, const ArrayOf& XQ, const std::wstring& method,
    const std::wstring& extrapolationMode, const ArrayOf& extrapolationValue,
    const Interpolation1DSpline& spline, ArrayOf& result)
{
    if (!isDenseFloat(V) || !isDenseFloat(XQ) || XQ.isComplex() || V.isEmpty()) {
        return false;
    }
    if (X != nullptr && (!isDenseFloat(*X) || X->isComplex())) {
        return false;
    }
    if (extrapolationMode == L"constant"
        && (!isDenseFloat(extrapolationValue) || extrapolationValue.isComplex()
            || !extrapolationValue.isScalar())) {
        return false;
    }
    ArrayOf x = samplePoints(X, V);
    indexType n = x.elementCount();
    int direction = n < 2 ? 0 : monotonicDirection(x);
    if (direction == 0) {
        return false;
    }
    bool isVectorV = V.isVector();
    if ((isVectorV && V.elementCount() != n) || (!isVectorV && V.rows() != n)) {
        return false;
    }
    // spline + constant compares the queries with the bounds in their own class
    if (method == L"spline" && extrapolationMode == L"constant"
        && (X == nullptr ? NLS_DOUBLE : X->dataClass()) != XQ.dataClass()) {
        return false;
    }
    if (method == L"cubic" || method == L"makima") {
        bool allDouble = isDenseRealDouble(x) && isDenseRealDouble(V) && isDenseRealDouble(XQ);
        if (!allDouble) {
            return false;
        }
        // the generic path warns (extrapolation, non-uniform grid) or errors (NaN query)
        if (method == L"cubic"
            && (extrapolationMode == L"extrap" || n < 3 || hasNaN(XQ)
                || !isUniformGrid(static_cast<const double*>(x.dataPointer()), n))) {
            return false;
        }
    }
    bool reversed = direction < 0;
    if (reversed) {
        x = columnVector(x.dataClass(), n, x.dataPointer(), 1, true);
    }
    indexType nq = XQ.elementCount();
    ArrayOf q = columnVector(XQ.dataClass(), nq, XQ.dataPointer(), 1, false);
    NelsonType outType = V.dataClass();
    indexType columns = V.elementCount() / n;
    indexType elementSize = floatElementSize(outType);
    void* data = ArrayOf::allocateArrayOf(outType, nq * columns);
    ArrayOf out(outType, Dimensions(nq, columns), data, false);
    for (indexType c = 0; c < columns; ++c) {
        const char* source = static_cast<const char*>(V.dataPointer()) + c * n * elementSize;
        ArrayOf y = columnVector(outType, n, source, 1, reversed);
        ArrayOf column
            = interpolateColumn(x, y, q, method, extrapolationMode, extrapolationValue, spline);
        column.promoteType(outType);
        if (nq > 0) {
            std::memcpy(static_cast<char*>(data) + c * nq * elementSize, column.dataPointer(),
                nq * elementSize);
        }
    }
    Dimensions dims = resultDimensions(V, XQ);
    out.reshape(dims);
    result = out;
    return true;
}
//=============================================================================
static ArrayOf
interpolateColumn(const ArrayOf& x, const ArrayOf& y, const ArrayOf& q, const std::wstring& method,
    const std::wstring& extrapolationMode, const ArrayOf& extrapolationValue,
    const Interpolation1DSpline& spline)
{
    bool useConstant = extrapolationMode == L"constant";
    if (method == L"spline") {
        ArrayOf values = spline(x, y, q);
        if (useConstant) {
            ArrayOf constant(extrapolationValue);
            constant.promoteType(values.dataClass());
            ArrayOf xd(x);
            xd.promoteType(NLS_DOUBLE);
            ArrayOf qd(q);
            qd.promoteType(NLS_DOUBLE);
            const auto* xp = static_cast<const double*>(xd.dataPointer());
            const auto* qp = static_cast<const double*>(qd.dataPointer());
            double lo = xp[0];
            double hi = xp[xd.elementCount() - 1];
            indexType elementSize = floatElementSize(values.dataClass());
            auto* ptr = static_cast<char*>(values.readWriteDataPointer());
            for (indexType k = 0; k < qd.elementCount(); ++k) {
                if (qp[k] < lo || qp[k] > hi) {
                    std::memcpy(ptr + k * elementSize, constant.dataPointer(), elementSize);
                }
            }
        }
        return values;
    }
    if (method == L"cubic" || method == L"makima") {
        indexType n = x.elementCount();
        indexType nq = q.elementCount();
        double constant = 0;
        if (useConstant) {
            ArrayOf value(extrapolationValue);
            value.promoteType(NLS_DOUBLE);
            constant = value.asDoubleScalar();
        }
        auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, nq));
        auto* xp = static_cast<const double*>(x.dataPointer());
        auto* yp = static_cast<const double*>(y.dataPointer());
        auto* qp = static_cast<const double*>(q.dataPointer());
        if (method == L"cubic") {
            cubicConvolution1D(xp, yp, n, qp, nq, useConstant, constant, ptr);
        } else {
            makima1D(xp, yp, n, qp, nq, useConstant, constant, ptr);
        }
        return ArrayOf(NLS_DOUBLE, Dimensions(nq, 1), ptr, false);
    }
    // pchip extrapolates by default
    if (useConstant) {
        return LinearInterpolation1D(x, y, q, method, L"constant", &extrapolationValue);
    }
    bool extrapolate = extrapolationMode == L"extrap" || method == L"pchip";
    return LinearInterpolation1D(x, y, q, method, extrapolate ? L"extrap" : L"default", nullptr);
}
//=============================================================================
static indexType
floatElementSize(NelsonType type)
{
    switch (type) {
    case NLS_SINGLE:
        return sizeof(single);
    case NLS_SCOMPLEX:
        return 2 * sizeof(single);
    case NLS_DCOMPLEX:
        return 2 * sizeof(double);
    default:
        return sizeof(double);
    }
}
//=============================================================================
static bool
isDenseFloat(const ArrayOf& value)
{
    return !value.isSparse() && (value.isDoubleClass() || value.isSingleClass());
}
//=============================================================================
static bool
isDenseRealDouble(const ArrayOf& value)
{
    return !value.isSparse() && value.dataClass() == NLS_DOUBLE;
}
//=============================================================================
static ArrayOf
columnVector(NelsonType type, indexType length, const void* source, indexType stride, bool reversed)
{
    indexType elementSize = floatElementSize(type);
    void* data = ArrayOf::allocateArrayOf(type, length);
    const auto* src = static_cast<const char*>(source);
    auto* dst = static_cast<char*>(data);
    for (indexType k = 0; k < length; ++k) {
        indexType from = reversed ? (length - 1 - k) : k;
        std::memcpy(dst + k * elementSize, src + from * stride * elementSize, elementSize);
    }
    return ArrayOf(type, Dimensions(length, 1), data, false);
}
//=============================================================================
static ArrayOf
samplePoints(const ArrayOf* X, const ArrayOf& V)
{
    if (X != nullptr) {
        ArrayOf x(*X);
        return columnVector(x.dataClass(), x.elementCount(), x.dataPointer(), 1, false);
    }
    indexType n = V.isVector() ? V.elementCount() : V.rows();
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    for (indexType k = 0; k < n; ++k) {
        ptr[k] = static_cast<double>(k + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(n, 1), ptr, false);
}
//=============================================================================
template <class T>
static int
monotonicDirectionT(const T* x, indexType n)
{
    bool increasing = true;
    bool decreasing = true;
    for (indexType k = 0; k + 1 < n; ++k) {
        T dx = x[k + 1] - x[k];
        increasing = increasing && (dx > 0);
        decreasing = decreasing && (dx < 0);
    }
    return increasing ? 1 : (decreasing ? -1 : 0);
}
//=============================================================================
static int
monotonicDirection(const ArrayOf& x)
{
    if (x.dataClass() == NLS_SINGLE) {
        return monotonicDirectionT(static_cast<const single*>(x.dataPointer()), x.elementCount());
    }
    return monotonicDirectionT(static_cast<const double*>(x.dataPointer()), x.elementCount());
}
//=============================================================================
static bool
hasNaN(const ArrayOf& value)
{
    const auto* ptr = static_cast<const double*>(value.dataPointer());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        if (std::isnan(ptr[k])) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static Dimensions
resultDimensions(const ArrayOf& V, const ArrayOf& XQ)
{
    Dimensions queryDims = XQ.dimensions();
    queryDims.simplify();
    if (V.isVector()) {
        return queryDims;
    }
    Dimensions valueDims = V.dimensions();
    valueDims.simplify();
    std::vector<indexType> dims;
    if (XQ.isVector()) {
        dims.push_back(XQ.elementCount());
    } else {
        for (indexType k = 0; k < queryDims.length(); ++k) {
            dims.push_back(queryDims.dimensionLength(k));
        }
    }
    for (indexType k = 1; k < valueDims.length(); ++k) {
        dims.push_back(valueDims.dimensionLength(k));
    }
    Dimensions result(dims);
    result.simplify();
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
