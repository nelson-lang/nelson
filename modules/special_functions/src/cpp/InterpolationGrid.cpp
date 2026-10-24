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
#include <limits>
#include <type_traits>
#include <vector>
#include "InterpolationGrid.hpp"
#include "InterpolationGridMakima.hpp"
#include "InterpolationND.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
interpolateOnGrid(std::vector<ArrayOf>& grids, const ArrayOf& V,
    const std::vector<ArrayOf>& queries, const std::wstring& method, bool useConstant,
    const ArrayOf& extrapolationValue, ArrayOf& result);
static bool
tryImplicitGridLinear2D(const ArrayOf& V, const ArrayOf& XQ, const ArrayOf& YQ,
    const std::wstring& method, bool useConstant, const ArrayOf& extrapolationValue,
    ArrayOf& result);
static bool
isDenseRealFloat(const ArrayOf& value);
static bool
isDenseRealDouble(const ArrayOf& value);
static Dimensions
significantDimensions(const ArrayOf& value);
static bool
isQueryVectorLike(const ArrayOf& value);
static ArrayOf
implicitGrid(indexType count);
static ArrayOf
stridedVector(const ArrayOf& source, indexType count, indexType stride);
static int
monotonicDirection(const ArrayOf& grid);
static ArrayOf
reversedVector(const ArrayOf& grid);
static ArrayOf
flipValues(
    const ArrayOf& V, const std::vector<indexType>& gridSize, const std::vector<bool>& flipped);
//=============================================================================
bool
Interpolation2D(const ArrayOf* X, const ArrayOf* Y, const ArrayOf& V, const ArrayOf& XQ,
    const ArrayOf& YQ, const std::wstring& method, bool useConstant,
    const ArrayOf& extrapolationValue, ArrayOf& result)
{
    Dimensions valueDims = V.dimensions();
    if (X == nullptr
        && tryImplicitGridLinear2D(V, XQ, YQ, method, useConstant, extrapolationValue, result)) {
        return true;
    }
    std::vector<ArrayOf> grids(2);
    if (X == nullptr) {
        grids[0] = implicitGrid(valueDims.dimensionLength(0));
        grids[1] = implicitGrid(valueDims.dimensionLength(1));
    } else {
        if (!isDenseRealFloat(*X) || !isDenseRealFloat(*Y)) {
            return false;
        }
        if (X->isVector() && Y->isVector()) {
            grids[0] = stridedVector(*Y, Y->elementCount(), 1);
            grids[1] = stridedVector(*X, X->elementCount(), 1);
        } else {
            // full grids: first row of X, first column of Y
            Dimensions dims = X->dimensions();
            if (!dims.equals(Y->dimensions()) || significantDimensions(*X).length() != 2) {
                return false;
            }
            grids[0] = stridedVector(*Y, dims.dimensionLength(0), 1);
            grids[1] = stridedVector(*X, dims.dimensionLength(1), dims.dimensionLength(0));
        }
    }
    return interpolateOnGrid(grids, V, { YQ, XQ }, method, useConstant, extrapolationValue, result);
}
//=============================================================================
static std::vector<indexType>
dimensionsVector(const Dimensions& dims)
{
    std::vector<indexType> values;
    for (indexType k = 0; k < dims.length(); k++) {
        values.push_back(dims.at(k, false));
    }
    return values;
}
//=============================================================================
static bool
sameDimensions(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions da = a.dimensions();
    Dimensions db = b.dimensions();
    if (da.length() != db.length()) {
        return false;
    }
    for (indexType k = 0; k < da.length(); k++) {
        if (da.at(k, false) != db.at(k, false)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static inline indexType
implicitGridCell(T query, indexType count)
{
    T t = query - static_cast<T>(1);
    indexType last = count - 2;
    if (!(t > static_cast<T>(0))) {
        return 0;
    }
    if (t >= static_cast<T>(last)) {
        return last;
    }
    return static_cast<indexType>(t);
}
//=============================================================================
template <class T>
static void
evaluateImplicitGridLinear2D(const T* values, indexType rows, indexType columns,
    indexType pageCount, const T* queryX, const T* queryY, indexType totalQueries, bool useConstant,
    T outsideValue, T* output)
{
    const indexType pageStride = rows * columns;
    for (indexType qi = 0; qi < totalQueries; ++qi) {
        const T qx = queryX[qi];
        const T qy = queryY[qi];
        if (!(qy >= static_cast<T>(1) && qy <= static_cast<T>(rows) && qx >= static_cast<T>(1)
                && qx <= static_cast<T>(columns))) {
            for (indexType page = 0; page < pageCount; ++page) {
                output[qi + totalQueries * page] = outsideValue;
            }
            continue;
        }
        const indexType iy = implicitGridCell(qy, rows);
        const indexType ix = implicitGridCell(qx, columns);
        const T fy = qy - static_cast<T>(iy + 1);
        const T fx = qx - static_cast<T>(ix + 1);
        const T wy0 = static_cast<T>(1) - fy;
        const T wx0 = static_cast<T>(1) - fx;
        const T* v = values + iy + ix * rows;
        for (indexType page = 0; page < pageCount; ++page) {
            output[qi + totalQueries * page]
                = wy0 * wx0 * v[0] + fy * wx0 * v[1] + wy0 * fx * v[rows] + fy * fx * v[rows + 1];
            v += pageStride;
        }
    }
    (void)useConstant;
}
//=============================================================================
template <class T>
static bool
tryImplicitGridLinear2DTyped(const ArrayOf& V, ArrayOf XQ, ArrayOf YQ, bool useConstant,
    const ArrayOf& extrapolationValue, ArrayOf& result)
{
    XQ.promoteType(std::is_same<T, single>::value ? NLS_SINGLE : NLS_DOUBLE);
    YQ.promoteType(std::is_same<T, single>::value ? NLS_SINGLE : NLS_DOUBLE);
    Dimensions valueDims = V.dimensions();
    const indexType rows = valueDims.dimensionLength(0);
    const indexType columns = valueDims.dimensionLength(1);
    const indexType gridCount = rows * columns;
    const indexType pageCount = V.elementCount() / gridCount;
    std::vector<indexType> outputDims = dimensionsVector(XQ.dimensions());
    for (indexType dim = 2; dim < valueDims.length(); ++dim) {
        outputDims.push_back(valueDims.at(dim, false));
    }
    T outsideValue = std::numeric_limits<T>::quiet_NaN();
    if (useConstant) {
        ArrayOf constant(extrapolationValue);
        constant.promoteType(std::is_same<T, single>::value ? NLS_SINGLE : NLS_DOUBLE);
        outsideValue = *static_cast<const T*>(constant.dataPointer());
    }
    void* output = ArrayOf::allocateArrayOf(V.dataClass(), XQ.elementCount() * pageCount);
    evaluateImplicitGridLinear2D(static_cast<const T*>(V.dataPointer()), rows, columns, pageCount,
        static_cast<const T*>(XQ.dataPointer()), static_cast<const T*>(YQ.dataPointer()),
        XQ.elementCount(), useConstant, outsideValue, static_cast<T*>(output));
    result = ArrayOf(V.dataClass(), Dimensions(outputDims), output);
    return true;
}
//=============================================================================
static bool
tryImplicitGridLinear2D(const ArrayOf& V, const ArrayOf& XQ, const ArrayOf& YQ,
    const std::wstring& method, bool useConstant, const ArrayOf& extrapolationValue,
    ArrayOf& result)
{
    if (!(method.empty() || method == L"linear") || V.isSparse() || V.isComplex()
        || !(V.isDoubleClass() || V.isSingleClass()) || V.isEmpty() || !isDenseRealFloat(XQ)
        || !isDenseRealFloat(YQ) || !sameDimensions(XQ, YQ)) {
        return false;
    }
    Dimensions valueDims = V.dimensions();
    if (valueDims.dimensionLength(0) < 2 || valueDims.dimensionLength(1) < 2) {
        return false;
    }
    if (useConstant
        && (!extrapolationValue.isNumeric() || extrapolationValue.isSparse()
            || extrapolationValue.isComplex() || extrapolationValue.elementCount() != 1)) {
        return false;
    }
    if (V.isSingleClass()) {
        return tryImplicitGridLinear2DTyped<single>(
            V, XQ, YQ, useConstant, extrapolationValue, result);
    }
    return tryImplicitGridLinear2DTyped<double>(V, XQ, YQ, useConstant, extrapolationValue, result);
}
//=============================================================================
bool
Interpolation3D(const ArrayOf* X, const ArrayOf* Y, const ArrayOf* Z, const ArrayOf& V,
    const ArrayOf& XQ, const ArrayOf& YQ, const ArrayOf& ZQ, const std::wstring& method,
    bool useConstant, const ArrayOf& extrapolationValue, ArrayOf& result)
{
    Dimensions valueDims = V.dimensions();
    std::vector<ArrayOf> grids(3);
    if (X == nullptr) {
        grids[0] = implicitGrid(valueDims.dimensionLength(0));
        grids[1] = implicitGrid(valueDims.dimensionLength(1));
        grids[2] = implicitGrid(valueDims.dimensionLength(2));
    } else {
        if (!isDenseRealFloat(*X) || !isDenseRealFloat(*Y) || !isDenseRealFloat(*Z)) {
            return false;
        }
        if (X->isVector() && Y->isVector() && Z->isVector()) {
            grids[0] = stridedVector(*Y, Y->elementCount(), 1);
            grids[1] = stridedVector(*X, X->elementCount(), 1);
            grids[2] = stridedVector(*Z, Z->elementCount(), 1);
        } else {
            // full grids: X(1, :, 1), Y(:, 1, 1), Z(1, 1, :)
            Dimensions dims = significantDimensions(*X);
            if (!dims.equals(significantDimensions(*Y)) || !dims.equals(significantDimensions(*Z))
                || dims.length() > 3) {
                return false;
            }
            indexType rows = dims.dimensionLength(0);
            indexType columns = dims.dimensionLength(1);
            grids[0] = stridedVector(*Y, rows, 1);
            grids[1] = stridedVector(*X, columns, rows);
            grids[2] = stridedVector(*Z, dims.dimensionLength(2), rows * columns);
        }
    }
    return interpolateOnGrid(
        grids, V, { YQ, XQ, ZQ }, method, useConstant, extrapolationValue, result);
}
//=============================================================================
static bool
interpolateOnGrid(std::vector<ArrayOf>& grids, const ArrayOf& V,
    const std::vector<ArrayOf>& queries, const std::wstring& method, bool useConstant,
    const ArrayOf& extrapolationValue, ArrayOf& result)
{
    if (V.isSparse() || !(V.isDoubleClass() || V.isSingleClass()) || V.isEmpty()) {
        return false;
    }
    if (useConstant
        && (!extrapolationValue.isNumeric() || extrapolationValue.isSparse()
            || extrapolationValue.elementCount() != 1)) {
        return false;
    }
    indexType nd = grids.size();
    Dimensions valueDims = V.dimensions();
    std::vector<indexType> gridSize(nd);
    std::vector<bool> flipped(nd, false);
    for (indexType dim = 0; dim < nd; ++dim) {
        gridSize[dim] = valueDims.dimensionLength(dim);
        if (grids[dim].elementCount() != gridSize[dim] || gridSize[dim] < 2) {
            return false;
        }
        int direction = monotonicDirection(grids[dim]);
        if (direction == 0) {
            return false;
        }
        flipped[dim] = direction < 0;
    }
    // query arrays of the same size are paired, otherwise they span a grid
    bool sameSize = true;
    bool sameSignificantSize = true;
    for (indexType dim = 0; dim < nd; ++dim) {
        if (!isDenseRealFloat(queries[dim])) {
            return false;
        }
        sameSize = sameSize && queries[dim].dimensions().equals(queries[0].dimensions());
        sameSignificantSize = sameSignificantSize
            && significantDimensions(queries[dim]).equals(significantDimensions(queries[0]));
    }
    if (sameSize != sameSignificantSize) {
        return false;
    }
    for (indexType dim = 0; dim < nd && !sameSize; ++dim) {
        if (!isQueryVectorLike(queries[dim])) {
            return false;
        }
    }
    bool isMakima = method == L"makima";
    if (isMakima) {
        bool allDouble
            = isDenseRealDouble(V) && (!useConstant || isDenseRealFloat(extrapolationValue));
        for (indexType dim = 0; dim < nd; ++dim) {
            allDouble
                = allDouble && isDenseRealDouble(grids[dim]) && isDenseRealDouble(queries[dim]);
        }
        if (!allDouble) {
            return false;
        }
    }
    ArrayOf values = V;
    bool anyFlipped = false;
    for (indexType dim = 0; dim < nd; ++dim) {
        if (flipped[dim]) {
            grids[dim] = reversedVector(grids[dim]);
            anyFlipped = true;
        }
    }
    if (anyFlipped) {
        values = flipValues(V, gridSize, flipped);
    }
    if (isMakima) {
        double constant = 0;
        if (useConstant) {
            ArrayOf value(extrapolationValue);
            value.promoteType(NLS_DOUBLE);
            constant = value.asDoubleScalar();
        }
        result = makimaOnGrid(grids, values, queries, sameSize, useConstant, constant);
        return true;
    }
    // spline extrapolates by default
    std::wstring mode = useConstant ? L"constant" : (method == L"spline" ? L"extrap" : L"default");
    ArrayOfVector args;
    args << values << ArrayOf::characterArray(method) << ArrayOf::characterArray(mode);
    args << (useConstant ? extrapolationValue : ArrayOf::doubleScalar(std::nan("")));
    for (indexType dim = 0; dim < nd; ++dim) {
        args << grids[dim];
    }
    for (indexType dim = 0; dim < nd; ++dim) {
        args << queries[dim];
    }
    result = InterpolationND(args);
    return true;
}
//=============================================================================
static bool
isDenseRealFloat(const ArrayOf& value)
{
    return !value.isSparse() && !value.isComplex()
        && (value.isDoubleClass() || value.isSingleClass());
}
//=============================================================================
static bool
isDenseRealDouble(const ArrayOf& value)
{
    return !value.isSparse() && value.dataClass() == NLS_DOUBLE;
}
//=============================================================================
static Dimensions
significantDimensions(const ArrayOf& value)
{
    Dimensions dims = value.dimensions();
    dims.simplify();
    return dims;
}
//=============================================================================
static bool
isQueryVectorLike(const ArrayOf& value)
{
    Dimensions dims = value.dimensions();
    indexType nonSingleton = 0;
    for (indexType k = 0; k < dims.length(); ++k) {
        nonSingleton += dims.dimensionLength(k) > 1 ? 1 : 0;
    }
    return nonSingleton <= 1;
}
//=============================================================================
static ArrayOf
implicitGrid(indexType count)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, count));
    for (indexType k = 0; k < count; ++k) {
        ptr[k] = static_cast<double>(k + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(count, 1), ptr, false);
}
//=============================================================================
static ArrayOf
stridedVector(const ArrayOf& source, indexType count, indexType stride)
{
    NelsonType type = source.dataClass();
    size_t elementSize = type == NLS_SINGLE ? sizeof(single) : sizeof(double);
    void* data = ArrayOf::allocateArrayOf(type, count);
    const auto* src = static_cast<const char*>(source.dataPointer());
    for (indexType k = 0; k < count; ++k) {
        std::memcpy(static_cast<char*>(data) + k * elementSize, src + k * stride * elementSize,
            elementSize);
    }
    return ArrayOf(type, Dimensions(count, 1), data, false);
}
//=============================================================================
template <class T>
static int
monotonicDirectionT(const T* g, indexType n)
{
    bool increasing = true;
    bool decreasing = true;
    for (indexType k = 0; k + 1 < n; ++k) {
        T dg = g[k + 1] - g[k];
        increasing = increasing && (dg > 0);
        decreasing = decreasing && (dg < 0);
    }
    return increasing ? 1 : (decreasing ? -1 : 0);
}
//=============================================================================
static int
monotonicDirection(const ArrayOf& grid)
{
    if (grid.dataClass() == NLS_SINGLE) {
        return monotonicDirectionT(
            static_cast<const single*>(grid.dataPointer()), grid.elementCount());
    }
    return monotonicDirectionT(static_cast<const double*>(grid.dataPointer()), grid.elementCount());
}
//=============================================================================
static ArrayOf
reversedVector(const ArrayOf& grid)
{
    indexType n = grid.elementCount();
    ArrayOf reversed = stridedVector(grid, n, 1);
    size_t elementSize = grid.dataClass() == NLS_SINGLE ? sizeof(single) : sizeof(double);
    auto* data = static_cast<char*>(reversed.readWriteDataPointer());
    const auto* src = static_cast<const char*>(grid.dataPointer());
    for (indexType k = 0; k < n; ++k) {
        std::memcpy(data + k * elementSize, src + (n - 1 - k) * elementSize, elementSize);
    }
    return reversed;
}
//=============================================================================
static ArrayOf
flipValues(
    const ArrayOf& V, const std::vector<indexType>& gridSize, const std::vector<bool>& flipped)
{
    // element (i1, ..., ind, page) takes the value of the element mirrored along
    // every flipped grid dimension
    indexType nd = gridSize.size();
    size_t elementSize = static_cast<size_t>(V.elementSize());
    indexType count = V.elementCount();
    void* data = ArrayOf::allocateArrayOf(V.dataClass(), count);
    const auto* src = static_cast<const char*>(V.dataPointer());
    auto* dst = static_cast<char*>(data);
    for (indexType index = 0; index < count; ++index) {
        indexType remainder = index;
        indexType source = 0;
        indexType stride = 1;
        for (indexType dim = 0; dim < nd; ++dim) {
            indexType sub = remainder % gridSize[dim];
            remainder /= gridSize[dim];
            source += (flipped[dim] ? gridSize[dim] - 1 - sub : sub) * stride;
            stride *= gridSize[dim];
        }
        source += remainder * stride;
        std::memcpy(dst + index * elementSize, src + source * elementSize, elementSize);
    }
    return ArrayOf(V.dataClass(), V.dimensions(), data, false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
