//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "InterpolationND.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <cmath>
#include <type_traits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class InterpolationNDMethod
{
    LINEAR,
    NEAREST,
    CUBIC
};
//=============================================================================
enum class InterpolationNDExtrapolation
{
    DEFAULT_NAN,
    EXTRAPOLATE,
    CONSTANT
};
//=============================================================================
static InterpolationNDMethod
parseNDMethod(const std::wstring& method)
{
    if (method == L"linear" || method.empty()) {
        return InterpolationNDMethod::LINEAR;
    }
    if (method == L"nearest") {
        return InterpolationNDMethod::NEAREST;
    }
    if (method == L"cubic" || method == L"makima" || method == L"spline" || method == L"pchip") {
        return InterpolationNDMethod::CUBIC;
    }
    Error(_W("Unsupported interpolation method."));
    return InterpolationNDMethod::LINEAR;
}
//=============================================================================
static InterpolationNDExtrapolation
parseNDExtrapolation(const std::wstring& mode)
{
    if (mode == L"extrap") {
        return InterpolationNDExtrapolation::EXTRAPOLATE;
    }
    if (mode == L"constant") {
        return InterpolationNDExtrapolation::CONSTANT;
    }
    return InterpolationNDExtrapolation::DEFAULT_NAN;
}
//=============================================================================
static bool
isFloatingNumeric(const ArrayOf& value)
{
    return value.isSingleClass() || value.isDoubleClass();
}
//=============================================================================
static std::vector<indexType>
getDimensionsVector(const Dimensions& dims)
{
    std::vector<indexType> values;
    for (indexType k = 0; k < dims.getLength(); k++) {
        values.push_back(dims.getAt(k, false));
    }
    return values;
}
//=============================================================================
static bool
sameDimensions(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions da = a.getDimensions();
    Dimensions db = b.getDimensions();
    if (da.getLength() != db.getLength()) {
        return false;
    }
    for (indexType k = 0; k < da.getLength(); k++) {
        if (da.getAt(k, false) != db.getAt(k, false)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
isQueryVectorLike(const ArrayOf& value)
{
    Dimensions dims = value.getDimensions();
    indexType nonSingleton = 0;
    for (indexType k = 0; k < dims.getLength(); k++) {
        if (dims.getAt(k, false) > 1) {
            nonSingleton++;
        }
    }
    return nonSingleton <= 1;
}
//=============================================================================
template <class T>
static T
numericNaNND()
{
    return static_cast<T>(std::nan(""));
}
//=============================================================================
template <class T>
static bool
strictlyIncreasing(const T* values, indexType count)
{
    for (indexType k = 1; k < count; k++) {
        if (!(values[k] > values[k - 1])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static indexType
findLeftIndexND(const T* grid, indexType count, T query)
{
    if (query <= grid[0]) {
        return 0;
    }
    if (query >= grid[count - 1]) {
        return count - 2;
    }
    const T* it = std::upper_bound(grid, grid + count, query);
    indexType idx = static_cast<indexType>(it - grid) - 1;
    return std::min(idx, count - 2);
}
//=============================================================================
template <class T> struct InterpolationNDStencil
{
    indexType count = 0;
    indexType indices[4] = { 0, 0, 0, 0 };
    T weights[4] = { static_cast<T>(0), static_cast<T>(0), static_cast<T>(0), static_cast<T>(0) };
};
//=============================================================================
template <class T>
static InterpolationNDStencil<T>
buildStencil(
    const T* grid, indexType count, T query, indexType left, T frac, InterpolationNDMethod method)
{
    InterpolationNDStencil<T> stencil;
    if (method != InterpolationNDMethod::CUBIC || count < 4) {
        stencil.count = 2;
        stencil.indices[0] = left;
        stencil.indices[1] = left + 1;
        stencil.weights[0] = static_cast<T>(1) - frac;
        stencil.weights[1] = frac;
        return stencil;
    }

    indexType start = 0;
    if (left > 0) {
        start = left - 1;
    }
    if (start + 4 > count) {
        start = count - 4;
    }
    stencil.count = 4;
    for (indexType k = 0; k < 4; k++) {
        stencil.indices[k] = start + k;
        stencil.weights[k] = static_cast<T>(1);
        for (indexType j = 0; j < 4; j++) {
            if (j != k) {
                T xk = grid[start + k];
                T xj = grid[start + j];
                stencil.weights[k] *= (query - xj) / (xk - xj);
            }
        }
    }
    return stencil;
}
//=============================================================================
template <class T>
static T
getScalarPart(const ArrayOf& value, bool imaginary)
{
    if (value.getElementCount() != 1) {
        Error(_W("Scalar extrapolation value expected."));
    }
    if (!value.isComplex()) {
        if (imaginary) {
            return static_cast<T>(0);
        }
        ArrayOf promoted(value);
        promoted.promoteType(std::is_same<T, single>::value ? NLS_SINGLE : NLS_DOUBLE);
        return std::is_same<T, single>::value ? static_cast<T>(promoted.getContentAsSingleScalar())
                                              : static_cast<T>(promoted.getContentAsDoubleScalar());
    }
    if (value.getDataClass() == NLS_SCOMPLEX) {
        const single* data = (const single*)value.getDataPointer();
        return static_cast<T>(imaginary ? data[1] : data[0]);
    }
    const double* data = (const double*)value.getDataPointer();
    return static_cast<T>(imaginary ? data[1] : data[0]);
}
//=============================================================================
template <class T>
static void
evaluateNDReal(const std::vector<const T*>& grids, const std::vector<indexType>& gridSize,
    const T* values, indexType pageCount, const std::vector<indexType>& strides,
    const std::vector<const T*>& queries, const std::vector<indexType>& querySize,
    indexType totalQueries, bool queryAsGrid, InterpolationNDMethod method,
    InterpolationNDExtrapolation extrapolation, T extrapolationValue, T* output)
{
    indexType nd = (indexType)grids.size();
    OMP_PARALLEL_FOR_LOOP(totalQueries)
    for (ompIndexType qi = 0; qi < (ompIndexType)totalQueries; qi++) {
        std::vector<indexType> left(nd, 0);
        std::vector<indexType> nearest(nd, 0);
        std::vector<T> frac(nd, static_cast<T>(0));
        std::vector<T> queryValues(nd, static_cast<T>(0));
        bool outside = false;

        indexType remIndex = (indexType)qi;
        for (indexType dim = 0; dim < nd; dim++) {
            indexType queryIndex = (indexType)qi;
            if (queryAsGrid) {
                queryIndex = remIndex % querySize[dim];
                remIndex = remIndex / querySize[dim];
            }
            T q = queries[dim][queryIndex];
            queryValues[dim] = q;
            const T* grid = grids[dim];
            indexType n = gridSize[dim];
            if (std::isnan((double)q) || q < grid[0] || q > grid[n - 1]) {
                outside = true;
            }
            left[dim] = findLeftIndexND(grid, n, q);
            frac[dim] = (q - grid[left[dim]]) / (grid[left[dim] + 1] - grid[left[dim]]);
            T dl = std::abs(q - grid[left[dim]]);
            T dr = std::abs(grid[left[dim] + 1] - q);
            nearest[dim] = (dl <= dr) ? left[dim] : left[dim] + 1;
        }

        if (outside && extrapolation == InterpolationNDExtrapolation::CONSTANT) {
            for (indexType page = 0; page < pageCount; page++) {
                output[qi + totalQueries * page] = extrapolationValue;
            }
            continue;
        }
        if (outside && extrapolation == InterpolationNDExtrapolation::DEFAULT_NAN) {
            for (indexType page = 0; page < pageCount; page++) {
                output[qi + totalQueries * page] = numericNaNND<T>();
            }
            continue;
        }

        if (method == InterpolationNDMethod::NEAREST) {
            indexType base = 0;
            for (indexType dim = 0; dim < nd; dim++) {
                base += nearest[dim] * strides[dim];
            }
            for (indexType page = 0; page < pageCount; page++) {
                output[qi + totalQueries * page] = values[base + strides[nd] * page];
            }
        } else {
            std::vector<InterpolationNDStencil<T>> stencils(nd);
            indexType stencilCount = 1;
            for (indexType dim = 0; dim < nd; dim++) {
                stencils[dim] = buildStencil(
                    grids[dim], gridSize[dim], queryValues[dim], left[dim], frac[dim], method);
                stencilCount *= stencils[dim].count;
            }
            for (indexType page = 0; page < pageCount; page++) {
                T accum = static_cast<T>(0);
                for (indexType corner = 0; corner < stencilCount; corner++) {
                    indexType remCorner = corner;
                    indexType base = 0;
                    T weight = static_cast<T>(1);
                    for (indexType dim = 0; dim < nd; dim++) {
                        indexType local = remCorner % stencils[dim].count;
                        remCorner = remCorner / stencils[dim].count;
                        base += stencils[dim].indices[local] * strides[dim];
                        weight *= stencils[dim].weights[local];
                    }
                    accum += weight * values[base + strides[nd] * page];
                }
                output[qi + totalQueries * page] = accum;
            }
        }
    }
}
//=============================================================================
template <class T>
static void
evaluateNDComplex(const std::vector<const T*>& grids, const std::vector<indexType>& gridSize,
    const T* values, indexType pageCount, const std::vector<indexType>& strides,
    const std::vector<const T*>& queries, const std::vector<indexType>& querySize,
    indexType totalQueries, bool queryAsGrid, InterpolationNDMethod method,
    InterpolationNDExtrapolation extrapolation, T extrapolationReal, T extrapolationImag, T* output)
{
    indexType nd = (indexType)grids.size();
    OMP_PARALLEL_FOR_LOOP(totalQueries)
    for (ompIndexType qi = 0; qi < (ompIndexType)totalQueries; qi++) {
        std::vector<indexType> left(nd, 0);
        std::vector<indexType> nearest(nd, 0);
        std::vector<T> frac(nd, static_cast<T>(0));
        std::vector<T> queryValues(nd, static_cast<T>(0));
        bool outside = false;

        indexType remIndex = (indexType)qi;
        for (indexType dim = 0; dim < nd; dim++) {
            indexType queryIndex = (indexType)qi;
            if (queryAsGrid) {
                queryIndex = remIndex % querySize[dim];
                remIndex = remIndex / querySize[dim];
            }
            T q = queries[dim][queryIndex];
            queryValues[dim] = q;
            const T* grid = grids[dim];
            indexType n = gridSize[dim];
            if (std::isnan((double)q) || q < grid[0] || q > grid[n - 1]) {
                outside = true;
            }
            left[dim] = findLeftIndexND(grid, n, q);
            frac[dim] = (q - grid[left[dim]]) / (grid[left[dim] + 1] - grid[left[dim]]);
            nearest[dim] = (std::abs(q - grid[left[dim]]) <= std::abs(grid[left[dim] + 1] - q))
                ? left[dim]
                : left[dim] + 1;
        }

        if (outside && extrapolation == InterpolationNDExtrapolation::CONSTANT) {
            for (indexType page = 0; page < pageCount; page++) {
                indexType outIndex = qi + totalQueries * page;
                output[2 * outIndex] = extrapolationReal;
                output[2 * outIndex + 1] = extrapolationImag;
            }
            continue;
        }
        if (outside && extrapolation == InterpolationNDExtrapolation::DEFAULT_NAN) {
            for (indexType page = 0; page < pageCount; page++) {
                indexType outIndex = qi + totalQueries * page;
                output[2 * outIndex] = numericNaNND<T>();
                output[2 * outIndex + 1] = numericNaNND<T>();
            }
            continue;
        }

        std::vector<InterpolationNDStencil<T>> stencils(nd);
        indexType stencilCount = 1;
        if (method != InterpolationNDMethod::NEAREST) {
            for (indexType dim = 0; dim < nd; dim++) {
                stencils[dim] = buildStencil(
                    grids[dim], gridSize[dim], queryValues[dim], left[dim], frac[dim], method);
                stencilCount *= stencils[dim].count;
            }
        }
        for (indexType page = 0; page < pageCount; page++) {
            T realAccum = static_cast<T>(0);
            T imagAccum = static_cast<T>(0);
            if (method == InterpolationNDMethod::NEAREST) {
                indexType base = 0;
                for (indexType dim = 0; dim < nd; dim++) {
                    base += nearest[dim] * strides[dim];
                }
                indexType valueIndex = base + strides[nd] * page;
                realAccum = values[2 * valueIndex];
                imagAccum = values[2 * valueIndex + 1];
            } else {
                for (indexType corner = 0; corner < stencilCount; corner++) {
                    indexType remCorner = corner;
                    indexType base = 0;
                    T weight = static_cast<T>(1);
                    for (indexType dim = 0; dim < nd; dim++) {
                        indexType local = remCorner % stencils[dim].count;
                        remCorner = remCorner / stencils[dim].count;
                        base += stencils[dim].indices[local] * strides[dim];
                        weight *= stencils[dim].weights[local];
                    }
                    indexType valueIndex = base + strides[nd] * page;
                    realAccum += weight * values[2 * valueIndex];
                    imagAccum += weight * values[2 * valueIndex + 1];
                }
            }
            indexType outIndex = qi + totalQueries * page;
            output[2 * outIndex] = realAccum;
            output[2 * outIndex + 1] = imagAccum;
        }
    }
}
//=============================================================================
template <class T>
static ArrayOf
evaluateTypedND(const ArrayOf& V, const std::vector<ArrayOf>& gridArrays,
    const std::vector<ArrayOf>& queryArrays, InterpolationNDMethod method,
    InterpolationNDExtrapolation extrapolation, const ArrayOf& extrapolationValue,
    const std::vector<indexType>& outputDims, const std::vector<indexType>& gridSize,
    const std::vector<indexType>& querySize, indexType totalQueries, bool queryAsGrid)
{
    indexType nd = (indexType)gridArrays.size();
    std::vector<const T*> grids(nd);
    std::vector<const T*> queries(nd);
    for (indexType dim = 0; dim < nd; dim++) {
        grids[dim] = (const T*)gridArrays[dim].getDataPointer();
        queries[dim] = (const T*)queryArrays[dim].getDataPointer();
    }

    std::vector<indexType> strides(nd + 1, 1);
    for (indexType dim = 1; dim <= nd; dim++) {
        strides[dim] = strides[dim - 1] * gridSize[dim - 1];
    }
    indexType gridCount = strides[nd];
    indexType pageCount = V.getElementCount() / gridCount;

    NelsonType outputType;
    if (std::is_same<T, single>::value) {
        outputType = V.isComplex() ? NLS_SCOMPLEX : NLS_SINGLE;
    } else {
        outputType = V.isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE;
    }
    void* output = ArrayOf::allocateArrayOf(outputType, totalQueries * pageCount);
    if (V.isComplex()) {
        evaluateNDComplex<T>(grids, gridSize, (const T*)V.getDataPointer(), pageCount, strides,
            queries, querySize, totalQueries, queryAsGrid, method, extrapolation,
            getScalarPart<T>(extrapolationValue, false), getScalarPart<T>(extrapolationValue, true),
            (T*)output);
    } else {
        evaluateNDReal<T>(grids, gridSize, (const T*)V.getDataPointer(), pageCount, strides,
            queries, querySize, totalQueries, queryAsGrid, method, extrapolation,
            getScalarPart<T>(extrapolationValue, false), (T*)output);
    }
    return ArrayOf(outputType, Dimensions(outputDims), output);
}
//=============================================================================
ArrayOf
InterpolationND(const ArrayOfVector& argIn)
{
    if (argIn.size() < 8 || ((argIn.size() - 4) % 2) != 0) {
        Error(_W("Wrong number of input arguments."));
    }
    const ArrayOf& Vinput = argIn[0];
    if (!isFloatingNumeric(Vinput) || Vinput.isSparse()) {
        Error(_W("dense single or double sample values expected."));
    }
    InterpolationNDMethod method = parseNDMethod(argIn[1].getContentAsWideString());
    InterpolationNDExtrapolation extrapolation
        = parseNDExtrapolation(argIn[2].getContentAsWideString());
    indexType nd = ((indexType)argIn.size() - 4) / 2;
    if (nd < 1) {
        Error(_W("At least one interpolation dimension expected."));
    }

    ArrayOf V(Vinput);
    bool useSingle = V.isSingleClass();
    V.promoteType(useSingle ? (V.isComplex() ? NLS_SCOMPLEX : NLS_SINGLE)
                            : (V.isComplex() ? NLS_DCOMPLEX : NLS_DOUBLE));
    std::vector<ArrayOf> grids(nd);
    std::vector<ArrayOf> queries(nd);
    std::vector<indexType> gridSize(nd);
    std::vector<indexType> querySize(nd);
    Dimensions valueDims = V.getDimensions();
    for (indexType dim = 0; dim < nd; dim++) {
        gridSize[dim] = valueDims.getDimensionLength(dim);
        grids[dim] = argIn[4 + dim];
        queries[dim] = argIn[4 + nd + dim];
        if (!isFloatingNumeric(grids[dim]) || grids[dim].isSparse() || grids[dim].isComplex()
            || !isFloatingNumeric(queries[dim]) || queries[dim].isSparse()
            || queries[dim].isComplex()) {
            Error(_W("real dense single or double grids and query points expected."));
        }
        if (grids[dim].getElementCount() != gridSize[dim]) {
            Error(_W("Grid vector length and sample value size are incompatible."));
        }
        grids[dim].promoteType(useSingle ? NLS_SINGLE : NLS_DOUBLE);
        queries[dim].promoteType(useSingle ? NLS_SINGLE : NLS_DOUBLE);
        if (useSingle) {
            if (!strictlyIncreasing((const single*)grids[dim].getDataPointer(), gridSize[dim])) {
                Error(_W("Grid vectors must be strictly increasing."));
            }
        } else {
            if (!strictlyIncreasing((const double*)grids[dim].getDataPointer(), gridSize[dim])) {
                Error(_W("Grid vectors must be strictly increasing."));
            }
        }
    }

    bool queryAsGrid = false;
    std::vector<indexType> outputDims;
    if (queries.empty()) {
        Error(_W("Query points expected."));
    }
    bool sameSize = true;
    for (indexType dim = 1; dim < nd; dim++) {
        sameSize = sameSize && sameDimensions(queries[0], queries[dim]);
    }
    if (sameSize) {
        outputDims = getDimensionsVector(queries[0].getDimensions());
        querySize.assign(nd, queries[0].getElementCount());
    } else {
        queryAsGrid = true;
        for (indexType dim = 0; dim < nd; dim++) {
            if (!isQueryVectorLike(queries[dim])) {
                Error(_W("Query arrays must have the same size."));
            }
            querySize[dim] = queries[dim].getElementCount();
            outputDims.push_back(querySize[dim]);
        }
    }
    indexType totalQueries = 1;
    for (indexType dim = 0; dim < (indexType)outputDims.size(); dim++) {
        totalQueries *= outputDims[dim];
    }

    indexType gridCount = 1;
    for (indexType dim = 0; dim < nd; dim++) {
        gridCount *= gridSize[dim];
    }
    indexType pageCount = V.getElementCount() / gridCount;
    if (pageCount > 1) {
        for (indexType dim = nd; dim < valueDims.getLength(); dim++) {
            outputDims.push_back(valueDims.getAt(dim, false));
        }
    }

    if (useSingle) {
        return evaluateTypedND<single>(V, grids, queries, method, extrapolation, argIn[3],
            outputDims, gridSize, querySize, totalQueries, queryAsGrid);
    }
    return evaluateTypedND<double>(V, grids, queries, method, extrapolation, argIn[3], outputDims,
        gridSize, querySize, totalQueries, queryAsGrid);
}
//=============================================================================
}
//=============================================================================
