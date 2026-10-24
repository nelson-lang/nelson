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
#include <cstring>
#include <vector>
#include "Difference.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<indexType>
significantSizes(const ArrayOf& x);
static ArrayOf
emptyDifference(const ArrayOf& x, const std::vector<indexType>& sizes);
static ArrayOf
sliceAlongDimension(const ArrayOf& x, indexType dim, indexType offset, indexType count);
static ArrayOf
differenceOnce(const ArrayOf& x, indexType dim, const DifferenceSubtraction& subtract);
static ArrayOf
differenceAlongDimension(const ArrayOf& x, const std::vector<indexType>& sizes, double order,
    indexType dim, const DifferenceSubtraction& subtract);
static ArrayOf
differenceAlongFirstDimensions(const ArrayOf& x, const std::vector<indexType>& sizes, double order,
    const DifferenceSubtraction& subtract);
//=============================================================================
ArrayOf
Difference(const ArrayOf& x, double order, indexType dim, const DifferenceSubtraction& subtract)
{
    std::vector<indexType> sizes = significantSizes(x);
    if (dim == 0) {
        return differenceAlongFirstDimensions(x, sizes, order, subtract);
    }
    return differenceAlongDimension(x, sizes, order, dim - 1, subtract);
}
//=============================================================================
static ArrayOf
differenceAlongDimension(const ArrayOf& x, const std::vector<indexType>& sizes, double order,
    indexType dim, const DifferenceSubtraction& subtract)
{
    if (static_cast<double>(sizes[dim]) <= order) {
        std::vector<indexType> emptySizes = sizes;
        emptySizes[dim] = 0;
        return emptyDifference(x, emptySizes);
    }
    ArrayOf result = x;
    auto steps = static_cast<indexType>(order);
    for (indexType k = 0; k < steps; ++k) {
        result = differenceOnce(result, dim, subtract);
    }
    return result;
}
//=============================================================================
static ArrayOf
differenceAlongFirstDimensions(const ArrayOf& x, const std::vector<indexType>& sizes, double order,
    const DifferenceSubtraction& subtract)
{
    indexType nd = static_cast<indexType>(sizes.size());
    indexType dim = 0;
    while (dim < nd && sizes[dim] == 1) {
        dim++;
    }
    if (dim >= nd) {
        dim = 0;
    }
    double reducible = 0;
    for (indexType len : sizes) {
        reducible += static_cast<double>(len) - 1;
    }
    if (reducible < order) {
        return emptyDifference(x, { 0, 0 });
    }
    ArrayOf result = x;
    double remaining = order;
    while (remaining > 0) {
        // Differencing along a dimension of length 0 leaves the (empty) result as
        // it is: there is nothing left to subtract.
        if (dim >= nd || sizes[dim] == 0) {
            break;
        }
        auto steps
            = static_cast<indexType>(std::min(remaining, static_cast<double>(sizes[dim] - 1)));
        for (indexType k = 0; k < steps; ++k) {
            result = differenceOnce(result, dim, subtract);
        }
        remaining -= static_cast<double>(steps);
        dim++;
    }
    if (result.isLogical() || result.isCharacterArray()) {
        // a difference of logical or char values is double, even when empty
        result.promoteType(NLS_DOUBLE);
    }
    return result;
}
//=============================================================================
static std::vector<indexType>
significantSizes(const ArrayOf& x)
{
    Dimensions dims = x.dimensions();
    dims.simplify();
    std::vector<indexType> sizes;
    indexType nd = std::max<indexType>(dims.length(), 2);
    sizes.reserve(nd);
    for (indexType k = 0; k < nd; ++k) {
        sizes.push_back(dims.dimensionLength(k));
    }
    return sizes;
}
//=============================================================================
static ArrayOf
emptyDifference(const ArrayOf& x, const std::vector<indexType>& sizes)
{
    // the class a subtraction of x would give: integers and single are kept
    NelsonType type = NLS_DOUBLE;
    if (x.isSingleClass()) {
        type = NLS_SINGLE;
    } else if (x.isIntegerType()) {
        type = x.dataClass();
    }
    return ArrayOf(type, Dimensions(sizes), ArrayOf::allocateArrayOf(type, 0), false);
}
//=============================================================================
static ArrayOf
sliceAlongDimension(const ArrayOf& x, indexType dim, indexType offset, indexType count)
{
    Dimensions dims = x.dimensions();
    indexType nd = std::max<indexType>(dims.length(), dim + 1);
    bool isDense = !x.isSparse() && (x.isNumeric() || x.isLogical() || x.isCharacterArray());
    if (!isDense) {
        ArrayOfVector index;
        for (indexType k = 0; k < nd; ++k) {
            if (k == dim) {
                std::vector<double> range(count);
                for (indexType i = 0; i < count; ++i) {
                    range[i] = static_cast<double>(offset + i + 1);
                }
                index << ArrayOf::doubleRowVector(range);
            } else {
                index << ArrayOf::characterArray(":");
            }
        }
        ArrayOf source = x;
        return source.nDimSubset(index);
    }
    indexType inner = 1;
    for (indexType k = 0; k < dim; ++k) {
        inner *= dims.dimensionLength(k);
    }
    indexType length = dims.dimensionLength(dim);
    indexType outer = 1;
    for (indexType k = dim + 1; k < nd; ++k) {
        outer *= dims.dimensionLength(k);
    }
    size_t blockBytes = static_cast<size_t>(inner * x.elementSize());
    size_t chunkBytes = blockBytes * count;
    NelsonType type = x.dataClass();
    void* data = ArrayOf::allocateArrayOf(type, inner * count * outer);
    const auto* src = static_cast<const char*>(x.dataPointer()) + offset * blockBytes;
    auto* dst = static_cast<char*>(data);
    for (indexType o = 0; o < outer; ++o) {
        std::memcpy(dst + o * chunkBytes, src + o * blockBytes * length, chunkBytes);
    }
    Dimensions sliceDims = dims;
    sliceDims.setDimensionLength(dim, count);
    // Same shape as indexing: trailing singletons are dropped unless the slice is empty.
    if (sliceDims.elementCount() > 0) {
        sliceDims.simplify();
    }
    return ArrayOf(type, sliceDims, data, false);
}
//=============================================================================
static ArrayOf
differenceOnce(const ArrayOf& x, indexType dim, const DifferenceSubtraction& subtract)
{
    indexType count = x.dimensions().dimensionLength(dim) - 1;
    ArrayOf upper = sliceAlongDimension(x, dim, 1, count);
    ArrayOf lower = sliceAlongDimension(x, dim, 0, count);
    return subtract(upper, lower);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
