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
//=============================================================================
ArrayOf
Difference(const ArrayOf& x, double order, indexType dim, const DifferenceSubtraction& subtract)
{
    std::vector<indexType> sizes = significantSizes(x);
    indexType along = 0;
    if (dim == 0) {
        // first non-singleton dimension; a scalar gives an empty 0x0 result
        auto found = std::find_if(sizes.begin(), sizes.end(), [](indexType n) { return n != 1; });
        if (found == sizes.end()) {
            return emptyDifference(x, { 0, 0 });
        }
        along = static_cast<indexType>(found - sizes.begin());
    } else {
        along = dim - 1;
        // dimensions after the last one have length 1
        if (along >= static_cast<indexType>(sizes.size())) {
            sizes.resize(along + 1, 1);
        }
    }
    return differenceAlongDimension(x, sizes, order, along, subtract);
}
//=============================================================================
static ArrayOf
differenceAlongDimension(const ArrayOf& x, const std::vector<indexType>& sizes, double order,
    indexType dim, const DifferenceSubtraction& subtract)
{
    if (sizes[dim] == 0 && x.isSparse()) {
        // nothing to subtract: an empty sparse array stays sparse
        ArrayOf result = x;
        if (result.isLogical()) {
            result.promoteType(NLS_DOUBLE);
        }
        return result;
    }
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
