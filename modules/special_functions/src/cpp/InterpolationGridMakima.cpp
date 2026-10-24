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
#include "InterpolationGridMakima.hpp"
#include "Interpolation1DKernels.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// The interpolant is evaluated dimension by dimension: the last dimension on
// every grid line first (its slopes do not depend on the query), then each
// preceding dimension over the values obtained at the query.
struct MakimaGrid
{
    std::vector<const double*> grids;
    std::vector<indexType> sizes;
    std::vector<indexType> strides;
    const double* values = nullptr;
    std::vector<double> lastSlopes;
    std::vector<double> query;
};
//=============================================================================
static double
evaluateDimension(const MakimaGrid& grid, indexType dim, indexType base);
static void
computeLastSlopes(MakimaGrid& grid, indexType elementCount);
//=============================================================================
ArrayOf
makimaOnGrid(const std::vector<ArrayOf>& grids, const ArrayOf& V,
    const std::vector<ArrayOf>& queries, bool sameSize, bool useConstant, double constant)
{
    indexType nd = grids.size();
    MakimaGrid grid;
    indexType gridCount = 1;
    for (indexType dim = 0; dim < nd; ++dim) {
        grid.grids.push_back(static_cast<const double*>(grids[dim].dataPointer()));
        grid.sizes.push_back(grids[dim].elementCount());
        grid.strides.push_back(gridCount);
        gridCount *= grids[dim].elementCount();
    }
    grid.values = static_cast<const double*>(V.dataPointer());
    indexType pages = V.elementCount() / gridCount;
    computeLastSlopes(grid, V.elementCount());
    std::vector<const double*> q(nd);
    std::vector<indexType> querySizes(nd);
    indexType total = sameSize ? queries[0].elementCount() : 1;
    for (indexType dim = 0; dim < nd; ++dim) {
        q[dim] = static_cast<const double*>(queries[dim].dataPointer());
        querySizes[dim] = queries[dim].elementCount();
        total = sameSize ? total : total * querySizes[dim];
    }
    auto* out = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, total * pages));
    grid.query.resize(nd);
    for (indexType qi = 0; qi < total; ++qi) {
        bool hasNaN = false;
        bool outside = false;
        indexType remainder = qi;
        for (indexType dim = 0; dim < nd; ++dim) {
            indexType sub = qi;
            if (!sameSize) {
                sub = remainder % querySizes[dim];
                remainder /= querySizes[dim];
            }
            double value = q[dim][sub];
            grid.query[dim] = value;
            const double* g = grid.grids[dim];
            hasNaN = hasNaN || std::isnan(value);
            outside = outside || value < g[0] || value > g[grid.sizes[dim] - 1];
        }
        for (indexType p = 0; p < pages; ++p) {
            double& value = out[qi + p * total];
            if (hasNaN) {
                value = std::nan("");
            } else if (outside && useConstant) {
                value = constant;
            } else {
                value = evaluateDimension(grid, 0, p * gridCount);
            }
        }
    }
    std::vector<indexType> dims;
    if (sameSize) {
        Dimensions queryDims = queries[0].dimensions();
        queryDims.simplify();
        for (indexType k = 0; k < queryDims.length(); ++k) {
            dims.push_back(queryDims.dimensionLength(k));
        }
    } else {
        dims = querySizes;
    }
    if (pages > 1) {
        Dimensions valueDims = V.dimensions();
        valueDims.simplify();
        for (indexType k = nd; k < valueDims.length(); ++k) {
            dims.push_back(valueDims.dimensionLength(k));
        }
    }
    Dimensions resultDims(dims);
    resultDims.simplify();
    return ArrayOf(NLS_DOUBLE, resultDims, out, false);
}
//=============================================================================
static double
evaluateDimension(const MakimaGrid& grid, indexType dim, indexType base)
{
    indexType n = grid.sizes[dim];
    indexType stride = grid.strides[dim];
    std::vector<double> nodes(n);
    if (dim + 1 == grid.sizes.size()) {
        std::vector<double> slopes(n);
        for (indexType k = 0; k < n; ++k) {
            nodes[k] = grid.values[base + k * stride];
            slopes[k] = grid.lastSlopes[base + k * stride];
        }
        return makimaHermite(grid.grids[dim], nodes.data(), slopes.data(), n, grid.query[dim]);
    }
    for (indexType j = 0; j < n; ++j) {
        nodes[j] = evaluateDimension(grid, dim + 1, base + j * stride);
    }
    std::vector<double> slopes = makimaSlopes(grid.grids[dim], nodes.data(), n);
    return makimaHermite(grid.grids[dim], nodes.data(), slopes.data(), n, grid.query[dim]);
}
//=============================================================================
static void
computeLastSlopes(MakimaGrid& grid, indexType elementCount)
{
    indexType last = grid.sizes.size() - 1;
    indexType n = grid.sizes[last];
    indexType stride = grid.strides[last];
    grid.lastSlopes.assign(elementCount, 0.);
    std::vector<double> line(n);
    for (indexType start = 0; start < elementCount; ++start) {
        // a grid line starts where the index along the last dimension is 0
        if ((start / stride) % n != 0) {
            continue;
        }
        for (indexType k = 0; k < n; ++k) {
            line[k] = grid.values[start + k * stride];
        }
        std::vector<double> d = makimaSlopes(grid.grids[last], line.data(), n);
        for (indexType k = 0; k < n; ++k) {
            grid.lastSlopes[start + k * stride] = d[k];
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
