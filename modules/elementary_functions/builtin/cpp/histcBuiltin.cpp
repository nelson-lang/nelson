//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include "histcBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
indexType
firstNonSingletonDim(Dimensions dims)
{
    for (indexType k = 0; k < dims.length(); ++k) {
        if (dims[k] != 1) {
            return k;
        }
    }
    return 0;
}
//=============================================================================
indexType
parseDim(const ArrayOf& arg)
{
    if (!arg.isNumeric() || !arg.isScalar() || arg.isComplex()) {
        Error(L"Nelson:elementary_functions:dimensionArgumentPositiveIntegerScalar",
            L"%s must be a positive integer scalar.", L"Dimension");
    }
    double dim = arg.asDoubleScalar();
    if (!std::isfinite(dim) || dim < 1 || std::floor(dim) != dim) {
        Error(L"Nelson:elementary_functions:dimensionArgumentPositiveIntegerScalar",
            L"%s must be a positive integer scalar.", L"Dimension");
    }
    return static_cast<indexType>(dim - 1);
}
//=============================================================================
indexType
findBinLinear(double value, const double* edges, indexType nEdges)
{
    if (nEdges == 0 || std::isnan(value)) {
        return 0;
    }
    if (value == edges[nEdges - 1]) {
        return nEdges;
    }
    for (indexType k = nEdges - 1; k > 0; --k) {
        if (value >= edges[k - 1] && value < edges[k]) {
            return k;
        }
    }
    return 0;
}
//=============================================================================
bool
isAscendingEdges(const double* edges, indexType nEdges)
{
    for (indexType k = 1; k < nEdges; ++k) {
        if (std::isnan(edges[k - 1]) || std::isnan(edges[k]) || edges[k] < edges[k - 1]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
indexType
findBinSorted(double value, const double* edges, indexType nEdges)
{
    if (nEdges == 0 || std::isnan(value)) {
        return 0;
    }
    if (value == edges[nEdges - 1]) {
        return nEdges;
    }
    if (value < edges[0] || value >= edges[nEdges - 1]) {
        return 0;
    }
    const double* it = std::upper_bound(edges, edges + nEdges, value);
    indexType bin = static_cast<indexType>(it - edges);
    return (bin > 0 && bin < nEdges) ? bin : 0;
}
//=============================================================================
indexType
counterIndex(
    indexType linearIndex, Dimensions inDims, Dimensions outDims, indexType dim, indexType bin)
{
    indexType remaining = linearIndex;
    indexType stride = 1;
    indexType result = 0;
    indexType nDims = outDims.length();
    for (indexType d = 0; d < nDims; ++d) {
        indexType coord = 0;
        if (d < inDims.length()) {
            coord = remaining % inDims[d];
            remaining /= inDims[d];
        }
        if (d == dim) {
            coord = bin - 1;
        }
        result += coord * stride;
        stride *= outDims[d];
    }
    return result;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::histcBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    if ((!argIn[0].isNumeric() && !argIn[0].isLogical()) || argIn[0].isComplex()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"First input");
    }
    if (!argIn[1].isNumeric() || !argIn[1].isVector() || argIn[1].isComplex()) {
        Error(
            L"Nelson:elementary_functions:edgesNumericVector", L"Edges must be a numeric vector.");
    }
    ArrayOf values(argIn[0]);
    values.promoteType(NLS_DOUBLE);
    ArrayOf edgeValues(argIn[1]);
    edgeValues.promoteType(NLS_DOUBLE);
    Dimensions inDims = values.dimensions();
    indexType dim = argIn.size() == 3 ? parseDim(argIn[2]) : firstNonSingletonDim(inDims);
    Dimensions outDims(inDims);
    if (dim >= outDims.length()) {
        outDims[dim] = 1;
    }
    outDims[dim] = edgeValues.elementCount();
    indexType nValues = values.elementCount();
    indexType nEdges = edgeValues.elementCount();
    double* counts
        = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, outDims.elementCount()));
    for (indexType k = 0; k < outDims.elementCount(); ++k) {
        counts[k] = 0.;
    }
    const double* valuePtr = static_cast<const double*>(values.dataPointer());
    const double* edgePtr = static_cast<const double*>(edgeValues.dataPointer());
    bool ascendingEdges = isAscendingEdges(edgePtr, nEdges);
    double* bin = nullptr;
    if (nLhs > 1) {
        bin = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, nValues));
    }
    if (values.isVector()) {
        for (indexType k = 0; k < nValues; ++k) {
            indexType b = ascendingEdges ? findBinSorted(valuePtr[k], edgePtr, nEdges)
                                         : findBinLinear(valuePtr[k], edgePtr, nEdges);
            if (bin != nullptr) {
                bin[k] = static_cast<double>(b);
            }
            if (b > 0) {
                counts[b - 1] += 1.;
            }
        }
    } else {
        for (indexType k = 0; k < nValues; ++k) {
            indexType b = ascendingEdges ? findBinSorted(valuePtr[k], edgePtr, nEdges)
                                         : findBinLinear(valuePtr[k], edgePtr, nEdges);
            if (bin != nullptr) {
                bin[k] = static_cast<double>(b);
            }
            if (b > 0) {
                counts[counterIndex(k, inDims, outDims, dim, b)] += 1.;
            }
        }
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_DOUBLE, outDims, counts, false);
    if (nLhs > 1) {
        retval << ArrayOf(NLS_DOUBLE, inDims, bin, false);
    }
    return retval;
}
//=============================================================================
