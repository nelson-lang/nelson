//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "num2cellBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "NewWithException.hpp"
#include <algorithm>
#include <cmath>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
std::vector<indexType>
parsePackedDimensions(const ArrayOf& arg)
{
    if (!arg.isNumeric() || arg.isComplex() || !arg.isVector()) {
        Error(L"Nelson:data_structures:dimensionArgumentVectorPositiveIntegers",
            L"Dimension argument must be a vector of positive integers.");
    }
    ArrayOf dims(arg);
    dims.promoteType(NLS_DOUBLE);
    const double* values = static_cast<const double*>(dims.dataPointer());
    std::vector<indexType> packed;
    packed.reserve(static_cast<size_t>(dims.elementCount()));
    for (indexType k = 0; k < dims.elementCount(); ++k) {
        double value = values[k];
        if (!std::isfinite(value) || value < 1 || std::floor(value) != value) {
            Error(L"Nelson:data_structures:dimensionArgumentVectorPositiveIntegers",
                L"Dimension argument must be a vector of positive integers.");
        }
        indexType dim = static_cast<indexType>(value);
        if (std::find(packed.begin(), packed.end(), dim) != packed.end()) {
            Error(L"Nelson:data_structures:dimensionArgumentContainRepeatedValues",
                L"Dimension argument must not contain repeated values.");
        }
        packed.push_back(dim);
    }
    return packed;
}
//=============================================================================
bool
containsDimension(const std::vector<indexType>& dimensions, indexType dim)
{
    return std::find(dimensions.begin(), dimensions.end(), dim) != dimensions.end();
}
//=============================================================================
ArrayOf
indexRange(indexType first, indexType count)
{
    if (count == 0) {
        return ArrayOf::empty(Dimensions(1, 0));
    }
    return ArrayOf::integerRange(first, 1, first + count - 1, false);
}
//=============================================================================
ArrayOfVector
num2cellEachElement(const ArrayOf& input)
{
    indexType nbElements = input.elementCount();
    Dimensions dims = input.dimensions();
    auto* dp = new_with_exception<ArrayOf>(nbElements, false);
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, dp);
    ArrayOf param(input);
    if (param.isSparse()) {
        param.makeDense();
    }
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (ompIndexType k = 0; k < (ompIndexType)nbElements; k++) {
        dp[k] = param.valueAtIndex(k);
    }
    ArrayOfVector retval;
    retval << res;
    return retval;
}
//=============================================================================
ArrayOfVector
num2cellPackedDimensions(const ArrayOf& input, const ArrayOf& dimensionsArg)
{
    std::vector<indexType> packedDimensions = parsePackedDimensions(dimensionsArg);
    if (packedDimensions.empty()) {
        return num2cellEachElement(input);
    }

    Dimensions inputDims = input.dimensions();
    indexType maxPackedDimension
        = *std::max_element(packedDimensions.begin(), packedDimensions.end());
    indexType dimCount = std::max(inputDims.length(), maxPackedDimension);
    Dimensions cellDims(dimCount);
    for (indexType dim = 1; dim <= dimCount; ++dim) {
        indexType length = inputDims.dimensionLength(dim - 1);
        cellDims[dim - 1] = containsDimension(packedDimensions, dim) ? 1 : length;
    }

    indexType cellCount = cellDims.elementCount();
    auto* cells = new_with_exception<ArrayOf>(cellCount, false);
    ArrayOf work(input);
    for (indexType linearIndex = 0; linearIndex < cellCount; ++linearIndex) {
        indexType remainder = linearIndex;
        ArrayOfVector indexes;
        indexes.reserve(static_cast<size_t>(dimCount));
        for (indexType dim = 1; dim <= dimCount; ++dim) {
            indexType length = cellDims.dimensionLength(dim - 1);
            indexType subscript = (length == 0) ? 0 : remainder % length;
            if (length != 0) {
                remainder /= length;
            }
            indexType inputLength = inputDims.dimensionLength(dim - 1);
            if (containsDimension(packedDimensions, dim)) {
                indexes.push_back(indexRange(1, inputLength));
            } else {
                indexes.push_back(ArrayOf::integerRange(subscript + 1, 1, subscript + 1, false));
            }
        }
        cells[linearIndex] = work.nDimSubset(indexes);
    }

    ArrayOfVector retval;
    retval << ArrayOf(NLS_CELL_ARRAY, cellDims, cells);
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::num2cellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        return num2cellEachElement(argIn[0]);
    }
    return num2cellPackedDimensions(argIn[0], argIn[1]);
}
//=============================================================================
