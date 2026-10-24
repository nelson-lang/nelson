//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include <numeric>
#include "mat2cellBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
std::vector<indexType>
parseSizes(const ArrayOf& arg, indexType expected)
{
    if (arg.isEmpty() && arg.isNumeric() && expected == 0) {
        // no block along an empty dimension
        return {};
    }
    if (!arg.isNumeric() || arg.isComplex() || !arg.isVector()) {
        Error(L"Nelson:data_structures:sizeArgumentsVectorsNonnegativeIntegers",
            L"Size arguments must be vectors of nonnegative integers.");
    }
    ArrayOf sizes(arg);
    sizes.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(sizes.dataPointer());
    std::vector<indexType> result(static_cast<size_t>(sizes.elementCount()));
    indexType total = 0;
    for (indexType k = 0; k < sizes.elementCount(); ++k) {
        double value = ptr[k];
        if (!std::isfinite(value) || value < 0 || std::floor(value) != value) {
            Error(L"Nelson:data_structures:sizeArgumentsVectorsNonnegativeIntegers",
                L"Size arguments must be vectors of nonnegative integers.");
        }
        result[static_cast<size_t>(k)] = static_cast<indexType>(value);
        total += static_cast<indexType>(value);
    }
    if (total != expected) {
        Error(L"Nelson:data_structures:sizeArgumentsSumInputSize",
            L"Size arguments must sum to the input size.");
    }
    return result;
}
//=============================================================================
ArrayOf
rangeArray(indexType first, indexType count)
{
    if (count == 0) {
        return ArrayOf::empty(Dimensions(1, 0));
    }
    return ArrayOf::integerRange(first, 1, first + count - 1, false);
}
//=============================================================================
ArrayOfVector
mat2cellDouble2D(const ArrayOf& input, const std::vector<std::vector<indexType>>& blockSizes,
    Dimensions cellDims)
{
    indexType inputRows = input.dimensions().dimensionLength(0);
    const double* source = static_cast<const double*>(input.dataPointer());
    indexType nCells = cellDims.elementCount();
    auto* cells = new_with_exception<ArrayOf>(nCells, false);
    indexType colStart = 0;
    const auto& rowSizes = blockSizes[0];
    const auto& colSizes = blockSizes[1];
    for (indexType colBlock = 0; colBlock < static_cast<indexType>(colSizes.size()); ++colBlock) {
        indexType rowStart = 0;
        indexType colCount = colSizes[static_cast<size_t>(colBlock)];
        for (indexType rowBlock = 0; rowBlock < static_cast<indexType>(rowSizes.size());
            ++rowBlock) {
            indexType rowCount = rowSizes[static_cast<size_t>(rowBlock)];
            indexType elementCount = rowCount * colCount;
            Dimensions dims(rowCount, colCount);
            if (elementCount == 0) {
                cells[rowBlock + colBlock * static_cast<indexType>(rowSizes.size())]
                    = ArrayOf::empty(dims);
            } else {
                double* destination
                    = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, elementCount));
                for (indexType col = 0; col < colCount; ++col) {
                    const double* srcColumn = source + (colStart + col) * inputRows + rowStart;
                    double* dstColumn = destination + col * rowCount;
                    std::memcpy(dstColumn, srcColumn, sizeof(double) * rowCount);
                }
                cells[rowBlock + colBlock * static_cast<indexType>(rowSizes.size())]
                    = ArrayOf(NLS_DOUBLE, dims, destination, false);
            }
            rowStart += rowCount;
        }
        colStart += colCount;
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_CELL_ARRAY, cellDims, cells);
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::DataStructuresGateway::mat2cellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf input(argIn[0]);
    Dimensions inputDims = input.dimensions();
    indexType nDims = std::max(static_cast<indexType>(argIn.size() - 1), inputDims.length());
    std::vector<std::vector<indexType>> blockSizes(static_cast<size_t>(nDims));
    Dimensions cellDims(nDims);
    for (indexType d = 0; d < nDims; ++d) {
        indexType dimLength = inputDims.dimensionLength(d);
        if (d + 1 < static_cast<indexType>(argIn.size())) {
            blockSizes[static_cast<size_t>(d)] = parseSizes(argIn[d + 1], dimLength);
        } else {
            blockSizes[static_cast<size_t>(d)] = { dimLength };
        }
        cellDims[d] = static_cast<indexType>(blockSizes[static_cast<size_t>(d)].size());
    }
    if (nDims == 2 && input.isDoubleType(true) && !input.isSparse() && !input.isComplex()) {
        return mat2cellDouble2D(input, blockSizes, cellDims);
    }
    indexType nCells = cellDims.elementCount();
    auto* cells = new_with_exception<ArrayOf>(nCells, false);
    std::vector<indexType> starts(static_cast<size_t>(nDims), 1);
    for (indexType linear = 0; linear < nCells; ++linear) {
        indexType remaining = linear;
        ArrayOfVector indexes;
        indexes.reserve(static_cast<size_t>(nDims));
        for (indexType d = 0; d < nDims; ++d) {
            const auto& sizes = blockSizes[static_cast<size_t>(d)];
            indexType sub = remaining % static_cast<indexType>(sizes.size());
            remaining /= static_cast<indexType>(sizes.size());
            indexType first = 1;
            for (indexType k = 0; k < sub; ++k) {
                first += sizes[static_cast<size_t>(k)];
            }
            indexes.push_back(rangeArray(first, sizes[static_cast<size_t>(sub)]));
        }
        cells[linear] = input.nDimSubset(indexes);
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_CELL_ARRAY, cellDims, cells);
    return retval;
}
//=============================================================================
