//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RegionalMinima.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <array>
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static int
validateConnectivity(double connectivity)
{
    if (!std::isfinite(connectivity) || (connectivity != 4 && connectivity != 8)) {
        Error(L"Nelson:image_processing:unsupportedConnectivity", L"Unsupported connectivity.");
    }
    return static_cast<int>(connectivity);
}
//=============================================================================
static std::vector<double>
finiteImageAsDouble(const ArrayOf& image)
{
    if (!image.isNumeric() && !image.isLogical()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Image");
    }
    if (image.isSparse()) {
        Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
            L"Sparse images");
    }
    if (image.isComplex()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"Image");
    }
    ArrayOf work(image);
    work.promoteType(NLS_DOUBLE);
    const auto* ptr = static_cast<const double*>(work.dataPointer());
    std::vector<double> values(ptr, ptr + work.elementCount());
    for (double value : values) {
        if (!std::isfinite(value)) {
            Error(L"Nelson:image_processing:imageValuesFinite", L"%s values must be finite.",
                L"Image");
        }
    }
    return values;
}
//=============================================================================
static std::vector<std::array<int, 2>>
connectivityOffsets(int connectivity)
{
    std::vector<std::array<int, 2>> offsets;
    for (int dc = -1; dc <= 1; dc++) {
        for (int dr = -1; dr <= 1; dr++) {
            if (dr == 0 && dc == 0) {
                continue;
            }
            if (connectivity == 4 && std::abs(dr) + std::abs(dc) != 1) {
                continue;
            }
            offsets.push_back({ dr, dc });
        }
    }
    return offsets;
}
//=============================================================================
static bool
inBounds(signedIndexType row, signedIndexType col, indexType rows, indexType cols)
{
    return row >= 0 && col >= 0 && row < static_cast<signedIndexType>(rows)
        && col < static_cast<signedIndexType>(cols);
}
//=============================================================================
static indexType
collectMinimumPlateau(indexType start, const std::vector<double>& values,
    std::vector<uint8_t>& visited, const std::vector<std::array<int, 2>>& offsets, indexType rows,
    indexType cols, std::vector<indexType>& queue, std::vector<indexType>& plateau, bool& isMinimum)
{
    double target = values[start];
    indexType head = 0;
    indexType tail = 1;
    indexType plateauCount = 1;
    queue[0] = start;
    plateau[0] = start;
    visited[start] = 1;
    isMinimum = true;
    while (head < tail) {
        indexType idx = queue[head++];
        indexType row = idx % rows;
        indexType col = idx / rows;
        for (const auto& offset : offsets) {
            signedIndexType rr = static_cast<signedIndexType>(row) + offset[0];
            signedIndexType cc = static_cast<signedIndexType>(col) + offset[1];
            if (!inBounds(rr, cc, rows, cols)) {
                continue;
            }
            indexType next = static_cast<indexType>(rr) + static_cast<indexType>(cc) * rows;
            double neighbor = values[next];
            if (neighbor < target) {
                isMinimum = false;
            } else if (neighbor == target && !visited[next]) {
                visited[next] = 1;
                queue[tail++] = next;
                plateau[plateauCount++] = next;
            }
        }
    }
    return plateauCount;
}
//=============================================================================
ArrayOf
RegionalMinima2D(const ArrayOf& image, double connectivity)
{
    int conn = validateConnectivity(connectivity);
    Dimensions dims = image.dimensions();
    indexType rows = image.rows();
    indexType cols = image.columns();
    indexType total = rows * cols;
    auto* output
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, total, stringVector(), true));
    ArrayOf out(NLS_LOGICAL, dims, output);
    if (total == 0) {
        return out;
    }
    std::vector<double> values = finiteImageAsDouble(image);
    std::vector<uint8_t> visited(total, 0);
    std::vector<indexType> queue(total);
    std::vector<indexType> plateau(total);
    std::vector<std::array<int, 2>> offsets = connectivityOffsets(conn);
    for (indexType idx = 0; idx < total; idx++) {
        if (visited[idx]) {
            continue;
        }
        bool isMinimum = false;
        indexType plateauCount = collectMinimumPlateau(
            idx, values, visited, offsets, rows, cols, queue, plateau, isMinimum);
        if (isMinimum) {
            for (indexType k = 0; k < plateauCount; k++) {
                output[plateau[k]] = true;
            }
        }
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
