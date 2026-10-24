//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Watershed.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <queue>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct WatershedPixelPriority
{
    indexType index;
    double value;
};
//=============================================================================
struct WatershedPixelCompare
{
    bool
    operator()(const WatershedPixelPriority& a, const WatershedPixelPriority& b) const
    {
        if (a.value == b.value) {
            return a.index > b.index;
        }
        return a.value > b.value;
    }
};
//=============================================================================
static void
validateImage(const ArrayOf& image)
{
    if (!image.isNumeric() && !image.isLogical()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Image");
    }
    if (image.isSparse()) {
        Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
            L"Sparse images");
    }
    if (image.dimensions().length() > 3) {
        Error(L"Nelson:image_processing:imageDDArray", L"%s must be a 2-D or 3-D array.", L"Image");
    }
    if (image.isComplex()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"Image");
    }
}
//=============================================================================
static int
validateConnectivity(double connectivity, bool isVolume)
{
    if (!std::isfinite(connectivity)) {
        Error(L"Nelson:image_processing:unsupportedConnectivity", L"Unsupported connectivity.");
    }
    if (!isVolume && (connectivity == 4 || connectivity == 8)) {
        return static_cast<int>(connectivity);
    }
    if (isVolume && (connectivity == 6 || connectivity == 18 || connectivity == 26)) {
        return static_cast<int>(connectivity);
    }
    if ((!isVolume && (connectivity == 6 || connectivity == 18 || connectivity == 26))
        || (isVolume && (connectivity == 4 || connectivity == 8))) {
        Error(L"Nelson:image_processing:unsupportedConnectivity", L"Unsupported connectivity.");
    }
    Error(L"Nelson:image_processing:unsupportedConnectivity", L"Unsupported connectivity.");
    return 0;
}
//=============================================================================
static std::vector<double>
finiteImageAsDouble(const ArrayOf& image)
{
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
static std::vector<std::array<int, 3>>
connectivityOffsets(int connectivity, bool isVolume)
{
    std::vector<std::array<int, 3>> offsets;
    for (int dp = isVolume ? -1 : 0; dp <= (isVolume ? 1 : 0); dp++) {
        for (int dc = -1; dc <= 1; dc++) {
            for (int dr = -1; dr <= 1; dr++) {
                if (dr == 0 && dc == 0 && dp == 0) {
                    continue;
                }
                int manhattan = std::abs(dr) + std::abs(dc) + std::abs(dp);
                if (!isVolume && connectivity == 4 && manhattan != 1) {
                    continue;
                }
                if (isVolume && connectivity == 6 && manhattan != 1) {
                    continue;
                }
                if (isVolume && connectivity == 18 && manhattan > 2) {
                    continue;
                }
                offsets.push_back({ dr, dc, dp });
            }
        }
    }
    return offsets;
}
//=============================================================================
static void
appendUniqueLabel(std::vector<double>& labels, double label)
{
    if (label <= 0) {
        return;
    }
    if (std::find(labels.begin(), labels.end(), label) == labels.end()) {
        labels.push_back(label);
    }
}
//=============================================================================
static bool
inBounds(signedIndexType row, signedIndexType col, signedIndexType plane, indexType rows,
    indexType cols, indexType planes)
{
    return row >= 0 && col >= 0 && row < static_cast<signedIndexType>(rows)
        && col < static_cast<signedIndexType>(cols) && plane >= 0
        && plane < static_cast<signedIndexType>(planes);
}
//=============================================================================
static void
collectPlateau(indexType start, const std::vector<double>& values, std::vector<uint8_t>& inLevel,
    std::vector<uint8_t>& visitedLevel, const std::vector<uint8_t>& processed,
    const std::vector<double>& labels, indexType rows, indexType cols, indexType planes,
    const std::vector<std::array<int, 3>>& offsets, std::vector<indexType>& members,
    std::vector<double>& neighborLabels)
{
    std::vector<indexType> pending;
    pending.push_back(start);
    visitedLevel[start] = 1;
    double level = values[start];
    for (size_t cursor = 0; cursor < pending.size(); cursor++) {
        indexType current = pending[cursor];
        members.push_back(current);
        indexType planeSize = rows * cols;
        indexType plane = current / planeSize;
        indexType inPlane = current - plane * planeSize;
        indexType row = inPlane % rows;
        indexType col = inPlane / rows;
        for (const auto& offset : offsets) {
            signedIndexType rr = static_cast<signedIndexType>(row) + offset[0];
            signedIndexType cc = static_cast<signedIndexType>(col) + offset[1];
            signedIndexType pp = static_cast<signedIndexType>(plane) + offset[2];
            if (!inBounds(rr, cc, pp, rows, cols, planes)) {
                continue;
            }
            indexType next = static_cast<indexType>(rr) + static_cast<indexType>(cc) * rows
                + static_cast<indexType>(pp) * rows * cols;
            if (inLevel[next] && !visitedLevel[next] && values[next] == level) {
                visitedLevel[next] = 1;
                pending.push_back(next);
            } else if (!inLevel[next] && processed[next]) {
                appendUniqueLabel(neighborLabels, labels[next]);
            }
        }
    }
}
//=============================================================================
static double
labelForPlateau(const std::vector<double>& neighborLabels, double& nextLabel)
{
    if (neighborLabels.empty()) {
        nextLabel++;
        return nextLabel;
    }
    if (neighborLabels.size() == 1) {
        return neighborLabels[0];
    }
    return 0;
}
//=============================================================================
static void
processLevel(const std::vector<indexType>& order, indexType begin, indexType end,
    const std::vector<double>& values, std::vector<uint8_t>& inLevel,
    std::vector<uint8_t>& visitedLevel, std::vector<uint8_t>& processed,
    std::vector<double>& labels, indexType rows, indexType cols, indexType planes,
    const std::vector<std::array<int, 3>>& offsets, double& nextLabel)
{
    for (indexType k = begin; k < end; k++) {
        inLevel[order[k]] = 1;
    }
    for (indexType k = begin; k < end; k++) {
        indexType start = order[k];
        if (visitedLevel[start]) {
            continue;
        }
        std::vector<indexType> members;
        std::vector<double> neighborLabels;
        collectPlateau(start, values, inLevel, visitedLevel, processed, labels, rows, cols, planes,
            offsets, members, neighborLabels);
        double label = labelForPlateau(neighborLabels, nextLabel);
        for (indexType member : members) {
            labels[member] = label;
        }
    }
    for (indexType k = begin; k < end; k++) {
        indexType idx = order[k];
        processed[idx] = 1;
        inLevel[idx] = 0;
        visitedLevel[idx] = 0;
    }
}
//=============================================================================
static std::vector<indexType>
priorityOrder(const std::vector<double>& values)
{
    std::priority_queue<WatershedPixelPriority, std::vector<WatershedPixelPriority>,
        WatershedPixelCompare>
        queue;
    for (indexType k = 0; k < values.size(); k++) {
        queue.push({ k, values[k] });
    }
    std::vector<indexType> order;
    order.reserve(values.size());
    while (!queue.empty()) {
        order.push_back(queue.top().index);
        queue.pop();
    }
    return order;
}
//=============================================================================
ArrayOf
Watershed(const ArrayOf& image, double connectivityValue)
{
    validateImage(image);
    Dimensions dims = image.dimensions();
    bool isVolume = dims.length() == 3;
    int connectivity = validateConnectivity(connectivityValue, isVolume);
    indexType total = image.elementCount();
    ArrayOf out(NLS_DOUBLE, dims, ArrayOf::allocateArrayOf(NLS_DOUBLE, total));
    auto* outPtr = static_cast<double*>(out.readWriteDataPointer());
    if (image.isEmpty()) {
        return out;
    }
    indexType rows = image.rows();
    indexType cols = image.columns();
    indexType planes = isVolume ? dims.dimensionLength(2) : 1;
    std::vector<double> values = finiteImageAsDouble(image);
    std::vector<indexType> order = priorityOrder(values);
    std::vector<double> labels(total, 0);
    std::vector<uint8_t> processed(total, 0);
    std::vector<uint8_t> inLevel(total, 0);
    std::vector<uint8_t> visitedLevel(total, 0);
    std::vector<std::array<int, 3>> offsets = connectivityOffsets(connectivity, isVolume);
    double nextLabel = 0;
    for (indexType begin = 0; begin < total;) {
        indexType end = begin + 1;
        while (end < total && values[order[end]] == values[order[begin]]) {
            end++;
        }
        processLevel(order, begin, end, values, inLevel, visitedLevel, processed, labels, rows,
            cols, planes, offsets, nextLabel);
        begin = end;
    }
    std::copy(labels.begin(), labels.end(), outPtr);
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
