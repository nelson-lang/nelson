//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ActiveContour.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <cmath>
#include <cwctype>
#include <limits>
#include <queue>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
lowerText(std::wstring text)
{
    std::transform(text.begin(), text.end(), text.begin(), ::towlower);
    return text;
}
//=============================================================================
static bool
isEdgeMethod(const std::wstring& method)
{
    std::wstring lower = lowerText(method);
    if (lower == L"edge") {
        return true;
    }
    if (lower == L"chan-vese") {
        return false;
    }
    Error(L"Nelson:image_processing:unsupportedActiveContourMethod", L"Unsupported \"%s\" option.",
        L"active contour method");
    return false;
}
//=============================================================================
static void
validateInputs(const ArrayOf& image, const ArrayOf& mask)
{
    if (!image.isNumeric() && !image.isLogical()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Image");
    }
    if (image.isSparse()) {
        Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
            L"Sparse images");
    }
    if (image.dimensions().length() > 2) {
        Error(L"Nelson:image_processing:imageDArray", L"%s must be a 2-D array.", L"Image");
    }
    if (image.isComplex()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"Image");
    }
    if (!mask.isLogical() || mask.isSparse() || !mask.dimensions().equals(image.dimensions())) {
        Error(L"Nelson:image_processing:initialMaskSameSizeImage",
            L"%s must have the same size as %s.", L"Initial mask", L"the image");
    }
}
//=============================================================================
template <typename T>
static std::vector<double>
scaledRealData(const ArrayOf& image, double offset, double scale)
{
    const auto* ptr = static_cast<const T*>(image.dataPointer());
    std::vector<double> values(image.elementCount());
    for (indexType k = 0; k < image.elementCount(); k++) {
        values[k] = (static_cast<double>(ptr[k]) + offset) / scale;
    }
    return values;
}
//=============================================================================
static std::vector<double>
imageAsUnitDouble(const ArrayOf& image)
{
    switch (image.dataClass()) {
    case NLS_LOGICAL:
        return scaledRealData<logical>(image, 0.0, 1.0);
    case NLS_DOUBLE:
        return scaledRealData<double>(image, 0.0, 1.0);
    case NLS_SINGLE:
        return scaledRealData<single>(image, 0.0, 1.0);
    case NLS_UINT8:
        return scaledRealData<uint8>(image, 0.0, 255.0);
    case NLS_UINT16:
        return scaledRealData<uint16>(image, 0.0, 65535.0);
    case NLS_UINT32:
        return scaledRealData<uint32>(image, 0.0, 4294967295.0);
    case NLS_UINT64:
        return scaledRealData<uint64>(image, 0.0, 18446744073709551615.0);
    case NLS_INT8:
        return scaledRealData<int8>(image, 128.0, 255.0);
    case NLS_INT16:
        return scaledRealData<int16>(image, 32768.0, 65535.0);
    case NLS_INT32:
        return scaledRealData<int32>(image, 2147483648.0, 4294967295.0);
    case NLS_INT64:
        return scaledRealData<int64>(image, 9223372036854775808.0, 18446744073709551615.0);
    default: {
        ArrayOf work(image);
        work.promoteType(NLS_DOUBLE);
        const auto* ptr = static_cast<const double*>(work.dataPointer());
        return std::vector<double>(ptr, ptr + work.elementCount());
    }
    }
}
//=============================================================================
static void
validateFinite(const std::vector<double>& values)
{
    for (double value : values) {
        if (!std::isfinite(value)) {
            Error(L"Nelson:image_processing:imageValuesFinite", L"%s values must be finite.",
                L"Image");
        }
    }
}
//=============================================================================
static std::vector<uint8_t>
maskAsBytes(const ArrayOf& mask)
{
    const auto* ptr = static_cast<const logical*>(mask.dataPointer());
    std::vector<uint8_t> values(mask.elementCount());
    for (indexType k = 0; k < mask.elementCount(); k++) {
        values[k] = ptr[k] ? 1 : 0;
    }
    return values;
}
//=============================================================================
static indexType
clampIndex(signedIndexType value, indexType limit)
{
    if (value < 0) {
        return 0;
    }
    if (value >= static_cast<signedIndexType>(limit)) {
        return limit - 1;
    }
    return static_cast<indexType>(value);
}
//=============================================================================
static std::vector<double>
edgeWeights(const std::vector<double>& image, indexType rows, indexType cols)
{
    std::vector<double> weights(image.size(), 1.0);
    OMP_PARALLEL_FOR_LOOP(cols, 4)
    for (signedIndexType colSigned = 0; colSigned < static_cast<signedIndexType>(cols);
        colSigned++) {
        indexType col = static_cast<indexType>(colSigned);
        indexType left = clampIndex(static_cast<signedIndexType>(col) - 1, cols);
        indexType right = clampIndex(static_cast<signedIndexType>(col) + 1, cols);
        for (indexType row = 0; row < rows; row++) {
            indexType up = clampIndex(static_cast<signedIndexType>(row) - 1, rows);
            indexType down = clampIndex(static_cast<signedIndexType>(row) + 1, rows);
            indexType idx = row + col * rows;
            double gx = (image[row + right * rows] - image[row + left * rows]) * 0.5;
            double gy = (image[down + col * rows] - image[up + col * rows]) * 0.5;
            weights[idx] = 1.0 / (1.0 + gx * gx + gy * gy);
        }
    }
    return weights;
}
//=============================================================================
static void
maskMeans(const std::vector<double>& image, const std::vector<uint8_t>& mask, double& insideMean,
    double& outsideMean, bool& valid)
{
    double insideSum = 0;
    double outsideSum = 0;
    indexType insideCount = 0;
    indexType outsideCount = 0;
    for (indexType k = 0; k < image.size(); k++) {
        if (mask[k]) {
            insideSum += image[k];
            insideCount++;
        } else {
            outsideSum += image[k];
            outsideCount++;
        }
    }
    valid = insideCount > 0 && outsideCount > 0;
    if (valid) {
        insideMean = insideSum / static_cast<double>(insideCount);
        outsideMean = outsideSum / static_cast<double>(outsideCount);
    }
}
//=============================================================================
static void
makeBand(
    const std::vector<uint8_t>& mask, std::vector<uint8_t>& band, indexType rows, indexType cols)
{
    OMP_PARALLEL_FOR_LOOP(cols, 4)
    for (signedIndexType colSigned = 0; colSigned < static_cast<signedIndexType>(cols);
        colSigned++) {
        indexType col = static_cast<indexType>(colSigned);
        for (indexType row = 0; row < rows; row++) {
            bool any = false;
            bool all = true;
            for (int dc = -1; dc <= 1; dc++) {
                for (int dr = -1; dr <= 1; dr++) {
                    indexType rr = clampIndex(static_cast<signedIndexType>(row) + dr, rows);
                    indexType cc = clampIndex(static_cast<signedIndexType>(col) + dc, cols);
                    bool v = mask[rr + cc * rows] != 0;
                    any = any || v;
                    all = all && v;
                }
            }
            band[row + col * rows] = any && !all;
        }
    }
}
//=============================================================================
static void
applyCandidate(const std::vector<double>& image, std::vector<uint8_t>& mask,
    const std::vector<uint8_t>& band, double insideMean, double outsideMean, double contractionBias)
{
    OMP_PARALLEL_FOR_LOOP(image.size(), 4)
    for (signedIndexType kSigned = 0; kSigned < static_cast<signedIndexType>(image.size());
        kSigned++) {
        indexType k = static_cast<indexType>(kSigned);
        if (!band[k]) {
            continue;
        }
        double insideCost = image[k] - insideMean;
        double outsideCost = image[k] - outsideMean;
        double score = insideCost * insideCost - outsideCost * outsideCost + contractionBias;
        mask[k] = (score <= 0.0) ? 1 : 0;
    }
}
//=============================================================================
static int
smoothingVoteThreshold(bool edge, double smoothFactor)
{
    int baseThreshold = edge ? 5 : 4;
    int extra = static_cast<int>(std::round(smoothFactor)) - 1;
    return std::clamp(baseThreshold + extra, 1, 8);
}
//=============================================================================
static void
smoothBand(std::vector<uint8_t>& mask, const std::vector<uint8_t>& band,
    const std::vector<double>& weights, bool edge, double smoothFactor, indexType rows,
    indexType cols)
{
    std::vector<uint8_t> smoothed(mask.size(), 0);
    int voteThreshold = smoothingVoteThreshold(edge, smoothFactor);
    OMP_PARALLEL_FOR_LOOP(cols, 4)
    for (signedIndexType colSigned = 0; colSigned < static_cast<signedIndexType>(cols);
        colSigned++) {
        indexType col = static_cast<indexType>(colSigned);
        for (indexType row = 0; row < rows; row++) {
            int votes = 0;
            for (int dc = -1; dc <= 1; dc++) {
                for (int dr = -1; dr <= 1; dr++) {
                    indexType rr = clampIndex(static_cast<signedIndexType>(row) + dr, rows);
                    indexType cc = clampIndex(static_cast<signedIndexType>(col) + dc, cols);
                    votes += mask[rr + cc * rows] ? 1 : 0;
                }
            }
            smoothed[row + col * rows] = votes >= voteThreshold;
        }
    }
    for (indexType k = 0; k < mask.size(); k++) {
        if (band[k] && (!edge || weights[k] > 0.5)) {
            mask[k] = smoothed[k];
        }
    }
}
//=============================================================================
static void
fillHoles(std::vector<uint8_t>& mask, indexType rows, indexType cols)
{
    std::vector<uint8_t> visited(mask.size(), 0);
    std::queue<indexType> pending;
    auto enqueue = [&](indexType idx) {
        if (!mask[idx] && !visited[idx]) {
            visited[idx] = 1;
            pending.push(idx);
        }
    };
    for (indexType col = 0; col < cols; col++) {
        enqueue(col * rows);
        enqueue(rows - 1 + col * rows);
    }
    for (indexType row = 0; row < rows; row++) {
        enqueue(row);
        enqueue(row + (cols - 1) * rows);
    }
    while (!pending.empty()) {
        indexType current = pending.front();
        pending.pop();
        indexType row = current % rows;
        indexType col = current / rows;
        if (row > 0) {
            enqueue(row - 1 + col * rows);
        }
        if (row + 1 < rows) {
            enqueue(row + 1 + col * rows);
        }
        if (col > 0) {
            enqueue(row + (col - 1) * rows);
        }
        if (col + 1 < cols) {
            enqueue(row + (col + 1) * rows);
        }
    }
    for (indexType k = 0; k < mask.size(); k++) {
        if (!mask[k] && !visited[k]) {
            mask[k] = 1;
        }
    }
}
//=============================================================================
ArrayOf
ActiveContour(const ArrayOf& image, const ArrayOf& mask, double iterations,
    const std::wstring& method, double smoothFactor, double contractionBias)
{
    validateInputs(image, mask);
    bool edge = isEdgeMethod(method);
    indexType total = image.elementCount();
    ArrayOf out(NLS_LOGICAL, image.dimensions(), ArrayOf::allocateArrayOf(NLS_LOGICAL, total));
    std::vector<uint8_t> current = maskAsBytes(mask);
    if (iterations == 0 || image.isEmpty()) {
        std::copy(
            current.begin(), current.end(), static_cast<logical*>(out.readWriteDataPointer()));
        return out;
    }
    std::vector<double> values = imageAsUnitDouble(image);
    validateFinite(values);
    indexType rows = image.rows();
    indexType cols = image.columns();
    std::vector<double> weights
        = edge ? edgeWeights(values, rows, cols) : std::vector<double>(total, 1.0);
    std::vector<uint8_t> previous(total);
    std::vector<uint8_t> band(total);
    indexType maxIterations = static_cast<indexType>(iterations);
    for (indexType iter = 0; iter < maxIterations; iter++) {
        previous = current;
        double insideMean = 0;
        double outsideMean = 0;
        bool valid = false;
        maskMeans(values, current, insideMean, outsideMean, valid);
        if (!valid) {
            break;
        }
        makeBand(current, band, rows, cols);
        applyCandidate(values, current, band, insideMean, outsideMean, contractionBias);
        if (smoothFactor > 0.0) {
            smoothBand(current, band, weights, edge, smoothFactor, rows, cols);
        }
        fillHoles(current, rows, cols);
        if (current == previous) {
            break;
        }
    }
    std::copy(current.begin(), current.end(), static_cast<logical*>(out.readWriteDataPointer()));
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
