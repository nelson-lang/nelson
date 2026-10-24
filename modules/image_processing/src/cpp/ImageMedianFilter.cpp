//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageMedianFilter.hpp"
#include "ImageProcessingCore.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class MedianPaddingKind
{
    Constant,
    Replicate,
    Symmetric
};
//=============================================================================
struct MedianPadding
{
    MedianPaddingKind kind { MedianPaddingKind::Constant };
    double constantValue { 0.0 };
};
//=============================================================================
static NelsonType
outputClassFor(const ArrayOf& image)
{
    switch (image.dataClass()) {
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_UINT32:
    case NLS_UINT64:
    case NLS_INT8:
    case NLS_INT16:
    case NLS_INT32:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_LOGICAL:
        return image.dataClass();
    default:
        return NLS_DOUBLE;
    }
}
//=============================================================================
static std::vector<double>
realDataAsDouble(const ArrayOf& value)
{
    ArrayOf work(value);
    work.promoteType(NLS_DOUBLE);
    const double* ptr = static_cast<const double*>(work.dataPointer());
    return std::vector<double>(ptr, ptr + work.elementCount());
}
//=============================================================================
static std::pair<indexType, indexType>
windowSize(const ArrayOf& window)
{
    if (!window.isNumeric() || window.isComplex() || window.isSparse()) {
        Error(L"Nelson:medfilt2:invalidNeighborhood", L"%s must contain positive integers.",
            L"Neighborhood size");
    }
    std::vector<double> values = realDataAsDouble(window);
    if (values.size() == 1) {
        values.push_back(values[0]);
    }
    if (values.size() != 2) {
        Error(L"Nelson:medfilt2:invalidNeighborhood", L"%s must contain positive integers.",
            L"Neighborhood size");
    }
    for (double value : values) {
        if (!std::isfinite(value) || value < 1 || value != std::floor(value)) {
            Error(L"Nelson:medfilt2:invalidNeighborhood", L"%s must contain positive integers.",
                L"Neighborhood size");
        }
    }
    return { static_cast<indexType>(values[0]), static_cast<indexType>(values[1]) };
}
//=============================================================================
static MedianPadding
paddingFor(const ArrayOf& image, const std::wstring& padding)
{
    if (padding == L"zeros") {
        return {};
    }
    if (padding == L"indexed") {
        MedianPadding options;
        options.constantValue = image.dataClass() == NLS_DOUBLE ? 1.0 : 0.0;
        return options;
    }
    if (padding == L"replicate") {
        return { MedianPaddingKind::Replicate, 0.0 };
    }
    if (padding == L"symmetric") {
        return { MedianPaddingKind::Symmetric, 0.0 };
    }
    Error(L"Nelson:image_processing:unsupportedPaddingOption", L"Unsupported \"%s\" option.",
        L"padding");
    return {};
}
//=============================================================================
static indexType
mapPadIndex(signedIndexType index, indexType length, MedianPaddingKind kind)
{
    if (kind == MedianPaddingKind::Replicate) {
        signedIndexType last = static_cast<signedIndexType>(length) - 1;
        return static_cast<indexType>(std::min(std::max(index, (signedIndexType)0), last));
    }
    if (length == 1) {
        return 0;
    }
    signedIndexType period = 2 * static_cast<signedIndexType>(length);
    signedIndexType value = index % period;
    if (value < 0) {
        value += period;
    }
    return static_cast<indexType>(
        value < static_cast<signedIndexType>(length) ? value : period - value - 1);
}
//=============================================================================
template <typename T>
static void
copyRealOutput(T* out, const std::vector<double>& values)
{
    for (size_t k = 0; k < values.size(); k++) {
        assignClampedPixel(out, k, values[k]);
    }
}
//=============================================================================
static void
copyOutput(ArrayOf& out, const std::vector<double>& values)
{
    switch (out.dataClass()) {
    case NLS_UINT8:
        copyRealOutput((uint8*)out.readWriteDataPointer(), values);
        break;
    case NLS_UINT16:
        copyRealOutput((uint16*)out.readWriteDataPointer(), values);
        break;
    case NLS_UINT32:
        copyRealOutput((uint32*)out.readWriteDataPointer(), values);
        break;
    case NLS_UINT64:
        copyRealOutput((uint64*)out.readWriteDataPointer(), values);
        break;
    case NLS_INT8:
        copyRealOutput((int8*)out.readWriteDataPointer(), values);
        break;
    case NLS_INT16:
        copyRealOutput((int16*)out.readWriteDataPointer(), values);
        break;
    case NLS_INT32:
        copyRealOutput((int32*)out.readWriteDataPointer(), values);
        break;
    case NLS_INT64:
        copyRealOutput((int64*)out.readWriteDataPointer(), values);
        break;
    case NLS_SINGLE:
        copyRealOutput((single*)out.readWriteDataPointer(), values);
        break;
    case NLS_LOGICAL:
        copyRealOutput((logical*)out.readWriteDataPointer(), values);
        break;
    default:
        std::copy(values.begin(), values.end(), (double*)out.readWriteDataPointer());
        break;
    }
}
//=============================================================================
static bool
medianLess(double lhs, double rhs)
{
    bool lhsNaN = std::isnan(lhs);
    bool rhsNaN = std::isnan(rhs);
    if (lhsNaN || rhsNaN) {
        return !lhsNaN && rhsNaN;
    }
    return lhs < rhs;
}
//=============================================================================
static std::vector<double>
medianFilterData(const std::vector<double>& input, indexType rows, indexType cols,
    indexType winRows, indexType winCols, const MedianPadding& padding)
{
    std::vector<double> output(input.size());
    std::vector<double> samples((size_t)(winRows * winCols));
    indexType padRows = winRows / 2;
    indexType padCols = winCols / 2;
    indexType medianIndex = (winRows * winCols - 1) / 2;
    for (indexType col = 0; col < cols; col++) {
        for (indexType row = 0; row < rows; row++) {
            size_t sampleIndex = 0;
            for (indexType wc = 0; wc < winCols; wc++) {
                for (indexType wr = 0; wr < winRows; wr++) {
                    signedIndexType sourceRow = static_cast<signedIndexType>(row)
                        + static_cast<signedIndexType>(wr) - static_cast<signedIndexType>(padRows);
                    signedIndexType sourceCol = static_cast<signedIndexType>(col)
                        + static_cast<signedIndexType>(wc) - static_cast<signedIndexType>(padCols);
                    double value = padding.constantValue;
                    if (sourceRow >= 0 && sourceRow < static_cast<signedIndexType>(rows)
                        && sourceCol >= 0 && sourceCol < static_cast<signedIndexType>(cols)) {
                        value = input[static_cast<indexType>(sourceRow)
                            + static_cast<indexType>(sourceCol) * rows];
                    } else if (padding.kind != MedianPaddingKind::Constant) {
                        indexType rr = mapPadIndex(sourceRow, rows, padding.kind);
                        indexType cc = mapPadIndex(sourceCol, cols, padding.kind);
                        value = input[rr + cc * rows];
                    }
                    samples[sampleIndex++] = value;
                }
            }
            std::nth_element(
                samples.begin(), samples.begin() + medianIndex, samples.end(), medianLess);
            output[row + col * rows] = samples[medianIndex];
        }
    }
    return output;
}
//=============================================================================
static uint16_t
histogramMedian(const std::array<uint16_t, 256>& histogram, indexType medianIndex)
{
    indexType cumulative = 0;
    for (indexType value = 0; value < histogram.size(); value++) {
        cumulative += histogram[value];
        if (cumulative > medianIndex) {
            return static_cast<uint16_t>(value);
        }
    }
    return 255;
}
//=============================================================================
template <typename T>
static double
sampleIntegerValue(const T* input, indexType rows, indexType cols, signedIndexType sourceRow,
    signedIndexType sourceCol, const MedianPadding& padding)
{
    if (sourceRow >= 0 && sourceRow < static_cast<signedIndexType>(rows) && sourceCol >= 0
        && sourceCol < static_cast<signedIndexType>(cols)) {
        return static_cast<double>(
            input[static_cast<indexType>(sourceRow) + static_cast<indexType>(sourceCol) * rows]);
    }
    if (padding.kind == MedianPaddingKind::Constant) {
        return padding.constantValue;
    }
    indexType rr = mapPadIndex(sourceRow, rows, padding.kind);
    indexType cc = mapPadIndex(sourceCol, cols, padding.kind);
    return static_cast<double>(input[rr + cc * rows]);
}
//=============================================================================
static bool
sampleLogicalValue(const logical* input, indexType rows, indexType cols, signedIndexType sourceRow,
    signedIndexType sourceCol, const MedianPadding& padding)
{
    if (sourceRow >= 0 && sourceRow < static_cast<signedIndexType>(rows) && sourceCol >= 0
        && sourceCol < static_cast<signedIndexType>(cols)) {
        return input[static_cast<indexType>(sourceRow) + static_cast<indexType>(sourceCol) * rows]
            != 0;
    }
    if (padding.kind == MedianPaddingKind::Constant) {
        return padding.constantValue != 0;
    }
    indexType rr = mapPadIndex(sourceRow, rows, padding.kind);
    indexType cc = mapPadIndex(sourceCol, cols, padding.kind);
    return input[rr + cc * rows] != 0;
}
//=============================================================================
static std::vector<double>
medianFilterLogicalData(const logical* input, indexType rows, indexType cols, indexType winRows,
    indexType winCols, const MedianPadding& padding)
{
    std::vector<double> output((size_t)(rows * cols));
    indexType padRows = winRows / 2;
    indexType padCols = winCols / 2;
    indexType medianIndex = (winRows * winCols - 1) / 2;
    for (indexType col = 0; col < cols; col++) {
        for (indexType row = 0; row < rows; row++) {
            indexType trueCount = 0;
            for (indexType wc = 0; wc < winCols; wc++) {
                for (indexType wr = 0; wr < winRows; wr++) {
                    signedIndexType sourceRow = static_cast<signedIndexType>(row)
                        + static_cast<signedIndexType>(wr) - static_cast<signedIndexType>(padRows);
                    signedIndexType sourceCol = static_cast<signedIndexType>(col)
                        + static_cast<signedIndexType>(wc) - static_cast<signedIndexType>(padCols);
                    if (sampleLogicalValue(input, rows, cols, sourceRow, sourceCol, padding)) {
                        trueCount++;
                    }
                }
            }
            output[row + col * rows] = trueCount > medianIndex ? 1.0 : 0.0;
        }
    }
    return output;
}
//=============================================================================
template <typename T>
static std::vector<double>
medianFilterHistogramData(const T* input, indexType rows, indexType cols, indexType winRows,
    indexType winCols, const MedianPadding& padding)
{
    std::vector<double> output((size_t)(rows * cols));
    indexType padRows = winRows / 2;
    indexType padCols = winCols / 2;
    indexType medianIndex = (winRows * winCols - 1) / 2;
    for (indexType col = 0; col < cols; col++) {
        for (indexType row = 0; row < rows; row++) {
            std::array<uint16_t, 256> histogram {};
            for (indexType wc = 0; wc < winCols; wc++) {
                for (indexType wr = 0; wr < winRows; wr++) {
                    signedIndexType sourceRow = static_cast<signedIndexType>(row)
                        + static_cast<signedIndexType>(wr) - static_cast<signedIndexType>(padRows);
                    signedIndexType sourceCol = static_cast<signedIndexType>(col)
                        + static_cast<signedIndexType>(wc) - static_cast<signedIndexType>(padCols);
                    double sample
                        = sampleIntegerValue(input, rows, cols, sourceRow, sourceCol, padding);
                    int bucket = static_cast<int>(std::round(sample));
                    bucket = std::clamp(bucket, 0, 255);
                    histogram[(size_t)bucket]++;
                }
            }
            output[row + col * rows] = static_cast<double>(histogramMedian(histogram, medianIndex));
        }
    }
    return output;
}
//=============================================================================
ArrayOf
ImageMedianFilter(const ArrayOf& image, const ArrayOf& window, const std::wstring& padding)
{
    ImageShape2D shape = validateImage2DOrRGB(image, false,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical."),
            L"Image"),
        formatI18nMessage(Nelson::resolveMessageTemplate(
                              L"Nelson:image_processing:imageDArray", L"%s must be a 2-D array."),
            L"Image"),
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported."),
            L"Sparse images"));
    if (image.isComplex()) {
        Error(L"Nelson:image_processing:complexNotSupported", L"%s are not supported.",
            L"Complex images");
    }
    auto [winRows, winCols] = windowSize(window);
    MedianPadding paddingOptions = paddingFor(image, padding);
    Dimensions outDims(shape.rows, shape.columns);
    NelsonType outClass = outputClassFor(image);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    if (image.isEmpty()) {
        return out;
    }
    std::vector<double> values;
    if (image.dataClass() == NLS_UINT8) {
        values = medianFilterHistogramData((const uint8*)image.dataPointer(), shape.rows,
            shape.columns, winRows, winCols, paddingOptions);
    } else if (image.dataClass() == NLS_LOGICAL) {
        values = medianFilterLogicalData((const logical*)image.dataPointer(), shape.rows,
            shape.columns, winRows, winCols, paddingOptions);
    } else {
        std::vector<double> input = realDataAsDouble(image);
        values
            = medianFilterData(input, shape.rows, shape.columns, winRows, winCols, paddingOptions);
    }
    copyOutput(out, values);
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
