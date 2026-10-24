//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageWarp.hpp"
#include "ImageProcessingCore.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <cmath>
#include <complex>
#include <vector>
//=============================================================================
namespace Nelson {
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
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
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
static std::vector<std::complex<double>>
dataAsComplexDouble(const ArrayOf& value)
{
    ArrayOf work(value);
    work.promoteType(NLS_DCOMPLEX);
    const auto* ptr = reinterpret_cast<const std::complex<double>*>(work.dataPointer());
    return std::vector<std::complex<double>>(ptr, ptr + work.elementCount());
}
//=============================================================================
template <typename T>
static T
samplePixel(const std::vector<T>& input, indexType rows, indexType cols, indexType channel,
    signedIndexType row, signedIndexType col, const std::vector<double>& fillValues)
{
    if (row < 0 || row >= static_cast<signedIndexType>(rows) || col < 0
        || col >= static_cast<signedIndexType>(cols)) {
        return (T)fillValues[channel];
    }
    return input[static_cast<indexType>(row) + static_cast<indexType>(col) * rows
        + channel * rows * cols];
}
//=============================================================================
template <typename T>
static T
nearestValue(const std::vector<T>& input, indexType rows, indexType cols, indexType channel,
    double x, double y, const std::vector<double>& fillValues)
{
    if (x < 1.0 || x > static_cast<double>(cols) || y < 1.0 || y > static_cast<double>(rows)) {
        return (T)fillValues[channel];
    }
    indexType col
        = static_cast<indexType>(std::clamp(std::round(x), 1.0, static_cast<double>(cols)) - 1.0);
    indexType row
        = static_cast<indexType>(std::clamp(std::round(y), 1.0, static_cast<double>(rows)) - 1.0);
    return input[row + col * rows + channel * rows * cols];
}
//=============================================================================
template <typename T>
static T
linearValue(const std::vector<T>& input, indexType rows, indexType cols, indexType channel,
    double x, double y, const std::vector<double>& fillValues)
{
    if (x < 1.0 || x > static_cast<double>(cols) || y < 1.0 || y > static_cast<double>(rows)) {
        return (T)fillValues[channel];
    }
    double x0d = std::floor(x);
    double y0d = std::floor(y);
    double x1d = std::min(x0d + 1.0, static_cast<double>(cols));
    double y1d = std::min(y0d + 1.0, static_cast<double>(rows));
    x0d = std::max(x0d, 1.0);
    y0d = std::max(y0d, 1.0);
    double wx = x - x0d;
    double wy = y - y0d;
    indexType x0 = static_cast<indexType>(x0d - 1.0);
    indexType y0 = static_cast<indexType>(y0d - 1.0);
    indexType x1 = static_cast<indexType>(x1d - 1.0);
    indexType y1 = static_cast<indexType>(y1d - 1.0);
    T v00 = input[y0 + x0 * rows + channel * rows * cols];
    T v10 = input[y0 + x1 * rows + channel * rows * cols];
    T v01 = input[y1 + x0 * rows + channel * rows * cols];
    T v11 = input[y1 + x1 * rows + channel * rows * cols];
    return (1.0 - wx) * (1.0 - wy) * v00 + wx * (1.0 - wy) * v10 + (1.0 - wx) * wy * v01
        + wx * wy * v11;
}
//=============================================================================
template <typename T>
static T
cubicInterpolate(T v0, T v1, T v2, T v3, double t)
{
    return v1
        + 0.5 * t
        * (v2 - v0 + t * (2.0 * v0 - 5.0 * v1 + 4.0 * v2 - v3 + t * (3.0 * (v1 - v2) + v3 - v0)));
}
//=============================================================================
template <typename T>
static T
cubicValue(const std::vector<T>& input, indexType rows, indexType cols, indexType channel, double x,
    double y, const std::vector<double>& fillValues)
{
    if (x < 1.0 || x > static_cast<double>(cols) || y < 1.0 || y > static_cast<double>(rows)) {
        return (T)fillValues[channel];
    }
    double x0d = std::floor(x);
    double y0d = std::floor(y);
    double dx = x - x0d;
    double dy = y - y0d;
    signedIndexType x0 = static_cast<signedIndexType>(x0d) - 1;
    signedIndexType y0 = static_cast<signedIndexType>(y0d) - 1;
    T values[4];
    for (indexType row = 0; row < 4; row++) {
        T colsValues[4];
        for (indexType col = 0; col < 4; col++) {
            colsValues[col] = samplePixel(input, rows, cols, channel,
                y0 + static_cast<signedIndexType>(row) - 1,
                x0 + static_cast<signedIndexType>(col) - 1, fillValues);
        }
        values[row]
            = cubicInterpolate(colsValues[0], colsValues[1], colsValues[2], colsValues[3], dx);
    }
    return cubicInterpolate(values[0], values[1], values[2], values[3], dy);
}
//=============================================================================
template <typename T>
static std::vector<T>
warpData(const std::vector<T>& input, const ImageShape2D& shape, const ImageWarpOptions& options)
{
    std::vector<T> output((size_t)(options.outputRows * options.outputColumns * shape.channels));
    indexType outputPlane = options.outputRows * options.outputColumns;
    indexType sourcePlane = shape.rows * shape.columns;
    OMP_PARALLEL_FOR_LOOP(outputPlane, 4)
    for (ompIndexType k = 0; k < (ompIndexType)outputPlane; k++) {
        indexType pixel = static_cast<indexType>(k);
        indexType col = pixel / options.outputRows;
        indexType row = pixel - col * options.outputRows;
        double xw = options.outputXWorldStart
            + (static_cast<double>(col) + 0.5) * options.outputPixelExtentX;
        double yw = options.outputYWorldStart
            + (static_cast<double>(row) + 0.5) * options.outputPixelExtentY;
        const auto& t = options.inverseTransform;
        double px = xw * t[0] + yw * t[1] + t[2];
        double py = xw * t[3] + yw * t[4] + t[5];
        double pw = xw * t[6] + yw * t[7] + t[8];
        double sourceX = (px / pw - options.sourceXWorldStart) / options.sourcePixelExtentX + 0.5;
        double sourceY = (py / pw - options.sourceYWorldStart) / options.sourcePixelExtentY + 0.5;
        for (indexType channel = 0; channel < shape.channels; channel++) {
            T value;
            if (!std::isfinite(sourceX) || !std::isfinite(sourceY)) {
                value = (T)options.fillValues[channel];
            } else if (options.interpolation == ImageWarpInterpolation::Nearest) {
                value = nearestValue(input, shape.rows, shape.columns, channel, sourceX, sourceY,
                    options.fillValues);
            } else if (options.interpolation == ImageWarpInterpolation::Cubic) {
                value = cubicValue(input, shape.rows, shape.columns, channel, sourceX, sourceY,
                    options.fillValues);
            } else {
                value = linearValue(input, shape.rows, shape.columns, channel, sourceX, sourceY,
                    options.fillValues);
            }
            output[pixel + channel * outputPlane] = value;
        }
    }
    return output;
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
copyOutput(ArrayOf& out, const std::vector<double>& realValues,
    const std::vector<std::complex<double>>& complexValues, bool complexInput)
{
    NelsonType outClass = out.dataClass();
    if (complexInput && outClass == NLS_DCOMPLEX) {
        auto* ptr = (std::complex<double>*)out.readWriteDataPointer();
        std::copy(complexValues.begin(), complexValues.end(), ptr);
        return;
    }
    if (complexInput && outClass == NLS_SCOMPLEX) {
        auto* ptr = (std::complex<single>*)out.readWriteDataPointer();
        for (size_t k = 0; k < complexValues.size(); k++) {
            ptr[k] = std::complex<single>(
                (single)complexValues[k].real(), (single)complexValues[k].imag());
        }
        return;
    }
    switch (outClass) {
    case NLS_UINT8:
        copyRealOutput((uint8*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_UINT16:
        copyRealOutput((uint16*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_UINT32:
        copyRealOutput((uint32*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_UINT64:
        copyRealOutput((uint64*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_INT8:
        copyRealOutput((int8*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_INT16:
        copyRealOutput((int16*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_INT32:
        copyRealOutput((int32*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_INT64:
        copyRealOutput((int64*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_SINGLE:
        copyRealOutput((single*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_LOGICAL:
        copyRealOutput((logical*)out.readWriteDataPointer(), realValues);
        break;
    default:
        std::copy(realValues.begin(), realValues.end(), (double*)out.readWriteDataPointer());
        break;
    }
}
//=============================================================================
ArrayOf
ImageWarp(const ArrayOf& image, const ImageWarpOptions& options)
{
    ImageShape2D shape = validateImage2DOrRGBA(image,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical."),
            L"Image"),
        formatI18nMessage(
            Nelson::resolveMessageTemplate(L"Nelson:image_processing:imageDRgbRgbaArray",
                L"%s must be a 2-D, RGB, or RGBA array."),
            L"Image"),
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported."),
            L"Sparse images"));
    Dimensions outDims = shape.channels == 1
        ? Dimensions(options.outputRows, options.outputColumns)
        : Dimensions(
              std::vector<indexType> { options.outputRows, options.outputColumns, shape.channels });
    NelsonType outClass = outputClassFor(image);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    if (image.isEmpty() || out.isEmpty()) {
        return out;
    }
    if (options.fillValues.size() != shape.channels) {
        Error(L"Nelson:image_processing:fillValuesScalarMatchNumberChannels",
            L"Fill values must be scalar or match the number of channels.");
    }
    if (image.isComplex()) {
        std::vector<std::complex<double>> input = dataAsComplexDouble(image);
        std::vector<std::complex<double>> values = warpData(input, shape, options);
        copyOutput(out, {}, values, true);
    } else {
        std::vector<double> input = realDataAsDouble(image);
        std::vector<double> values = warpData(input, shape, options);
        copyOutput(out, values, {}, false);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
