//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageGaussianFilter.hpp"
#include "ImageProcessingCore.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <cmath>
#include <complex>
#include <vector>
#include "ErrorID.hpp"
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
static indexType
mapPadIndex(signedIndexType index, indexType length, ImageFilterPaddingKind kind)
{
    if (kind == ImageFilterPaddingKind::Replicate) {
        signedIndexType last = static_cast<signedIndexType>(length) - 1;
        return static_cast<indexType>(std::min(std::max(index, (signedIndexType)0), last));
    }
    if (kind == ImageFilterPaddingKind::Circular) {
        signedIndexType value = index % static_cast<signedIndexType>(length);
        return static_cast<indexType>(value < 0 ? value + length : value);
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
static std::vector<double>
gaussianKernel(indexType size, double sigma)
{
    std::vector<double> kernel((size_t)size);
    signedIndexType center = static_cast<signedIndexType>(size / 2);
    double total = 0.0;
    for (indexType k = 0; k < size; k++) {
        double distance = static_cast<double>(static_cast<signedIndexType>(k) - center);
        double value = std::exp(-(distance * distance) / (2.0 * sigma * sigma));
        kernel[k] = value;
        total += value;
    }
    for (double& value : kernel) {
        value /= total;
    }
    return kernel;
}
//=============================================================================
template <typename T>
static T
sampleSame(const std::vector<T>& input, indexType rows, indexType cols, indexType channel,
    signedIndexType row, signedIndexType col, const ImageFilterPadding& padding)
{
    if (row >= 0 && row < static_cast<signedIndexType>(rows) && col >= 0
        && col < static_cast<signedIndexType>(cols)) {
        return input[static_cast<indexType>(row) + static_cast<indexType>(col) * rows
            + channel * rows * cols];
    }
    if (padding.kind == ImageFilterPaddingKind::Constant) {
        return (T)padding.constantValue;
    }
    indexType rr = mapPadIndex(row, rows, padding.kind);
    indexType cc = mapPadIndex(col, cols, padding.kind);
    return input[rr + cc * rows + channel * rows * cols];
}
//=============================================================================
template <typename T>
static std::vector<T>
horizontalPass(const std::vector<T>& input, indexType rows, indexType cols, indexType channels,
    const std::vector<double>& kernel, const ImageFilterPadding& padding)
{
    std::vector<T> output(input.size());
    signedIndexType radius = static_cast<signedIndexType>(kernel.size() / 2);
    indexType totalRows = rows * channels;
    OMP_PARALLEL_FOR_LOOP(totalRows, 4)
    for (ompIndexType item = 0; item < (ompIndexType)totalRows; item++) {
        indexType channel = static_cast<indexType>(item) / rows;
        indexType row = static_cast<indexType>(item) - channel * rows;
        for (indexType col = 0; col < cols; col++) {
            T accum = 0;
            for (indexType kk = 0; kk < kernel.size(); kk++) {
                signedIndexType sourceCol
                    = static_cast<signedIndexType>(col) + static_cast<signedIndexType>(kk) - radius;
                accum
                    += sampleSame(input, rows, cols, channel, row, sourceCol, padding) * kernel[kk];
            }
            output[row + col * rows + channel * rows * cols] = accum;
        }
    }
    return output;
}
//=============================================================================
template <typename T>
static std::vector<T>
verticalPass(const std::vector<T>& input, indexType rows, indexType cols, indexType channels,
    const std::vector<double>& kernel, const ImageFilterPadding& padding)
{
    std::vector<T> output(input.size());
    signedIndexType radius = static_cast<signedIndexType>(kernel.size() / 2);
    indexType totalCols = cols * channels;
    OMP_PARALLEL_FOR_LOOP(totalCols, 4)
    for (ompIndexType item = 0; item < (ompIndexType)totalCols; item++) {
        indexType channel = static_cast<indexType>(item) / cols;
        indexType col = static_cast<indexType>(item) - channel * cols;
        for (indexType row = 0; row < rows; row++) {
            T accum = 0;
            for (indexType kk = 0; kk < kernel.size(); kk++) {
                signedIndexType sourceRow
                    = static_cast<signedIndexType>(row) + static_cast<signedIndexType>(kk) - radius;
                accum
                    += sampleSame(input, rows, cols, channel, sourceRow, col, padding) * kernel[kk];
            }
            output[row + col * rows + channel * rows * cols] = accum;
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
ImageGaussianFilter(const ArrayOf& image, const ImageGaussianFilterOptions& options)
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
        ? Dimensions(shape.rows, shape.columns)
        : Dimensions(std::vector<indexType> { shape.rows, shape.columns, shape.channels });
    NelsonType outClass = outputClassFor(image);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    if (image.isEmpty()) {
        return out;
    }
    std::vector<double> rowKernel = gaussianKernel(options.filterRows, options.sigmaRows);
    std::vector<double> colKernel = gaussianKernel(options.filterColumns, options.sigmaColumns);
    if (image.isComplex()) {
        std::vector<std::complex<double>> input = dataAsComplexDouble(image);
        auto horizontal = horizontalPass(
            input, shape.rows, shape.columns, shape.channels, colKernel, options.padding);
        auto values = verticalPass(
            horizontal, shape.rows, shape.columns, shape.channels, rowKernel, options.padding);
        copyOutput(out, {}, values, true);
    } else {
        std::vector<double> input = realDataAsDouble(image);
        auto horizontal = horizontalPass(
            input, shape.rows, shape.columns, shape.channels, colKernel, options.padding);
        auto values = verticalPass(
            horizontal, shape.rows, shape.columns, shape.channels, rowKernel, options.padding);
        copyOutput(out, values, {}, false);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
