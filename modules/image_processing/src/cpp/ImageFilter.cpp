//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageFilter.hpp"
#include "ImageProcessingCore.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <complex>
#include <vector>
//=============================================================================
namespace Nelson {
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
        return value < 0 ? value + length : value;
    }
    if (kind == ImageFilterPaddingKind::Symmetric) {
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
    return -1;
}
//=============================================================================
static NelsonType
outputClassFor(const ArrayOf& image)
{
    switch (image.dataClass()) {
    case NLS_UINT8:
    case NLS_UINT16:
    case NLS_INT8:
    case NLS_INT16:
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
static std::vector<double>
kernelAsConvolutionInput(const ArrayOf& kernel, const ImageFilterOptions& options)
{
    std::vector<double> h = realDataAsDouble(kernel);
    indexType rows = kernel.rows();
    indexType cols = kernel.columns();
    if (options.convolution) {
        return h;
    }
    std::vector<double> rotated(h.size());
    for (indexType c = 0; c < cols; c++) {
        for (indexType r = 0; r < rows; r++) {
            rotated[r + c * rows] = h[(rows - r - 1) + (cols - c - 1) * rows];
        }
    }
    return rotated;
}
//=============================================================================
template <typename T>
static void
copyRealOutput(T* out, const std::vector<double>& values)
{
    for (size_t k = 0; k < values.size(); k++) {
        assignClampedPixel(out, (size_t)k, values[k]);
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
    case NLS_INT8:
        copyRealOutput((int8*)out.readWriteDataPointer(), realValues);
        break;
    case NLS_INT16:
        copyRealOutput((int16*)out.readWriteDataPointer(), realValues);
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
template <typename T>
static T
sampleSame(const std::vector<T>& input, indexType rows, indexType cols, indexType channels,
    signedIndexType row, signedIndexType col, indexType channel, const ImageFilterPadding& padding)
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
static void
filterSame(const std::vector<T>& input, std::vector<T>& output, indexType rows, indexType cols,
    indexType channels, const std::vector<double>& kernel, indexType kernelRows,
    indexType kernelCols, const ImageFilterPadding& padding)
{
    indexType padRows = kernelRows / 2;
    indexType padCols = kernelCols / 2;
    size_t total = output.size();
    for (size_t idx = 0; idx < total; idx++) {
        indexType plane = rows * cols;
        indexType channel = static_cast<indexType>(idx) / plane;
        indexType inPlane = static_cast<indexType>(idx) - channel * plane;
        indexType col = inPlane / rows;
        indexType row = inPlane - col * rows;
        T accum = 0;
        for (indexType kc = 0; kc < kernelCols; kc++) {
            for (indexType kr = 0; kr < kernelRows; kr++) {
                signedIndexType sourceRow = static_cast<signedIndexType>(row)
                    + static_cast<signedIndexType>(kr) - static_cast<signedIndexType>(padRows);
                signedIndexType sourceCol = static_cast<signedIndexType>(col)
                    + static_cast<signedIndexType>(kc) - static_cast<signedIndexType>(padCols);
                T value = sampleSame(
                    input, rows, cols, channels, sourceRow, sourceCol, channel, padding);
                double kval = kernel[(kernelRows - kr - 1) + (kernelCols - kc - 1) * kernelRows];
                accum += value * kval;
            }
        }
        output[idx] = accum;
    }
}
//=============================================================================
template <typename T>
static void
filterFullOrValid(const std::vector<T>& input, std::vector<T>& output, indexType rows,
    indexType cols, indexType channels, const std::vector<double>& kernel, indexType kernelRows,
    indexType kernelCols, ImageFilterShape shape, indexType outRows, indexType outCols)
{
    signedIndexType rowOffset = shape == ImageFilterShape::Valid ? kernelRows - 1 : 0;
    signedIndexType colOffset = shape == ImageFilterShape::Valid ? kernelCols - 1 : 0;
    size_t total = output.size();
    for (size_t idx = 0; idx < total; idx++) {
        indexType plane = outRows * outCols;
        indexType channel = static_cast<indexType>(idx) / plane;
        indexType inPlane = static_cast<indexType>(idx) - channel * plane;
        indexType outCol = inPlane / outRows;
        indexType outRow = inPlane - outCol * outRows;
        signedIndexType sourceRowEnd = static_cast<signedIndexType>(outRow) + rowOffset;
        signedIndexType sourceColEnd = static_cast<signedIndexType>(outCol) + colOffset;
        signedIndexType minRow = std::max(
            (signedIndexType)0, sourceRowEnd - static_cast<signedIndexType>(kernelRows) + 1);
        signedIndexType maxRow = std::min(static_cast<signedIndexType>(rows) - 1, sourceRowEnd);
        signedIndexType minCol = std::max(
            (signedIndexType)0, sourceColEnd - static_cast<signedIndexType>(kernelCols) + 1);
        signedIndexType maxCol = std::min(static_cast<signedIndexType>(cols) - 1, sourceColEnd);
        T accum = 0;
        for (signedIndexType c = minCol; c <= maxCol; c++) {
            for (signedIndexType r = minRow; r <= maxRow; r++) {
                indexType kr = static_cast<indexType>(sourceRowEnd - r);
                indexType kc = static_cast<indexType>(sourceColEnd - c);
                indexType source = static_cast<indexType>(r) + static_cast<indexType>(c) * rows
                    + channel * rows * cols;
                accum += input[source] * kernel[kr + kc * kernelRows];
            }
        }
        output[idx] = accum;
    }
}
//=============================================================================
template <typename T>
static std::vector<T>
filterData(const std::vector<T>& input, indexType rows, indexType cols, indexType channels,
    const std::vector<double>& kernel, indexType kernelRows, indexType kernelCols,
    const ImageFilterOptions& options, indexType outRows, indexType outCols)
{
    std::vector<T> output((size_t)(outRows * outCols * channels));
    if (options.shape == ImageFilterShape::Same) {
        filterSame(
            input, output, rows, cols, channels, kernel, kernelRows, kernelCols, options.padding);
    } else if (!output.empty()) {
        filterFullOrValid(input, output, rows, cols, channels, kernel, kernelRows, kernelCols,
            options.shape, outRows, outCols);
    }
    return output;
}
//=============================================================================
ArrayOf
ImageFilter(const ArrayOf& image, const ArrayOf& kernel, const ImageFilterOptions& options)
{
    ImageShape2D shape = validateImage2DOrRGB(image, true,
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical."),
            L"Image"),
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:image_processing:imageDRgbArray",
                              L"%s must be a 2-D or RGB array."),
            L"Image"),
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported."),
            L"Sparse images"));
    if (!kernel.isNumeric() || kernel.isComplex() || kernel.isSparse() || kernel.isEmpty()
        || kernel.dimensions().length() > 2) {
        Error(L"Nelson:imfilter:invalidKernel",
            L"Filter kernel must be a nonempty 2-D numeric real array.");
    }
    indexType kernelRows = kernel.rows();
    indexType kernelCols = kernel.columns();
    indexType outRows = shape.rows;
    indexType outCols = shape.columns;
    if (options.shape == ImageFilterShape::Full) {
        outRows = shape.rows + kernelRows - 1;
        outCols = shape.columns + kernelCols - 1;
    } else if (options.shape == ImageFilterShape::Valid) {
        outRows = std::max((indexType)0, shape.rows - kernelRows + 1);
        outCols = std::max((indexType)0, shape.columns - kernelCols + 1);
    }
    Dimensions outDims = shape.channels == 1
        ? Dimensions(outRows, outCols)
        : Dimensions(std::vector<indexType> { outRows, outCols, shape.channels });
    NelsonType outClass = outputClassFor(image);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    std::vector<double> h = kernelAsConvolutionInput(kernel, options);
    if (image.isComplex()) {
        std::vector<std::complex<double>> input = dataAsComplexDouble(image);
        std::vector<std::complex<double>> values = filterData(input, shape.rows, shape.columns,
            shape.channels, h, kernelRows, kernelCols, options, outRows, outCols);
        copyOutput(out, {}, values, true);
    } else {
        std::vector<double> input = realDataAsDouble(image);
        std::vector<double> values = filterData(input, shape.rows, shape.columns, shape.channels, h,
            kernelRows, kernelCols, options, outRows, outCols);
        copyOutput(out, values, {}, false);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
