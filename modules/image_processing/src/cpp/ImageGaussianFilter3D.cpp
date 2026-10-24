//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageGaussianFilter3D.hpp"
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
struct VolumeShape3D
{
    indexType rows { 0 };
    indexType columns { 0 };
    indexType planes { 0 };
};
//=============================================================================
static NelsonType
outputClassFor(const ArrayOf& volume)
{
    switch (volume.dataClass()) {
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
        return volume.dataClass();
    default:
        return NLS_DOUBLE;
    }
}
//=============================================================================
static VolumeShape3D
validateVolume3D(const ArrayOf& volume)
{
    if (!volume.isNumeric() && !volume.isLogical()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Volume");
    }
    if (volume.isSparse()) {
        Error(L"Nelson:image_processing:sparseVolumesNotSupported", L"%s are not supported.",
            L"Sparse volumes");
    }
    Dimensions dims = volume.dimensions();
    if (dims.length() != 3) {
        Error(L"Nelson:image_processing:volumeDArray", L"%s must be a 3-D array.", L"Volume");
    }
    return { dims.dimensionLength(0), dims.dimensionLength(1), dims.dimensionLength(2) };
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
sampleVolume(const std::vector<T>& input, const VolumeShape3D& shape, signedIndexType row,
    signedIndexType col, signedIndexType plane, const ImageFilterPadding& padding)
{
    if (row >= 0 && row < static_cast<signedIndexType>(shape.rows) && col >= 0
        && col < static_cast<signedIndexType>(shape.columns) && plane >= 0
        && plane < static_cast<signedIndexType>(shape.planes)) {
        return input[static_cast<indexType>(row) + static_cast<indexType>(col) * shape.rows
            + static_cast<indexType>(plane) * shape.rows * shape.columns];
    }
    if (padding.kind == ImageFilterPaddingKind::Constant) {
        return (T)padding.constantValue;
    }
    indexType rr = mapPadIndex(row, shape.rows, padding.kind);
    indexType cc = mapPadIndex(col, shape.columns, padding.kind);
    indexType pp = mapPadIndex(plane, shape.planes, padding.kind);
    return input[rr + cc * shape.rows + pp * shape.rows * shape.columns];
}
//=============================================================================
template <typename T>
static std::vector<T>
filterAlongRows(const std::vector<T>& input, const VolumeShape3D& shape,
    const std::vector<double>& kernel, const ImageFilterPadding& padding)
{
    std::vector<T> output(input.size());
    signedIndexType radius = static_cast<signedIndexType>(kernel.size() / 2);
    indexType totalColumns = shape.columns * shape.planes;
    OMP_PARALLEL_FOR_LOOP(totalColumns, 4)
    for (ompIndexType item = 0; item < (ompIndexType)totalColumns; item++) {
        indexType plane = static_cast<indexType>(item) / shape.columns;
        indexType col = static_cast<indexType>(item) - plane * shape.columns;
        for (indexType row = 0; row < shape.rows; row++) {
            T accum = 0;
            for (indexType kk = 0; kk < kernel.size(); kk++) {
                signedIndexType sourceRow
                    = static_cast<signedIndexType>(row) + static_cast<signedIndexType>(kk) - radius;
                accum += sampleVolume(input, shape, sourceRow, col, plane, padding) * kernel[kk];
            }
            output[row + col * shape.rows + plane * shape.rows * shape.columns] = accum;
        }
    }
    return output;
}
//=============================================================================
template <typename T>
static std::vector<T>
filterAlongColumns(const std::vector<T>& input, const VolumeShape3D& shape,
    const std::vector<double>& kernel, const ImageFilterPadding& padding)
{
    std::vector<T> output(input.size());
    signedIndexType radius = static_cast<signedIndexType>(kernel.size() / 2);
    indexType totalRows = shape.rows * shape.planes;
    OMP_PARALLEL_FOR_LOOP(totalRows, 4)
    for (ompIndexType item = 0; item < (ompIndexType)totalRows; item++) {
        indexType plane = static_cast<indexType>(item) / shape.rows;
        indexType row = static_cast<indexType>(item) - plane * shape.rows;
        for (indexType col = 0; col < shape.columns; col++) {
            T accum = 0;
            for (indexType kk = 0; kk < kernel.size(); kk++) {
                signedIndexType sourceCol
                    = static_cast<signedIndexType>(col) + static_cast<signedIndexType>(kk) - radius;
                accum += sampleVolume(input, shape, row, sourceCol, plane, padding) * kernel[kk];
            }
            output[row + col * shape.rows + plane * shape.rows * shape.columns] = accum;
        }
    }
    return output;
}
//=============================================================================
template <typename T>
static std::vector<T>
filterAlongPlanes(const std::vector<T>& input, const VolumeShape3D& shape,
    const std::vector<double>& kernel, const ImageFilterPadding& padding)
{
    std::vector<T> output(input.size());
    signedIndexType radius = static_cast<signedIndexType>(kernel.size() / 2);
    indexType totalPixels = shape.rows * shape.columns;
    OMP_PARALLEL_FOR_LOOP(totalPixels, 4)
    for (ompIndexType pixel = 0; pixel < (ompIndexType)totalPixels; pixel++) {
        indexType col = static_cast<indexType>(pixel) / shape.rows;
        indexType row = static_cast<indexType>(pixel) - col * shape.rows;
        for (indexType plane = 0; plane < shape.planes; plane++) {
            T accum = 0;
            for (indexType kk = 0; kk < kernel.size(); kk++) {
                signedIndexType sourcePlane = static_cast<signedIndexType>(plane)
                    + static_cast<signedIndexType>(kk) - radius;
                accum += sampleVolume(input, shape, row, col, sourcePlane, padding) * kernel[kk];
            }
            output[row + col * shape.rows + plane * shape.rows * shape.columns] = accum;
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
ImageGaussianFilter3D(const ArrayOf& volume, const ImageGaussianFilter3DOptions& options)
{
    VolumeShape3D shape = validateVolume3D(volume);
    Dimensions outDims(std::vector<indexType> { shape.rows, shape.columns, shape.planes });
    NelsonType outClass = outputClassFor(volume);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    if (volume.isEmpty()) {
        return out;
    }
    std::vector<double> rowKernel = gaussianKernel(options.filterRows, options.sigmaRows);
    std::vector<double> colKernel = gaussianKernel(options.filterColumns, options.sigmaColumns);
    std::vector<double> planeKernel = gaussianKernel(options.filterPlanes, options.sigmaPlanes);
    if (volume.isComplex()) {
        std::vector<std::complex<double>> input = dataAsComplexDouble(volume);
        auto rows = filterAlongRows(input, shape, rowKernel, options.padding);
        auto columns = filterAlongColumns(rows, shape, colKernel, options.padding);
        auto values = filterAlongPlanes(columns, shape, planeKernel, options.padding);
        copyOutput(out, {}, values, true);
    } else {
        std::vector<double> input = realDataAsDouble(volume);
        auto rows = filterAlongRows(input, shape, rowKernel, options.padding);
        auto columns = filterAlongColumns(rows, shape, colKernel, options.padding);
        auto values = filterAlongPlanes(columns, shape, planeKernel, options.padding);
        copyOutput(out, values, {}, false);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
