//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageResize3D.hpp"
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
static std::vector<indexType>
nearestMap(indexType inputLength, indexType outputLength)
{
    std::vector<indexType> map(outputLength);
    double scale = static_cast<double>(inputLength) / static_cast<double>(outputLength);
    for (indexType k = 0; k < outputLength; k++) {
        double coord = (static_cast<double>(k) + 0.5) * scale + 0.5;
        double rounded = std::round(coord);
        rounded = std::clamp(rounded, 1.0, static_cast<double>(inputLength));
        map[k] = static_cast<indexType>(rounded - 1.0);
    }
    return map;
}
//=============================================================================
static void
linearMap(indexType inputLength, indexType outputLength, std::vector<indexType>& lo,
    std::vector<indexType>& hi, std::vector<double>& alpha)
{
    lo.resize(outputLength);
    hi.resize(outputLength);
    alpha.resize(outputLength);
    double scale = static_cast<double>(inputLength) / static_cast<double>(outputLength);
    for (indexType k = 0; k < outputLength; k++) {
        double coord = (static_cast<double>(k) + 0.5) * scale + 0.5;
        double low = std::floor(coord);
        double high = low + 1.0;
        alpha[k] = coord - low;
        low = std::clamp(low, 1.0, static_cast<double>(inputLength));
        high = std::clamp(high, 1.0, static_cast<double>(inputLength));
        lo[k] = static_cast<indexType>(low - 1.0);
        hi[k] = static_cast<indexType>(high - 1.0);
    }
}
//=============================================================================
template <typename T>
static void
nearestCopy(const T* input, T* output, const VolumeShape3D& inShape, const VolumeShape3D& outShape)
{
    auto rowMap = nearestMap(inShape.rows, outShape.rows);
    auto columnMap = nearestMap(inShape.columns, outShape.columns);
    auto planeMap = nearestMap(inShape.planes, outShape.planes);
    indexType outputPlaneSize = outShape.rows * outShape.columns;
    OMP_PARALLEL_FOR_LOOP(outShape.planes, 1)
    for (ompIndexType planeItem = 0; planeItem < (ompIndexType)outShape.planes; planeItem++) {
        indexType outPlane = static_cast<indexType>(planeItem);
        indexType srcPlane = planeMap[outPlane];
        for (indexType outCol = 0; outCol < outShape.columns; outCol++) {
            indexType srcCol = columnMap[outCol];
            for (indexType outRow = 0; outRow < outShape.rows; outRow++) {
                indexType srcRow = rowMap[outRow];
                output[outRow + outCol * outShape.rows + outPlane * outputPlaneSize] = input[srcRow
                    + srcCol * inShape.rows + srcPlane * inShape.rows * inShape.columns];
            }
        }
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
static std::vector<T>
resizeLinearRows(const std::vector<T>& input, const VolumeShape3D& inShape, indexType outRows)
{
    std::vector<indexType> lo;
    std::vector<indexType> hi;
    std::vector<double> alpha;
    linearMap(inShape.rows, outRows, lo, hi, alpha);
    VolumeShape3D outShape { outRows, inShape.columns, inShape.planes };
    std::vector<T> output(outRows * inShape.columns * inShape.planes);
    indexType totalColumns = inShape.columns * inShape.planes;
    OMP_PARALLEL_FOR_LOOP(totalColumns, 4)
    for (ompIndexType item = 0; item < (ompIndexType)totalColumns; item++) {
        indexType plane = static_cast<indexType>(item) / inShape.columns;
        indexType col = static_cast<indexType>(item) - plane * inShape.columns;
        for (indexType row = 0; row < outRows; row++) {
            T low = input[lo[row] + col * inShape.rows + plane * inShape.rows * inShape.columns];
            T high = input[hi[row] + col * inShape.rows + plane * inShape.rows * inShape.columns];
            output[row + col * outShape.rows + plane * outShape.rows * outShape.columns]
                = low * (1.0 - alpha[row]) + high * alpha[row];
        }
    }
    return output;
}
//=============================================================================
template <typename T>
static std::vector<T>
resizeLinearColumns(const std::vector<T>& input, const VolumeShape3D& inShape, indexType outColumns)
{
    std::vector<indexType> lo;
    std::vector<indexType> hi;
    std::vector<double> alpha;
    linearMap(inShape.columns, outColumns, lo, hi, alpha);
    VolumeShape3D outShape { inShape.rows, outColumns, inShape.planes };
    std::vector<T> output(inShape.rows * outColumns * inShape.planes);
    indexType totalRows = inShape.rows * inShape.planes;
    OMP_PARALLEL_FOR_LOOP(totalRows, 4)
    for (ompIndexType item = 0; item < (ompIndexType)totalRows; item++) {
        indexType plane = static_cast<indexType>(item) / inShape.rows;
        indexType row = static_cast<indexType>(item) - plane * inShape.rows;
        for (indexType col = 0; col < outColumns; col++) {
            T low = input[row + lo[col] * inShape.rows + plane * inShape.rows * inShape.columns];
            T high = input[row + hi[col] * inShape.rows + plane * inShape.rows * inShape.columns];
            output[row + col * outShape.rows + plane * outShape.rows * outShape.columns]
                = low * (1.0 - alpha[col]) + high * alpha[col];
        }
    }
    return output;
}
//=============================================================================
template <typename T>
static std::vector<T>
resizeLinearPlanes(const std::vector<T>& input, const VolumeShape3D& inShape, indexType outPlanes)
{
    std::vector<indexType> lo;
    std::vector<indexType> hi;
    std::vector<double> alpha;
    linearMap(inShape.planes, outPlanes, lo, hi, alpha);
    VolumeShape3D outShape { inShape.rows, inShape.columns, outPlanes };
    std::vector<T> output(inShape.rows * inShape.columns * outPlanes);
    indexType planeSize = inShape.rows * inShape.columns;
    OMP_PARALLEL_FOR_LOOP(planeSize, 4)
    for (ompIndexType pixelItem = 0; pixelItem < (ompIndexType)planeSize; pixelItem++) {
        indexType pixel = static_cast<indexType>(pixelItem);
        indexType col = pixel / inShape.rows;
        indexType row = pixel - col * inShape.rows;
        for (indexType plane = 0; plane < outPlanes; plane++) {
            T low = input[row + col * inShape.rows + lo[plane] * planeSize];
            T high = input[row + col * inShape.rows + hi[plane] * planeSize];
            output[row + col * outShape.rows + plane * outShape.rows * outShape.columns]
                = low * (1.0 - alpha[plane]) + high * alpha[plane];
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
template <typename T>
static bool
tryNearestCopy(const ArrayOf& volume, ArrayOf& out, const VolumeShape3D& inShape,
    const VolumeShape3D& outShape, NelsonType dataClass)
{
    if (volume.dataClass() != dataClass || out.dataClass() != dataClass) {
        return false;
    }
    const T* input = static_cast<const T*>(volume.dataPointer());
    T* output = static_cast<T*>(out.readWriteDataPointer());
    nearestCopy(input, output, inShape, outShape);
    return true;
}
//=============================================================================
static bool
nearestCopyByClass(const ArrayOf& volume, ArrayOf& out, const VolumeShape3D& inShape,
    const VolumeShape3D& outShape)
{
    switch (volume.dataClass()) {
    case NLS_UINT8:
        return tryNearestCopy<uint8>(volume, out, inShape, outShape, NLS_UINT8);
    case NLS_UINT16:
        return tryNearestCopy<uint16>(volume, out, inShape, outShape, NLS_UINT16);
    case NLS_UINT32:
        return tryNearestCopy<uint32>(volume, out, inShape, outShape, NLS_UINT32);
    case NLS_UINT64:
        return tryNearestCopy<uint64>(volume, out, inShape, outShape, NLS_UINT64);
    case NLS_INT8:
        return tryNearestCopy<int8>(volume, out, inShape, outShape, NLS_INT8);
    case NLS_INT16:
        return tryNearestCopy<int16>(volume, out, inShape, outShape, NLS_INT16);
    case NLS_INT32:
        return tryNearestCopy<int32>(volume, out, inShape, outShape, NLS_INT32);
    case NLS_INT64:
        return tryNearestCopy<int64>(volume, out, inShape, outShape, NLS_INT64);
    case NLS_SINGLE:
        return tryNearestCopy<single>(volume, out, inShape, outShape, NLS_SINGLE);
    case NLS_DOUBLE:
        return tryNearestCopy<double>(volume, out, inShape, outShape, NLS_DOUBLE);
    case NLS_LOGICAL:
        return tryNearestCopy<logical>(volume, out, inShape, outShape, NLS_LOGICAL);
    case NLS_SCOMPLEX:
        return tryNearestCopy<std::complex<single>>(volume, out, inShape, outShape, NLS_SCOMPLEX);
    case NLS_DCOMPLEX:
        return tryNearestCopy<std::complex<double>>(volume, out, inShape, outShape, NLS_DCOMPLEX);
    default:
        return false;
    }
}
//=============================================================================
ArrayOf
ImageResize3D(const ArrayOf& volume, const ImageResize3DOptions& options)
{
    VolumeShape3D inShape = validateVolume3D(volume);
    VolumeShape3D outShape { options.rows, options.columns, options.planes };
    Dimensions outDims(std::vector<indexType> { outShape.rows, outShape.columns, outShape.planes });
    NelsonType outClass = outputClassFor(volume);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    if (volume.isEmpty() || out.isEmpty()) {
        return out;
    }
    if (options.interpolation == ImageResize3DInterpolation::Nearest
        && nearestCopyByClass(volume, out, inShape, outShape)) {
        return out;
    }
    VolumeShape3D rowsShape { outShape.rows, inShape.columns, inShape.planes };
    VolumeShape3D columnsShape { outShape.rows, outShape.columns, inShape.planes };
    if (volume.isComplex()) {
        std::vector<std::complex<double>> input = dataAsComplexDouble(volume);
        auto rows = resizeLinearRows(input, inShape, outShape.rows);
        auto columns = resizeLinearColumns(rows, rowsShape, outShape.columns);
        auto values = resizeLinearPlanes(columns, columnsShape, outShape.planes);
        copyOutput(out, {}, values, true);
    } else {
        std::vector<double> input = realDataAsDouble(volume);
        auto rows = resizeLinearRows(input, inShape, outShape.rows);
        auto columns = resizeLinearColumns(rows, rowsShape, outShape.columns);
        auto values = resizeLinearPlanes(columns, columnsShape, outShape.planes);
        copyOutput(out, values, {}, false);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
