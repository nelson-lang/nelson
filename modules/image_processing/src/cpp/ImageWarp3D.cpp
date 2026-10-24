//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageWarp3D.hpp"
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
            L"Image");
    }
    if (volume.isSparse()) {
        Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
            L"Sparse images");
    }
    Dimensions dims = volume.dimensions();
    if (dims.length() != 3) {
        Error(L"Nelson:image_processing:volumeDArray", L"%s must be a 3-D array.", L"Volume");
    }
    return { dims.dimensionLength(0), dims.dimensionLength(1), dims.dimensionLength(2) };
}
//=============================================================================
static std::vector<double>
realDataAsDouble(const ArrayOf& value)
{
    ArrayOf work(value);
    work.promoteType(NLS_DOUBLE);
    const auto* ptr = static_cast<const double*>(work.dataPointer());
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
nearestValue(const std::vector<T>& input, const VolumeShape3D& shape, double x, double y, double z,
    double fillValue)
{
    if (x < 1.0 || x > static_cast<double>(shape.columns) || y < 1.0
        || y > static_cast<double>(shape.rows) || z < 1.0
        || z > static_cast<double>(shape.planes)) {
        return static_cast<T>(fillValue);
    }
    indexType column = static_cast<indexType>(
        std::clamp(std::round(x), 1.0, static_cast<double>(shape.columns)) - 1.0);
    indexType row = static_cast<indexType>(
        std::clamp(std::round(y), 1.0, static_cast<double>(shape.rows)) - 1.0);
    indexType plane = static_cast<indexType>(
        std::clamp(std::round(z), 1.0, static_cast<double>(shape.planes)) - 1.0);
    return input[row + column * shape.rows + plane * shape.rows * shape.columns];
}
//=============================================================================
template <typename T>
static T
linearValue(const std::vector<T>& input, const VolumeShape3D& shape, double x, double y, double z,
    double fillValue)
{
    if (x < 1.0 || x > static_cast<double>(shape.columns) || y < 1.0
        || y > static_cast<double>(shape.rows) || z < 1.0
        || z > static_cast<double>(shape.planes)) {
        return static_cast<T>(fillValue);
    }
    double x0d = std::max(std::floor(x), 1.0);
    double y0d = std::max(std::floor(y), 1.0);
    double z0d = std::max(std::floor(z), 1.0);
    double x1d = std::min(x0d + 1.0, static_cast<double>(shape.columns));
    double y1d = std::min(y0d + 1.0, static_cast<double>(shape.rows));
    double z1d = std::min(z0d + 1.0, static_cast<double>(shape.planes));
    double wx = x - x0d;
    double wy = y - y0d;
    double wz = z - z0d;
    indexType x0 = static_cast<indexType>(x0d - 1.0);
    indexType y0 = static_cast<indexType>(y0d - 1.0);
    indexType z0 = static_cast<indexType>(z0d - 1.0);
    indexType x1 = static_cast<indexType>(x1d - 1.0);
    indexType y1 = static_cast<indexType>(y1d - 1.0);
    indexType z1 = static_cast<indexType>(z1d - 1.0);
    indexType planeSize = shape.rows * shape.columns;
    T v000 = input[y0 + x0 * shape.rows + z0 * planeSize];
    T v100 = input[y0 + x1 * shape.rows + z0 * planeSize];
    T v010 = input[y1 + x0 * shape.rows + z0 * planeSize];
    T v110 = input[y1 + x1 * shape.rows + z0 * planeSize];
    T v001 = input[y0 + x0 * shape.rows + z1 * planeSize];
    T v101 = input[y0 + x1 * shape.rows + z1 * planeSize];
    T v011 = input[y1 + x0 * shape.rows + z1 * planeSize];
    T v111 = input[y1 + x1 * shape.rows + z1 * planeSize];
    return v000 * (1.0 - wx) * (1.0 - wy) * (1.0 - wz) + v100 * wx * (1.0 - wy) * (1.0 - wz)
        + v010 * (1.0 - wx) * wy * (1.0 - wz) + v110 * wx * wy * (1.0 - wz)
        + v001 * (1.0 - wx) * (1.0 - wy) * wz + v101 * wx * (1.0 - wy) * wz
        + v011 * (1.0 - wx) * wy * wz + v111 * wx * wy * wz;
}
//=============================================================================
template <typename T>
static std::vector<T>
warpVolume(const std::vector<T>& input, const VolumeShape3D& shape, const ImageWarp3DOptions& opt)
{
    indexType outputPlaneSize = opt.outputRows * opt.outputColumns;
    indexType outputCount = outputPlaneSize * opt.outputPlanes;
    std::vector<T> output(outputCount);
    OMP_PARALLEL_FOR_LOOP(outputCount, 4)
    for (ompIndexType item = 0; item < (ompIndexType)outputCount; item++) {
        indexType pixel = static_cast<indexType>(item);
        indexType plane = pixel / outputPlaneSize;
        indexType inPlanePixel = pixel - plane * outputPlaneSize;
        indexType column = inPlanePixel / opt.outputRows;
        indexType row = inPlanePixel - column * opt.outputRows;
        double xw
            = opt.outputXWorldStart + (static_cast<double>(column) + 0.5) * opt.outputPixelExtentX;
        double yw
            = opt.outputYWorldStart + (static_cast<double>(row) + 0.5) * opt.outputPixelExtentY;
        double zw
            = opt.outputZWorldStart + (static_cast<double>(plane) + 0.5) * opt.outputPixelExtentZ;
        const auto& t = opt.inverseTransform;
        double px = xw * t[0] + yw * t[1] + zw * t[2] + t[3];
        double py = xw * t[4] + yw * t[5] + zw * t[6] + t[7];
        double pz = xw * t[8] + yw * t[9] + zw * t[10] + t[11];
        double pw = xw * t[12] + yw * t[13] + zw * t[14] + t[15];
        double sourceX = (px / pw - opt.sourceXWorldStart) / opt.sourcePixelExtentX + 0.5;
        double sourceY = (py / pw - opt.sourceYWorldStart) / opt.sourcePixelExtentY + 0.5;
        double sourceZ = (pz / pw - opt.sourceZWorldStart) / opt.sourcePixelExtentZ + 0.5;
        if (!std::isfinite(sourceX) || !std::isfinite(sourceY) || !std::isfinite(sourceZ)) {
            output[pixel] = static_cast<T>(opt.fillValue);
        } else if (opt.interpolation == ImageWarpInterpolation::Nearest) {
            output[pixel] = nearestValue(input, shape, sourceX, sourceY, sourceZ, opt.fillValue);
        } else {
            output[pixel] = linearValue(input, shape, sourceX, sourceY, sourceZ, opt.fillValue);
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
            ptr[k] = std::complex<single>(static_cast<single>(complexValues[k].real()),
                static_cast<single>(complexValues[k].imag()));
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
ImageWarp3D(const ArrayOf& volume, const ImageWarp3DOptions& options)
{
    VolumeShape3D shape = validateVolume3D(volume);
    Dimensions outDims(
        std::vector<indexType> { options.outputRows, options.outputColumns, options.outputPlanes });
    NelsonType outClass = outputClassFor(volume);
    ArrayOf out(outClass, outDims, ArrayOf::allocateArrayOf(outClass, outDims.elementCount()));
    if (volume.isEmpty() || out.isEmpty()) {
        return out;
    }
    if (volume.isComplex()) {
        std::vector<std::complex<double>> input = dataAsComplexDouble(volume);
        std::vector<std::complex<double>> values = warpVolume(input, shape, options);
        copyOutput(out, {}, values, true);
    } else {
        std::vector<double> input = realDataAsDouble(volume);
        std::vector<double> values = warpVolume(input, shape, options);
        copyOutput(out, values, {}, false);
    }
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
