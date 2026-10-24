//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BWConnComp.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NewWithException.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
validateBinaryImage(const ArrayOf& image)
{
    if (!image.isNumeric() && !image.isLogical()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Image");
    }
    if (image.isSparse()) {
        Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
            L"Sparse images");
    }
    Dimensions dims = image.dimensions();
    if (dims.length() > 3) {
        Error(L"Nelson:image_processing:imageDDArray", L"%s must be a 2-D or 3-D array.", L"Image");
    }
}
//=============================================================================
static int
validateConnectivity(double value, bool isVolume)
{
    bool valid2D = value == 4 || value == 8;
    bool valid3D = value == 6 || value == 18 || value == 26;
    if (!std::isfinite(value) || (isVolume ? !valid3D : !valid2D)) {
        Error(L"Nelson:image_processing:unsupportedConnectivity", L"Unsupported connectivity.");
    }
    return static_cast<int>(value);
}
//=============================================================================
static std::vector<double>
imageAsDouble(const ArrayOf& image)
{
    ArrayOf work(image);
    work.promoteType(NLS_DOUBLE);
    const auto* ptr = static_cast<const double*>(work.dataPointer());
    return std::vector<double>(ptr, ptr + work.elementCount());
}
//=============================================================================
template <typename T>
static std::vector<uint8_t>
realDataAsForegroundMask(const ArrayOf& image)
{
    const auto* ptr = static_cast<const T*>(image.dataPointer());
    std::vector<uint8_t> mask(image.elementCount(), 0);
    for (indexType k = 0; k < image.elementCount(); k++) {
        mask[k] = ptr[k] != 0;
    }
    return mask;
}
//=============================================================================
static std::vector<uint8_t>
imageAsForegroundMask(const ArrayOf& image)
{
    if (image.isComplex()) {
        std::vector<double> data = imageAsDouble(image);
        std::vector<uint8_t> mask(data.size(), 0);
        for (size_t k = 0; k < data.size(); k++) {
            mask[k] = data[k] != 0;
        }
        return mask;
    }
    switch (image.dataClass()) {
    case NLS_LOGICAL:
        return realDataAsForegroundMask<logical>(image);
    case NLS_DOUBLE:
        return realDataAsForegroundMask<double>(image);
    case NLS_SINGLE:
        return realDataAsForegroundMask<single>(image);
    case NLS_INT8:
        return realDataAsForegroundMask<int8>(image);
    case NLS_INT16:
        return realDataAsForegroundMask<int16>(image);
    case NLS_INT32:
        return realDataAsForegroundMask<int32>(image);
    case NLS_INT64:
        return realDataAsForegroundMask<int64>(image);
    case NLS_UINT8:
        return realDataAsForegroundMask<uint8>(image);
    case NLS_UINT16:
        return realDataAsForegroundMask<uint16>(image);
    case NLS_UINT32:
        return realDataAsForegroundMask<uint32>(image);
    case NLS_UINT64:
        return realDataAsForegroundMask<uint64>(image);
    default: {
        std::vector<double> data = imageAsDouble(image);
        std::vector<uint8_t> mask(data.size(), 0);
        for (size_t k = 0; k < data.size(); k++) {
            mask[k] = data[k] != 0;
        }
        return mask;
    }
    }
}
//=============================================================================
static ArrayOf
doubleColumnVectorFromIndices(const std::vector<indexType>& indices)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, indices.size()));
    for (size_t k = 0; k < indices.size(); k++) {
        ptr[k] = static_cast<double>(indices[k] + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(indices.size(), 1), ptr);
}
//=============================================================================
static ArrayOf
pixelIdxListCell(const std::vector<std::vector<indexType>>& components)
{
    Dimensions dims(1, components.size());
    auto* elements = new_with_exception<ArrayOf>(dims.elementCount(), false);
    for (size_t k = 0; k < components.size(); k++) {
        elements[k] = doubleColumnVectorFromIndices(components[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
static ArrayOf
imageSizeRow(const std::vector<indexType>& imageSize)
{
    auto* ptr = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, imageSize.size()));
    for (size_t k = 0; k < imageSize.size(); k++) {
        ptr[k] = static_cast<double>(imageSize[k]);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, imageSize.size()), ptr);
}
//=============================================================================
static ArrayOf
componentStruct(int conn, const std::vector<indexType>& imageSize,
    const std::vector<std::vector<indexType>>& components)
{
    stringVector fieldnames { "Connectivity", "ImageSize", "NumObjects", "PixelIdxList" };
    auto* elements
        = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, 1, fieldnames, false));
    ArrayOf result(NLS_STRUCT_ARRAY, Dimensions(1, 1), elements, false, fieldnames);
    ArrayOfVector connectivityValue { ArrayOf::doubleScalar(conn) };
    ArrayOfVector imageSizeValue { imageSizeRow(imageSize) };
    ArrayOfVector numObjectsValue { ArrayOf::doubleScalar(static_cast<double>(components.size())) };
    ArrayOfVector pixelIdxListValue { pixelIdxListCell(components) };
    result.setFieldAsList("Connectivity", connectivityValue);
    result.setFieldAsList("ImageSize", imageSizeValue);
    result.setFieldAsList("NumObjects", numObjectsValue);
    result.setFieldAsList("PixelIdxList", pixelIdxListValue);
    return result;
}
//=============================================================================
static void
appendNeighbors(std::vector<indexType>& pending, std::vector<uint8_t>& visited,
    const std::vector<uint8_t>& data, indexType rows, indexType columns, indexType planes,
    indexType current, int conn)
{
    indexType y = current % rows;
    indexType x = (current / rows) % columns;
    indexType z = current / (rows * columns);
    int minDz = planes > 1 ? -1 : 0;
    int maxDz = planes > 1 ? 1 : 0;
    bool isVolume = planes > 1;
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            for (int dz = minDz; dz <= maxDz; dz++) {
                int manhattan = std::abs(dx) + std::abs(dy) + std::abs(dz);
                if (manhattan == 0 || (!isVolume && conn == 4 && manhattan != 1)
                    || (isVolume && conn == 6 && manhattan != 1)
                    || (isVolume && conn == 18 && manhattan > 2)) {
                    continue;
                }
                signedIndexType yy = static_cast<signedIndexType>(y) + dy;
                signedIndexType xx = static_cast<signedIndexType>(x) + dx;
                signedIndexType zz = static_cast<signedIndexType>(z) + dz;
                if (yy < 0 || xx < 0 || zz < 0 || yy >= static_cast<signedIndexType>(rows)
                    || xx >= static_cast<signedIndexType>(columns)
                    || zz >= static_cast<signedIndexType>(planes)) {
                    continue;
                }
                indexType next = static_cast<indexType>(yy) + static_cast<indexType>(xx) * rows
                    + static_cast<indexType>(zz) * rows * columns;
                if (data[next] && !visited[next]) {
                    visited[next] = 1;
                    pending.push_back(next);
                }
            }
        }
    }
}
//=============================================================================
static std::vector<std::vector<indexType>>
findComponents(
    const std::vector<uint8_t>& data, indexType rows, indexType columns, indexType planes, int conn)
{
    std::vector<uint8_t> visited(data.size(), 0);
    std::vector<std::vector<indexType>> components;
    for (indexType start = 0; start < data.size(); start++) {
        if (!data[start] || visited[start]) {
            continue;
        }
        std::vector<indexType> pending;
        std::vector<indexType> members;
        visited[start] = 1;
        pending.push_back(start);
        for (size_t cursor = 0; cursor < pending.size(); cursor++) {
            indexType current = pending[cursor];
            members.push_back(current);
            appendNeighbors(pending, visited, data, rows, columns, planes, current, conn);
        }
        std::sort(members.begin(), members.end());
        components.push_back(std::move(members));
    }
    return components;
}
//=============================================================================
ArrayOf
BWConnComp(const ArrayOf& image, double connectivityValue)
{
    validateBinaryImage(image);
    Dimensions dims = image.dimensions();
    bool isVolume = dims.length() > 2;
    int conn = validateConnectivity(connectivityValue, isVolume);
    indexType rows = image.rows();
    indexType columns = image.columns();
    indexType planes = isVolume ? dims.dimensionLength(2) : 1;
    std::vector<uint8_t> data = imageAsForegroundMask(image);
    std::vector<std::vector<indexType>> components
        = findComponents(data, rows, columns, planes, conn);
    std::vector<indexType> imageSize { rows, columns };
    if (isVolume) {
        imageSize.push_back(planes);
    }
    return componentStruct(conn, imageSize, components);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
