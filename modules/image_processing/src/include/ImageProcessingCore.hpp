//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <type_traits>
//=============================================================================
namespace Nelson {
//=============================================================================
struct ImageShape2D
{
    indexType rows { 0 };
    indexType columns { 0 };
    indexType channels { 1 };
};
//=============================================================================
inline ImageShape2D
validateImage2DOrRGB(const ArrayOf& image, bool allowRGB, const std::wstring& numericMessage,
    const std::wstring& dimensionMessage, const std::wstring& sparseMessage)
{
    if (!image.isNumeric() && !image.isLogical()) {
        Error(L"Nelson:image_processing:expectedNumericImage", numericMessage);
    }
    Dimensions dims = image.dimensions();
    if (image.isSparse()) {
        Error(L"Nelson:image_processing:sparseNotSupported", sparseMessage);
    }
    if (dims.length() > 3) {
        Error(L"Nelson:image_processing:invalidImageDimensions", dimensionMessage);
    }
    indexType channels = (dims.length() > 2) ? dims.dimensionLength(2) : 1;
    if ((!allowRGB && channels != 1) || (allowRGB && channels != 1 && channels != 3)) {
        Error(L"Nelson:image_processing:invalidImageDimensions", dimensionMessage);
    }
    return { dims.rows(), dims.columns(), channels };
}
//=============================================================================
inline ImageShape2D
validateImage2DOrRGBA(const ArrayOf& image, const std::wstring& numericMessage,
    const std::wstring& dimensionMessage, const std::wstring& sparseMessage)
{
    if (!image.isNumeric() && !image.isLogical()) {
        Error(L"Nelson:image_processing:expectedNumericImage", numericMessage);
    }
    Dimensions dims = image.dimensions();
    if (image.isSparse()) {
        Error(L"Nelson:image_processing:sparseNotSupported", sparseMessage);
    }
    if (dims.length() > 3) {
        Error(L"Nelson:image_processing:invalidImageDimensions", dimensionMessage);
    }
    indexType channels = (dims.length() > 2) ? dims.dimensionLength(2) : 1;
    if (channels != 1 && channels != 3 && channels != 4) {
        Error(L"Nelson:image_processing:invalidImageDimensions", dimensionMessage);
    }
    return { dims.rows(), dims.columns(), channels };
}
//=============================================================================
template <typename T>
inline void
assignClampedPixel(T* outputData, size_t index, double value)
{
    if constexpr (std::is_same<T, uint8_t>::value) {
        int roundedValue = static_cast<int>(std::round(value));
        outputData[index] = static_cast<uint8_t>(std::clamp(roundedValue, 0, 255));
    } else if constexpr (std::is_same<T, uint16_t>::value) {
        int roundedValue = static_cast<int>(std::round(value));
        outputData[index] = static_cast<uint16_t>(std::clamp(roundedValue, 0, 65535));
    } else if constexpr (std::is_same<T, uint32_t>::value) {
        double roundedValue = std::round(value);
        if (roundedValue < 0) {
            outputData[index] = 0;
        } else if (roundedValue > UINT32_MAX) {
            outputData[index] = UINT32_MAX;
        } else {
            outputData[index] = static_cast<uint32_t>(roundedValue);
        }
    } else if constexpr (std::is_same<T, uint64_t>::value) {
        double roundedValue = std::round(value);
        if (roundedValue < 0) {
            outputData[index] = 0;
        } else if (roundedValue > static_cast<double>(UINT64_MAX)) {
            outputData[index] = UINT64_MAX;
        } else {
            outputData[index] = static_cast<uint64_t>(roundedValue);
        }
    } else if constexpr (std::is_integral<T>::value) {
        double roundedValue = std::round(value);
        if (roundedValue < std::numeric_limits<T>::min()) {
            outputData[index] = std::numeric_limits<T>::min();
        } else if (roundedValue > std::numeric_limits<T>::max()) {
            outputData[index] = std::numeric_limits<T>::max();
        } else {
            outputData[index] = static_cast<T>(roundedValue);
        }
    } else {
        outputData[index] = static_cast<T>(value);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
