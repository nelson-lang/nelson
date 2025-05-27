//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _USE_MATH_DEFINES
#endif
//=============================================================================
#include <cmath>
#include "ImageRotation.hpp"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
handleNinetyDegreeRotation(const ArrayOf& image, double angle, BoundingBox boundingBox);
//=============================================================================
// Helper template function to handle type-specific output assignment
template <typename T>
inline void
assignOutput(T* outputData, size_t index, double value)
{
    if constexpr (std::is_same<T, uint8_t>::value) {
        // For uint8, clamp to [0, 255]
        int rounded_value = static_cast<int>(std::round(value));
        outputData[index] = static_cast<uint8_t>(std::clamp(rounded_value, 0, 255));
    } else if constexpr (std::is_same<T, uint16_t>::value) {
        // For uint16, clamp to [0, 65535]
        int rounded_value = static_cast<int>(std::round(value));
        outputData[index] = static_cast<uint16_t>(std::clamp(rounded_value, 0, 65535));
    } else if constexpr (std::is_same<T, uint32_t>::value) {
        // For uint32, clamp to [0, UINT32_MAX]
        double rounded_value = std::round(value);
        if (rounded_value < 0) {
            outputData[index] = 0;
        } else if (rounded_value > UINT32_MAX) {
            outputData[index] = UINT32_MAX;
        } else {
            outputData[index] = static_cast<uint32_t>(rounded_value);
        }
    } else if constexpr (std::is_same<T, uint64_t>::value) {
        // For uint64, clamp to [0, UINT64_MAX]
        double rounded_value = std::round(value);
        if (rounded_value < 0) {
            outputData[index] = 0;
        } else if (rounded_value > static_cast<double>(UINT64_MAX)) {
            outputData[index] = UINT64_MAX;
        } else {
            outputData[index] = static_cast<uint64_t>(rounded_value);
        }
    } else if constexpr (std::is_integral<T>::value) {
        // For other integer types (signed), clamp to type limits
        double rounded_value = std::round(value);
        if (rounded_value < std::numeric_limits<T>::min()) {
            outputData[index] = std::numeric_limits<T>::min();
        } else if (rounded_value > std::numeric_limits<T>::max()) {
            outputData[index] = std::numeric_limits<T>::max();
        } else {
            outputData[index] = static_cast<T>(rounded_value);
        }
    } else {
        // For floating point types, direct assignment
        outputData[index] = static_cast<T>(value);
    }
}
//=============================================================================
template <typename T>
void
interpolateAndRotate(const T* inputData, T* outputData, size_t width, size_t height,
    size_t channels, size_t new_width, size_t new_height, double center_x, double center_y,
    double new_center_x, double new_center_y, double cos_angle, double sin_angle,
    InterpolationMethod method, T default_value)
{
    size_t totalPixels = new_height * new_width;

    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;

    // Precompute constants for bicubic interpolation
    auto cubicWeight = [](double x) {
        x = std::abs(x);
        if (x <= 1.0)
            return 1.5 * x * x * x - 2.5 * x * x + 1.0;
        else if (x < 2.0)
            return -0.5 * x * x * x + 2.5 * x * x - 4.0 * x + 2.0;
        return 0.0;
    };

    OMP_PARALLEL_FOR_LOOP(totalPixels, 4)
    for (size_t i = 0; i < totalPixels; i++) {
        size_t x = i / new_height;
        size_t y = i % new_height;

        double centered_x = x - new_center_x;
        double centered_y = y - new_center_y;

        double orig_x = centered_x * cos_angle + centered_y * sin_angle + center_x;
        double orig_y = -centered_x * sin_angle + centered_y * cos_angle + center_y;

        if (orig_x >= 0 && orig_x < width && orig_y >= 0 && orig_y < height) {
            if (method == InterpolationMethod::Nearest) {
                // Nearest Neighbor implementation - optimized
                size_t src_x = static_cast<size_t>(std::round(orig_x));
                size_t src_y = static_cast<size_t>(std::round(orig_y));
                src_x = std::min(src_x, width - 1);
                src_y = std::min(src_y, height - 1);

                size_t src_base = src_y + src_x * height;
                size_t dst_base = y + x * new_height;

                for (size_t c = 0; c < channels; ++c) {
                    outputData[dst_base + c * planeSizeOut] = inputData[src_base + c * planeSizeIn];
                }
            } else if (method == InterpolationMethod::Bilinear) {
                // Bilinear implementation - with improved clarity
                int x0 = static_cast<int>(std::floor(orig_x));
                int y0 = static_cast<int>(std::floor(orig_y));
                int x1 = x0 + 1;
                int y1 = y0 + 1;

                // Clamp coordinates to valid range
                x0 = std::clamp(x0, 0, static_cast<int>(width) - 1);
                y0 = std::clamp(y0, 0, static_cast<int>(height) - 1);
                x1 = std::clamp(x1, 0, static_cast<int>(width) - 1);
                y1 = std::clamp(y1, 0, static_cast<int>(height) - 1);

                // Calculate interpolation weights
                double wx = orig_x - x0;
                double wy = orig_y - y0;

                // Calculate destination index once
                size_t dst_idx = y + x * new_height;
                for (size_t c = 0; c < channels; ++c) {
                    size_t channel_offset = c * width * height;
                    size_t idx00 = y0 + x0 * height + channel_offset;
                    size_t idx01 = y1 + x0 * height + channel_offset;
                    size_t idx10 = y0 + x1 * height + channel_offset;
                    size_t idx11 = y1 + x1 * height + channel_offset;

                    // Perform bilinear interpolation
                    double value = (1 - wx) * (1 - wy) * inputData[idx00]
                        + wx * (1 - wy) * inputData[idx10] + (1 - wx) * wy * inputData[idx01]
                        + wx * wy * inputData[idx11];

                    // Type-specific handling
                    assignOutput(outputData, dst_idx + c * new_width * new_height, value);
                }
            } else if (method == InterpolationMethod::Bicubic) {
                // True Bicubic implementation
                int x0 = static_cast<int>(std::floor(orig_x - 1.0));
                int y0 = static_cast<int>(std::floor(orig_y - 1.0));

                double fx = orig_x - std::floor(orig_x);
                double fy = orig_y - std::floor(orig_y);

                size_t dst_idx = y + x * new_height;

                for (size_t c = 0; c < channels; ++c) {
                    double value = 0.0;
                    double weightSum = 0.0;

                    // 4x4 pixel neighborhood
                    for (int ky = 0; ky < 4; ky++) {
                        int yi = y0 + ky;

                        // Skip if y is out of bounds
                        if (yi < 0 || yi >= static_cast<int>(height))
                            continue;

                        double wy = cubicWeight(fy - (ky - 1));

                        for (int kx = 0; kx < 4; kx++) {
                            int xi = x0 + kx;

                            // Skip if x is out of bounds
                            if (xi < 0 || xi >= static_cast<int>(width))
                                continue;

                            double wx = cubicWeight(fx - (kx - 1));
                            double weight = wx * wy;

                            // Use clamped coordinates and add weighted contribution
                            size_t src_idx = yi + xi * height + c * width * height;
                            value += weight * static_cast<double>(inputData[src_idx]);
                            weightSum += weight;
                        }
                    }

                    // Normalize by weight sum to handle edge cases
                    if (weightSum > 0.0) {
                        value /= weightSum;
                    }

                    assignOutput(outputData, dst_idx + c * new_width * new_height, value);
                }
            }
        } else {
            // Outside the original image - fill with default value
            size_t dst_base = y + x * new_height;
            for (size_t c = 0; c < channels; ++c) {
                outputData[dst_base + c * new_width * new_height] = default_value;
            }
        }
    }
}
//=============================================================================
// Function to generate a 1D cubic interpolation kernel
template <typename T>
std::vector<double>
generateCubicKernel(double x)
{
    std::vector<double> kernel(4);
    const double a = -0.5; // Commonly used value for Catmull-Rom spline

    x = std::abs(x);
    double x2 = x * x;
    double x3 = x2 * x;

    // Catmull-Rom cubic kernel
    if (x < 1) {
        kernel[0] = a * x3 - 2 * a * x2 + a;
        kernel[1] = (a + 2) * x3 - (a + 3) * x2 + 1;
        kernel[2] = -(a + 2) * x3 + (2 * a + 3) * x2 - a;
        kernel[3] = -a * x3 + a * x2;
    } else if (x < 2) {
        kernel[0] = -a * x3 + 5 * a * x2 - 8 * a * x + 4 * a;
        kernel[1] = (a + 2) * x3 - (8 * a + 12) * x2 + (10 * a + 12) * x - 4 * a - 2;
        kernel[2] = -(a + 2) * x3 + (7 * a + 8) * x2 - (9 * a + 6) * x + 3 * a + 1;
        kernel[3] = a * x3 - 2 * a * x2 + a * x;
    } else {
        kernel[0] = kernel[1] = kernel[2] = kernel[3] = 0;
    }

    return kernel;
}
//=============================================================================
ArrayOf
ImageRotation(
    const ArrayOf& image, double angle, InterpolationMethod method, BoundingBox boundingBox)
{
    Dimensions dims = image.getDimensions();
    size_t height = dims.getRows();
    size_t width = dims.getColumns();
    size_t channels = (dims.getLength() > 2) ? dims.getDimensionLength(2) : 1;

    bool isMultipleOf90 = std::abs(std::fmod(angle, 90.0)) == 0.0;
    if (isMultipleOf90 && boundingBox != BoundingBox::Crop) {
        return handleNinetyDegreeRotation(image, angle, boundingBox);
    }
    angle = std::fmod(angle, 360.0);
    if (angle < 0) {
        angle += 360.0;
    }
    // Convert angle to radians
    double radians = -angle * M_PI / 180.0;
    double cos_angle = cos(radians);
    double sin_angle = sin(radians);

    // Calculate output dimensions for "loose" bounding box
    size_t new_height, new_width;

    if (boundingBox == BoundingBox::Loose) {
        // Calculate corners of the rotated image
        double corners[4][2] = { { -(double)width / 2.0, -(double)height / 2.0 },
            { (double)width / 2.0, -(double)height / 2.0 },
            { (double)width / 2.0, (double)height / 2.0 },
            { -(double)width / 2.0, (double)height / 2.0 } };

        double min_x = 0, min_y = 0, max_x = 0, max_y = 0;
        for (int i = 0; i < 4; i++) {
            double x = corners[i][0] * cos_angle - corners[i][1] * sin_angle;
            double y = corners[i][0] * sin_angle + corners[i][1] * cos_angle;

            min_x = std::min(min_x, x);
            max_x = std::max(max_x, x);
            min_y = std::min(min_y, y);
            max_y = std::max(max_y, y);
        }

        new_width = static_cast<size_t>(ceil(max_x - min_x));
        new_height = static_cast<size_t>(ceil(max_y - min_y));
    } else {
        // "crop" case - keep original dimensions
        new_width = width;
        new_height = height;
    }

    // Create output array with the appropriate type
    ArrayOf outputImage;
    Dimensions outputDims;

    if (channels == 1) {
        outputDims = Dimensions((indexType)new_height, (indexType)new_width);
    } else {
        std::vector<indexType> dimsAsVector
            = { (indexType)new_height, (indexType)new_width, (indexType)channels };
        outputDims = Dimensions(dimsAsVector);
    }

    NelsonType dataClass = image.getDataClass();
    outputImage = ArrayOf(
        dataClass, outputDims, ArrayOf::allocateArrayOf(dataClass, outputDims.getElementCount()));

    // Center coordinates
    double center_x = width / 2.0;
    double center_y = height / 2.0;
    double new_center_x = new_width / 2.0;
    double new_center_y = new_height / 2.0;

    // Direct pixel manipulation based on data type
    switch (dataClass) {
    case NLS_DOUBLE: {
        interpolateAndRotate<double>((const double*)image.getDataPointer(),
            (double*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0.0);
    } break;
    case NLS_SINGLE: {
        interpolateAndRotate<single>((const single*)image.getDataPointer(),
            (single*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0.0);
    } break;
    case NLS_INT8: {
        interpolateAndRotate<int8>((const int8*)image.getDataPointer(),
            (int8*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);

    } break;
    case NLS_INT16: {
        interpolateAndRotate<int16>((const int16*)image.getDataPointer(),
            (int16*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_INT32: {
        interpolateAndRotate<int32>((const int32*)image.getDataPointer(),
            (int32*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_INT64: {
        interpolateAndRotate<int64>((const int64*)image.getDataPointer(),
            (int64*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT8: {
        interpolateAndRotate<uint8>((const uint8*)image.getDataPointer(),
            (uint8*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT16: {
        interpolateAndRotate<uint16>((const uint16*)image.getDataPointer(),
            (uint16*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT32: {
        interpolateAndRotate<uint32>((const uint32*)image.getDataPointer(),
            (uint32*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT64: {
        interpolateAndRotate<uint64>((const uint64*)image.getDataPointer(),
            (uint64*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_LOGICAL: {
        interpolateAndRotate<logical>((const logical*)image.getDataPointer(),
            (logical*)outputImage.getDataPointer(), width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }

    return outputImage;
}
//=============================================================================
template <typename T>
ArrayOf
flip180(NelsonType nelsonType, const Dimensions& dims, size_t channels, size_t planeSize, T* data)
{
    size_t total = planeSize * channels;
    T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, total);
    size_t width = dims.getColumns();
    size_t height = dims.getRows();
    OMP_PARALLEL_FOR_LOOP(total, 4)
    for (size_t i = 0; i < total; ++i) {
        size_t c = i / planeSize;
        size_t offset = i % planeSize;
        size_t y = offset % height;
        size_t x = offset / height;
        size_t new_y = height - 1 - y;
        size_t new_x = width - 1 - x;
        size_t dst_offset = new_x * height + new_y;
        ptr[c * planeSize + dst_offset] = data[i];
    }
    return ArrayOf(nelsonType, dims, ptr);
}
//=============================================================================
static ArrayOf
handleNinetyDegreeRotation(const ArrayOf& image, double angle, BoundingBox boundingBox)
{
    Dimensions dims = image.getDimensions();
    size_t height = dims.getRows();
    size_t width = dims.getColumns();
    size_t channels = (dims.getLength() > 2) ? dims.getDimensionLength(2) : 1;

    int multiple_of_ninety = static_cast<int>(std::round(-angle / 90.0)) % 4;
    if (multiple_of_ninety < 0) {
        multiple_of_ninety += 4;
    }

    if (multiple_of_ninety == 0) {
        return image; // No rotation needed
    }

    NelsonType dataClass = image.getDataClass();
    size_t planeSize = width * height;

    // Handle 180° rotation
    if (multiple_of_ninety == 2) {
        switch (dataClass) {
        case NLS_DOUBLE:
            return flip180<double>(
                dataClass, dims, channels, planeSize, (double*)image.getDataPointer());
        case NLS_SINGLE:
            return flip180<single>(
                dataClass, dims, channels, planeSize, (single*)image.getDataPointer());
        case NLS_INT8:
            return flip180<int8>(
                dataClass, dims, channels, planeSize, (int8*)image.getDataPointer());
        case NLS_INT16:
            return flip180<int16>(
                dataClass, dims, channels, planeSize, (int16*)image.getDataPointer());
        case NLS_INT32:
            return flip180<int32>(
                dataClass, dims, channels, planeSize, (int32*)image.getDataPointer());
        case NLS_INT64:
            return flip180<int64>(
                dataClass, dims, channels, planeSize, (int64*)image.getDataPointer());
        case NLS_UINT8:
            return flip180<uint8>(
                dataClass, dims, channels, planeSize, (uint8*)image.getDataPointer());
        case NLS_UINT16:
            return flip180<uint16>(
                dataClass, dims, channels, planeSize, (uint16*)image.getDataPointer());
        case NLS_UINT32:
            return flip180<uint32>(
                dataClass, dims, channels, planeSize, (uint32*)image.getDataPointer());
        case NLS_UINT64:
            return flip180<uint64>(
                dataClass, dims, channels, planeSize, (uint64*)image.getDataPointer());
        case NLS_LOGICAL:
            return flip180<logical>(
                dataClass, dims, channels, planeSize, (logical*)image.getDataPointer());
        default: {
            Error(_W("Type not managed."));
            return image;
        }
        }
    }

    // Determine new dimensions for 90° or 270°
    size_t new_width = (boundingBox == BoundingBox::Crop && height != width) ? width : height;
    size_t new_height = (boundingBox == BoundingBox::Crop && height != width) ? height : width;

    std::vector<indexType> newDims = (channels > 1)
        ? std::vector<indexType> { (indexType)new_height, (indexType)new_width,
              (indexType)channels }
        : std::vector<indexType> { (indexType)new_height, (indexType)new_width };

    ArrayOf result = ArrayOf(dataClass, Dimensions(newDims),
        ArrayOf::allocateArrayOf(dataClass, new_height * new_width * channels));

    auto rotate = [&](auto* inputData, auto* outputData) {
        size_t planeSize = height * width;
        size_t total = planeSize * channels;

        OMP_PARALLEL_FOR_LOOP(total, 4)
        for (size_t i = 0; i < total; ++i) {
            size_t c = i / planeSize;
            size_t offset = i % planeSize;
            size_t y = offset % height;
            size_t x = offset / height;

            size_t new_x = 0, new_y = 0;

            if (multiple_of_ninety == 1) { // 90°
                new_x = height - 1 - y;
                new_y = x;
            } else if (multiple_of_ninety == 3) { // 270°
                new_x = y;
                new_y = width - 1 - x;
            }
            size_t dst_idx = new_y + new_x * new_height + c * new_width * new_height;
            outputData[dst_idx] = inputData[i];
        }
    };
    // Switch by type
    switch (dataClass) {
    case NLS_DOUBLE:
        rotate((double*)image.getDataPointer(), (double*)result.getDataPointer());
        break;
    case NLS_SINGLE:
        rotate((single*)image.getDataPointer(), (single*)result.getDataPointer());
        break;
    case NLS_INT8:
        rotate((int8*)image.getDataPointer(), (int8*)result.getDataPointer());
        break;
    case NLS_INT16:
        rotate((int16*)image.getDataPointer(), (int16*)result.getDataPointer());
        break;
    case NLS_INT32:
        rotate((int32*)image.getDataPointer(), (int32*)result.getDataPointer());
        break;
    case NLS_INT64:
        rotate((int64*)image.getDataPointer(), (int64*)result.getDataPointer());
        break;
    case NLS_UINT8:
        rotate((uint8_t*)image.getDataPointer(), (uint8_t*)result.getDataPointer());
        break;
    case NLS_UINT16:
        rotate((uint16_t*)image.getDataPointer(), (uint16_t*)result.getDataPointer());
        break;
    case NLS_UINT32:
        rotate((uint32_t*)image.getDataPointer(), (uint32_t*)result.getDataPointer());
        break;
    case NLS_UINT64:
        rotate((uint64_t*)image.getDataPointer(), (uint64_t*)result.getDataPointer());
        break;
    case NLS_LOGICAL:
        rotate((logical*)image.getDataPointer(), (logical*)result.getDataPointer());
        break;
    default:
        Error(_W("Type not managed."));
        break;
    }

    return result;
}
//=============================================================================
}
//=============================================================================
