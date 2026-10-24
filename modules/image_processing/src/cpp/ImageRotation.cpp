//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
#include <type_traits>
#include "ImageRotation.hpp"
#include "ImageProcessingCore.hpp"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
handleNinetyDegreeRotation(const ArrayOf& image, double angle, BoundingBox boundingBox);
//=============================================================================
template <typename T>
inline void
assignOutput(T* outputData, size_t index, double value)
{
    assignClampedPixel(outputData, index, value);
}
//=============================================================================
template <typename T>
void
nearestRotate(const T* inputData, T* outputData, size_t width, size_t height, size_t channels,
    size_t new_width, size_t new_height, double center_x, double center_y, double new_center_x,
    double new_center_y, double cos_angle, double sin_angle, T default_value)
{
    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;

    if (channels == 3) {
        size_t secondPlaneIn = 2 * planeSizeIn;
        size_t secondPlaneOut = 2 * planeSizeOut;
        OMP_PARALLEL_FOR_LOOP(new_width, 4)
        for (signedIndexType xSigned = 0; xSigned < static_cast<signedIndexType>(new_width);
            xSigned++) {
            size_t x = static_cast<size_t>(xSigned);
            double centered_x = static_cast<double>(x) - new_center_x;
            double xCos = centered_x * cos_angle;
            double xSin = centered_x * sin_angle;
            double orig_x = xCos - new_center_y * sin_angle + center_x;
            double orig_y = -xSin - new_center_y * cos_angle + center_y;
            for (size_t y = 0; y < new_height; y++, orig_x += sin_angle, orig_y += cos_angle) {
                size_t dst_base = y + x * new_height;
                if (orig_x >= 0 && orig_x < width && orig_y >= 0 && orig_y < height) {
                    size_t src_x = static_cast<size_t>(orig_x + 0.5);
                    size_t src_y = static_cast<size_t>(orig_y + 0.5);
                    src_x = std::min(src_x, width - 1);
                    src_y = std::min(src_y, height - 1);
                    size_t src_base = src_y + src_x * height;
                    outputData[dst_base] = inputData[src_base];
                    outputData[dst_base + planeSizeOut] = inputData[src_base + planeSizeIn];
                    outputData[dst_base + secondPlaneOut] = inputData[src_base + secondPlaneIn];
                } else {
                    outputData[dst_base] = default_value;
                    outputData[dst_base + planeSizeOut] = default_value;
                    outputData[dst_base + secondPlaneOut] = default_value;
                }
            }
        }
        return;
    }

    if (channels == 1) {
        OMP_PARALLEL_FOR_LOOP(new_width, 4)
        for (signedIndexType xSigned = 0; xSigned < static_cast<signedIndexType>(new_width);
            xSigned++) {
            size_t x = static_cast<size_t>(xSigned);
            double centered_x = static_cast<double>(x) - new_center_x;
            double xCos = centered_x * cos_angle;
            double xSin = centered_x * sin_angle;
            double orig_x = xCos - new_center_y * sin_angle + center_x;
            double orig_y = -xSin - new_center_y * cos_angle + center_y;
            for (size_t y = 0; y < new_height; y++, orig_x += sin_angle, orig_y += cos_angle) {
                size_t dst_base = y + x * new_height;
                if (orig_x >= 0 && orig_x < width && orig_y >= 0 && orig_y < height) {
                    size_t src_x = static_cast<size_t>(orig_x + 0.5);
                    size_t src_y = static_cast<size_t>(orig_y + 0.5);
                    src_x = std::min(src_x, width - 1);
                    src_y = std::min(src_y, height - 1);
                    outputData[dst_base] = inputData[src_y + src_x * height];
                } else {
                    outputData[dst_base] = default_value;
                }
            }
        }
        return;
    }

    OMP_PARALLEL_FOR_LOOP(new_width, 4)
    for (signedIndexType xSigned = 0; xSigned < static_cast<signedIndexType>(new_width);
        xSigned++) {
        size_t x = static_cast<size_t>(xSigned);
        double centered_x = static_cast<double>(x) - new_center_x;
        double xCos = centered_x * cos_angle;
        double xSin = centered_x * sin_angle;
        double orig_x = xCos - new_center_y * sin_angle + center_x;
        double orig_y = -xSin - new_center_y * cos_angle + center_y;
        for (size_t y = 0; y < new_height; y++, orig_x += sin_angle, orig_y += cos_angle) {
            size_t dst_base = y + x * new_height;
            if (orig_x >= 0 && orig_x < width && orig_y >= 0 && orig_y < height) {
                size_t src_x = static_cast<size_t>(orig_x + 0.5);
                size_t src_y = static_cast<size_t>(orig_y + 0.5);
                src_x = std::min(src_x, width - 1);
                src_y = std::min(src_y, height - 1);
                size_t src_base = src_y + src_x * height;
                for (size_t c = 0; c < channels; ++c) {
                    outputData[dst_base + c * planeSizeOut] = inputData[src_base + c * planeSizeIn];
                }
            } else {
                for (size_t c = 0; c < channels; ++c) {
                    outputData[dst_base + c * planeSizeOut] = default_value;
                }
            }
        }
    }
}
//=============================================================================
template <typename T>
void
bilinearRotate(const T* inputData, T* outputData, size_t width, size_t height, size_t channels,
    size_t new_width, size_t new_height, double center_x, double center_y, double new_center_x,
    double new_center_y, double cos_angle, double sin_angle, T default_value)
{
    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;

    OMP_PARALLEL_FOR_LOOP(new_width, 4)
    for (signedIndexType xSigned = 0; xSigned < static_cast<signedIndexType>(new_width);
        xSigned++) {
        size_t x = static_cast<size_t>(xSigned);
        double centered_x = static_cast<double>(x) - new_center_x;
        double xCos = centered_x * cos_angle;
        double xSin = centered_x * sin_angle;
        double orig_x = xCos - new_center_y * sin_angle + center_x;
        double orig_y = -xSin - new_center_y * cos_angle + center_y;
        for (size_t y = 0; y < new_height; y++, orig_x += sin_angle, orig_y += cos_angle) {
            size_t dst_idx = y + x * new_height;
            if (orig_x < 0 || orig_x >= width || orig_y < 0 || orig_y >= height) {
                for (size_t c = 0; c < channels; ++c) {
                    outputData[dst_idx + c * planeSizeOut] = default_value;
                }
                continue;
            }
            int x0 = static_cast<int>(std::floor(orig_x));
            int y0 = static_cast<int>(std::floor(orig_y));
            int x1 = x0 + 1;
            int y1 = y0 + 1;
            x0 = std::clamp(x0, 0, static_cast<int>(width) - 1);
            y0 = std::clamp(y0, 0, static_cast<int>(height) - 1);
            x1 = std::clamp(x1, 0, static_cast<int>(width) - 1);
            y1 = std::clamp(y1, 0, static_cast<int>(height) - 1);
            double wx = orig_x - x0;
            double wy = orig_y - y0;
            size_t idx00 = y0 + x0 * height;
            size_t idx01 = y1 + x0 * height;
            size_t idx10 = y0 + x1 * height;
            size_t idx11 = y1 + x1 * height;
            for (size_t c = 0; c < channels; ++c) {
                size_t channel_offset = c * planeSizeIn;
                double value = (1 - wx) * (1 - wy) * inputData[idx00 + channel_offset]
                    + wx * (1 - wy) * inputData[idx10 + channel_offset]
                    + (1 - wx) * wy * inputData[idx01 + channel_offset]
                    + wx * wy * inputData[idx11 + channel_offset];
                assignOutput(outputData, dst_idx + c * planeSizeOut, value);
            }
        }
    }
}
//=============================================================================
static inline uint8
roundClampUint8(double value)
{
    int roundedValue = static_cast<int>(value + 0.5);
    return static_cast<uint8>(std::clamp(roundedValue, 0, 255));
}
//=============================================================================
static void
bilinearRotateUint8RGB(const uint8* inputData, uint8* outputData, size_t width, size_t height,
    size_t new_width, size_t new_height, double center_x, double center_y, double new_center_x,
    double new_center_y, double cos_angle, double sin_angle)
{
    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;
    size_t secondPlaneIn = 2 * planeSizeIn;
    size_t secondPlaneOut = 2 * planeSizeOut;

    OMP_PARALLEL_FOR_LOOP(new_width, 4)
    for (signedIndexType xSigned = 0; xSigned < static_cast<signedIndexType>(new_width);
        xSigned++) {
        size_t x = static_cast<size_t>(xSigned);
        double centered_x = static_cast<double>(x) - new_center_x;
        double xCos = centered_x * cos_angle;
        double xSin = centered_x * sin_angle;
        double orig_x = xCos - new_center_y * sin_angle + center_x;
        double orig_y = -xSin - new_center_y * cos_angle + center_y;
        for (size_t y = 0; y < new_height; y++, orig_x += sin_angle, orig_y += cos_angle) {
            size_t dst_idx = y + x * new_height;
            if (orig_x < 0 || orig_x >= width || orig_y < 0 || orig_y >= height) {
                outputData[dst_idx] = 0;
                outputData[dst_idx + planeSizeOut] = 0;
                outputData[dst_idx + secondPlaneOut] = 0;
                continue;
            }
            int x0 = static_cast<int>(std::floor(orig_x));
            int y0 = static_cast<int>(std::floor(orig_y));
            int x1 = std::min(x0 + 1, static_cast<int>(width) - 1);
            int y1 = std::min(y0 + 1, static_cast<int>(height) - 1);
            double wx = orig_x - x0;
            double wy = orig_y - y0;
            double w00 = (1 - wx) * (1 - wy);
            double w10 = wx * (1 - wy);
            double w01 = (1 - wx) * wy;
            double w11 = wx * wy;
            size_t idx00 = y0 + x0 * height;
            size_t idx01 = y1 + x0 * height;
            size_t idx10 = y0 + x1 * height;
            size_t idx11 = y1 + x1 * height;
            double value = w00 * inputData[idx00] + w10 * inputData[idx10] + w01 * inputData[idx01]
                + w11 * inputData[idx11];
            outputData[dst_idx] = roundClampUint8(value);
            value = w00 * inputData[idx00 + planeSizeIn] + w10 * inputData[idx10 + planeSizeIn]
                + w01 * inputData[idx01 + planeSizeIn] + w11 * inputData[idx11 + planeSizeIn];
            outputData[dst_idx + planeSizeOut] = roundClampUint8(value);
            value = w00 * inputData[idx00 + secondPlaneIn] + w10 * inputData[idx10 + secondPlaneIn]
                + w01 * inputData[idx01 + secondPlaneIn] + w11 * inputData[idx11 + secondPlaneIn];
            outputData[dst_idx + secondPlaneOut] = roundClampUint8(value);
        }
    }
}
//=============================================================================
static void
bilinearRotateUint8RGBA(const uint8* inputData, uint8* outputData, size_t width, size_t height,
    size_t new_width, size_t new_height, double center_x, double center_y, double new_center_x,
    double new_center_y, double cos_angle, double sin_angle)
{
    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;
    size_t secondPlaneIn = 2 * planeSizeIn;
    size_t secondPlaneOut = 2 * planeSizeOut;
    size_t thirdPlaneIn = 3 * planeSizeIn;
    size_t thirdPlaneOut = 3 * planeSizeOut;

    OMP_PARALLEL_FOR_LOOP(new_width, 4)
    for (signedIndexType xSigned = 0; xSigned < static_cast<signedIndexType>(new_width);
        xSigned++) {
        size_t x = static_cast<size_t>(xSigned);
        double centered_x = static_cast<double>(x) - new_center_x;
        double xCos = centered_x * cos_angle;
        double xSin = centered_x * sin_angle;
        double orig_x = xCos - new_center_y * sin_angle + center_x;
        double orig_y = -xSin - new_center_y * cos_angle + center_y;
        for (size_t y = 0; y < new_height; y++, orig_x += sin_angle, orig_y += cos_angle) {
            size_t dst_idx = y + x * new_height;
            if (orig_x < 0 || orig_x >= width || orig_y < 0 || orig_y >= height) {
                outputData[dst_idx] = 0;
                outputData[dst_idx + planeSizeOut] = 0;
                outputData[dst_idx + secondPlaneOut] = 0;
                outputData[dst_idx + thirdPlaneOut] = 0;
                continue;
            }
            int x0 = static_cast<int>(std::floor(orig_x));
            int y0 = static_cast<int>(std::floor(orig_y));
            int x1 = std::min(x0 + 1, static_cast<int>(width) - 1);
            int y1 = std::min(y0 + 1, static_cast<int>(height) - 1);
            double wx = orig_x - x0;
            double wy = orig_y - y0;
            double w00 = (1 - wx) * (1 - wy);
            double w10 = wx * (1 - wy);
            double w01 = (1 - wx) * wy;
            double w11 = wx * wy;
            size_t idx00 = y0 + x0 * height;
            size_t idx01 = y1 + x0 * height;
            size_t idx10 = y0 + x1 * height;
            size_t idx11 = y1 + x1 * height;
            double value = w00 * inputData[idx00] + w10 * inputData[idx10] + w01 * inputData[idx01]
                + w11 * inputData[idx11];
            outputData[dst_idx] = roundClampUint8(value);
            value = w00 * inputData[idx00 + planeSizeIn] + w10 * inputData[idx10 + planeSizeIn]
                + w01 * inputData[idx01 + planeSizeIn] + w11 * inputData[idx11 + planeSizeIn];
            outputData[dst_idx + planeSizeOut] = roundClampUint8(value);
            value = w00 * inputData[idx00 + secondPlaneIn] + w10 * inputData[idx10 + secondPlaneIn]
                + w01 * inputData[idx01 + secondPlaneIn] + w11 * inputData[idx11 + secondPlaneIn];
            outputData[dst_idx + secondPlaneOut] = roundClampUint8(value);
            value = w00 * inputData[idx00 + thirdPlaneIn] + w10 * inputData[idx10 + thirdPlaneIn]
                + w01 * inputData[idx01 + thirdPlaneIn] + w11 * inputData[idx11 + thirdPlaneIn];
            outputData[dst_idx + thirdPlaneOut] = roundClampUint8(value);
        }
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

    auto cubicWeight = [](double x) {
        x = std::abs(x);
        if (x <= 1.0) {
            return 1.5 * x * x * x - 2.5 * x * x + 1.0;
        } else if (x < 2.0) {
            return -0.5 * x * x * x + 2.5 * x * x - 4.0 * x + 2.0;
        }
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
                int x0 = static_cast<int>(std::floor(orig_x));
                int y0 = static_cast<int>(std::floor(orig_y));
                int x1 = x0 + 1;
                int y1 = y0 + 1;

                x0 = std::clamp(x0, 0, static_cast<int>(width) - 1);
                y0 = std::clamp(y0, 0, static_cast<int>(height) - 1);
                x1 = std::clamp(x1, 0, static_cast<int>(width) - 1);
                y1 = std::clamp(y1, 0, static_cast<int>(height) - 1);

                double wx = orig_x - x0;
                double wy = orig_y - y0;

                size_t dst_idx = y + x * new_height;
                for (size_t c = 0; c < channels; ++c) {
                    size_t channel_offset = c * width * height;
                    size_t idx00 = y0 + x0 * height + channel_offset;
                    size_t idx01 = y1 + x0 * height + channel_offset;
                    size_t idx10 = y0 + x1 * height + channel_offset;
                    size_t idx11 = y1 + x1 * height + channel_offset;

                    double value = (1 - wx) * (1 - wy) * inputData[idx00]
                        + wx * (1 - wy) * inputData[idx10] + (1 - wx) * wy * inputData[idx01]
                        + wx * wy * inputData[idx11];

                    assignOutput(outputData, dst_idx + c * new_width * new_height, value);
                }
            } else if (method == InterpolationMethod::Bicubic) {
                int x0 = static_cast<int>(std::floor(orig_x - 1.0));
                int y0 = static_cast<int>(std::floor(orig_y - 1.0));

                double fx = orig_x - std::floor(orig_x);
                double fy = orig_y - std::floor(orig_y);

                size_t dst_idx = y + x * new_height;

                for (size_t c = 0; c < channels; ++c) {
                    double value = 0.0;
                    double weightSum = 0.0;

                    for (int ky = 0; ky < 4; ky++) {
                        int yi = y0 + ky;

                        if (yi < 0 || yi >= static_cast<int>(height)) {
                            continue;
                        }

                        double wy = cubicWeight(fy - (ky - 1));

                        for (int kx = 0; kx < 4; kx++) {
                            int xi = x0 + kx;

                            if (xi < 0 || xi >= static_cast<int>(width)) {
                                continue;
                            }

                            double wx = cubicWeight(fx - (kx - 1));
                            double weight = wx * wy;

                            size_t src_idx = yi + xi * height + c * width * height;
                            value += weight * static_cast<double>(inputData[src_idx]);
                            weightSum += weight;
                        }
                    }

                    if (weightSum > 0.0) {
                        value /= weightSum;
                    }

                    assignOutput(outputData, dst_idx + c * new_width * new_height, value);
                }
            }
        } else {
            size_t dst_base = y + x * new_height;
            for (size_t c = 0; c < channels; ++c) {
                outputData[dst_base + c * new_width * new_height] = default_value;
            }
        }
    }
}
//=============================================================================
template <typename T>
static void
rotateTyped(const ArrayOf& image, ArrayOf& outputImage, size_t width, size_t height,
    size_t channels, size_t new_width, size_t new_height, double center_x, double center_y,
    double new_center_x, double new_center_y, double cos_angle, double sin_angle,
    InterpolationMethod method, T default_value)
{
    const T* input = (const T*)image.dataPointer();
    T* output = (T*)outputImage.dataPointer();
    if (method == InterpolationMethod::Nearest) {
        nearestRotate<T>(input, output, width, height, channels, new_width, new_height, center_x,
            center_y, new_center_x, new_center_y, cos_angle, sin_angle, default_value);
    } else if (method == InterpolationMethod::Bilinear) {
        if constexpr (std::is_same<T, uint8>::value) {
            if (channels == 3) {
                bilinearRotateUint8RGB(input, output, width, height, new_width, new_height,
                    center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle);
                return;
            }
            if (channels == 4) {
                bilinearRotateUint8RGBA(input, output, width, height, new_width, new_height,
                    center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle);
                return;
            }
        }
        bilinearRotate<T>(input, output, width, height, channels, new_width, new_height, center_x,
            center_y, new_center_x, new_center_y, cos_angle, sin_angle, default_value);
    } else {
        interpolateAndRotate<T>(input, output, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method,
            default_value);
    }
}
//=============================================================================
template <typename T>
std::vector<double>
generateCubicKernel(double x)
{
    std::vector<double> kernel(4);
    const double a = -0.5; // Commonly used value for Catmull-Rom spline

    x = std::abs(x);
    double x2 = x * x;
    double x3 = x2 * x;

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
    Dimensions dims = image.dimensions();
    size_t height = dims.rows();
    size_t width = dims.columns();
    size_t channels = (dims.length() > 2) ? dims.dimensionLength(2) : 1;

    bool isMultipleOf90 = std::abs(std::fmod(angle, 90.0)) == 0.0;
    bool safeCropRightAngle = boundingBox == BoundingBox::Crop
        && (height == width || std::abs(std::fmod(angle, 180.0)) == 0.0);
    if (isMultipleOf90 && (boundingBox != BoundingBox::Crop || safeCropRightAngle)) {
        return handleNinetyDegreeRotation(image, angle, boundingBox);
    }
    angle = std::fmod(angle, 360.0);
    if (angle < 0) {
        angle += 360.0;
    }
    double radians = -angle * M_PI / 180.0;
    double cos_angle = cos(radians);
    double sin_angle = sin(radians);

    size_t new_height, new_width;

    if (boundingBox == BoundingBox::Loose) {
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
        new_width = width;
        new_height = height;
    }

    ArrayOf outputImage;
    Dimensions outputDims;

    if (channels == 1) {
        outputDims = Dimensions((indexType)new_height, (indexType)new_width);
    } else {
        std::vector<indexType> dimsAsVector
            = { (indexType)new_height, (indexType)new_width, (indexType)channels };
        outputDims = Dimensions(dimsAsVector);
    }

    NelsonType dataClass = image.dataClass();
    outputImage = ArrayOf(
        dataClass, outputDims, ArrayOf::allocateArrayOf(dataClass, outputDims.elementCount()));

    double center_x = width / 2.0;
    double center_y = height / 2.0;
    double new_center_x = new_width / 2.0;
    double new_center_y = new_height / 2.0;

    switch (dataClass) {
    case NLS_DOUBLE: {
        rotateTyped<double>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0.0);
    } break;
    case NLS_SINGLE: {
        rotateTyped<single>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method,
            (single)0);
    } break;
    case NLS_INT8: {
        rotateTyped<int8>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_INT16: {
        rotateTyped<int16>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_INT32: {
        rotateTyped<int32>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_INT64: {
        rotateTyped<int64>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT8: {
        rotateTyped<uint8>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT16: {
        rotateTyped<uint16>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT32: {
        rotateTyped<uint32>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_UINT64: {
        rotateTyped<uint64>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method, 0);
    } break;
    case NLS_LOGICAL: {
        rotateTyped<logical>(image, outputImage, width, height, channels, new_width, new_height,
            center_x, center_y, new_center_x, new_center_y, cos_angle, sin_angle, method,
            (logical)0);
    } break;
    default: {
        Error(L"Nelson:image_processing:typeNotSupported", L"Type not supported.");
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
    size_t width = dims.columns();
    size_t height = dims.rows();
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
    Dimensions dims = image.dimensions();
    size_t height = dims.rows();
    size_t width = dims.columns();
    size_t channels = (dims.length() > 2) ? dims.dimensionLength(2) : 1;

    int multiple_of_ninety = static_cast<int>(std::round(-angle / 90.0)) % 4;
    if (multiple_of_ninety < 0) {
        multiple_of_ninety += 4;
    }

    if (multiple_of_ninety == 0) {
        return image;
    }

    NelsonType dataClass = image.dataClass();
    size_t planeSize = width * height;

    if (multiple_of_ninety == 2) {
        switch (dataClass) {
        case NLS_DOUBLE:
            return flip180<double>(
                dataClass, dims, channels, planeSize, (double*)image.dataPointer());
        case NLS_SINGLE:
            return flip180<single>(
                dataClass, dims, channels, planeSize, (single*)image.dataPointer());
        case NLS_INT8:
            return flip180<int8>(dataClass, dims, channels, planeSize, (int8*)image.dataPointer());
        case NLS_INT16:
            return flip180<int16>(
                dataClass, dims, channels, planeSize, (int16*)image.dataPointer());
        case NLS_INT32:
            return flip180<int32>(
                dataClass, dims, channels, planeSize, (int32*)image.dataPointer());
        case NLS_INT64:
            return flip180<int64>(
                dataClass, dims, channels, planeSize, (int64*)image.dataPointer());
        case NLS_UINT8:
            return flip180<uint8>(
                dataClass, dims, channels, planeSize, (uint8*)image.dataPointer());
        case NLS_UINT16:
            return flip180<uint16>(
                dataClass, dims, channels, planeSize, (uint16*)image.dataPointer());
        case NLS_UINT32:
            return flip180<uint32>(
                dataClass, dims, channels, planeSize, (uint32*)image.dataPointer());
        case NLS_UINT64:
            return flip180<uint64>(
                dataClass, dims, channels, planeSize, (uint64*)image.dataPointer());
        case NLS_LOGICAL:
            return flip180<logical>(
                dataClass, dims, channels, planeSize, (logical*)image.dataPointer());
        default: {
            Error(L"Nelson:image_processing:typeNotSupported", L"Type not supported.");
            return image;
        }
        }
    }

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

            if (multiple_of_ninety == 1) {
                new_x = height - 1 - y;
                new_y = x;
            } else if (multiple_of_ninety == 3) {
                new_x = y;
                new_y = width - 1 - x;
            }
            size_t dst_idx = new_y + new_x * new_height + c * new_width * new_height;
            outputData[dst_idx] = inputData[i];
        }
    };
    switch (dataClass) {
    case NLS_DOUBLE:
        rotate((double*)image.dataPointer(), (double*)result.dataPointer());
        break;
    case NLS_SINGLE:
        rotate((single*)image.dataPointer(), (single*)result.dataPointer());
        break;
    case NLS_INT8:
        rotate((int8*)image.dataPointer(), (int8*)result.dataPointer());
        break;
    case NLS_INT16:
        rotate((int16*)image.dataPointer(), (int16*)result.dataPointer());
        break;
    case NLS_INT32:
        rotate((int32*)image.dataPointer(), (int32*)result.dataPointer());
        break;
    case NLS_INT64:
        rotate((int64*)image.dataPointer(), (int64*)result.dataPointer());
        break;
    case NLS_UINT8:
        rotate((uint8_t*)image.dataPointer(), (uint8_t*)result.dataPointer());
        break;
    case NLS_UINT16:
        rotate((uint16_t*)image.dataPointer(), (uint16_t*)result.dataPointer());
        break;
    case NLS_UINT32:
        rotate((uint32_t*)image.dataPointer(), (uint32_t*)result.dataPointer());
        break;
    case NLS_UINT64:
        rotate((uint64_t*)image.dataPointer(), (uint64_t*)result.dataPointer());
        break;
    case NLS_LOGICAL:
        rotate((logical*)image.dataPointer(), (logical*)result.dataPointer());
        break;
    default:
        Error(L"Nelson:image_processing:typeNotSupported", L"Type not supported.");
        break;
    }

    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
