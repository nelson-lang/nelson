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
#include <algorithm>
#include <vector>
#include <functional>
#include "ImageResize.hpp"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
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
// Lanczos kernel function
inline double
lanczosKernel(double x, int a)
{
    if (x == 0.0)
        return 1.0;
    if (std::abs(x) >= a)
        return 0.0;

    double pix = M_PI * x;
    return a * std::sin(pix) * std::sin(pix / a) / (pix * pix);
}
//=============================================================================
// Cubic interpolation kernel (Catmull-Rom)
inline double
cubicKernel(double x)
{
    x = std::abs(x);
    if (x <= 1.0) {
        return 1.5 * x * x * x - 2.5 * x * x + 1.0;
    } else if (x < 2.0) {
        return -0.5 * x * x * x + 2.5 * x * x - 4.0 * x + 2.0;
    }
    return 0.0;
}
//=============================================================================
// Box filter kernel
inline double
boxKernel(double x)
{
    return (std::abs(x) <= 0.5) ? 1.0 : 0.0;
}
//=============================================================================
// Bilinear kernel
inline double
bilinearKernel(double x)
{
    x = std::abs(x);
    return (x <= 1.0) ? (1.0 - x) : 0.0;
}
//=============================================================================
// Structure to hold pixel contribution information
struct ResizeContribution
{
    int pixel;
    double weight;
};

struct ResizeContributions
{
    std::vector<ResizeContribution> contributions;
    double totalWeight;
};
//=============================================================================
// Pre-compute contributions for a single dimension
std::vector<ResizeContributions>
computeContributions(size_t srcSize, size_t dstSize, ResizeInterpolationMethod method,
    bool useAntialiasing, double scale)
{
    std::vector<ResizeContributions> contributions(dstSize);

    // Determine kernel radius and function
    int kernelRadius = 1;
    std::function<double(double)> kernelFunc;

    switch (method) {
    case ResizeInterpolationMethod::Bilinear:
        kernelRadius = 1;
        kernelFunc = bilinearKernel;
        break;
    case ResizeInterpolationMethod::Bicubic:
        kernelRadius = 2;
        kernelFunc = cubicKernel;
        break;
    case ResizeInterpolationMethod::Box:
        kernelRadius = 1;
        kernelFunc = boxKernel;
        break;
    case ResizeInterpolationMethod::Lanczos2:
        kernelRadius = 2;
        kernelFunc = [](double x) { return lanczosKernel(x, 2); };
        break;
    case ResizeInterpolationMethod::Lanczos3:
        kernelRadius = 3;
        kernelFunc = [](double x) { return lanczosKernel(x, 3); };
        break;
    default:
        kernelRadius = 1;
        kernelFunc = bilinearKernel;
    }

    // Adjust kernel radius for antialiasing
    if (useAntialiasing && scale > 1.0) {
        kernelRadius = static_cast<int>(std::ceil(kernelRadius * scale));
    }

    for (size_t dst = 0; dst < dstSize; ++dst) {
        double center = (dst + 0.5) * scale - 0.5;
        int left = static_cast<int>(std::floor(center)) - kernelRadius + 1;
        int right = static_cast<int>(std::floor(center)) + kernelRadius;

        contributions[dst].totalWeight = 0.0;

        for (int src = left; src <= right; ++src) {
            if (src >= 0 && src < static_cast<int>(srcSize)) {
                double distance = center - src;
                double weight;

                if (useAntialiasing && scale > 1.0) {
                    weight = kernelFunc(distance / scale) / scale;
                } else {
                    weight = kernelFunc(distance);
                }

                if (weight != 0.0) {
                    contributions[dst].contributions.push_back({ src, weight });
                    contributions[dst].totalWeight += weight;
                }
            }
        }

        // Normalize weights
        if (contributions[dst].totalWeight > 0.0) {
            for (auto& contrib : contributions[dst].contributions) {
                contrib.weight /= contributions[dst].totalWeight;
            }
        }
    }

    return contributions;
}
//=============================================================================
// Optimized nearest neighbor resize
template <typename T>
void
resizeNearestNeighbor(const T* inputData, T* outputData, size_t width, size_t height,
    size_t channels, size_t new_width, size_t new_height)
{
    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;

    double scale_x = static_cast<double>(width) / static_cast<double>(new_width);
    double scale_y = static_cast<double>(height) / static_cast<double>(new_height);

    // Pre-compute mapping arrays for better cache performance
    std::vector<size_t> x_map(new_width);
    std::vector<size_t> y_map(new_height);

    for (size_t x = 0; x < new_width; ++x) {
        double src_x = (x + 0.5) * scale_x - 0.5;
        int nearest_x = static_cast<int>(std::round(src_x));
        x_map[x] = std::clamp(nearest_x, 0, static_cast<int>(width) - 1);
    }

    for (size_t y = 0; y < new_height; ++y) {
        double src_y = (y + 0.5) * scale_y - 0.5;
        int nearest_y = static_cast<int>(std::round(src_y));
        y_map[y] = std::clamp(nearest_y, 0, static_cast<int>(height) - 1);
    }

    // Process each channel separately for better memory access pattern
    for (size_t c = 0; c < channels; ++c) {
        const T* srcPlane = inputData + c * planeSizeIn;
        T* dstPlane = outputData + c * planeSizeOut;

        OMP_PARALLEL_FOR_LOOP(new_width, 4)
        for (size_t x = 0; x < new_width; ++x) {
            size_t src_x = x_map[x];

            for (size_t y = 0; y < new_height; ++y) {
                size_t src_y = y_map[y];
                dstPlane[y + x * new_height] = srcPlane[src_y + src_x * height];
            }
        }
    }
}
//=============================================================================
// Main optimized resize function
template <typename T>
void
resizeImageOptimized(const T* inputData, T* outputData, size_t width, size_t height,
    size_t channels, size_t new_width, size_t new_height, ResizeInterpolationMethod method,
    bool useAntialiasing)
{
    // Handle nearest neighbor separately for maximum performance
    if (method == ResizeInterpolationMethod::Nearest) {
        resizeNearestNeighbor(
            inputData, outputData, width, height, channels, new_width, new_height);
        return;
    }

    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;

    double scale_x = static_cast<double>(width) / static_cast<double>(new_width);
    double scale_y = static_cast<double>(height) / static_cast<double>(new_height);

    bool needsAntialiasing = useAntialiasing && (scale_x > 1.0 || scale_y > 1.0);

    // Pre-compute contributions for both dimensions
    auto x_contributions
        = computeContributions(width, new_width, method, needsAntialiasing, scale_x);
    auto y_contributions
        = computeContributions(height, new_height, method, needsAntialiasing, scale_y);

    // Use two-pass approach for better cache performance
    // Allocate temporary buffer for horizontal pass result
    std::vector<double> tempBuffer(new_width * height * channels);

    // Horizontal pass - process rows
    OMP_PARALLEL_FOR_LOOP(height * channels, 4)
    for (size_t idx = 0; idx < height * channels; ++idx) {
        size_t y = idx % height;
        size_t c = idx / height;

        const T* srcRow = inputData + y + c * planeSizeIn;
        double* dstRow = tempBuffer.data() + y + c * (new_width * height);

        for (size_t x = 0; x < new_width; ++x) {
            double sum = 0.0;

            const auto& contrib = x_contributions[x];
            for (const auto& weight_pixel : contrib.contributions) {
                sum += weight_pixel.weight
                    * static_cast<double>(srcRow[weight_pixel.pixel * height]);
            }

            dstRow[x * height] = sum;
        }
    }

    // Vertical pass - process columns
    OMP_PARALLEL_FOR_LOOP(new_width * channels, 4)
    for (size_t idx = 0; idx < new_width * channels; ++idx) {
        size_t x = idx % new_width;
        size_t c = idx / new_width;

        const double* srcCol = tempBuffer.data() + x * height + c * (new_width * height);
        T* dstCol = outputData + x * new_height + c * planeSizeOut;

        for (size_t y = 0; y < new_height; ++y) {
            double sum = 0.0;

            const auto& contrib = y_contributions[y];
            for (const auto& weight_pixel : contrib.contributions) {
                sum += weight_pixel.weight * srcCol[weight_pixel.pixel];
            }

            assignOutput(dstCol, y, sum);
        }
    }
}
//=============================================================================
// Legacy resize function (kept for compatibility, but calls optimized version)
template <typename T>
void
resizeImage(const T* inputData, T* outputData, size_t width, size_t height, size_t channels,
    size_t new_width, size_t new_height, ResizeInterpolationMethod method, bool useAntialiasing)
{
    resizeImageOptimized(inputData, outputData, width, height, channels, new_width, new_height,
        method, useAntialiasing);
}
//=============================================================================
ArrayOf
ImageResize(const ArrayOf& image, double scale, const ResizeOptions& options)
{
    if (scale <= 0.0) {
        Error(_W("Scale factor must be positive."));
    }

    Dimensions dims = image.getDimensions();
    size_t height = dims.getRows();
    size_t width = dims.getColumns();
    size_t channels = (dims.getLength() > 2) ? dims.getDimensionLength(2) : 1;

    size_t new_height = static_cast<size_t>(std::round(height * scale));
    size_t new_width = static_cast<size_t>(std::round(width * scale));

    // Ensure minimum size of 1x1
    new_height = std::max(new_height, static_cast<size_t>(1));
    new_width = std::max(new_width, static_cast<size_t>(1));

    return ImageResize(image, new_height, new_width, options);
}
//=============================================================================
ArrayOf
ImageResize(const ArrayOf& image, size_t numrows, size_t numcols, const ResizeOptions& options)
{
    Dimensions dims = image.getDimensions();
    size_t height = dims.getRows();
    size_t width = dims.getColumns();
    size_t channels = (dims.getLength() > 2) ? dims.getDimensionLength(2) : 1;

    if (numrows == 0 || numcols == 0) {
        Error(_W("Output dimensions must be positive."));
    }

    // If input and output dimensions are the same, return copy
    if (height == numrows && width == numcols) {
        return image;
    }

    // Create output array with the appropriate type
    ArrayOf outputImage;
    Dimensions outputDims;

    if (channels == 1) {
        outputDims = Dimensions((indexType)numrows, (indexType)numcols);
    } else {
        std::vector<indexType> dimsAsVector
            = { (indexType)numrows, (indexType)numcols, (indexType)channels };
        outputDims = Dimensions(dimsAsVector);
    }

    NelsonType dataClass = image.getDataClass();
    outputImage = ArrayOf(
        dataClass, outputDims, ArrayOf::allocateArrayOf(dataClass, outputDims.getElementCount()));

    // Determine antialiasing setting
    bool useAntialiasing = false;
    if (options.antialiasing == ResizeAntialiasing::On) {
        useAntialiasing = true;
    } else if (options.antialiasing == ResizeAntialiasing::Auto) {
        double scale_x = static_cast<double>(width) / static_cast<double>(numcols);
        double scale_y = static_cast<double>(height) / static_cast<double>(numrows);
        useAntialiasing = (scale_x > 1.0 || scale_y > 1.0);
    }

    // Process based on data type using optimized functions
    switch (dataClass) {
    case NLS_DOUBLE: {
        resizeImageOptimized<double>((const double*)image.getDataPointer(),
            (double*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_SINGLE: {
        resizeImageOptimized<single>((const single*)image.getDataPointer(),
            (single*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_INT8: {
        resizeImageOptimized<int8>((const int8*)image.getDataPointer(),
            (int8*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_INT16: {
        resizeImageOptimized<int16>((const int16*)image.getDataPointer(),
            (int16*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_INT32: {
        resizeImageOptimized<int32>((const int32*)image.getDataPointer(),
            (int32*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_INT64: {
        resizeImageOptimized<int64>((const int64*)image.getDataPointer(),
            (int64*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT8: {
        resizeImageOptimized<uint8>((const uint8*)image.getDataPointer(),
            (uint8*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT16: {
        resizeImageOptimized<uint16>((const uint16*)image.getDataPointer(),
            (uint16*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT32: {
        resizeImageOptimized<uint32>((const uint32*)image.getDataPointer(),
            (uint32*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT64: {
        resizeImageOptimized<uint64>((const uint64*)image.getDataPointer(),
            (uint64*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_LOGICAL: {
        resizeImageOptimized<logical>((const logical*)image.getDataPointer(),
            (logical*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }

    return outputImage;
}
//=============================================================================
std::pair<ArrayOf, ArrayOf>
ImageResizeIndexed(const ArrayOf& X, const ArrayOf& map, double scale, const ResizeOptions& options)
{
    Dimensions dims = X.getDimensions();
    size_t height = dims.getRows();
    size_t width = dims.getColumns();

    size_t new_height = static_cast<size_t>(std::round(height * scale));
    size_t new_width = static_cast<size_t>(std::round(width * scale));

    // Ensure minimum size of 1x1
    new_height = std::max(new_height, static_cast<size_t>(1));
    new_width = std::max(new_width, static_cast<size_t>(1));

    return ImageResizeIndexed(X, map, new_height, new_width, options);
}
//=============================================================================
std::pair<ArrayOf, ArrayOf>
ImageResizeIndexed(const ArrayOf& X, const ArrayOf& map, size_t numrows, size_t numcols,
    const ResizeOptions& options)
{
    // Validate indexed image
    if (!X.isNumeric()) {
        Error(_W("Indexed image must be numeric."));
    }

    // Validate colormap
    Dimensions mapDims = map.getDimensions();
    if (mapDims.getColumns() != 3 || !map.isNumeric()) {
        Error(_W("Colormap must be an Nx3 numeric array."));
    }

    // For indexed images, we typically use nearest neighbor to preserve indices
    ResizeOptions indexedOptions = options;
    if (options.method != ResizeInterpolationMethod::Nearest) {
        // For non-nearest methods, we might need to handle differently
        // For now, we'll use the specified method but be aware of potential issues
    }

    // Resize the indexed image
    ArrayOf resizedX = ImageResize(X, numrows, numcols, indexedOptions);

    // The colormap doesn't change
    ArrayOf newMap = map;

    return std::make_pair(resizedX, newMap);
}
//=============================================================================
}
//=============================================================================
