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
// Helper template function to assign output pixel values based on type
template <typename T>
inline void
assignOutput(T* outputData, size_t index, double value)
{
    // Assigns the computed value to the output array, clamping or rounding as needed for each type
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
    // Lanczos resampling kernel, used for high-quality interpolation
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
    // Catmull-Rom cubic interpolation kernel
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
    // Box filter kernel, used for simple averaging
    return (std::abs(x) <= 0.5) ? 1.0 : 0.0;
}
//=============================================================================
// Bilinear kernel
inline double
bilinearKernel(double x)
{
    // Bilinear interpolation kernel
    x = std::abs(x);
    return (x <= 1.0) ? (1.0 - x) : 0.0;
}
//=============================================================================
// Structure to hold pixel contribution information
struct ResizeContribution
{
    // Represents a single pixel's contribution and its weight for resizing
    int pixel;
    double weight;
};
//=============================================================================
struct ResizeContributions
{
    // Holds all pixel contributions for a single output pixel and the total weight
    std::vector<ResizeContribution> contributions;
    double totalWeight;
};
//=============================================================================
// Pre-compute contributions for a single dimension
std::vector<ResizeContributions>
computeContributions(size_t srcSize, size_t dstSize, ResizeInterpolationMethod method,
    bool useAntialiasing, double scale)
{
    // Pre-computes the weights and source pixel indices for each output pixel
    std::vector<ResizeContributions> contributions(dstSize);

    // Determine kernel radius and function
    int kernelRadius = 1;
    std::function<double(double)> kernelFunc;

    switch (method) {
    case ResizeInterpolationMethod::Bilinear:
        // Use bilinear kernel for interpolation
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
    // Performs fast nearest neighbor resizing for images
    size_t planeSizeIn = width * height;
    size_t planeSizeOut = new_width * new_height;

    double scale_x = static_cast<double>(width) / static_cast<double>(new_width);
    double scale_y = static_cast<double>(height) / static_cast<double>(new_height);

    // Pre-compute mapping arrays for better cache performance
    std::vector<size_t> x_map(new_width);
    std::vector<size_t> y_map(new_height);

    OMP_PARALLEL_FOR_LOOP(new_width, 4)
    for (size_t x = 0; x < new_width; ++x) {
        double src_x = (x + 0.5) * scale_x - 0.5;
        int nearest_x = static_cast<int>(std::round(src_x));
        x_map[x] = std::clamp(nearest_x, 0, static_cast<int>(width) - 1);
    }

    OMP_PARALLEL_FOR_LOOP(new_height, 4)
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
template <typename T>
void
resizeImage(const T* inputData, T* outputData, size_t width, size_t height, size_t channels,
    size_t new_width, size_t new_height, ResizeInterpolationMethod method, bool useAntialiasing)
{
    // Main function for optimized image resizing using two-pass interpolation
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
ArrayOf
ImageResize(const ArrayOf& image, double scale, const ResizeOptions& options)
{
    // Resize image by a scale factor
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
    // Resize image to specific dimensions
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

    // Process based on data type
    switch (dataClass) {
    case NLS_DOUBLE: {
        resizeImage<double>((const double*)image.getDataPointer(),
            (double*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_SINGLE: {
        resizeImage<single>((const single*)image.getDataPointer(),
            (single*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_INT8: {
        resizeImage<int8>((const int8*)image.getDataPointer(), (int8*)outputImage.getDataPointer(),
            width, height, channels, numcols, numrows, options.method, useAntialiasing);
    } break;
    case NLS_INT16: {
        resizeImage<int16>((const int16*)image.getDataPointer(),
            (int16*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_INT32: {
        resizeImage<int32>((const int32*)image.getDataPointer(),
            (int32*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_INT64: {
        resizeImage<int64>((const int64*)image.getDataPointer(),
            (int64*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT8: {
        resizeImage<uint8>((const uint8*)image.getDataPointer(),
            (uint8*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT16: {
        resizeImage<uint16>((const uint16*)image.getDataPointer(),
            (uint16*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT32: {
        resizeImage<uint32>((const uint32*)image.getDataPointer(),
            (uint32*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_UINT64: {
        resizeImage<uint64>((const uint64*)image.getDataPointer(),
            (uint64*)outputImage.getDataPointer(), width, height, channels, numcols, numrows,
            options.method, useAntialiasing);
    } break;
    case NLS_LOGICAL: {
        resizeImage<logical>((const logical*)image.getDataPointer(),
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
    // Resize indexed image and its colormap by a scale factor
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
    // Resize indexed image and its colormap to specific dimensions
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
    indexedOptions.method
        = ResizeInterpolationMethod::Nearest; // Force nearest neighbor for indexed images

    // Resize the indexed image
    ArrayOf resizedX = ImageResize(X, numrows, numcols, indexedOptions);

    // Handle colormap based on colormapMode
    ArrayOf newMap;
    if (options.colormapMode == L"optimized") {
        // If colormapMode is optimized, only keep used colors in the colormap
        NelsonType dataClass = resizedX.getDataClass();
        void* dataPtr = (void*)resizedX.getDataPointer();
        size_t numElements = resizedX.getElementCount();

        std::vector<bool> usedIndices(mapDims.getRows(), false);

        switch (dataClass) {
        case NLS_UINT8: {
            uint8_t* data = static_cast<uint8_t*>(dataPtr);
            OMP_PARALLEL_FOR_LOOP(numElements, 4)
            for (size_t i = 0; i < numElements; i++) {
                if (data[i] < mapDims.getRows()) {
                    usedIndices[data[i]] = true;
                }
            }
        } break;
        case NLS_UINT16: {
            uint16_t* data = static_cast<uint16_t*>(dataPtr);
            OMP_PARALLEL_FOR_LOOP(numElements, 4)
            for (size_t i = 0; i < numElements; i++) {
                if (data[i] < mapDims.getRows()) {
                    usedIndices[data[i]] = true;
                }
            }
        } break;
        case NLS_UINT32: {
            uint32_t* data = static_cast<uint32_t*>(dataPtr);
            OMP_PARALLEL_FOR_LOOP(numElements, 4)
            for (size_t i = 0; i < numElements; i++) {
                if (data[i] < mapDims.getRows()) {
                    usedIndices[data[i]] = true;
                }
            }
        } break;
        case NLS_INT8: {
            int8_t* data = static_cast<int8_t*>(dataPtr);
            OMP_PARALLEL_FOR_LOOP(numElements, 4)
            for (size_t i = 0; i < numElements; i++) {
                if (data[i] >= 0 && static_cast<size_t>(data[i]) < mapDims.getRows()) {
                    usedIndices[data[i]] = true;
                }
            }
        } break;
        case NLS_INT16: {
            int16_t* data = static_cast<int16_t*>(dataPtr);
            OMP_PARALLEL_FOR_LOOP(numElements, 4)
            for (size_t i = 0; i < numElements; i++) {
                if (data[i] >= 0 && static_cast<size_t>(data[i]) < mapDims.getRows()) {
                    usedIndices[data[i]] = true;
                }
            }
        } break;
        case NLS_INT32: {
            int32_t* data = static_cast<int32_t*>(dataPtr);
            OMP_PARALLEL_FOR_LOOP(numElements, 4)
            for (size_t i = 0; i < numElements; i++) {
                if (data[i] >= 0 && static_cast<size_t>(data[i]) < mapDims.getRows()) {
                    usedIndices[data[i]] = true;
                }
            }
        } break;
        default:
            Error(_W("Unsupported data type for indexed image. Must be uint8, uint16, uint32, "
                     "int8, int16, or int32."));
            break;
        }

        size_t newSize = std::count(usedIndices.begin(), usedIndices.end(), true);
        Dimensions newMapDims(static_cast<indexType>(newSize), 3);
        double* srcMap = (double*)(map.getDataPointer());
        double* dstMap
            = (double*)ArrayOf::allocateArrayOf(map.getDataClass(), newMapDims.getElementCount());
        newMap = ArrayOf(map.getDataClass(), newMapDims, dstMap);

        size_t newIdx = 0;
        for (size_t i = 0; i < mapDims.getRows(); i++) {
            if (usedIndices[i]) {
                dstMap[newIdx] = srcMap[i];
                dstMap[newIdx + newSize] = srcMap[i + mapDims.getRows()];
                dstMap[newIdx + 2 * newSize] = srcMap[i + 2 * mapDims.getRows()];
                newIdx++;
            }
        }

    } else {
        newMap = map;
    }

    if (options.dither) {
        // If dithering is enabled, apply error diffusion dithering to the resized indexed image
        NelsonType dataClass = resizedX.getDataClass();
        if (dataClass == NLS_UINT8) {
            uint8_t* data = (uint8_t*)(resizedX.getDataPointer());
            OMP_PARALLEL_FOR_LOOP(numrows, 4)
            for (size_t y = 0; y < numrows; y++) {
                std::vector<double> localErrorBuffer(numcols + 2, 0.0);
                double error = 0.0;

                for (size_t x = 0; x < numcols; x++) {
                    size_t idx = y * numcols + x;
                    double oldPixel = data[idx] + error + localErrorBuffer[x + 1];
                    uint8_t newPixel
                        = static_cast<uint8_t>(std::round(std::clamp(oldPixel, 0.0, 255.0)));
                    data[idx] = newPixel;

                    double quantError = oldPixel - newPixel;
                    error = quantError * 7.0 / 16.0;
                    localErrorBuffer[x] += quantError * 3.0 / 16.0;
                    localErrorBuffer[x + 1] = quantError * 5.0 / 16.0;
                    localErrorBuffer[x + 2] += quantError * 1.0 / 16.0;
                }
            }
        }
    }

    return std::make_pair(resizedX, newMap);
}
//=============================================================================
}
//=============================================================================
