//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "imresizeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "ImageResize.hpp"
#include <string>
#include <algorithm>
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
static ResizeInterpolationMethod
parseInterpolationMethod(const std::wstring& method);
static ResizeAntialiasing
parseAntialiasing(const std::wstring& antialiasing);
//=============================================================================
ArrayOfVector
imresizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;

    // Check minimum number of arguments
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 2, 10); // Flexible range to handle various parameter combinations

    // First argument must be the image
    ArrayOf image = argIn[0];
    if (!image.isNumeric() && !image.isLogical()) {
        Error(_W("First argument must be a numeric or logical array representing an image."));
    }

    // Check if this is an indexed image (second argument is colormap)
    bool isIndexedImage = false;
    ArrayOf colormap;

    if (argIn.size() > 2) {
        // Check if second argument could be a colormap (Nx3 numeric array)
        ArrayOf possibleColormap = argIn[1];
        if (possibleColormap.isNumeric()) {
            Dimensions mapDims = possibleColormap.getDimensions();
            if (mapDims.getColumns() == 3 && mapDims.getLength() == 2) {
                isIndexedImage = true;
                colormap = possibleColormap;
            }
        }
    }

    // Determine scale/size parameter position
    size_t scaleParamIndex = isIndexedImage ? 2 : 1;

    if (argIn.size() <= scaleParamIndex) {
        Error(_W("Scale factor or output size must be provided."));
    }

    ArrayOf scaleOrSize = argIn[scaleParamIndex];

    // Parse options from remaining arguments
    ResizeOptions options;

    // Accept method as last positional argument (MATLAB style)
    size_t lastArgIndex = argIn.size() - 1;
    bool hasMethodAsLastArg = false;
    if (lastArgIndex > scaleParamIndex
        && (argIn[lastArgIndex].isRowVectorCharacterArray()
            || argIn[lastArgIndex].isScalarStringArray())) {
        // If the number of remaining args after scaleParamIndex is odd, last one is method
        size_t numOptionArgs = lastArgIndex - (scaleParamIndex + 1) + 1;
        if (numOptionArgs % 2 == 1) {
            std::wstring methodStr = argIn[lastArgIndex].getContentAsWideString();
            options.method = parseInterpolationMethod(methodStr);
            hasMethodAsLastArg = true;
        }
    }

    // Process parameter-value pairs starting after the scale/size parameter
    size_t paramEnd = hasMethodAsLastArg ? lastArgIndex : argIn.size();
    for (size_t i = scaleParamIndex + 1; i < paramEnd; i += 2) {
        if (i + 1 >= paramEnd) {
            Error(_W("Parameter-value pairs must be complete."));
        }

        if (!argIn[i].isRowVectorCharacterArray() && !argIn[i].isScalarStringArray()) {
            Error(_W("Parameter names must be strings."));
        }

        std::wstring paramName = argIn[i].getContentAsWideString();
        std::transform(paramName.begin(), paramName.end(), paramName.begin(), ::towlower);
        ArrayOf paramValue = argIn[i + 1];

        if (paramName == L"antialiasing") {
            if (paramValue.isRowVectorCharacterArray() || paramValue.isScalarStringArray()) {
                std::wstring antialiasingStr = paramValue.getContentAsWideString();
                options.antialiasing = parseAntialiasing(antialiasingStr);
            } else if (paramValue.isLogical() || paramValue.isNumeric()) {
                bool antialiasingValue = paramValue.getContentAsLogicalScalar();
                options.antialiasing
                    = antialiasingValue ? ResizeAntialiasing::On : ResizeAntialiasing::Off;
            } else {
                Error(_W("Antialiasing parameter must be a logical value or string."));
            }
        } else if (paramName == L"colormap" && isIndexedImage) {
            if (!paramValue.isRowVectorCharacterArray() && !paramValue.isScalarStringArray()) {
                Error(_W("Colormap parameter must be a string ('optimized' or 'original')."));
            }
            std::wstring colormapMode = paramValue.getContentAsWideString();
            std::transform(
                colormapMode.begin(), colormapMode.end(), colormapMode.begin(), ::towlower);
            if (colormapMode != L"optimized" && colormapMode != L"original") {
                Error(_W("Colormap parameter must be 'optimized' or 'original'."));
            }
            options.colormapMode = colormapMode;
        } else if (paramName == L"dither" && isIndexedImage) {
            if (!paramValue.isLogical() && !paramValue.isNumeric()) {
                Error(_W("Dither parameter must be a logical value."));
            }
            options.dither = paramValue.getContentAsLogicalScalar();
        } else {
            Error(_W("Unknown parameter: ") + paramName);
        }
    }

    // Handle the scale/size parameter
    if (options.outputSize) {
        // OutputSize parameter overrides the scale/size argument
        if (isIndexedImage) {
            auto result = ImageResizeIndexed(image, colormap, options.outputSizeValue.first,
                options.outputSizeValue.second, options);
            retval.push_back(result.first);
            if (nLhs > 1) {
                retval.push_back(result.second);
            }
        } else {
            ArrayOf resizedImage = ImageResize(
                image, options.outputSizeValue.first, options.outputSizeValue.second, options);
            retval.push_back(resizedImage);
        }

    } else if (scaleOrSize.isNumeric()) {
        Dimensions scaleDims = scaleOrSize.getDimensions();

        if (scaleDims.getElementCount() == 1) {
            // Single scale factor
            double scale = scaleOrSize.getContentAsDoubleScalar();
            if (scale <= 0.0) {
                Error(_W("Scale factor must be positive."));
            }

            if (isIndexedImage) {
                auto result = ImageResizeIndexed(image, colormap, scale, options);
                retval.push_back(result.first);
                if (nLhs > 1) {
                    retval.push_back(result.second);
                }
            } else {
                ArrayOf resizedImage = ImageResize(image, scale, options);
                retval.push_back(resizedImage);
            }

        } else if (scaleDims.getElementCount() == 2) {
            // Two-element vector [rows, cols]
            double* scaleData = (double*)scaleOrSize.getDataPointer();

            bool isNaN1 = std::isnan(scaleData[0]);
            bool isNaN2 = std::isnan(scaleData[1]);

            if (isNaN1 && isNaN2) {
                Error(_W("At least one output dimension must be specified (not NaN)."));
            }

            Dimensions imageDims = image.getDimensions();
            size_t srcHeight = imageDims.getRows();
            size_t srcWidth = imageDims.getColumns();
            double aspectRatio = static_cast<double>(srcWidth) / static_cast<double>(srcHeight);

            size_t newRows, newCols;

            if (isNaN1) {
                newCols = static_cast<size_t>(std::max(1.0, std::round(std::abs(scaleData[1]))));
                newRows = static_cast<size_t>(std::max(1.0, std::round(newCols / aspectRatio)));
            } else if (isNaN2) {
                newRows = static_cast<size_t>(std::max(1.0, std::round(std::abs(scaleData[0]))));
                newCols = static_cast<size_t>(std::max(1.0, std::round(newRows * aspectRatio)));
            } else {
                newRows = static_cast<size_t>(std::max(1.0, std::round(std::abs(scaleData[0]))));
                newCols = static_cast<size_t>(std::max(1.0, std::round(std::abs(scaleData[1]))));
            }

            if (isIndexedImage) {
                auto result = ImageResizeIndexed(image, colormap, newRows, newCols, options);
                retval.push_back(result.first);
                if (nLhs > 1) {
                    retval.push_back(result.second);
                }
            } else {
                ArrayOf resizedImage = ImageResize(image, newRows, newCols, options);
                retval.push_back(resizedImage);
            }
        } else {
            Error(_W("Scale parameter must be a scalar or 2-element vector."));
        }
    } else {
        Error(_W("Scale parameter must be numeric."));
    }

    return retval;
}
//=============================================================================
// Helper function to parse interpolation method from string
ResizeInterpolationMethod
parseInterpolationMethod(const std::wstring& method)
{
    std::wstring lowerMethod = method;
    std::transform(lowerMethod.begin(), lowerMethod.end(), lowerMethod.begin(), ::towlower);

    if (lowerMethod == L"nearest") {
        return ResizeInterpolationMethod::Nearest;
    } else if (lowerMethod == L"bilinear") {
        return ResizeInterpolationMethod::Bilinear;
    } else if (lowerMethod == L"bicubic") {
        return ResizeInterpolationMethod::Bicubic;
    } else if (lowerMethod == L"box") {
        return ResizeInterpolationMethod::Box;
    } else if (lowerMethod == L"lanczos2") {
        return ResizeInterpolationMethod::Lanczos2;
    } else if (lowerMethod == L"lanczos3") {
        return ResizeInterpolationMethod::Lanczos3;
    }
    Error(_W("Unknown interpolation method: ") + method);
    return ResizeInterpolationMethod::Nearest;
}
//=============================================================================
// Helper function to parse antialiasing setting from string
ResizeAntialiasing
parseAntialiasing(const std::wstring& antialiasing)
{
    std::wstring lowerAntialiasing = antialiasing;
    std::transform(
        lowerAntialiasing.begin(), lowerAntialiasing.end(), lowerAntialiasing.begin(), ::towlower);

    if (lowerAntialiasing == L"true" || lowerAntialiasing == L"on") {
        return ResizeAntialiasing::On;
    } else if (lowerAntialiasing == L"false" || lowerAntialiasing == L"off") {
        return ResizeAntialiasing::Off;
    }
    Error(_W("Antialiasing must be 'true', 'false', 'on', or 'off'."));
    return ResizeAntialiasing::Off;
}
//=============================================================================
}; // namespace Nelson::GraphicsGateway
//=============================================================================
