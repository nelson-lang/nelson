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
#include "PredefinedErrorMessages.hpp"
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
        raiseError(L"Nelson:image_processing:ERROR_FIRST_ARG_IMAGE_NUMERIC_OR_LOGICAL",
            ERROR_FIRST_ARG_IMAGE_NUMERIC_OR_LOGICAL);
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
        raiseError(L"Nelson:image_processing:ERROR_SCALE_FACTOR_OR_OUTPUT_SIZE_MUST_BE_PROVIDED",
            ERROR_SCALE_FACTOR_OR_OUTPUT_SIZE_MUST_BE_PROVIDED);
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
            raiseError(L"Nelson:image_processing:ERROR_PARAMETER_VALUE_PAIRS_MUST_BE_COMPLETE",
                ERROR_PARAMETER_VALUE_PAIRS_MUST_BE_COMPLETE);
        }

        if (!argIn[i].isRowVectorCharacterArray() && !argIn[i].isScalarStringArray()) {
            raiseError(L"Nelson:image_processing:ERROR_PARAMETER_NAMES_MUST_BE_STRINGS",
                ERROR_PARAMETER_NAMES_MUST_BE_STRINGS);
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
                raiseError(
                    L"Nelson:image_processing:ERROR_ANTIALIASING_PARAM_MUST_BE_LOGICAL_OR_STRING",
                    ERROR_ANTIALIASING_PARAM_MUST_BE_LOGICAL_OR_STRING);
            }
        } else if (paramName == L"colormap" && isIndexedImage) {
            if (!paramValue.isRowVectorCharacterArray() && !paramValue.isScalarStringArray()) {
                raiseError(L"Nelson:image_processing:ERROR_COLORMAP_PARAM_MUST_BE_STRING",
                    ERROR_COLORMAP_PARAM_MUST_BE_STRING);
            }
            std::wstring colormapMode = paramValue.getContentAsWideString();
            std::transform(
                colormapMode.begin(), colormapMode.end(), colormapMode.begin(), ::towlower);
            if (colormapMode != L"optimized" && colormapMode != L"original") {
                raiseError(
                    L"Nelson:image_processing:ERROR_COLORMAP_PARAM_MUST_BE_OPTIMIZED_OR_ORIGINAL",
                    ERROR_COLORMAP_PARAM_MUST_BE_OPTIMIZED_OR_ORIGINAL);
            }
            options.colormapMode = colormapMode;
        } else if (paramName == L"dither" && isIndexedImage) {
            if (!paramValue.isLogical() && !paramValue.isNumeric()) {
                raiseError(L"Nelson:image_processing:ERROR_DITHER_PARAM_MUST_BE_LOGICAL",
                    ERROR_DITHER_PARAM_MUST_BE_LOGICAL);
            }
            options.dither = paramValue.getContentAsLogicalScalar();
        } else {
            raiseError(L"Nelson:image_processing:ERROR_UNKNOWN_PARAMETER", ERROR_UNKNOWN_PARAMETER,
                paramName);
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
                raiseError(L"Nelson:image_processing:ERROR_SCALE_FACTOR_MUST_BE_POSITIVE",
                    ERROR_SCALE_FACTOR_MUST_BE_POSITIVE);
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
                raiseError(L"Nelson:image_processing:ERROR_AT_LEAST_ONE_OUTPUT_DIMENSION_SPECIFIED",
                    ERROR_AT_LEAST_ONE_OUTPUT_DIMENSION_SPECIFIED);
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
            raiseError(
                L"Nelson:image_processing:ERROR_SCALE_PARAMETER_MUST_BE_SCALAR_OR_2_ELEMENT_VECTOR",
                ERROR_SCALE_PARAMETER_MUST_BE_SCALAR_OR_2_ELEMENT_VECTOR);
        }
    } else {
        raiseError(L"Nelson:image_processing:ERROR_SCALE_PARAMETER_MUST_BE_NUMERIC",
            ERROR_SCALE_PARAMETER_MUST_BE_NUMERIC);
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
    raiseError(L"Nelson:image_processing:ERROR_UNKNOWN_INTERPOLATION_METHOD",
        ERROR_UNKNOWN_INTERPOLATION_METHOD, method);
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
    raiseError(L"Nelson:image_processing:ERROR_ANTIALIASING_MUST_BE_TRUE_FALSE_ON_OFF",
        ERROR_ANTIALIASING_MUST_BE_TRUE_FALSE_ON_OFF);
    return ResizeAntialiasing::Off;
}
//=============================================================================
}; // namespace Nelson::GraphicsGateway
//=============================================================================
