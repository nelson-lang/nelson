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
#include "ErrorID.hpp"
#include "ImageProcessingCore.hpp"
#include "ImageResize.hpp"
#include <string>
#include <algorithm>
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
static ResizeInterpolationMethod
parseInterpolationMethod(const std::wstring& method);
static bool
isInterpolationMethodName(const std::wstring& method);
static ResizeAntialiasing
parseAntialiasing(const std::wstring& antialiasing);
//=============================================================================
ArrayOfVector
imresizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;

    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 2, 10);

    ArrayOf image = argIn[0];
    validateImage2DOrRGB(image, true,
        Nelson::resolveMessageTemplate(L"Nelson:image_processing:firstNumericLogicalArray",
            L"First argument must be a numeric or logical array representing an image."),
        formatI18nMessage(Nelson::resolveMessageTemplate(L"Nelson:image_processing:imageDRgbArray",
                              L"%s must be a 2-D or RGB array."),
            L"Input image"),
        formatI18nMessage(
            Nelson::resolveMessageTemplate(
                L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported."),
            L"Sparse images"));

    bool isIndexedImage = false;
    ArrayOf colormap;

    if (argIn.size() > 2) {
        ArrayOf possibleColormap = argIn[1];
        if (possibleColormap.isNumeric()) {
            Dimensions mapDims = possibleColormap.dimensions();
            if (mapDims.columns() == 3 && mapDims.length() == 2) {
                isIndexedImage = true;
                colormap = possibleColormap;
            }
        }
    }

    size_t scaleParamIndex = isIndexedImage ? 2 : 1;

    if (argIn.size() <= scaleParamIndex) {
        Error(L"Nelson:imresize:missingSize", L"Scale factor or output size must be provided.");
    }

    ArrayOf scaleOrSize = argIn[scaleParamIndex];

    ResizeOptions options;

    size_t lastArgIndex = argIn.size() - 1;
    bool hasMethodAsLastArg = false;
    size_t pairStart = scaleParamIndex + 1;

    // A method may be given as the first positional argument after the
    // scale/size, before any name-value pairs:
    //   imresize(A, scale, 'bicubic', 'Antialiasing', true)
    if (pairStart <= lastArgIndex
        && (argIn[pairStart].isRowVectorCharacterArray() || argIn[pairStart].isScalarStringArray())
        && isInterpolationMethodName(argIn[pairStart].asWideString())) {
        options.method = parseInterpolationMethod(argIn[pairStart].asWideString());
        pairStart = pairStart + 1;
    }

    // A method may also appear as the last positional argument when the
    // remaining argument count is odd: imresize(A, scale, Name, Value, method).
    if (pairStart <= lastArgIndex
        && (argIn[lastArgIndex].isRowVectorCharacterArray()
            || argIn[lastArgIndex].isScalarStringArray())) {
        size_t numOptionArgs = lastArgIndex - pairStart + 1;
        if (numOptionArgs % 2 == 1
            && isInterpolationMethodName(argIn[lastArgIndex].asWideString())) {
            options.method = parseInterpolationMethod(argIn[lastArgIndex].asWideString());
            hasMethodAsLastArg = true;
        }
    }

    size_t paramEnd = hasMethodAsLastArg ? lastArgIndex : argIn.size();
    for (size_t i = pairStart; i < paramEnd; i += 2) {
        if (i + 1 >= paramEnd) {
            Error(L"Nelson:image_processing:nameValueArgumentsAppearPairs",
                L"Name-value arguments must be paired.");
        }

        if (!argIn[i].isRowVectorCharacterArray() && !argIn[i].isScalarStringArray()) {
            Error(L"Nelson:imresize:invalidParamName", L"Parameter names must be strings.");
        }

        std::wstring paramName = argIn[i].asWideString();
        std::transform(paramName.begin(), paramName.end(), paramName.begin(), ::towlower);
        ArrayOf paramValue = argIn[i + 1];

        if (paramName == L"antialiasing") {
            if (paramValue.isRowVectorCharacterArray() || paramValue.isScalarStringArray()) {
                std::wstring antialiasingStr = paramValue.asWideString();
                options.antialiasing = parseAntialiasing(antialiasingStr);
            } else if (paramValue.isLogical() || paramValue.isNumeric()) {
                bool antialiasingValue = paramValue.asLogicalScalar();
                options.antialiasing
                    = antialiasingValue ? ResizeAntialiasing::On : ResizeAntialiasing::Off;
            } else {
                Error(L"Nelson:imresize:invalidAntialiasingType",
                    L"Antialiasing parameter must be a logical value or string.");
            }
        } else if (paramName == L"colormap" && isIndexedImage) {
            if (!paramValue.isRowVectorCharacterArray() && !paramValue.isScalarStringArray()) {
                Error(L"Nelson:imresize:colormapParameterNotString",
                    L"Colormap parameter must be a string ('optimized' or 'original').");
            }
            std::wstring colormapMode = paramValue.asWideString();
            std::transform(
                colormapMode.begin(), colormapMode.end(), colormapMode.begin(), ::towlower);
            if (colormapMode != L"optimized" && colormapMode != L"original") {
                Error(L"Nelson:imresize:invalidColormapParameter",
                    L"Colormap parameter must be 'optimized' or 'original'.");
            }
            options.colormapMode = colormapMode;
        } else if (paramName == L"dither" && isIndexedImage) {
            if (!paramValue.isLogical() && !paramValue.isNumeric()) {
                Error(
                    L"Nelson:imresize:invalidDither", L"Dither parameter must be a logical value.");
            }
            options.dither = paramValue.asLogicalScalar();
        } else if (paramName == L"method") {
            if (!paramValue.isRowVectorCharacterArray() && !paramValue.isScalarStringArray()) {
                Error(L"Nelson:imresize:invalidMethodType", L"Method parameter must be a string.");
            }
            options.method = parseInterpolationMethod(paramValue.asWideString());
        } else {
            Error(L"Nelson:imresize:unknownParameter", L"Unsupported \"%s\" option.", paramName);
        }
    }

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
        Dimensions scaleDims = scaleOrSize.dimensions();

        if (scaleDims.elementCount() == 1) {
            double scale = scaleOrSize.asDoubleScalar();
            if (scale <= 0.0) {
                Error(
                    L"Nelson:validators:mustBePositive", L"%s must be positive.", L"Scale factor");
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

        } else if (scaleDims.elementCount() == 2) {
            // Two-element vector [rows, cols]
            double* scaleData = (double*)scaleOrSize.dataPointer();

            bool isNaN1 = std::isnan(scaleData[0]);
            bool isNaN2 = std::isnan(scaleData[1]);

            if (isNaN1 && isNaN2) {
                Error(L"Nelson:imresize:outputSizeAllNaN",
                    L"At least one output dimension must be specified (not NaN).");
            }

            Dimensions imageDims = image.dimensions();
            size_t srcHeight = imageDims.rows();
            size_t srcWidth = imageDims.columns();
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
            Error(L"Nelson:imresize:scaleWrongSize",
                L"Scale parameter must be a scalar or %d-element vector.", 2);
        }
    } else {
        Error(L"Nelson:imresize:scaleNotNumeric", L"Scale parameter must be numeric.");
    }

    return retval;
}
//=============================================================================
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
    Error(L"Nelson:imresize:unknownMethod", L"Unsupported \"%s\" option.", method);
    return ResizeInterpolationMethod::Nearest;
}
//=============================================================================
bool
isInterpolationMethodName(const std::wstring& method)
{
    std::wstring lowerMethod = method;
    std::transform(lowerMethod.begin(), lowerMethod.end(), lowerMethod.begin(), ::towlower);
    return lowerMethod == L"nearest" || lowerMethod == L"bilinear" || lowerMethod == L"bicubic"
        || lowerMethod == L"box" || lowerMethod == L"lanczos2" || lowerMethod == L"lanczos3";
}
//=============================================================================
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
    Error(L"Nelson:imresize:invalidAntialiasingValue",
        L"Antialiasing must be 'true', 'false', 'on', or 'off'.");
    return ResizeAntialiasing::Off;
}
//=============================================================================
}; // namespace Nelson::GraphicsGateway
//=============================================================================
