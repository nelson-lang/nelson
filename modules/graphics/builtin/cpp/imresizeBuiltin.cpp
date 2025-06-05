//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
namespace Nelson::GraphicsGateway {
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

    // Process parameter-value pairs starting after the scale/size parameter
    for (size_t i = scaleParamIndex + 1; i < argIn.size(); i += 2) {
        if (i + 1 >= argIn.size()) {
            Error(_W("Parameter-value pairs must be complete."));
        }

        if (!argIn[i].isRowVectorCharacterArray()) {
            Error(_W("Parameter names must be strings."));
        }

        std::wstring paramName = argIn[i].getContentAsWideString();
        std::transform(paramName.begin(), paramName.end(), paramName.begin(), ::towlower);
        ArrayOf paramValue = argIn[i + 1];

        if (paramName == L"method") {
            if (!paramValue.isRowVectorCharacterArray()) {
                Error(_W("Method parameter must be a string."));
            }
            std::wstring methodStr = paramValue.getContentAsWideString();
            options.method = parseInterpolationMethod(methodStr);

        } else if (paramName == L"antialiasing") {
            if (paramValue.isRowVectorCharacterArray()) {
                std::wstring antialiasingStr = paramValue.getContentAsWideString();
                options.antialiasing = parseAntialiasing(antialiasingStr);
            } else if (paramValue.isLogical() || paramValue.isNumeric()) {
                bool antialiasingValue = paramValue.getContentAsLogicalScalar();
                options.antialiasing
                    = antialiasingValue ? ResizeAntialiasing::On : ResizeAntialiasing::Off;
            } else {
                Error(_W("Antialiasing parameter must be a logical value or string."));
            }

        } else if (paramName == L"outputsize") {
            if (!paramValue.isNumeric()) {
                Error(_W("OutputSize parameter must be numeric."));
            }
            Dimensions outputDims = paramValue.getDimensions();
            if (outputDims.getElementCount() != 2) {
                Error(_W("OutputSize must be a 2-element vector [rows, cols]."));
            }

            double* outputSizeData = (double*)paramValue.getDataPointer();
            options.outputSize = true;
            options.outputSizeValue = { static_cast<size_t>(outputSizeData[0]),
                static_cast<size_t>(outputSizeData[1]) };

        } else if (paramName == L"printsize") {
            if (!paramValue.isLogical() && !paramValue.isNumeric()) {
                Error(_W("PrintSize parameter must be logical."));
            }
            options.printSize = paramValue.getContentAsLogicalScalar();

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
            // Two-element vector [rows, cols] or [scale_x, scale_y]
            double* scaleData = (double*)scaleOrSize.getDataPointer();

            // Check if these are scales or absolute dimensions
            // If values are > 0 and <= 1, treat as scales; otherwise as dimensions
            bool areScales
                = (scaleData[0] > 0 && scaleData[0] <= 1 && scaleData[1] > 0 && scaleData[1] <= 1);

            if (areScales) {
                // Handle as separate scale factors for rows and cols
                Dimensions imageDims = image.getDimensions();
                size_t newRows
                    = static_cast<size_t>(std::round(imageDims.getRows() * scaleData[0]));
                size_t newCols
                    = static_cast<size_t>(std::round(imageDims.getColumns() * scaleData[1]));

                newRows = std::max(newRows, static_cast<size_t>(1));
                newCols = std::max(newCols, static_cast<size_t>(1));

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
                // Handle as absolute dimensions
                size_t newRows = static_cast<size_t>(scaleData[0]);
                size_t newCols = static_cast<size_t>(scaleData[1]);

                if (newRows == 0 || newCols == 0) {
                    Error(_W("Output dimensions must be positive."));
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
            }
        } else {
            Error(_W("Scale parameter must be a scalar or 2-element vector."));
        }
    } else {
        Error(_W("Scale parameter must be numeric."));
    }

    // Handle PrintSize option
    if (options.printSize && retval.size() > 0) {
        Dimensions outputDims = retval[0].getDimensions();
        // Print size information (in Nelson, this might go to command window)
        // This is a placeholder - actual implementation would depend on Nelson's output system
        std::wcout << L"Image resized to: " << outputDims.getRows() << L" x "
                   << outputDims.getColumns() << std::endl;
    }

    return retval;
}
//=============================================================================
}; // namespace Nelson::GraphicsGateway
//=============================================================================
