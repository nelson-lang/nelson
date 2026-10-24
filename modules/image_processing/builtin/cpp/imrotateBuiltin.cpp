//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "imrotateBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ImageProcessingCore.hpp"
#include "ImageRotation.hpp"
#include <cwctype>
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
static std::wstring
toLowerAscii(std::wstring value)
{
    for (auto& ch : value) {
        ch = static_cast<wchar_t>(std::towlower(ch));
    }
    return value;
}
//=============================================================================
static void
validateImage2DOrRGBA(const ArrayOf& image)
{
    if (!image.isNumeric() && !image.isLogical()) {
        Error(L"Nelson:validators:mustBeNumericOrLogical", L"%s must be numeric or logical.",
            L"Input image");
    }
    if (image.isSparse()) {
        Error(L"Nelson:image_processing:sparseImagesNotSupported", L"%s are not supported.",
            L"Sparse images");
    }
    if (!image.isReal()) {
        Error(L"Nelson:validators:mustBeReal", L"%s must be real.", L"Input image");
    }
    Dimensions dims = image.dimensions();
    if (dims.length() > 3) {
        Error(L"Nelson:image_processing:imageDRgbRgbaArray",
            L"%s must be a 2-D, RGB, or RGBA array.", L"Input image");
    }
    indexType channels = (dims.length() > 2) ? dims.dimensionLength(2) : 1;
    if (channels != 1 && channels != 3 && channels != 4) {
        Error(L"Nelson:image_processing:imageDRgbRgbaArray",
            L"%s must be a 2-D, RGB, or RGBA array.", L"Input image");
    }
}
//=============================================================================
ArrayOfVector
imrotateBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);

    InterpolationMethod method = InterpolationMethod::Nearest;
    BoundingBox boundingBox = BoundingBox::Loose;

    ArrayOf inputImage = argIn[0];
    validateImage2DOrRGBA(inputImage);

    if (!argIn[1].isNumeric() || !argIn[1].isScalar()) {
        Error("Nelson:imrotate:invalidAngleType", "Angle must be a numeric scalar.");
    }
    double angle = argIn[1].asDoubleScalar();
    if (!std::isfinite(angle)) {
        Error("Nelson:imrotate:invalidAngleValue", "Angle must be a finite value.");
    }

    std::wstring methodAsString = L"nearest";
    if (argIn.size() >= 3) {
        if (!(argIn[2].isScalarStringArray() || argIn[2].isRowVectorCharacterArray())) {
            Error("Nelson:imrotate:invalidMethodType", "Interpolation method must be a string.");
        }
        methodAsString = toLowerAscii(argIn[2].asWideString());
    }
    if (methodAsString == L"nearest") {
        method = InterpolationMethod::Nearest;
    } else if (methodAsString == L"bilinear") {
        method = InterpolationMethod::Bilinear;
    } else if (methodAsString == L"bicubic") {
        method = InterpolationMethod::Bicubic;
    } else {
        Error("Nelson:imrotate:invalidMethodValue",
            "Interpolation method must be 'nearest', 'bilinear', or 'bicubic'.");
    }

    std::wstring bbox = L"loose";
    if (argIn.size() >= 4) {
        if (!(argIn[3].isScalarStringArray() || argIn[3].isRowVectorCharacterArray())) {
            Error("Nelson:imrotate:invalidBoundingBoxType", "Bounding box must be a string.");
        }
        bbox = toLowerAscii(argIn[3].asWideString());
    }

    if (bbox == L"loose") {
        boundingBox = BoundingBox::Loose;
    } else if (bbox == L"crop") {
        boundingBox = BoundingBox::Crop;
    } else {
        Error("Nelson:imrotate:invalidBoundingBoxValue", "Bounding box must be 'loose' or 'crop'.");
    }
    ArrayOfVector retval;
    retval << ImageRotation(inputImage, angle, method, boundingBox);
    return retval;
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
