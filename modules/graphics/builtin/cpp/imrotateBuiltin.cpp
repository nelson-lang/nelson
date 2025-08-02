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
#include "ImageRotation.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
imrotateBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);

    InterpolationMethod method = InterpolationMethod::Nearest;
    BoundingBox boundingBox = BoundingBox::Loose;

    // Extract input image
    ArrayOf inputImage = argIn[0];
    if (!inputImage.isNumeric() && !inputImage.isLogical()) {
        Error(_("Input image must be numeric or logical."));
    }

    // Extract dimensions
    Dimensions dims = inputImage.getDimensions();
    if (inputImage.isSparse() || dims.getLength() > 3) {
        Error(_("Input image must be 2-D."));
    }

    // Extract rotation angle
    if (!argIn[1].isNumeric() || !argIn[1].isScalar()) {
        Error(_("Angle must be a numeric scalar."));
    }
    double angle = argIn[1].getContentAsDoubleScalar();
    if (!std::isfinite(angle)) {
        Error(_("Angle must be a finite value."));
    }

    // Default interpolation method: "nearest"
    std::wstring methodAsString = L"nearest";
    if (argIn.size() >= 3
        && (argIn[2].isScalarStringArray() || argIn[2].isRowVectorCharacterArray())) {
        methodAsString = argIn[2].getContentAsWideString();
    }
    if (methodAsString == L"nearest") {
        method = InterpolationMethod::Nearest;
    } else if (methodAsString == L"bilinear") {
        method = InterpolationMethod::Bilinear;
    } else if (methodAsString == L"bicubic") {
        method = InterpolationMethod::Bicubic;
    } else {
        Error(_("Interpolation method must be 'nearest', 'bilinear', or 'bicubic'."));
    }

    // Default bounding box: "loose"
    std::wstring bbox = L"loose";
    if (argIn.size() >= 4
        && (argIn[3].isScalarStringArray() || argIn[3].isRowVectorCharacterArray())) {
        bbox = argIn[3].getContentAsWideString();
    }

    if (bbox == L"loose") {
        boundingBox = BoundingBox::Loose;
    } else if (bbox == L"crop") {
        boundingBox = BoundingBox::Crop;
    } else {
        Error(_("Bounding box must be 'loose' or 'crop'."));
    }
    ArrayOfVector retval;
    retval << ImageRotation(inputImage, angle, method, boundingBox);
    return retval;
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
