//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsImage_processing_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class InterpolationMethod
{
    Nearest, // Nearest-neighbor interpolation
    Bilinear, // Bilinear interpolation
    Bicubic // Bicubic interpolation
};
//=============================================================================
enum class BoundingBox
{
    Crop, // Output image is cropped to the same size as input
    Loose // Output image is enlarged to fit the entire rotated image
};
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageRotation(
    const ArrayOf& image, double angle, InterpolationMethod method, BoundingBox boundingBox);
//=============================================================================
}
//=============================================================================
