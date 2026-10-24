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
#include <array>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class ImageWarpInterpolation
{
    Nearest,
    Linear,
    Cubic
};
//=============================================================================
struct ImageWarpOptions
{
    std::array<double, 9> inverseTransform;
    double sourceXWorldStart { 0.5 };
    double sourceYWorldStart { 0.5 };
    double sourcePixelExtentX { 1.0 };
    double sourcePixelExtentY { 1.0 };
    double outputXWorldStart { 0.5 };
    double outputYWorldStart { 0.5 };
    double outputPixelExtentX { 1.0 };
    double outputPixelExtentY { 1.0 };
    indexType outputRows { 0 };
    indexType outputColumns { 0 };
    ImageWarpInterpolation interpolation { ImageWarpInterpolation::Linear };
    std::vector<double> fillValues;
};
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageWarp(const ArrayOf& image, const ImageWarpOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
