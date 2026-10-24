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
#include "ImageWarp.hpp"
#include "nlsImage_processing_exports.h"
#include <array>
//=============================================================================
namespace Nelson {
//=============================================================================
struct ImageWarp3DOptions
{
    std::array<double, 16> inverseTransform;
    double sourceXWorldStart { 0.5 };
    double sourceYWorldStart { 0.5 };
    double sourceZWorldStart { 0.5 };
    double sourcePixelExtentX { 1.0 };
    double sourcePixelExtentY { 1.0 };
    double sourcePixelExtentZ { 1.0 };
    double outputXWorldStart { 0.5 };
    double outputYWorldStart { 0.5 };
    double outputZWorldStart { 0.5 };
    double outputPixelExtentX { 1.0 };
    double outputPixelExtentY { 1.0 };
    double outputPixelExtentZ { 1.0 };
    indexType outputRows { 0 };
    indexType outputColumns { 0 };
    indexType outputPlanes { 0 };
    ImageWarpInterpolation interpolation { ImageWarpInterpolation::Linear };
    double fillValue { 0 };
};
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageWarp3D(const ArrayOf& volume, const ImageWarp3DOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
