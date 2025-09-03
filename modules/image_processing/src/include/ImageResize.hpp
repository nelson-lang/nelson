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
enum class ResizeInterpolationMethod
{
    Nearest,
    Bilinear,
    Bicubic,
    Box,
    Lanczos2,
    Lanczos3
};
//=============================================================================
enum class ResizeAntialiasing
{
    Auto,
    On,
    Off
};
//=============================================================================
struct ResizeOptions
{
    ResizeInterpolationMethod method = ResizeInterpolationMethod::Bicubic;
    ResizeAntialiasing antialiasing = ResizeAntialiasing::Auto;
    bool outputSize = false;
    std::pair<size_t, size_t> outputSizeValue = { 0, 0 };
    std::wstring colormapMode = L"optimized";
    bool dither = true;
    // Default constructor
    ResizeOptions() = default;
};
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageResize(const ArrayOf& image, double scale, const ResizeOptions& options = ResizeOptions());
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageResize(const ArrayOf& image, size_t numrows, size_t numcols,
    const ResizeOptions& options = ResizeOptions());
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP std::pair<ArrayOf, ArrayOf>
ImageResizeIndexed(const ArrayOf& X, const ArrayOf& map, double scale,
    const ResizeOptions& options = ResizeOptions());
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP std::pair<ArrayOf, ArrayOf>
ImageResizeIndexed(const ArrayOf& X, const ArrayOf& map, size_t numrows, size_t numcols,
    const ResizeOptions& options = ResizeOptions());
//=============================================================================
}
//=============================================================================
