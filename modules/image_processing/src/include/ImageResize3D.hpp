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
enum class ImageResize3DInterpolation
{
    Nearest,
    Linear
};
//=============================================================================
struct ImageResize3DOptions
{
    indexType rows { 0 };
    indexType columns { 0 };
    indexType planes { 0 };
    ImageResize3DInterpolation interpolation { ImageResize3DInterpolation::Linear };
};
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageResize3D(const ArrayOf& volume, const ImageResize3DOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
