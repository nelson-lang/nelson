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
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class ImageFilterShape
{
    Same,
    Full,
    Valid
};
//=============================================================================
enum class ImageFilterPaddingKind
{
    Constant,
    Replicate,
    Symmetric,
    Circular
};
//=============================================================================
struct ImageFilterPadding
{
    ImageFilterPaddingKind kind { ImageFilterPaddingKind::Constant };
    double constantValue { 0.0 };
};
//=============================================================================
struct ImageFilterOptions
{
    ImageFilterShape shape { ImageFilterShape::Same };
    ImageFilterPadding padding;
    bool convolution { false };
};
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageFilter(const ArrayOf& image, const ArrayOf& kernel, const ImageFilterOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
