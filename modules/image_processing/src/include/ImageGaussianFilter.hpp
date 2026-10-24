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
#include "ImageFilter.hpp"
#include "nlsImage_processing_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
struct ImageGaussianFilterOptions
{
    double sigmaRows { 0.5 };
    double sigmaColumns { 0.5 };
    indexType filterRows { 3 };
    indexType filterColumns { 3 };
    ImageFilterPadding padding { ImageFilterPaddingKind::Replicate, 0.0 };
};
//=============================================================================
NLSIMAGE_PROCESSING_IMPEXP ArrayOf
ImageGaussianFilter(const ArrayOf& image, const ImageGaussianFilterOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
