//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsGraphics_exports.h"
#include "GOWindow.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum IMAGE_FORMAT
{
    PNG_EXPORT,
    JPG_EXPORT,
    PDF_EXPORT,
    PS_EXPORT,
    EPS_EXPORT,
    SVG_EXPORT,
    ERROR_EXPORT
};
//=============================================================================
NLSGRAPHICS_IMPEXP bool
isSupportedImageFormatExtension(const std::wstring& extension);
//=============================================================================
NLSGRAPHICS_IMPEXP IMAGE_FORMAT
getExportImageFormatFromString(const std::wstring& extension);
//=============================================================================
NLSGRAPHICS_IMPEXP std::wstring
getExportImageFormatAsString(IMAGE_FORMAT exportFormat);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
ExportGraphics(GOWindow* f, const std::wstring& filename, IMAGE_FORMAT exportFormat);
//=============================================================================
}
//=============================================================================
