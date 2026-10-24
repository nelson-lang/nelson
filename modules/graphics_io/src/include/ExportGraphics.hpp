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
#include <string>
#include "nlsGraphics_io_exports.h"
#include "GOFigureHost.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum IMAGE_FORMAT
{
    PNG_EXPORT = 0,
    JPG_EXPORT,
    GIF_EXPORT,
    TIF_EXPORT,
    PDF_EXPORT,
    PS_EXPORT,
    EPS_EXPORT,
    SVG_EXPORT,
    WEBP_EXPORT,
    BMP_EXPORT,
    TGA_EXPORT,
    PBM_EXPORT,
    PGM_EXPORT,
    PPM_EXPORT,
    PNM_EXPORT,
    PCX_EXPORT,
    ERROR_EXPORT
};
//=============================================================================
using ExportGraphicsDialogProvider = bool (*)(GOFigureHost*);
NLSGRAPHICS_IO_IMPEXP void
setExportGraphicsDialogProvider(ExportGraphicsDialogProvider provider);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
isSupportedImageFormatExtension(const std::wstring& extension);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP IMAGE_FORMAT
getExportImageFormatFromString(const std::wstring& extension);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP std::wstring
getExportImageFormatAsString(IMAGE_FORMAT exportFormat);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
ExportGraphics(GOFigureHost* f, const std::wstring& filename, IMAGE_FORMAT exportFormat);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
ExportGraphicsAtScale(
    GOFigureHost* f, const std::wstring& filename, IMAGE_FORMAT exportFormat, double exportScale);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP bool
ExportGraphicsGUI(GOFigureHost* f);
//=============================================================================
} // namespace Nelson
//=============================================================================
extern "C"
{
    NLSGRAPHICS_IO_IMPEXP bool
    ExportGraphicsGUI(void* f);
};
//=============================================================================
