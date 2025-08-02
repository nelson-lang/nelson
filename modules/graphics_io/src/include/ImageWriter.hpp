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
#include <map>
#include "nlsGraphics_io_exports.h"
#include "nlsBuildConfig.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DEFAULT_QUALITY 75
#define DEFAULT_DELAY -1
#define DEFAULT_LOOPCOUNT 0
//=============================================================================
#if WITH_GIF
NLSGRAPHICS_IO_IMPEXP void
writeGif(const std::wstring& filename, bool append, const ArrayOf& A, const ArrayOf& colorMap,
    const ArrayOf& alphaMap, int quality, int delayTime, int loopCount,
    const std::map<std::wstring, wstringVector>& nameValue);
#endif
//=============================================================================
#if WITH_TIFF
NLSGRAPHICS_IO_IMPEXP void
writeTiff(const std::wstring& filename, const ArrayOf& A, const ArrayOf& colorMap,
    const ArrayOf& alphaMap, const std::map<std::wstring, wstringVector>& nameValue);
#endif
//=============================================================================
NLSGRAPHICS_IO_IMPEXP void
writePcx(const std::wstring& filename, const ArrayOf& A, const ArrayOf& colorMap,
    const ArrayOf& alphaMap, int quality, const std::map<std::wstring, wstringVector>& nameValue);
//=============================================================================
NLSGRAPHICS_IO_IMPEXP void
writeImage(const std::wstring& filename, const ArrayOf& A, const ArrayOf& colorMap,
    const std::wstring& format, const ArrayOf& alphaMap, int quality,
    const std::map<std::wstring, wstringVector>& nameValue);
//=============================================================================
}
//=============================================================================
