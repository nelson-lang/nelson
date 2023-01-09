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
#include <map>
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define DEFAULT_QUALITY 75
//=============================================================================
NLSGRAPHICS_IMPEXP void
imageWriter(const std::wstring& filename, const ArrayOf& A, const ArrayOf& colorMap,
    const std::wstring& format, const ArrayOf& alphaMap, int quality,
    const std::map<std::wstring, wstringVector>& nameValue);

//=============================================================================
}
//=============================================================================
