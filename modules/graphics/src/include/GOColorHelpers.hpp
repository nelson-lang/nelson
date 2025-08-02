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
#include <vector>
#include <string>
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IMPEXP bool
ParseColorToRGB(const ArrayOf& arg, std::vector<double>& data);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
ParseColorToRGB(const std::wstring& colorString, bool withNone, std::vector<double>& data);
//=============================================================================
}
//=============================================================================
