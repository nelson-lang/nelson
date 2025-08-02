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
#include "Types.hpp"
#include "nlsDisplay_format_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatHex(double number, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatHex(single number, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatComplexHex(double realPart, double imgPart, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatComplexHex(single realPart, single imgPart, bool trim = false);
//=============================================================================
} // namespace Nelson
//=============================================================================
