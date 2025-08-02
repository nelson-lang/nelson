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
formatPlus(double number, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatPlus(single number, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatComplexPlus(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatComplexPlus(single realPart, single imagPart, bool trim = false);
//=============================================================================
} // namespace Nelson
//=============================================================================
