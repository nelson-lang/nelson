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
#include "Types.hpp"
#include "nlsDisplay_format_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatBank(double number, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatBank(single number, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatComplexBank(double realPart, double imagPart, bool trim = false);
//=============================================================================
NLSDISPLAY_FORMAT_IMPEXP
std::wstring
formatComplexBank(single realPart, single imagPart, bool trim = false);
//=============================================================================
} // namespace Nelson
//=============================================================================
