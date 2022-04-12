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
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
NLSTIME_IMPEXP double
DateNumber(double year, double month, double day, double hour = 0, double minutes = 0,
    double secondes = 0);
NLSTIME_IMPEXP double
DateNumber(const std::wstring& datestring, const std::wstring& formatIn, bool& bParsed);
NLSTIME_IMPEXP double
DateNumber(const std::wstring& datestring, bool& bParsed);
} // namespace Nelson
//=============================================================================
