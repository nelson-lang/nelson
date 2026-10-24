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
#include <memory>
#include <string>
#include "nlsTime_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTIME_IMPEXP double
DateNumber(double year, double month, double day, double hour = 0, double minutes = 0,
    double secondes = 0, double milliseconds = 0);
//=============================================================================
NLSTIME_IMPEXP double
DateNumber(const std::wstring& datestring, const std::wstring& formatIn, bool withPivot,
    int pivotYear, bool& bParsed);
//=============================================================================
NLSTIME_IMPEXP double
DateNumber(const std::wstring& datestring, bool withPivot, int pivotYear, bool& bParsed);
//=============================================================================
// Precompiled date format: build once, reuse for many date strings. The
// expensive regex compilation and format-list construction happen here, not on
// every element, which keeps datenum(cellstr, format) linear instead of
// recompiling the whole format list per string.
struct DateFormatCompiled;
//=============================================================================
// Compile a date format once. An empty formatIn selects the auto-detect mode
// (try every standard format); a non-empty formatIn selects the explicit-format
// mode (candidates matching formatIn, then the token-based fallback).
NLSTIME_IMPEXP std::shared_ptr<DateFormatCompiled>
compileDateNumberFormat(const std::wstring& formatIn, bool withPivot, int pivotYear);
//=============================================================================
NLSTIME_IMPEXP double
DateNumber(const std::wstring& datestring, const DateFormatCompiled& compiled, bool& bParsed);
//=============================================================================
} // namespace Nelson
//=============================================================================
