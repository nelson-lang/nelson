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
#include <vector>
#include "nlsTime_exports.h"
#include "Types.hpp"
#include "Dimensions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSTIME_IMPEXP std::wstring
epochToUserFormatDateTimeString(
    int Y, int M, int D, int H, int MN, int S, const std::wstring& userFormat, bool isLocalized);
//=============================================================================
NLSTIME_IMPEXP std::wstring
epochToUserFormatDateTimeString(
    double dateSerial, const std::wstring& userFormat, bool isLocalized);
//=============================================================================
NLSTIME_IMPEXP wstringVector
dateToUserFormatString(std::vector<double> epochValues, const Dimensions& dimensionsIn,
    const std::wstring& userFormat, bool isLocalized);
//=============================================================================
} // namespace Nelson
//=============================================================================
