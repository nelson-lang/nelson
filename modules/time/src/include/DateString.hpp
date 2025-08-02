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
epochToDateTimeString(int Y, int M, int D, int H, int MN, int S, int MS, bool forceFullFormat,
    bool isLocalized, int predefinedFormatOutValue);
//=============================================================================
NLSTIME_IMPEXP std::wstring
epochToDateTimeString(
    double dateSerial, bool forceFullFormat, bool isLocalized, int predefinedFormatOutValue);
//=============================================================================
NLSTIME_IMPEXP wstringVector
DateString(std::vector<double> epochValues, const Dimensions& dimensionsIn, bool isLocalized,
    int predefinedFormatOutValue);
//=============================================================================
} // namespace Nelson
//=============================================================================
