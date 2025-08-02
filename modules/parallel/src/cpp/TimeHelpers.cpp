//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <ctime>
#include "TimeHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
uint64
getEpoch()
{
    return (uint64)std::time(nullptr);
}
//=============================================================================
std::wstring
epochToDateString(uint64 epoch)
{
    std::time_t result = (std::time_t)epoch;
    std::string asString = std::asctime(std::localtime(&result));
    asString.pop_back();
    return utf8_to_wstring(asString);
}
//=============================================================================
std::wstring
milliSecondsToDHMSMsString(uint64 n)
{
    uint64 days = n / (24 * 3600 * 1000);
    n = n % (24 * 3600 * 1000);
    uint64 hours = n / (3600 * 1000);
    n %= 3600 * 1000;
    uint64 minutes = n / (60 * 1000);
    n %= (60 * 1000);
    uint64 seconds = n / 1000;
    n %= 1000;
    uint64 ms = n;

    return std::to_wstring(days) + L" days " + std::to_wstring(hours) + L"h "
        + std::to_wstring(minutes) + L"m " + std::to_wstring(seconds) + L"s " + std::to_wstring(ms)
        + L"ms";
}
//=============================================================================
}
//=============================================================================
