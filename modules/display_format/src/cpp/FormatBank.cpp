//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "FormatBank.hpp"
#include "IEEEFP.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatBank(double number, bool trim)
{
    std::wstring result;

    if (std::isnan(number)) {
        std::wstring format = L"%*s";
        result = fmt::sprintf(format, 13, L"NaN");
    } else if (std::isinf(number)) {
        std::wstring format = L"%*s";
        if (number < 0) {
            result = fmt::sprintf(format, 13, L"-Inf");
        } else {
            result = fmt::sprintf(format, 13, L"Inf");
        }
    } else {
        std::wstring format = L"%13.2f";
        result = fmt::sprintf(format, number);
    }
    if (result != L" ") {
        result = L" " + result;
    }
    if (trim) {
        StringHelpers::trim_left(result);
    }
    return result;
}
//=============================================================================
std::wstring
formatBank(single number, bool trim)
{
    return formatBank((double)number, trim);
}
//=============================================================================
std::wstring
formatComplexBank(double realPart, double imagPart, bool trim)
{
    return formatBank(realPart, trim);
}
//=============================================================================
std::wstring
formatComplexBank(single realPart, single imagPart, bool trim)
{
    return formatBank(realPart, trim);
}
//=============================================================================
}
//=============================================================================
