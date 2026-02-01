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
    const int width = 13;
    std::wstring result;
    if (std::isnan(number)) {
        result = fmt::format(L"{:>{}}", L"NaN", width);
    } else if (std::isinf(number)) {
        result = (number < 0) ? fmt::format(L"{:>{}}", L"-Inf", width)
                              : fmt::format(L"{:>{}}", L"Inf", width);
    } else {
        result = fmt::format(L"{:13.2f}", number);
    }
    result = L" " + result;
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
