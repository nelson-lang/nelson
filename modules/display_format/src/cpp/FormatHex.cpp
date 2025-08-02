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
#include "FormatHex.hpp"
#include "IEEEFP.hpp"
#include "Hexify.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatHex(double number, bool trim)
{
    std::wstring result;
    result = double2hexastr(number);
    if (trim) {
        StringHelpers::trim_left(result);
    }
    return result;
}
//=============================================================================
std::wstring
formatHex(single number, bool trim)
{
    std::wstring result;
    result = single2hexastr(number);
    if (trim) {
        StringHelpers::trim_left(result);
    }
    return result;
}
//=============================================================================
std::wstring
formatComplexHex(double realPart, double imgPart, bool trim)
{
    return formatHex(realPart, trim) + L"   " + formatHex(imgPart, trim) + L"i";
}
//=============================================================================
std::wstring
formatComplexHex(single realPart, single imgPart, bool trim)
{
    return formatHex(realPart, trim) + L"   " + formatHex(imgPart, trim) + L"i";
}
//=============================================================================
}
//=============================================================================
