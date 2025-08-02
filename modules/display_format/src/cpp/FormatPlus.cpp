//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "IEEEFP.hpp"
#include "FormatPlus.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatPlus(double number, bool trim)
{
    std::wstring result;
    if (number == 0) {
        if (trim) {
            result = L"";
        } else {
            result = L" ";
        }
    } else if (number < 0) {
        result = L"-";
    } else {
        result = L"+";
    }
    return result;
}
//=============================================================================
std::wstring
formatPlus(single number, bool trim)
{
    return formatPlus((double)number, trim);
}
//=============================================================================
std::wstring
formatComplexPlus(double realPart, double imagPart, bool trim)
{
    return formatPlus(realPart, trim);
}
//=============================================================================
std::wstring
formatComplexPlus(single realPart, single imagPart, bool trim)
{
    return formatPlus(realPart, trim);
}
//=============================================================================
}
//=============================================================================
