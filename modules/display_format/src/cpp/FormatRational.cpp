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
#include "StringHelpers.hpp"
#include "IEEEFP.hpp"
#include "FormatRational.hpp"
#include "FloatNumberToRational.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
formatRational(double number, size_t width, size_t lengthWithoutBlanks, bool trim)
{
    std::wstring str;
    if (IsIntegerForm(number)) {
        str = fmt::sprintf(L"%.f", number);
        size_t withoutBlanks = (number < 0) ? lengthWithoutBlanks - 2 : lengthWithoutBlanks - 1;
        if (str.length() >= withoutBlanks) {
            str = L"*";
        }
        str = fmt::sprintf(L"%*s", width, str);

    } else if (fabs(number) < 1e-10) {
        if (number < 0) {
            str = floatNumberToApproxRational<double, int64>(fabs(number), lengthWithoutBlanks - 1);
            str = L"-" + str;
        } else {
            str = floatNumberToApproxRational<double, int64>(number, lengthWithoutBlanks);
        }
        size_t withoutBlanks = (number < 0) ? lengthWithoutBlanks - 2 : lengthWithoutBlanks - 1;
        if (str.length() >= withoutBlanks) {
            str = L"*";
        }
        str = fmt::sprintf(L"%*s", width, str);
    } else {
        if (number < 0) {
            str = floatNumberToApproxRational<double, int>(fabs(number), lengthWithoutBlanks - 1);
            str = L"-" + str;
        } else {
            str = floatNumberToApproxRational<double, int>(number, lengthWithoutBlanks);
        }
        str = fmt::sprintf(L"%*s", width, str);
    }
    if (trim) {
        StringHelpers::trim_left(str);
    }
    return str;
}
//=============================================================================
std::wstring
formatRational(single number, size_t width, bool trim)
{
    std::wstring str;
    if (std::abs(number) < 1e-10) {
        str = floatNumberToApproxRational<double, int64>(number, width);
    } else {
        str = floatNumberToApproxRational<double, int>(number, width);
    }
    if (trim) {
        StringHelpers::trim_left(str);
    }
    return str;
}
//=============================================================================
}
//=============================================================================
