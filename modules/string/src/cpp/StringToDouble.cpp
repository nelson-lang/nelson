//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <algorithm>
#include <cmath>
#include <limits>
#include <cwctype>
#include <fast_float/fast_float.h>

#include "StringToDouble.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define STR2DOUBLE_MAX_DIGIT_FORMAT L"%lg"
//=============================================================================
static std::wstring
ToUpper(const std::wstring& A)
{
    std::wstring res = A;
    transform(res.begin(), res.end(), res.begin(), towupper);
    return res;
}
//=============================================================================
static double
returnInfinity(bool bPositive)
{
    double res = std::numeric_limits<double>::infinity();
    if (!bPositive) {
        res = -res;
    }
    return res;
}
//=============================================================================
double
stringToDouble(const std::wstring& str, bool& wasConverted)
{
    double res = nan("");
    wasConverted = false;
    if (str.empty()) {
        wasConverted = true;
        return res;
    }
    std::wstring STR = ToUpper(str);
    if (STR == ToUpper(NanString) || STR == ToUpper(NegNanString) || STR == ToUpper(PosNanString)) {
        res = nan("");
        wasConverted = true;
    } else if (STR == ToUpper(NegInfString)) {
        res = returnInfinity(false);
        wasConverted = true;
    } else if (STR == ToUpper(InfString) || STR == ToUpper(PosInfString)) {
        res = returnInfinity(true);
        wasConverted = true;
    } else {
        if (StringHelpers::contains(str, L",")) {
            StringHelpers::replace_all(STR, L",", L"");
        }
        if (StringHelpers::contains(STR, L" ")) {
            StringHelpers::trim_left(STR);
            StringHelpers::trim_right(STR);
        }
        bool isnegative = false;
        if (STR[0] == L'-') {
            STR.erase(0, 1);
            isnegative = true;
        } else if (STR[0] == L'+') {
            STR.erase(0, 1);
        }

        std::string utf8str = wstring_to_utf8(STR);
        fast_float::parse_options options { fast_float::chars_format::fortran };
        auto answer = fast_float::from_chars_advanced(
            utf8str.data(), utf8str.data() + utf8str.size(), res, options);
        size_t len = strlen(answer.ptr);
        if (answer.ec == std::errc() && len == 0) {
            if (isnegative) {
                res = -res;
            }
            wasConverted = true;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
