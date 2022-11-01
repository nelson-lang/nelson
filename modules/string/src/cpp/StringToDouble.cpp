//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
#include "StringToDouble.hpp"
#include "StringHelpers.hpp"
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
        res = nan("");
        wasConverted = true;
    } else {
        std::wstring STR = ToUpper(str);
        if (STR == ToUpper(NanString) || STR == ToUpper(NegNanString)
            || STR == ToUpper(PosNanString)) {
            res = nan("");
            wasConverted = true;
        } else if (STR == ToUpper(NegInfString)) {
            res = returnInfinity(false);
            wasConverted = true;
        } else if (STR == ToUpper(InfString) || STR == ToUpper(PosInfString)) {
            res = returnInfinity(true);
            wasConverted = true;
        } else {
            STR = str;
            if (StringHelpers::contains(str, L",")) {
                StringHelpers::replace_all(STR, L",", L"");
            }
            if (StringHelpers::contains(STR, L" ")) {
                StringHelpers::trim_left(STR);
                StringHelpers::trim_right(STR);
            }
            if (StringHelpers::contains(STR, L"d")) {
                StringHelpers::replace_all(STR, L"d", L"e");
            }
            if (StringHelpers::contains(STR, L"D")) {
                StringHelpers::replace_all(STR, L"D", L"e");
            }
            double v = nan("");
            int err = swscanf(STR.c_str(), STR2DOUBLE_MAX_DIGIT_FORMAT, &v);
            if (err == 1) {
                double v2;
                wchar_t* pEnd = nullptr;
                v2 = wcstod(STR.c_str(), &pEnd);
                if (pEnd != nullptr) {
                    if (wcslen(pEnd) == 0) {
                        res = v2;
                        wasConverted = true;
                    } else {
                        res = nan("");
                        wasConverted = true;
                    }
                } else {
                    wasConverted = true;
                    res = nan("");
                }
            } else {
                wasConverted = true;
                res = nan("");
            }
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
