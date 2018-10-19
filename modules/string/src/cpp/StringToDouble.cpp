//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include "StringToDouble.hpp"
#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <math.h>
#include <limits>
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
        if (STR.compare(ToUpper(NanString)) == 0 || STR.compare(ToUpper(NegNanString)) == 0
            || STR.compare(ToUpper(PosNanString)) == 0) {
            res = nan("");
            wasConverted = true;
        } else if (STR.compare(ToUpper(NegInfString)) == 0) {
            res = returnInfinity(false);
            wasConverted = true;
        } else if (STR.compare(ToUpper(InfString)) == 0
            || STR.compare(ToUpper(PosInfString)) == 0) {
            res = returnInfinity(true);
            wasConverted = true;
        } else {
            STR = str;
            if (boost::algorithm::contains(str, L",")) {
                boost::replace_all(STR, L",", L"");
            }
            if (boost::algorithm::contains(STR, L" ")) {
                boost::algorithm::trim_left(STR);
                boost::algorithm::trim_right(STR);
            }
            if (boost::algorithm::contains(STR, L"d")) {
                boost::replace_all(STR, L"d", L"e");
            }
            if (boost::algorithm::contains(STR, L"D")) {
                boost::replace_all(STR, L"D", L"e");
            }
            double v = nan("");
            int err = swscanf(STR.c_str(), STR2DOUBLE_MAX_DIGIT_FORMAT, &v);
            if (err == 1) {
                double v2;
                wchar_t* pEnd = NULL;
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
}
//=============================================================================
