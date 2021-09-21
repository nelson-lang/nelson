//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <limits>
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
std::wstring
floatNumberToApproxRational(T val, int len = 10)
{
    std::wstring s = L"NaN/NaN";
    if (std::isinf(val)) {
        if (val > 0) {
            s = L"1/0";
        } else {
            s = L"-1/0";
        }
        return s;
    }
    if (std::isnan(val)) {
        return L"0/0";
    }
    if (val < std::numeric_limits<int>::min() || val > std::numeric_limits<int>::max()
        || T(int(val)) == val) {
        return fmt::to_wstring(static_cast<int>(val));
    }
    T numerator = round(val);
    T denominator = 1;
    T lastNumerator = 1;
    T lastDenominator = 0;
    T frac = val - numerator;
    s = fmt::to_wstring(static_cast<int>(numerator));

    if (len <= 0) {
        len = 10;
    }
    while (true) {
        T nextNumerator = numerator;
        T nextDenominator = denominator;
        T flip = 1 / frac;
        T step = round(flip);
        if (std::abs(flip) > static_cast<T>(std::numeric_limits<int>::max())) {
            lastNumerator = numerator;
            lastDenominator = denominator;
            break;
        }
        lastNumerator = nextNumerator;
        lastDenominator = nextDenominator;
        frac = flip - step;
        numerator = step * numerator + lastNumerator;
        denominator = step * denominator + lastDenominator;

        s = fmt::to_wstring(static_cast<int>(numerator)) + L"/"
            + fmt::to_wstring(static_cast<int>(denominator));

        if (numerator >= 0 && denominator >= 0) {
            if (s.length() > static_cast<unsigned int>(len)) {
                break;
            }
        } else {
            if (s.length() > static_cast<unsigned int>(len + 2)) {
                break;
            }
        }

        if (std::abs(numerator) > std::numeric_limits<int>::max()
            || std::abs(denominator) > std::numeric_limits<int>::max()) {
            break;
        }
    }

    if (lastDenominator < 0) {
        s = fmt::to_wstring(static_cast<int>(-lastNumerator)) + L"/"
            + fmt::to_wstring(static_cast<int>(-lastDenominator));
    }
    return s;
}
//=============================================================================
}
//=============================================================================
