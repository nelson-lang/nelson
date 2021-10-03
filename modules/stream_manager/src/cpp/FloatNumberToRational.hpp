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
#include <iomanip>
#include <string>
#include <limits>
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T, typename CAST>
std::wstring
formatAsRational(T numerator, T denominator)
{
    if (denominator < 0) {
        numerator = -numerator;
        denominator = -denominator;
    }
    return fmt::to_wstring(static_cast<CAST>(numerator)) + L"/"
        + fmt::to_wstring(static_cast<CAST>(denominator));
}
//=============================================================================
template <typename T, typename CAST>
std::wstring
floatNumberToApproxRational(T value, int length = 10)
{
    if (std::isinf(value)) {
        if (value > 0) {
            return L"1/0";
        }
        return L"-1/0";
    }
    if (std::isnan(value)) {
        return L"0/0";
    }
    if (value < std::numeric_limits<CAST>::min() || value > std::numeric_limits<CAST>::max()
        || T(CAST(value)) == value) {
        return fmt::to_wstring(static_cast<CAST>(value));
    } else {
        if (length <= 0) {
            length = 10;
        }

        T lastNumerator = 1;
        T lastDenominator = 0;
        T numerator = round(value);
        T denominator = 1;
        T fraction = value - numerator;

        std::wstring result = fmt::to_wstring(static_cast<CAST>(numerator));

        while (true) {
            T flip = 1 / fraction;
            T step = round(flip);
            T nextNumerator = numerator;
            T nextDenominator = denominator;
            if (std::abs(flip) > static_cast<T>(std::numeric_limits<CAST>::max())) {
                lastNumerator = numerator;
                lastDenominator = denominator;
                break;
            }

            fraction = flip - step;
            numerator = step * numerator + lastNumerator;
            denominator = step * denominator + lastDenominator;
            lastNumerator = nextNumerator;
            lastDenominator = nextDenominator;

            std::wstring asRational = formatAsRational<T, CAST>(numerator, denominator);
            size_t sLen = asRational.length();

            if (numerator >= 0 && denominator >= 0) {
                if (sLen > static_cast<size_t>(length)) {
                    if (std::abs(value) < 1e-10) {
                        return asRational;
                    }
                    break;
                }
            } else {
                if (sLen > static_cast<size_t>(length + 2)) {
                    if (std::abs(value) < 1e-10) {
                        return asRational;
                    }
                    break;
                }
            }

            if (std::abs(numerator) > std::numeric_limits<CAST>::max()
                || std::abs(denominator) > std::numeric_limits<CAST>::max()) {
                break;
            }
            result = asRational;
        }

        if (lastDenominator < 0) {
            result = formatAsRational<T, CAST>(-lastNumerator, -lastDenominator);
        }
        if (result == L"0") {
            return L"NaN/NaN";
        }
        return result;
    }
    return L"NaN/NaN";
}
//=============================================================================
}
//=============================================================================
