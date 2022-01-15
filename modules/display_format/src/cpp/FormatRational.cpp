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
#include <boost/algorithm/string.hpp>
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
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
        boost::trim_left(str);
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
        boost::trim_left(str);
    }
    return str;
}
//=============================================================================
}
//=============================================================================
