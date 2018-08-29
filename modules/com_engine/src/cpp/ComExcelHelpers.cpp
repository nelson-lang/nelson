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
#define _SCL_SECURE_NO_WARNINGS
#include "ComExcelHelpers.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
xlsIndexToRange(indexType m, indexType n)
{
    std::wstring range = L"";
    size_t alpha_size = 'Z' - 'A' + 1;
    if (n > alpha_size * alpha_size) {
        std::wstring c_col1;
        c_col1.push_back((wchar_t)(L'A' + (n - 1) / (alpha_size * alpha_size) - 1));
        std::wstring c_col3;
        c_col3.push_back((wchar_t)(L'A' + (n - 1) % alpha_size));
        size_t value_col3 = (c_col3[0] - L'A') + 1;
        size_t value_col1 = (c_col1[0] - L'A') + 1;
        size_t value_col2 = (n - value_col3 - +(value_col1 * alpha_size * alpha_size)) / alpha_size;
        std::wstring c_col2;
        c_col2.push_back((wchar_t)(L'A' + value_col2 - 1));
        range = c_col1 + c_col2 + c_col3 + std::to_wstring(m);
    } else if (n > alpha_size) {
        std::wstring c_col1;
        c_col1.push_back((wchar_t)(L'A' + (n - 1) / alpha_size - 1));
        std::wstring c_col2;
        c_col2.push_back((wchar_t)(L'A' + (n - 1) % alpha_size));
        range = c_col1 + c_col2 + std::to_wstring(m);
    } else {
        std::wstring c_col;
        c_col.push_back((wchar_t)(L'A' + (n - 1) % alpha_size));
        range = c_col + std::to_wstring(m);
    }
    return range;
}
//=============================================================================
bool
isValidRange(std::wstring range)
{
    wstringVector splittedStrings;
    boost::split(splittedStrings, range, boost::is_any_of(L":"));
    if (splittedStrings.size() == 2) {
        std::wstring R1 = splittedStrings[0];
        std::wstring R2 = splittedStrings[1];
        boost::wregex expr{ L"^(?<column>[A-Z]+)(?<row>[1-9]\\d*)$" };
        try {
            return boost::regex_match(R1, expr) && boost::regex_match(R2, expr);
        } catch (const boost::regex_error&) {
            return false;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
