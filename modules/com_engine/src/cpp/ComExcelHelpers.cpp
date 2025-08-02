//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
#include <regex>
#include "StringHelpers.hpp"
#include "ComExcelHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
xlsIndexToRange(indexType m, indexType n)
{
    std::wstring range;
    size_t alpha_size = 'Z' - 'A' + 1;
    if (n > (indexType(alpha_size * alpha_size))) {
        std::wstring c_col1;
        c_col1.push_back(static_cast<wchar_t>(L'A' + (n - 1) / (alpha_size * alpha_size) - 1));
        std::wstring c_col3;
        c_col3.push_back(static_cast<wchar_t>(L'A' + (n - 1) % alpha_size));
        size_t value_col3 = (c_col3[0] - L'A') + 1;
        size_t value_col1 = (c_col1[0] - L'A') + 1;
        size_t value_col2 = (n - value_col3 - +(value_col1 * alpha_size * alpha_size)) / alpha_size;
        std::wstring c_col2;
        c_col2.push_back(static_cast<wchar_t>(L'A' + value_col2 - 1));
        range = c_col1 + c_col2 + c_col3 + std::to_wstring(m);
    } else if (n > (indexType)alpha_size) {
        std::wstring c_col1;
        c_col1.push_back(static_cast<wchar_t>(L'A' + (n - 1) / alpha_size - 1));
        std::wstring c_col2;
        c_col2.push_back(static_cast<wchar_t>(L'A' + (n - 1) % alpha_size));
        range = c_col1 + c_col2 + std::to_wstring(m);
    } else {
        std::wstring c_col;
        c_col.push_back(static_cast<wchar_t>(L'A' + (n - 1) % alpha_size));
        range = c_col + std::to_wstring(m);
    }
    return range;
}
//=============================================================================
bool
isValidRange(const std::wstring& range)
{
    wstringVector splittedStrings;
    StringHelpers::split(splittedStrings, range, L':');
    if (splittedStrings.size() == 2) {
        std::wstring R1 = splittedStrings[0];
        std::wstring R2 = splittedStrings[1];
        std::wregex expr { L"[A-Z]+[1-9]+" };
        try {
            return std::regex_match(R1, expr) && std::regex_match(R2, expr);
        } catch (const std::regex_error&) {
            return false;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
