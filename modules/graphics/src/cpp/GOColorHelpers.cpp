//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOColorHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ParseColorToRGB(ArrayOf arg, std::vector<double>& data)
{
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        std::wstring cp = arg.getContentAsWideString();
        if (cp == GO_PROPERTY_VALUE_NONE_STR) {
            data.clear();
            data.push_back(-1);
            data.push_back(-1);
            data.push_back(-1);
        } else if (cp == L"y" || cp == L"yellow") {
            data.clear();
            data.push_back(1);
            data.push_back(1);
            data.push_back(0);
        } else if (cp == L"m" || cp == L"magenta") {
            data.clear();
            data.push_back(1);
            data.push_back(0);
            data.push_back(1);
        } else if (cp == L"c" || cp == L"cyan") {
            data.clear();
            data.push_back(0);
            data.push_back(1);
            data.push_back(1);
        } else if (cp == L"r" || cp == L"red") {
            data.clear();
            data.push_back(1);
            data.push_back(0);
            data.push_back(0);
        } else if (cp == L"g" || cp == L"green") {
            data.clear();
            data.push_back(0);
            data.push_back(1);
            data.push_back(0);
        } else if (cp == L"b" || cp == L"blue") {
            data.clear();
            data.push_back(0);
            data.push_back(0);
            data.push_back(1);
        } else if (cp == L"w" || cp == L"white") {
            data.clear();
            data.push_back(1);
            data.push_back(1);
            data.push_back(1);
        } else if (cp == L"k" || cp == L"black") {
            data.clear();
            data.push_back(0);
            data.push_back(0);
            data.push_back(0);
        } else
            return false;
    } else {
        if (arg.getElementCount() != 3) {
            return false;
        }
        arg.promoteType(NLS_DOUBLE);
        const double* dp = (const double*)arg.getDataPointer();
        if (((dp[0] < 0) || (dp[0] > 1)) || ((dp[1] < 0) || (dp[1] > 1))
            || ((dp[2] < 0) || (dp[2] > 1))) {
            return false;
        }
        data.clear();
        data.push_back(dp[0]);
        data.push_back(dp[1]);
        data.push_back(dp[2]);
    }
    return true;
}
//=============================================================================
}
//=============================================================================
