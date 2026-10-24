//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <algorithm>
#include "ErrorID.hpp"
#include "assert_extraBuiltins.hpp"
#include "assert_modernHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::AssertFunctionsGateway;
//=============================================================================
static std::string
asFieldName(const ArrayOf& value)
{
    if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray())) {
        Error(L"Nelson:asserts:invalidField",
            L"Field name must be a string scalar or character vector.");
    }
    return value.asUtf8String();
}
//=============================================================================
static stringVector
fieldList(const ArrayOf& value)
{
    if (!(value.isRowVectorCharacterArray() || value.isScalarStringArray()
            || value.isCellArrayOfCharacterVectors() || value.isStringArray())) {
        Error(L"Nelson:asserts:invalidArgument:stringExpected",
            L"String, character vector, string array, or cell of character vectors expected.");
    }
    return value.asUtf8StringVector(false);
}
//=============================================================================
static bool
containsField(const stringVector& names, const std::string& expected)
{
    return std::find(names.begin(), names.end(), expected) != names.end();
}
//=============================================================================
static std::wstring
joinFields(stringVector names)
{
    std::sort(names.begin(), names.end());
    std::wstring result;
    for (size_t k = 0; k < names.size(); ++k) {
        if (k > 0) {
            result += L", ";
        }
        result += std::wstring(names[k].begin(), names[k].end());
    }
    return result;
}
//=============================================================================
