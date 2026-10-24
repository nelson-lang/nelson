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
#include <cmath>
#include <limits>
#include <vector>
#include "assert_extraBuiltins.hpp"
#include "assert_modernHelpers.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::AssertFunctionsGateway;
//=============================================================================
static std::wstring
formatSize(const Dimensions& dims)
{
    return dims.toWideString();
}
//=============================================================================
static std::wstring
formatExpectedSize(const std::vector<double>& expected)
{
    std::wstring result;
    for (size_t k = 0; k < expected.size(); ++k) {
        if (k > 0) {
            result += L"\u00D7";
        }
        result += fmt::format(L"{0}", expected[k]);
    }
    return result;
}
//=============================================================================
static bool
expectedSizeMatches(const Dimensions& actual, const std::vector<double>& expected)
{
    if (expected.empty() || expected.size() != actual.length()) {
        return false;
    }
    for (indexType k = 0; k < actual.length(); ++k) {
        double expectedValue = expected[k];
        if (expectedValue < 0. || expectedValue != std::floor(expectedValue)
            || static_cast<indexType>(expectedValue) != actual.dimensionLength(k)) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static std::wstring
joinWideStrings(const wstringVector& values)
{
    std::wstring result;
    for (size_t k = 0; k < values.size(); ++k) {
        if (k > 0) {
            result += L", ";
        }
        result += values[k];
    }
    return result;
}
//=============================================================================
static bool
isStringScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static wstringVector
getExpectedTypes(const ArrayOf& value)
{
    if (isStringScalar(value)) {
        return { value.asWideString() };
    }
    if (value.isCellArrayOfCharacterVectors() || value.isStringArray()) {
        return value.asWideStringVector(false);
    }
    Error(L"Nelson:asserts:invalidExpectedType",
        L"Expected type must be a string, character vector, string array, or cell of character "
        L"vectors.");
    return {};
}
//=============================================================================
static bool
containsString(const wstringVector& values, const std::wstring& expected)
{
    for (const std::wstring& value : values) {
        if (value == expected) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
validateElementCount(const ArrayOf& value, indexType& elementCount)
{
    if (!value.isNumeric() || value.isComplex() || value.isSparse() || !value.isScalar()) {
        return false;
    }
    double count = value.asDoubleScalar();
    if (!std::isfinite(count) || count < 0. || std::floor(count) != count
        || count > static_cast<double>(std::numeric_limits<indexType>::max())) {
        return false;
    }
    elementCount = static_cast<indexType>(count);
    return true;
}
//=============================================================================
