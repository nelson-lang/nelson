//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <cwctype>
#include "mustBeSortedBuiltin.hpp"
#include "ValidatorsInternal.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isTextScalar(const ArrayOf& arg);
static std::wstring
lowerTextScalar(const ArrayOf& arg);
static bool
isOneOf(const std::wstring& value, const wstringVector& choices);
static indexType
parseDimension(const ArrayOf& arg);
//=============================================================================
// mustBeSorted(A)
// mustBeSorted(A, dim)
// mustBeSorted(___, direction)
// mustBeSorted(___, MissingPlacement=..., ComparisonMethod=...)
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::mustBeSortedBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1);
    const wstringVector directions = { L"ascend", L"descend", L"monotonic", L"strictascend",
        L"strictdescend", L"strictmonotonic" };
    size_t pos = 1;
    indexType dim = 0;
    std::wstring direction = L"ascend";
    std::wstring missingPlacement = L"auto";
    std::wstring comparisonMethod = L"auto";
    if (pos < argIn.size() && !isTextScalar(argIn[pos])) {
        dim = parseDimension(argIn[pos]);
        pos++;
    }
    if (pos < argIn.size() && isOneOf(lowerTextScalar(argIn[pos]), directions)) {
        direction = lowerTextScalar(argIn[pos]);
        pos++;
    }
    if ((argIn.size() - pos) % 2 != 0) {
        Error(L"Nelson:validators:invalidSortDirection",
            L"Sorting direction must be 'ascend', 'descend', 'monotonic', 'strictascend', "
            L"'strictdescend' or 'strictmonotonic'.");
    }
    for (; pos < argIn.size(); pos += 2) {
        std::wstring name = lowerTextScalar(argIn[pos]);
        std::wstring value = lowerTextScalar(argIn[pos + 1]);
        if (name == L"missingplacement") {
            if (!isOneOf(value, { L"auto", L"first", L"last" })) {
                Error(L"Nelson:validators:invalidMissingPlacement",
                    L"MissingPlacement must be 'auto', 'first' or 'last'.");
            }
            missingPlacement = value;
        } else if (name == L"comparisonmethod") {
            if (!isOneOf(value, { L"auto", L"real", L"abs" })) {
                Error(L"Nelson:validators:invalidComparisonMethod",
                    L"ComparisonMethod must be 'auto', 'real' or 'abs'.");
            }
            comparisonMethod = value;
        } else {
            Error(L"Nelson:validators:invalidNameValueArgument",
                L"Name-value argument must be 'MissingPlacement' or 'ComparisonMethod'.");
        }
    }
    mustBeSorted(argIn[0], dim, direction, missingPlacement, comparisonMethod, -1, true);
    return retval;
}
//=============================================================================
bool
isTextScalar(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar());
}
//=============================================================================
std::wstring
lowerTextScalar(const ArrayOf& arg)
{
    if (!isTextScalar(arg)) {
        return {};
    }
    std::wstring text = arg.asWideString();
    std::transform(text.begin(), text.end(), text.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return text;
}
//=============================================================================
bool
isOneOf(const std::wstring& value, const wstringVector& choices)
{
    return std::find(choices.begin(), choices.end(), value) != choices.end();
}
//=============================================================================
indexType
parseDimension(const ArrayOf& arg)
{
    bool isValid = arg.isNumeric() && arg.isScalar() && !arg.isComplex() && !arg.isSparse();
    double value = isValid ? arg.asDoubleScalar() : 0.;
    if (!isValid || !std::isfinite(value) || value < 1 || value != std::floor(value)) {
        Error(L"Nelson:validators:invalidDimension",
            L"Dimension argument must be a positive integer scalar.");
    }
    return static_cast<indexType>(value);
}
//=============================================================================
