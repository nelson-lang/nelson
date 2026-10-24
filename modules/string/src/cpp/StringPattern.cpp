//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringPattern.hpp"
#include "RegularExpression.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isTextContainer(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isStringArray()
        || value.isCellArrayOfCharacterVectors();
}
//=============================================================================
static std::vector<std::wstring>
textElements(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray()) {
        return { value.asWideString() };
    }
    return value.asWideStringVector(false);
}
//=============================================================================
static Dimensions
textDimensions(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray()) {
        return Dimensions(1, 1);
    }
    return value.dimensions();
}
//=============================================================================
static ArrayOf
textOutput(const ArrayOf& like, const std::vector<std::wstring>& values, const Dimensions& dims)
{
    if (like.isRowVectorCharacterArray()) {
        return ArrayOf::characterArray(values.empty() ? L"" : values[0]);
    }
    if (like.isStringArray()) {
        return ArrayOf::stringArray(values, dims);
    }
    ArrayOfVector cells;
    cells.reserve(values.size());
    for (const auto& value : values) {
        cells.push_back(ArrayOf::characterArray(value));
    }
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, values.size(), stringVector(), false));
    for (size_t k = 0; k < values.size(); ++k) {
        elements[k] = cells[k];
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
bool
IsPatternObject(const ArrayOf& value)
{
    return value.isClassType() && value.classType() == "pattern" && value.isField("Expression");
}
//=============================================================================
std::wstring
PatternExpression(const ArrayOf& value)
{
    if (!IsPatternObject(value)) {
        Error(L"Nelson:string:patternExpected", L"Pattern expected.");
    }
    return value.field("Expression").asWideString();
}
//=============================================================================
static std::wstring
escapeRegex(const std::wstring& input)
{
    const std::wstring special = L"\\.^$|()[]{}*+?";
    std::wstring out;
    for (wchar_t c : input) {
        if (special.find(c) != std::wstring::npos) {
            out.push_back(L'\\');
        }
        out.push_back(c);
    }
    return out;
}
//=============================================================================
std::wstring
PatternRegexOf(const ArrayOf& value)
{
    if (IsPatternObject(value)) {
        return PatternExpression(value);
    }
    if (value.isRowVectorCharacterArray() || (value.isStringArray() && value.isScalar())) {
        return escapeRegex(value.asWideString());
    }
    Error(L"Nelson:string:patternStringScalarCharacterVector",
        L"Pattern, string scalar, or character vector expected.");
    return L"";
}
//=============================================================================
ArrayOf
PatternLogical(const ArrayOf& str, const std::wstring& expression, bool ignoreCase,
    PatternLogicalMode mode, bool& needToOverload)
{
    needToOverload = false;
    if (!isTextContainer(str)) {
        needToOverload = true;
        return ArrayOf();
    }
    std::wstring expr = expression;
    if (mode == PatternLogicalMode::StartsWith) {
        expr = L"^(?:" + expression + L")";
    } else if (mode == PatternLogicalMode::EndsWith) {
        expr = L"(?:" + expression + L")$";
    } else if (mode == PatternLogicalMode::Matches) {
        expr = L"^(?:" + expression + L")$";
    }
    std::vector<std::wstring> texts = textElements(str);
    Dimensions dims = textDimensions(str);
    logical* result = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, texts.size(), stringVector(), true));
    for (size_t k = 0; k < texts.size(); ++k) {
        result[k] = RegexIsMatch(texts[k], expr, ignoreCase) ? 1 : 0;
    }
    return ArrayOf(NLS_LOGICAL, dims, result);
}
//=============================================================================
ArrayOf
PatternCount(
    const ArrayOf& str, const std::wstring& expression, bool ignoreCase, bool& needToOverload)
{
    needToOverload = false;
    if (!isTextContainer(str)) {
        needToOverload = true;
        return ArrayOf();
    }
    std::vector<std::wstring> texts = textElements(str);
    Dimensions dims = textDimensions(str);
    double* result = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, texts.size(), stringVector(), true));
    for (size_t k = 0; k < texts.size(); ++k) {
        result[k] = RegexCountMatches(texts[k], expression, ignoreCase);
    }
    return ArrayOf(NLS_DOUBLE, dims, result);
}
//=============================================================================
ArrayOf
PatternReplace(const ArrayOf& str, const std::wstring& expression, const std::wstring& replacement,
    bool ignoreCase, bool& needToOverload)
{
    needToOverload = false;
    if (!isTextContainer(str)) {
        needToOverload = true;
        return ArrayOf();
    }
    std::vector<std::wstring> texts = textElements(str);
    Dimensions dims = textDimensions(str);
    std::vector<std::wstring> values;
    values.reserve(texts.size());
    for (const auto& text : texts) {
        values.push_back(RegexReplaceText(text, expression, replacement, ignoreCase, true));
    }
    return textOutput(str, values, dims);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
