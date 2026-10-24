//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "extractBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RegularExpression.hpp"
#include "StringPattern.hpp"
#include <algorithm>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
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
cellArrayFromVector(const ArrayOfVector& values, const Dimensions& dims)
{
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, values.size(), stringVector(), false));
    for (size_t k = 0; k < values.size(); ++k) {
        elements[k] = values[k];
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
static ArrayOf
cellRowFromStrings(const std::vector<std::wstring>& values)
{
    ArrayOfVector cells;
    cells.reserve(values.size());
    for (const auto& value : values) {
        cells.push_back(ArrayOf::characterArray(value));
    }
    return cellArrayFromVector(cells, Dimensions(1, static_cast<indexType>(cells.size())));
}
//=============================================================================
static ArrayOf
cellColumnFromStrings(const std::vector<std::wstring>& values)
{
    ArrayOfVector cells;
    cells.reserve(values.size());
    for (const auto& value : values) {
        cells.push_back(ArrayOf::characterArray(value));
    }
    return cellArrayFromVector(cells, Dimensions(static_cast<indexType>(cells.size()), 1));
}
//=============================================================================
static std::wstring
extractNumericOne(const std::wstring& text, double oneBasedIndex)
{
    int pos = static_cast<int>(oneBasedIndex);
    if (pos < 1 || static_cast<size_t>(pos) > text.size()) {
        return L"";
    }
    return text.substr(static_cast<size_t>(pos - 1), 1);
}
//=============================================================================
static ArrayOf
numericExtract(const ArrayOf& str, const ArrayOf& pat)
{
    if (!isTextContainer(str) || !pat.isNumeric()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    std::vector<std::wstring> texts = textElements(str);
    std::vector<double> positions = pat.asDoubleVector();
    std::vector<std::wstring> values;
    values.reserve(texts.size());
    for (size_t k = 0; k < texts.size(); ++k) {
        double pos
            = positions.size() == 1 ? positions[0] : positions[std::min(k, positions.size() - 1)];
        values.push_back(extractNumericOne(texts[k], pos));
    }
    if (str.isRowVectorCharacterArray()) {
        return ArrayOf::characterArray(values.empty() ? L"" : values[0]);
    }
    if (str.isStringArray()) {
        return ArrayOf::stringArray(values, textDimensions(str));
    }
    ArrayOfVector cells;
    cells.reserve(values.size());
    for (const auto& value : values) {
        cells.push_back(ArrayOf::characterArray(value));
    }
    return cellArrayFromVector(cells, textDimensions(str));
}
//=============================================================================
static ArrayOf
stringExtractOutput(const std::vector<std::vector<std::wstring>>& matches, const Dimensions& dims)
{
    if (matches.size() == 1 && matches[0].size() > 1) {
        return ArrayOf::stringArray(
            matches[0], Dimensions(static_cast<indexType>(matches[0].size()), 1));
    }
    bool allScalar = true;
    size_t maxMatches = 1;
    for (const auto& item : matches) {
        allScalar = allScalar && item.size() <= 1;
        maxMatches = std::max(maxMatches, item.size());
    }
    std::vector<std::wstring> values;
    if (allScalar) {
        values.reserve(matches.size());
        for (const auto& item : matches) {
            values.push_back(item.empty() ? L"" : item[0]);
        }
        return ArrayOf::stringArray(values, dims);
    }
    values.assign(matches.size() * maxMatches, L"");
    for (size_t r = 0; r < matches.size(); ++r) {
        for (size_t c = 0; c < matches[r].size(); ++c) {
            values[r + c * matches.size()] = matches[r][c];
        }
    }
    return ArrayOf::stringArray(values,
        Dimensions(static_cast<indexType>(matches.size()), static_cast<indexType>(maxMatches)));
}
//=============================================================================
static ArrayOf
cellExtractOutput(const std::vector<std::vector<std::wstring>>& matches, const Dimensions& dims)
{
    ArrayOfVector values;
    values.reserve(matches.size());
    for (const auto& item : matches) {
        if (item.size() > 1) {
            values.push_back(cellRowFromStrings(item));
        } else {
            values.push_back(ArrayOf::characterArray(item.empty() ? L"" : item[0]));
        }
    }
    return cellArrayFromVector(values, dims);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::StringGateway::extractBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    if (argIn[1].isNumeric()) {
        ArrayOfVector retval;
        retval << numericExtract(argIn[0], argIn[1]);
        return retval;
    }
    if (!isTextContainer(argIn[0])) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    std::wstring expression = PatternRegexOf(argIn[1]);
    std::vector<std::wstring> texts = textElements(argIn[0]);
    std::vector<std::vector<std::wstring>> matches;
    matches.reserve(texts.size());
    bool includeEmptyMatches = IsPatternObject(argIn[1]);
    for (const auto& text : texts) {
        matches.push_back(RegexMatchTexts(text, expression, false, includeEmptyMatches));
    }
    ArrayOf result;
    if (argIn[0].isStringArray()) {
        result = stringExtractOutput(matches, textDimensions(argIn[0]));
    } else if (argIn[0].isRowVectorCharacterArray() && matches.size() == 1
        && matches[0].size() > 1) {
        result = cellColumnFromStrings(matches[0]);
    } else {
        result = cellExtractOutput(matches, textDimensions(argIn[0]));
    }
    ArrayOfVector retval;
    retval << result;
    return retval;
}
//=============================================================================
