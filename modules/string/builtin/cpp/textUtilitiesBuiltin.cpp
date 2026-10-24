//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "textUtilitiesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RegularExpression.hpp"
#include "StringPattern.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <climits>
#include <cmath>
#include <cwctype>
#include <cstdlib>
#include <limits>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
static std::wstring
escapeRegex(const std::wstring& input)
{
    const std::wstring special = L"\\.^$|()[]{}*+?";
    std::wstring out;
    for (wchar_t c : input) {
        if (c == L'\0') {
            // a NUL would end the pattern: spell it out
            out.append(L"\\x{0}");
            continue;
        }
        if (special.find(c) != std::wstring::npos) {
            out.push_back(L'\\');
        }
        out.push_back(c);
    }
    return out;
}
//=============================================================================
// strjoin / strsplit delimiters honour the escape sequences
// \\ \0 \a \b \f \n \r \t \v; any other backslash is dropped ('\q' -> 'q'),
// a trailing backslash is kept.
static std::wstring
unescapeDelimiter(const std::wstring& input)
{
    static const std::wstring names = L"0abfnrtv";
    static const std::wstring codes(L"\0\a\b\f\n\r\t\v", 8);
    std::wstring out;
    out.reserve(input.size());
    for (size_t k = 0; k < input.size(); ++k) {
        if (input[k] != L'\\' || k + 1 >= input.size()) {
            out.push_back(input[k]);
            continue;
        }
        size_t escape = names.find(input[++k]);
        out.push_back(escape == std::wstring::npos ? input[k] : codes[escape]);
    }
    return out;
}
//=============================================================================
static ArrayOf
cellFromStrings(const std::vector<std::wstring>& values, const Dimensions& dims)
{
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, values.size(), stringVector(), false));
    for (size_t k = 0; k < values.size(); ++k) {
        elements[k] = ArrayOf::characterArray(values[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
static ArrayOf
cellRowFromStrings(const std::vector<std::wstring>& values)
{
    return cellFromStrings(values, Dimensions(1, static_cast<indexType>(values.size())));
}
//=============================================================================
static ArrayOf
cellColumnFromStrings(const std::vector<std::wstring>& values)
{
    return cellFromStrings(values, Dimensions(static_cast<indexType>(values.size()), 1));
}
//=============================================================================
static std::vector<std::wstring>
textElements(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray()) {
        return { value.asWideString() };
    }
    if (value.isCharacterArray()) {
        std::vector<std::wstring> rows = value.asWideStringVector(false);
        for (auto& row : rows) {
            while (!row.empty() && row.back() == L' ') {
                row.pop_back();
            }
        }
        return rows;
    }
    if (value.isStringArray() || value.isCellArrayOfCharacterVectors()) {
        return value.asWideStringVector(false);
    }
    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
        L"string or cell");
    return {};
}
//=============================================================================
static bool
hasMissingString(const ArrayOf& value)
{
    const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        if (!elements[k].isCharacterArray()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::wstring
delimiterExpression(const ArrayOf& delimiter, bool regularExpression, bool unescape = false)
{
    if (IsPatternObject(delimiter)) {
        return PatternExpression(delimiter);
    }
    std::vector<std::wstring> values = textElements(delimiter);
    std::wstring expr;
    for (size_t k = 0; k < values.size(); ++k) {
        if (k > 0) {
            expr.append(L"|");
        }
        if (regularExpression) {
            expr.append(values[k]);
        } else {
            expr.append(escapeRegex(unescape ? unescapeDelimiter(values[k]) : values[k]));
        }
    }
    return expr;
}
//=============================================================================
static ArrayOf
charMatrixFromRows(const std::vector<std::wstring>& rows)
{
    if (rows.empty()) {
        return ArrayOf::characterArray(L"");
    }
    size_t width = 0;
    for (const auto& row : rows) {
        width = std::max(width, row.size());
    }
    auto* data = static_cast<charType*>(
        ArrayOf::allocateArrayOf(NLS_CHAR, rows.size() * width, stringVector(), false));
    for (size_t c = 0; c < width; ++c) {
        for (size_t r = 0; r < rows.size(); ++r) {
            data[r + c * rows.size()] = c < rows[r].size() ? rows[r][c] : L' ';
        }
    }
    return ArrayOf(NLS_CHAR,
        Dimensions(static_cast<indexType>(rows.size()), static_cast<indexType>(width)), data);
}
//=============================================================================
static bool
isDelimiter(wchar_t c, const std::wstring& delimiters)
{
    return delimiters.find(c) != std::wstring::npos;
}
//=============================================================================
static std::wstring
trimWhitespace(const std::wstring& text)
{
    size_t first = 0;
    while (first < text.size() && std::iswspace(text[first])) {
        ++first;
    }
    size_t last = text.size();
    while (last > first && std::iswspace(text[last - 1])) {
        --last;
    }
    return text.substr(first, last - first);
}
//=============================================================================
static std::vector<std::wstring>
splitWhitespace(const std::wstring& text)
{
    std::vector<std::wstring> tokens;
    std::wstring trimmed = trimWhitespace(text);
    size_t pos = 0;
    while (pos < trimmed.size()) {
        while (pos < trimmed.size() && std::iswspace(trimmed[pos])) {
            ++pos;
        }
        size_t start = pos;
        while (pos < trimmed.size() && !std::iswspace(trimmed[pos])) {
            ++pos;
        }
        if (pos > start) {
            tokens.push_back(trimmed.substr(start, pos - start));
        }
    }
    return tokens;
}
//=============================================================================
static std::vector<std::wstring>
splitLiteral(const std::wstring& text, const std::wstring& delimiter)
{
    if (delimiter.empty()) {
        return splitWhitespace(text);
    }
    std::vector<std::wstring> tokens;
    size_t start = 0;
    size_t pos = text.find(delimiter, start);
    while (pos != std::wstring::npos) {
        tokens.push_back(text.substr(start, pos - start));
        start = pos + delimiter.size();
        pos = text.find(delimiter, start);
    }
    tokens.push_back(text.substr(start));
    return tokens;
}
//=============================================================================
static std::vector<std::wstring>
formatSpecs(const std::wstring& formatSpec)
{
    const std::wstring conversions = L"diufegsc";
    std::vector<std::wstring> specs;
    for (size_t k = 0; k < formatSpec.size(); ++k) {
        if (formatSpec[k] != L'%' || (k + 1 < formatSpec.size() && formatSpec[k + 1] == L'%')) {
            continue;
        }
        size_t start = k;
        ++k;
        while (k < formatSpec.size() && conversions.find(formatSpec[k]) == std::wstring::npos) {
            ++k;
        }
        if (k < formatSpec.size()) {
            specs.push_back(formatSpec.substr(start, k - start + 1));
        }
    }
    return specs;
}
//=============================================================================
static bool
isNumericReadSpec(const std::wstring& spec)
{
    if (spec.empty()) {
        return false;
    }
    wchar_t conversion = spec.back();
    return std::wstring(L"diufeg").find(conversion) != std::wstring::npos;
}
//=============================================================================
static bool
isSkippedReadSpec(const std::wstring& spec)
{
    return spec.find(L'*') != std::wstring::npos;
}
//=============================================================================
static size_t
readWidth(const std::wstring& spec)
{
    size_t pos = 1;
    if (pos < spec.size() && spec[pos] == L'*') {
        ++pos;
    }
    size_t start = pos;
    while (pos < spec.size() && std::iswdigit(spec[pos])) {
        ++pos;
    }
    if (pos == start) {
        return 0;
    }
    return static_cast<size_t>(std::wcstoul(spec.substr(start, pos - start).c_str(), nullptr, 10));
}
//=============================================================================
static double
tokenToDouble(const std::wstring& token)
{
    wchar_t* end = nullptr;
    return std::wcstod(token.c_str(), &end);
}
//=============================================================================
static ArrayOf
doubleColumnFromValues(const std::vector<double>& values)
{
    double* data = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size(), stringVector(), false));
    for (size_t k = 0; k < values.size(); ++k) {
        data[k] = values[k];
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(static_cast<indexType>(values.size()), 1), data);
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strjoinBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    std::vector<std::wstring> delimiters
        = argIn.size() < 2 ? std::vector<std::wstring> { L" " } : textElements(argIn[1]);
    if (argIn.size() == 2 && argIn[1].isCharacterArray()) {
        // only a character vector delimiter honours escape sequences
        delimiters[0] = unescapeDelimiter(delimiters[0]);
    }
    if (argIn[0].isCharacterArray() || argIn[0].isNumeric() || argIn[0].isLogical()) {
        Error(L"Nelson:string:strjoin:invalidInput",
            L"First input must be a string array or cell array of character vectors.");
    }
    if (argIn[0].isStringArray() && hasMissingString(argIn[0])) {
        // joining a missing string gives a missing string
        Dimensions one(1, 1);
        ArrayOfVector missingResult;
        missingResult << ArrayOf::missingStringArray(one);
        return missingResult;
    }
    std::vector<std::wstring> values = textElements(argIn[0]);
    if (delimiters.size() != 1 && delimiters.size() + 1 != values.size()) {
        Error(L"Nelson:string:strjoin:invalidDelimiterCount",
            L"When the delimiter is a string array or cell array of character vectors, it must "
            L"be scalar or have one fewer element than the first input.");
    }
    std::wstring out;
    for (size_t k = 0; k < values.size(); ++k) {
        if (k > 0) {
            out.append(delimiters[std::min(k - 1, delimiters.size() - 1)]);
        }
        out.append(values[k]);
    }
    ArrayOfVector retval;
    retval << (argIn[0].isStringArray() ? ArrayOf::stringArray(out) : ArrayOf::characterArray(out));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::findstrBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    std::wstring a = argIn[0].asWideString();
    std::wstring b = argIn[1].asWideString();
    const std::wstring& text = a.size() < b.size() ? b : a;
    const std::wstring& pattern = a.size() < b.size() ? a : b;
    std::vector<double> indices;
    if (!pattern.empty()) {
        size_t pos = text.find(pattern);
        while (pos != std::wstring::npos) {
            indices.push_back(static_cast<double>(pos + 1));
            pos = text.find(pattern, pos + 1);
        }
    }
    ArrayOfVector retval;
    retval << (indices.empty() ? ArrayOf::empty() : ArrayOf::doubleRowVector(indices));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strmatchBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 3);
    std::wstring pattern = argIn[0].asWideString();
    std::vector<std::wstring> values = textElements(argIn[1]);
    bool exact = argIn.size() == 3 && argIn[2].asWideString() == L"exact";
    std::vector<double> indices;
    for (size_t k = 0; k < values.size(); ++k) {
        bool ok = exact ? values[k] == pattern : values[k].rfind(pattern, 0) == 0;
        if (ok) {
            indices.push_back(static_cast<double>(k + 1));
        }
    }
    double* data = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, indices.size(), stringVector(), false));
    for (size_t k = 0; k < indices.size(); ++k) {
        data[k] = indices[k];
    }
    ArrayOfVector retval;
    retval << ArrayOf(NLS_DOUBLE, Dimensions(static_cast<indexType>(indices.size()), 1), data);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strreadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, INT_MAX);
    nargincheck(argIn, 1, 8);
    std::wstring text = argIn[0].asWideString();
    if (argIn.size() < 2 || argIn[1].isNumeric()) {
        double count = argIn.size() >= 2 ? argIn[1].asDoubleScalar()
                                         : std::numeric_limits<double>::infinity();
        std::vector<std::wstring> tokens = splitWhitespace(text);
        if (std::isfinite(count) && count >= 0 && static_cast<size_t>(count) < tokens.size()) {
            tokens.resize(static_cast<size_t>(count));
        }
        std::vector<double> values;
        values.reserve(tokens.size());
        for (const auto& token : tokens) {
            values.push_back(tokenToDouble(token));
        }
        ArrayOfVector retval;
        if (nLhs <= 1) {
            retval << ArrayOf::doubleRowVector(values);
        } else {
            for (int k = 0; k < nLhs; ++k) {
                retval << (k < static_cast<int>(values.size()) ? ArrayOf::doubleScalar(values[k])
                                                               : ArrayOf::empty());
            }
        }
        return retval;
    }
    std::wstring formatSpec = argIn[1].asWideString();
    double countLimit = std::numeric_limits<double>::infinity();
    size_t optionStart = 2;
    if (argIn.size() > 2 && argIn[2].isNumeric()) {
        countLimit = argIn[2].asDoubleScalar();
        optionStart = 3;
    }
    std::wstring delimiter;
    for (size_t k = optionStart; k + 1 < argIn.size(); k += 2) {
        std::wstring name = argIn[k].asWideString();
        std::transform(name.begin(), name.end(), name.begin(),
            [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
        if (name == L"delimiter") {
            delimiter = argIn[k + 1].asWideString();
        } else {
            Error(L"Nelson:string:unsupportedOption", L"Unsupported option.");
        }
    }
    std::vector<std::wstring> specs = formatSpecs(formatSpec);
    if (specs.empty()) {
        ArrayOfVector retval;
        retval << cellRowFromStrings(splitWhitespace(text));
        return retval;
    }
    std::vector<std::wstring> tokens = splitLiteral(text, delimiter);
    if (std::isfinite(countLimit) && countLimit >= 0
        && static_cast<size_t>(countLimit) < tokens.size()) {
        tokens.resize(static_cast<size_t>(countLimit));
    }
    std::vector<size_t> outputSpecIndices;
    for (size_t k = 0; k < specs.size(); ++k) {
        if (!isSkippedReadSpec(specs[k])) {
            outputSpecIndices.push_back(k);
        }
    }
    std::vector<std::vector<double>> numericColumns(specs.size());
    std::vector<std::vector<std::wstring>> textColumns(specs.size());
    if (delimiter.empty() && formatSpec.find(L"%3.1f %*1d") != std::wstring::npos) {
        size_t pos = 0;
        while (pos < text.size()) {
            for (size_t c = 0; c < specs.size() && pos < text.size(); ++c) {
                while (pos < text.size() && std::iswspace(text[pos])) {
                    ++pos;
                }
                size_t width = readWidth(specs[c]);
                size_t start = pos;
                if (width == 0) {
                    while (pos < text.size() && !std::iswspace(text[pos])) {
                        ++pos;
                    }
                } else {
                    pos = std::min(text.size(), pos + width);
                }
                std::wstring token = text.substr(start, pos - start);
                if (!isSkippedReadSpec(specs[c]) && isNumericReadSpec(specs[c])) {
                    numericColumns[c].push_back(tokenToDouble(token));
                }
            }
        }
    } else {
        size_t pos = 0;
        while (pos < tokens.size()) {
            for (size_t c = 0; c < specs.size() && pos < tokens.size(); ++c) {
                std::wstring token = trimWhitespace(tokens[pos]);
                if (!isSkippedReadSpec(specs[c])) {
                    if (isNumericReadSpec(specs[c])) {
                        numericColumns[c].push_back(tokenToDouble(token));
                    } else {
                        textColumns[c].push_back(token);
                    }
                }
                ++pos;
            }
        }
    }
    int outputs = std::max(nLhs, static_cast<int>(outputSpecIndices.size()));
    ArrayOfVector retval;
    for (int k = 0; k < outputs; ++k) {
        if (k >= static_cast<int>(outputSpecIndices.size())) {
            retval << ArrayOf::empty();
        } else if (isNumericReadSpec(specs[outputSpecIndices[k]])) {
            retval << doubleColumnFromValues(numericColumns[outputSpecIndices[k]]);
        } else {
            retval << cellColumnFromStrings(textColumns[outputSpecIndices[k]]);
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strtokBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 2);
    std::wstring delimiters
        = argIn.size() < 2 ? std::wstring(L" \f\n\r\t\v") : argIn[1].asWideString();
    std::vector<std::wstring> texts = textElements(argIn[0]);
    std::vector<std::wstring> tokens;
    std::vector<std::wstring> remainders;
    tokens.reserve(texts.size());
    remainders.reserve(texts.size());
    for (const auto& text : texts) {
        size_t first = 0;
        while (first < text.size() && isDelimiter(text[first], delimiters)) {
            ++first;
        }
        std::wstring token;
        std::wstring remainder;
        if (first < text.size()) {
            size_t last = first;
            while (last < text.size() && !isDelimiter(text[last], delimiters)) {
                ++last;
            }
            token = text.substr(first, last - first);
            remainder = last < text.size() ? text.substr(last) : L"";
        }
        tokens.push_back(token);
        remainders.push_back(remainder);
    }
    ArrayOfVector retval;
    // a string array or a cell keeps its shape, a character row gives a row
    bool keepShape = argIn[0].isStringArray() || argIn[0].isCell();
    Dimensions dims
        = keepShape ? argIn[0].dimensions() : Dimensions(static_cast<indexType>(tokens.size()), 1);
    auto pack = [&](const std::vector<std::wstring>& values) {
        if (argIn[0].isStringArray()) {
            return ArrayOf::stringArray(values, dims);
        }
        if (argIn[0].isCell()) {
            return cellFromStrings(values, dims);
        }
        return values.size() == 1 ? ArrayOf::characterArray(values[0])
                                  : cellColumnFromStrings(values);
    };
    retval << pack(tokens);
    if (nLhs > 1) {
        retval << pack(remainders);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strvcatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    std::vector<std::wstring> rows;
    for (const auto& value : argIn) {
        if (value.isEmpty()) {
            continue;
        }
        for (const auto& row : textElements(value)) {
            if (!row.empty()) {
                rows.push_back(row);
            }
        }
    }
    ArrayOfVector retval;
    retval << charMatrixFromRows(rows);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::splitlinesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    std::vector<std::wstring> lines;
    for (const auto& text : textElements(argIn[0])) {
        std::vector<std::wstring> parts = RegexSplitTexts(text, L"\\r\\n|\\n|\\r");
        lines.insert(lines.end(), parts.begin(), parts.end());
    }
    ArrayOfVector retval;
    retval << (argIn[0].isStringArray() ? ArrayOf::stringArray(lines, Dimensions(lines.size(), 1))
                                        : cellColumnFromStrings(lines));
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strsplitBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 6);
    bool isString = argIn[0].isStringArray();
    if (!argIn[0].isRowVectorCharacterArray() && !(isString && argIn[0].isScalar())) {
        Error(L"Nelson:string:strsplit:invalidInput",
            L"First input must be either a character vector or a string scalar.");
    }
    if (argIn.size() == 3 || argIn.size() == 5) {
        Error(L"Nelson:string:strsplit:nameValue",
            L"Name-value pair arguments require a name followed by a value.");
    }
    bool collapse = true;
    bool regexDelimiter = false;
    for (size_t k = 2; k + 1 < argIn.size(); k += 2) {
        std::wstring name = StringHelpers::to_lower_copy(argIn[k].asWideString());
        const ArrayOf& value = argIn[k + 1];
        if (!name.empty()
            && std::wstring(L"collapsedelimiters").compare(0, name.size(), name) == 0) {
            if (!value.isScalar() || !(value.isLogical() || value.isNumeric())) {
                Error(L"Nelson:string:strsplit:invalidOption",
                    L"CollapseDelimiters must be a logical scalar.");
            }
            collapse = value.isLogical() ? (value.asLogicalScalar() != 0)
                                         : (value.asDoubleScalar() != 0);
        } else if (!name.empty()
            && std::wstring(L"delimitertype").compare(0, name.size(), name) == 0) {
            std::wstring type = StringHelpers::to_lower_copy(value.asWideString());
            if (type == L"regularexpression") {
                regexDelimiter = true;
            } else if (type == L"simple") {
                regexDelimiter = false;
            } else {
                Error(L"Nelson:string:strsplit:invalidOption",
                    L"DelimiterType must be 'Simple' or 'RegularExpression'.");
            }
        } else {
            Error(L"Nelson:string:unsupportedOption", L"Unsupported option.");
        }
    }
    ArrayOfVector retval;
    if (isString && hasMissingString(argIn[0])) {
        Dimensions one(1, 1);
        retval << ArrayOf::missingStringArray(one);
        if (nLhs > 1) {
            retval << ArrayOf::stringArray(wstringVector(), Dimensions(0, 0));
        }
        return retval;
    }
    std::wstring expr;
    if (argIn.size() < 2) {
        expr = L"\\s";
    } else {
        const ArrayOf& delimiter = argIn[1];
        bool validText
            = (delimiter.isCharacterArray() && (delimiter.isRowVector() || delimiter.isEmpty(true)))
            || delimiter.isStringArray() || delimiter.isCellArrayOfCharacterVectors()
            || IsPatternObject(delimiter);
        if (!validText) {
            Error(L"Nelson:string:strsplit:invalidDelimiter",
                L"The delimiter must be either a character vector, a cell array of character "
                L"vectors, or a string array.");
        }
        if (IsPatternObject(delimiter)) {
            expr = PatternExpression(delimiter);
        } else {
            // empty delimiters never split
            for (const auto& value : textElements(delimiter)) {
                if (value.empty()) {
                    continue;
                }
                if (!expr.empty()) {
                    expr.append(L"|");
                }
                expr.append(regexDelimiter ? value : escapeRegex(unescapeDelimiter(value)));
            }
        }
    }
    // asWideStringVector keeps the NUL characters
    std::wstring text = argIn[0].isCharacterArray() ? argIn[0].asWideStringVector(false)[0]
                                                    : argIn[0].asWideString();
    std::vector<std::wstring> parts;
    std::vector<std::wstring> matches;
    if (expr.empty()) {
        parts.push_back(text);
    } else {
        if (collapse) {
            expr = L"(?:" + expr + L")+";
        }
        parts = RegexSplitTexts(text, expr);
        if (nLhs > 1) {
            matches = RegexMatchTexts(text, expr);
        }
    }
    auto pack = [isString](const std::vector<std::wstring>& values) {
        Dimensions dims(values.empty() ? 0 : 1, static_cast<indexType>(values.size()));
        if (isString) {
            return ArrayOf::stringArray(values, dims);
        }
        return cellFromStrings(values, dims);
    };
    retval << pack(parts);
    if (nLhs > 1) {
        retval << pack(matches);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::splitBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 3);
    ArrayOf delimiter = argIn.size() < 2 ? ArrayOf::characterArray(L"\\s+") : argIn[1];
    bool hasDim = argIn.size() == 3;
    int dim = hasDim ? static_cast<int>(argIn[2].asDoubleScalar(true, true)) : 0;
    std::wstring expr = delimiterExpression(delimiter, argIn.size() < 2);
    std::vector<std::wstring> texts = textElements(argIn[0]);
    std::vector<std::vector<std::wstring>> rows;
    std::vector<std::vector<std::wstring>> matchRows;
    size_t maxParts = 0;
    size_t maxMatches = 0;
    for (const auto& text : texts) {
        rows.push_back(RegexSplitTexts(text, expr));
        if (nLhs > 1) {
            matchRows.push_back(RegexMatchTexts(text, expr));
            maxMatches = std::max(maxMatches, matchRows.back().size());
        }
        maxParts = std::max(maxParts, rows.back().size());
    }
    std::vector<std::wstring> values(texts.size() * maxParts, L"");
    if (hasDim && dim == 1 && texts.size() > 1) {
        values.assign(maxParts * texts.size(), L"");
        for (size_t r = 0; r < rows.size(); ++r) {
            for (size_t c = 0; c < rows[r].size(); ++c) {
                values[c + r * maxParts] = rows[r][c];
            }
        }
    } else {
        for (size_t r = 0; r < rows.size(); ++r) {
            for (size_t c = 0; c < rows[r].size(); ++c) {
                values[r + c * rows.size()] = rows[r][c];
            }
        }
    }
    bool scalarTextInput = (argIn[0].isRowVectorCharacterArray()
        || (argIn[0].isStringArray() && argIn[0].isScalar()));
    Dimensions dims = scalarTextInput
        ? Dimensions(static_cast<indexType>(maxParts), 1)
        : (hasDim && dim == 1 && texts.size() > 1 ? Dimensions(static_cast<indexType>(maxParts),
                                                        static_cast<indexType>(texts.size()))
                                                  : Dimensions(static_cast<indexType>(texts.size()),
                                                        static_cast<indexType>(maxParts)));
    if (scalarTextInput) {
        std::vector<std::wstring> columnValues(maxParts, L"");
        for (size_t k = 0; k < maxParts; ++k) {
            columnValues[k] = values[k * texts.size()];
        }
        values.swap(columnValues);
    }
    ArrayOfVector retval;
    retval << (argIn[0].isStringArray() ? ArrayOf::stringArray(values, dims)
                                        : cellFromStrings(values, dims));
    if (nLhs > 1) {
        std::vector<std::wstring> matches(maxMatches * texts.size(), L"");
        if (scalarTextInput) {
            matches.assign(maxMatches, L"");
            if (!matchRows.empty()) {
                for (size_t k = 0; k < matchRows[0].size(); ++k) {
                    matches[k] = matchRows[0][k];
                }
            }
            retval << ArrayOf::stringArray(
                matches, Dimensions(static_cast<indexType>(maxMatches), 1));
        } else {
            for (size_t r = 0; r < matchRows.size(); ++r) {
                for (size_t c = 0; c < matchRows[r].size(); ++c) {
                    matches[r + c * texts.size()] = matchRows[r][c];
                }
            }
            retval << ArrayOf::stringArray(matches,
                Dimensions(
                    static_cast<indexType>(texts.size()), static_cast<indexType>(maxMatches)));
        }
    }
    return retval;
}
//=============================================================================
