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
#include <cwctype>
#include "readlinesBuiltin.hpp"
#include "ReadLines.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isTextScalar(const ArrayOf& value);
static std::wstring
toLower(std::wstring text);
static std::wstring
unescapeText(const std::wstring& text);
static size_t
matchChoice(const ArrayOf& value, const std::wstring& name, const wstringVector& choices);
static void
setOption(ReadLinesOptions& options, const std::wstring& name, const ArrayOf& value);
//=============================================================================
// lines = readlines(filename)
// lines = readlines(filename, Name, Value)
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::readlinesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!isTextScalar(argIn[0])) {
        Error(L"Nelson:readlines:invalidFilename",
            L"\"filename\" must be a string scalar or character vector.");
    }
    std::wstring filename = argIn[0].asWideString();
    if (argIn.size() % 2 == 0) {
        Error(L"Nelson:readlines:oddNameValuePairs",
            L"All parameters must have an associated value.");
    }
    ReadLinesOptions options;
    for (size_t k = 1; k < argIn.size(); k += 2) {
        if (!isTextScalar(argIn[k])) {
            Error(L"Nelson:readlines:invalidParameterName",
                L"Parameter name must be a string scalar or character vector.");
        }
        setOption(options, argIn[k].asWideString(), argIn[k + 1]);
    }
    retval << ReadLines(filename, options);
    return retval;
}
//=============================================================================
void
setOption(ReadLinesOptions& options, const std::wstring& name, const ArrayOf& value)
{
    std::wstring key = toLower(name);
    if (key == L"lineending") {
        wstringVector endings;
        if (value.isCharacterArray() && (value.isRowVector() || value.isEmpty())) {
            endings.push_back(value.asWideString());
        } else if (value.isStringArray() || value.isCellArrayOfCharacterVectors()) {
            endings = value.asWideStringVector(false);
        } else {
            Error(L"Nelson:readlines:invalidLineEnding",
                L"LineEnding must be a string array, character vector, or cell array of "
                L"character vectors.");
        }
        for (std::wstring& ending : endings) {
            ending = unescapeText(ending);
        }
        options.lineEndings = endings;
    } else if (key == L"whitespace") {
        if (!isTextScalar(value)) {
            Error(L"Nelson:readlines:invalidWhitespace",
                L"Whitespace must be a string scalar or character vector.");
        }
        options.whitespace = unescapeText(value.asWideString());
    } else if (key == L"emptylinerule") {
        const ReadLinesEmptyLineRule rules[] = { ReadLinesEmptyLineRule::READ,
            ReadLinesEmptyLineRule::SKIP, ReadLinesEmptyLineRule::ERROR_ON_EMPTY };
        options.emptyLineRule = rules[matchChoice(value, name, { L"read", L"skip", L"error" })];
    } else if (key == L"whitespacerule") {
        const ReadLinesWhitespaceRule rules[]
            = { ReadLinesWhitespaceRule::PRESERVE, ReadLinesWhitespaceRule::TRIM,
                  ReadLinesWhitespaceRule::TRIM_LEADING, ReadLinesWhitespaceRule::TRIM_TRAILING };
        options.whitespaceRule = rules[matchChoice(
            value, name, { L"preserve", L"trim", L"trimleading", L"trimtrailing" })];
    } else if (key == L"encoding") {
        if (!isTextScalar(value)) {
            Error(L"Nelson:readlines:encodingTypeExpected",
                L"Encoding must be a string scalar or character vector.");
        }
        std::wstring encoding = value.asWideString();
        if (encoding.empty()) {
            options.encoding = L"auto";
        } else if (toLower(encoding) == L"system") {
            options.encoding = utf8_to_wstring(getSystemEncoding());
        } else if (toLower(encoding) != L"auto" && isSupportedEncoding(encoding)) {
            options.encoding = toLower(encoding) == L"utf-8" ? L"UTF-8" : encoding;
        } else {
            Error(
                L"Nelson:readlines:invalidEncoding", L"The encoding '%s' is not valid.", encoding);
        }
    } else {
        Error(L"Nelson:readlines:unknownParameter", L"Unknown Parameter '%s'.", name);
    }
}
//=============================================================================
size_t
matchChoice(const ArrayOf& value, const std::wstring& name, const wstringVector& choices)
{
    if (isTextScalar(value)) {
        std::wstring text = toLower(value.asWideString());
        auto it = std::find(choices.begin(), choices.end(), text);
        if (it != choices.end()) {
            return static_cast<size_t>(it - choices.begin());
        }
    }
    std::wstring expected;
    for (const std::wstring& choice : choices) {
        expected += (expected.empty() ? L"'" : L", '") + choice + L"'";
    }
    Error(L"Nelson:readlines:invalidOptionValue", L"%s must be one of these values: %s.", name,
        expected);
    return 0;
}
//=============================================================================
bool
isTextScalar(const ArrayOf& value)
{
    return (value.isCharacterArray() && (value.isRowVector() || value.isEmpty()))
        || value.isScalarStringArray();
}
//=============================================================================
std::wstring
toLower(std::wstring text)
{
    std::transform(text.begin(), text.end(), text.begin(),
        [](wchar_t c) { return static_cast<wchar_t>(std::towlower(c)); });
    return text;
}
//=============================================================================
std::wstring
unescapeText(const std::wstring& text)
{
    std::wstring result;
    result.reserve(text.size());
    for (size_t k = 0; k < text.size(); ++k) {
        if (text[k] != L'\\' || k + 1 == text.size()) {
            result.push_back(text[k]);
            continue;
        }
        wchar_t next = text[k + 1];
        const std::wstring escaped = L"nrtbfv\\";
        const std::wstring replacement = L"\n\r\t\b\f\v\\";
        size_t pos = escaped.find(next);
        if (pos == std::wstring::npos) {
            result.push_back(text[k]);
            continue;
        }
        result.push_back(replacement[pos]);
        ++k;
    }
    return result;
}
//=============================================================================
