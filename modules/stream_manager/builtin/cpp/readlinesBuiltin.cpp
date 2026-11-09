//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "readlinesBuiltin.hpp"
#include "ReadLines.hpp"
#include "helpers.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
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
    enum
    {
        LINE_ENDING,
        WHITESPACE,
        EMPTY_LINE_RULE,
        WHITESPACE_RULE,
        ENCODING
    };
    const std::wstring errorId = L"Nelson:readlines:invalidOptionValue";
    switch (matchChoicePrefix(
        name, { L"lineending", L"whitespace", L"emptylinerule", L"whitespacerule", L"encoding" })) {
    case LINE_ENDING: {
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
    } break;
    case WHITESPACE: {
        if (!isTextScalar(value)) {
            Error(L"Nelson:readlines:invalidWhitespace",
                L"Whitespace must be a string scalar or character vector.");
        }
        options.whitespace = unescapeText(value.asWideString());
    } break;
    case EMPTY_LINE_RULE: {
        const ReadLinesEmptyLineRule rules[] = { ReadLinesEmptyLineRule::READ,
            ReadLinesEmptyLineRule::SKIP, ReadLinesEmptyLineRule::ERROR_ON_EMPTY };
        options.emptyLineRule = rules[matchOptionValue(
            value, L"EmptyLineRule", { L"read", L"skip", L"error" }, errorId)];
    } break;
    case WHITESPACE_RULE: {
        const ReadLinesWhitespaceRule rules[]
            = { ReadLinesWhitespaceRule::PRESERVE, ReadLinesWhitespaceRule::TRIM,
                  ReadLinesWhitespaceRule::TRIM_LEADING, ReadLinesWhitespaceRule::TRIM_TRAILING };
        options.whitespaceRule = rules[matchOptionValue(value, L"WhitespaceRule",
            { L"preserve", L"trim", L"trimleading", L"trimtrailing" }, errorId)];
    } break;
    case ENCODING: {
        if (!isTextScalar(value)) {
            Error(L"Nelson:readlines:encodingTypeExpected",
                L"Encoding must be a string scalar or character vector.");
        }
        std::wstring encoding = value.asWideString();
        std::wstring lower = toLowerText(encoding);
        if (encoding.empty()) {
            options.encoding = L"auto";
        } else if (lower == L"system") {
            options.encoding = utf8_to_wstring(getSystemEncoding());
        } else if (lower != L"auto" && isSupportedEncoding(encoding)) {
            options.encoding = lower == L"utf-8" ? L"UTF-8" : encoding;
        } else {
            Error(
                L"Nelson:readlines:invalidEncoding", L"The encoding '%s' is not valid.", encoding);
        }
    } break;
    default: {
        Error(L"Nelson:readlines:unknownParameter", L"Unknown Parameter '%s'.", name);
    } break;
    }
}
//=============================================================================
