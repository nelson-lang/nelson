//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "writelinesBuiltin.hpp"
#include "WriteLines.hpp"
#include "helpers.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
setOption(WriteLinesOptions& options, const std::wstring& name, const ArrayOf& value);
//=============================================================================
// writelines(lines, filename)
// writelines(lines, filename, Name, Value)
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::writelinesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 0);
    std::vector<TextSpan> lines;
    if (!textSpans(argIn[0], lines)) {
        Error(L"Nelson:writelines:invalidText",
            L"Invalid argument at position 1. Value must be a character vector, string array, or "
            L"cell array of character vectors.");
    }
    if (!isTextScalar(argIn[1]) || argIn[1].asWideString().empty()) {
        Error(L"Nelson:writelines:invalidFilename",
            L"Invalid argument at position 2. Value must be a nonempty character vector or "
            L"string scalar.");
    }
    std::wstring filename = argIn[1].asWideString();
    if (argIn.size() % 2 != 0) {
        Error(L"Nelson:writelines:oddNameValuePairs",
            L"All parameters must have an associated value.");
    }
    WriteLinesOptions options;
    for (size_t k = 2; k < argIn.size(); k += 2) {
        if (!isTextScalar(argIn[k])) {
            Error(L"Nelson:writelines:invalidParameterName",
                L"Parameter name must be a string scalar or character vector.");
        }
        setOption(options, argIn[k].asWideString(), argIn[k + 1]);
    }
    WriteLines(lines, filename, options);
    return {};
}
//=============================================================================
void
setOption(WriteLinesOptions& options, const std::wstring& name, const ArrayOf& value)
{
    enum
    {
        LINE_ENDING,
        ENCODING,
        WRITE_MODE,
        TRAILING_LINE_ENDING_RULE
    };
    const std::wstring errorId = L"Nelson:writelines:invalidOptionValue";
    switch (matchChoicePrefix(
        name, { L"lineending", L"encoding", L"writemode", L"trailinglineendingrule" })) {
    case LINE_ENDING: {
        if (!isTextScalar(value) || value.asWideString().empty()) {
            Error(L"Nelson:writelines:invalidLineEnding",
                L"LineEnding must be a nonempty character vector or string scalar.");
        }
        options.lineEnding = unescapeText(value.asWideString());
    } break;
    case ENCODING: {
        if (!isTextScalar(value)) {
            Error(L"Nelson:writelines:encodingTypeExpected",
                L"Encoding must be a string scalar or character vector.");
        }
        std::wstring encoding = value.asWideString();
        std::wstring lower = toLowerText(encoding);
        if (encoding.empty() || lower == L"utf-8" || lower == L"utf8") {
            options.encoding = "UTF-8";
        } else if (lower != L"system" && lower != L"auto" && isSupportedEncoding(encoding)) {
            options.encoding = wstring_to_utf8(encoding);
        } else {
            Error(
                L"Nelson:writelines:invalidEncoding", L"The encoding '%s' is not valid.", encoding);
        }
    } break;
    case WRITE_MODE: {
        options.append
            = matchOptionValue(value, L"WriteMode", { L"overwrite", L"append" }, errorId) == 1;
    } break;
    case TRAILING_LINE_ENDING_RULE: {
        const WriteLinesTrailingRule rules[] = { WriteLinesTrailingRule::AUTO,
            WriteLinesTrailingRule::ALWAYS, WriteLinesTrailingRule::NEVER };
        options.trailingRule = rules[matchOptionValue(
            value, L"TrailingLineEndingRule", { L"auto", L"always", L"never" }, errorId)];
    } break;
    default: {
        Error(L"Nelson:writelines:unknownParameter", L"Unknown Parameter '%s'.", name);
    } break;
    }
}
//=============================================================================
