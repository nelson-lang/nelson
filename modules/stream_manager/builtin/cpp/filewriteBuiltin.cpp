//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "filewriteBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileWrite.hpp"
#include "WriteLines.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// filewrite(filename, txt [, eol, encoding])
// eol == 'native' (system default), 'pc' ("\r\n"), 'unix' ("\n")
//=============================================================================
static std::wstring
getEol(const std::wstring& str);
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::filewriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring filename = param1.asWideString();
    std::wstring eol = argIn.size() >= 3 ? getEol(argIn[2].asWideString()) : nativeLineEnding();
    std::string encoding = "UTF-8";

    if (argIn.size() == 4) {
        ArrayOf param4 = argIn[3];
        encoding = param4.asUtf8String();
        if (!isSupportedEncoding(encoding)) {
            Error(L"Nelson:stream_manager:invalidArgument", L"Wrong value for #%d argument.", 4);
        }
    }

    // Char matrices are split in rows, other text is viewed without copy.
    std::vector<TextSpan> lines;
    wstringVector rows;
    if (!textSpans(param2, lines)) {
        rows = param2.asWideStringVector(false);
        lines.reserve(rows.size());
        for (const std::wstring& row : rows) {
            lines.push_back({ row.data(), row.size() });
        }
    }
    std::wstring errorMessage;
    if (!writeFile(filename, lines, eol, encoding, errorMessage)) {
        Error(L"Nelson:filewrite:writeFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
static std::wstring
getEol(const std::wstring& str)
{
    if (str == L"pc") {
        return L"\r\n";
    }
    if (str == L"unix") {
        return L"\n";
    }
    if (str != L"native") {
        Error(L"Nelson:stream_manager:invalidArgument", L"Wrong value for #%d argument.", 3);
    }
    return nativeLineEnding();
}
//=============================================================================
