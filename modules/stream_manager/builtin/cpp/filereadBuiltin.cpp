//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <iostream>
#include <sstream>
#include "filereadBuiltin.hpp"
#include "FileSystemWrapper.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "MapFileRead.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// txt = fileread(filename, type)
// txt = fileread(filename, 'char', eol)
// eol == 'native' (system), 'pc' ("\r\n"), 'unix' ("\n")
// eol default is "\n"
//=============================================================================
static wstringVector
getLines(const std::wstring& s)
{
    std::wstring line;
    wstringVector parts;
    std::wstringstream wss(s);
    while (std::getline(wss, line)) {
        if (!line.empty() && line[line.size() - 1] == L'\r') {
            line.pop_back();
        }
        parts.push_back(line);
    }
    return parts;
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::filereadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    std::wstring fileToRead = argIn[0].asWideString();
    bool bIsFile = FileSystemWrapper::Path::is_regular_file(fileToRead);
    if (!bIsFile) {
        Error(L"Nelson:fileread:invalidFilename", L"A valid filename expected.");
    }
    std::wstring outputClass = L"char";
    if (argIn.size() > 1) {
        ArrayOf param2 = argIn[1];
        std::wstring str = param2.asWideString();
        if (str == L"char" || str == L"cell" || str == L"string") {
            outputClass = str;
        } else {
            Error(L"Nelson:stream_manager:invalidArgument", L"Wrong value for #%d argument.", 2);
        }
    }
    std::wstring eol = L"\n";
    if (argIn.size() > 2) {
        ArrayOf param3 = argIn[2];
        std::wstring str = param3.asWideString();
        if (str == L"native" || str == L"pc" || str == L"unix") {
            if (str == L"native") {
#ifdef _MSC_VER
                eol = L"\r\n";
#else
                eol = L"\n";
#endif
            } else {
                if (str == L"pc") {
                    eol = L"\r\n";
                } else {
                    eol = L"\n";
                }
            }
        } else {
            Error(L"Nelson:stream_manager:invalidArgument", L"Wrong value for #%d argument.", 3);
        }
        if (outputClass == L"cell") {
            Error(L"Nelson:stream_manager:invalidArgument", L"Wrong value for #%d argument.", 2);
        }
    }
    std::wstring encoding = L"UTF-8";
    if (argIn.size() > 3) {
        ArrayOf param4 = argIn[3];
        encoding = param4.asWideString();
        if (encoding != L"auto") {
            if (!isSupportedEncoding(encoding)) {
                Error(
                    L"Nelson:stream_manager:invalidArgument", L"Wrong value for #%d argument.", 4);
            }
        }
    }
    std::wstring content;
    std::wstring errorMessage;
    if (MapFileRead(fileToRead, eol, encoding, content, errorMessage)) {
        ArrayOf res;
        if (outputClass == L"char") {
            res = ArrayOf::characterArray(content);
        } else {
            wstringVector results = getLines(content);
            if (outputClass == L"string") {
                Dimensions dims(results.size(), 1);
                res = ArrayOf::stringArray(results, dims);
            } else {
                res = ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(results);
            }
        }
        retval << res;
    } else {
        if (!errorMessage.empty()) {
            Error(L"Nelson:fileread:readFailed", errorMessage);
        }
    }
    return retval;
}
//=============================================================================
