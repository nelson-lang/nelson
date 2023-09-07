//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include <fstream>
#include <iostream>
#include <sstream>
#include "filewriteBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// filewrite(filename, txt [, eol, encoding])
// eol == 'native' (system default), 'pc' ("\r\n"), 'unix' ("\n")
ArrayOfVector
Nelson::StreamGateway::filewriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 4); //-V112
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring filename = param1.getContentAsWideString();
    std::wstring weol;
    std::string eol;
    std::string encoding = "UTF-8";
#ifdef _MSC_VER
    weol = L"\r\n";
    eol = "\r\n";
#else
    weol = L"\n";
    eol = "\n";
#endif
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        std::wstring str = param3.getContentAsWideString();
        if (str == L"native" || str == L"pc" || str == L"unix") {
            if (str == L"pc") {
                weol = L"\r\n";
                eol = "\r\n";
            }
            if (str == L"unix") {
                weol = L"\n";
                eol = "\n";
            }
        } else {
            Error(_W("Wrong value for #3 argument."));
        }
    }
    wstringVector lines = param2.getContentAsWideStringVector(false);
    if (argIn.size() == 4) { //-V112
        ArrayOf param4 = argIn[3];
        encoding = param4.getContentAsCString();
        if (!isSupportedEncoding(encoding)) {
            Error(_W("Wrong value for #4 argument."));
        }
    }

    if (encoding == "UTF-8") {
#ifdef _MSC_VER
        std::wofstream wof(filename, std::ios::trunc | std::ios::binary);
#else
        std::wofstream wof(wstring_to_utf8(filename), std::ios::trunc | std::ios::binary);
#endif
        if (!wof.is_open()) {
            Error(_W("Cannot open file."));
        }
        for (size_t k = 0; k < lines.size(); ++k) {
            std::wstring line = lines[k];
            StringHelpers::replace_all(line, L"\r\n", L"\n");
            StringHelpers::replace_all(line, L"\n", weol);
            wof << line;
            if (!StringHelpers::ends_with(line, weol)) {
                if (k != lines.size() - 1) {
                    wof << weol;
                }
            }
        }
        wof.close();
    } else {
#ifdef _MSC_VER
        std::ofstream of(filename, std::ios::trunc | std::ios::binary);
#else
        std::ofstream of(wstring_to_utf8(filename), std::ios::trunc | std::ios::binary);
#endif
        if (!of.is_open()) {
            Error(_W("Cannot open file."));
        }
        for (size_t k = 0; k < lines.size(); ++k) {
            std::string asUtf8 = wstring_to_utf8(lines[k]);
            StringHelpers::replace_all(asUtf8, "\r\n", "\n");
            StringHelpers::replace_all(asUtf8, "\n", eol);
            std::string data;
            if (utf8ToCharsetConverter(asUtf8, data, encoding)) {
                of << data;
                if (!StringHelpers::ends_with(data, eol)) {
                    if (k != lines.size() - 1) {
                        of << eol;
                    }
                }
            } else {
                of.close();
                Error(_W("Encoding not supported."));
            }
        }
        of.close();
    }
    return retval;
}
//=============================================================================
