//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "filewriteBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileWrite.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// filewrite(filename, txt [, eol, encoding])
// eol == 'native' (system default), 'pc' ("\r\n"), 'unix' ("\n")
//=============================================================================
static std::pair<std::wstring, std::string>
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
    std::wstring filename = param1.getContentAsWideString();
    std::wstring weol;
    std::string eol;
    std::string encoding = "UTF-8";

    if (argIn.size() >= 3) {
        ArrayOf param3 = argIn[2];
        std::tie(weol, eol) = getEol(param3.getContentAsWideString());
    } else {
#ifdef _MSC_VER
        weol = L"\r\n";
        eol = "\r\n";
#else
        weol = L"\n";
        eol = "\n";
#endif
    }

    if (argIn.size() == 4) {
        ArrayOf param4 = argIn[3];
        encoding = param4.getContentAsCString();
        if (!isSupportedEncoding(encoding)) {
            Error(_W("Wrong value for #4 argument."));
        }
    }

    wstringVector lines = param2.getContentAsWideStringVector(false);
    std::wstring errorMessage;
    if (!writeFile(filename, lines, weol, eol, encoding, errorMessage)) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
static std::pair<std::wstring, std::string>
getEol(const std::wstring& str)
{
    if (str == L"pc") {
        return { L"\r\n", "\r\n" };
    } else if (str == L"unix") {
        return { L"\n", "\n" };
    } else if (str == L"native") {
#ifdef _MSC_VER
        return { L"\r\n", "\r\n" };
#else
        return { L"\n", "\n" };
#endif
    } else {
        Error(_W("Wrong value for #3 argument."));
        return { L"", "" }; // This line will never be reached
    }
}
//=============================================================================
