//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/filesystem.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include "filereadBuiltin.hpp"
#include "Error.hpp"
#include "MapFileRead.hpp"
#include "ToCellString.hpp"
#include "characters_encoding.hpp"
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
Nelson::StreamGateway::filereadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4); //-V112
    nargoutcheck(nLhs, 0, 1);
    std::wstring fileToRead = argIn[0].getContentAsWideString();
    bool bIsFile
        = boost::filesystem::exists(fileToRead) && !boost::filesystem::is_directory(fileToRead);
    if (!bIsFile) {
        Error(_W("A valid filename expected."));
    }
    std::wstring outputClass = L"char";
    if (argIn.size() > 1) {
        ArrayOf param2 = argIn[1];
        std::wstring str = param2.getContentAsWideString();
        if (str == L"char" || str == L"cell" || str == L"string") {
            outputClass = str;
        } else {
            Error(_W("Wrong value for #2 argument."));
        }
    }
    std::wstring eol = L"\n";
    if (argIn.size() > 2) {
        ArrayOf param3 = argIn[2];
        std::wstring str = param3.getContentAsWideString();
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
            Error(_W("Wrong value for #3 argument."));
        }
        if (outputClass == L"cell") {
            Error(_W("Wrong value for #2 argument."));
        }
    }
    std::wstring encoding = L"UTF-8";
    if (argIn.size() > 3) {
        ArrayOf param4 = argIn[3];
        encoding = param4.getContentAsWideString();
        if (encoding != L"auto") {
            if (!isSupportedEncoding(encoding)) {
                Error(_W("Wrong value for #4 argument."));
            }
        }
    }
    std::wstring content;
    std::wstring errorMessage;
    if (MapFileRead(fileToRead, eol, encoding, content, errorMessage)) {
        ArrayOf res;
        if (outputClass == L"char") {
            res = ArrayOf::characterArrayConstructor(content);
        } else {
            wstringVector results = getLines(content);
            if (outputClass == L"string") {
                Dimensions dims(results.size(), 1);
                res = ArrayOf::stringArrayConstructor(results, dims);
            } else {
                // cell
                res = ToCellStringAsColumn(results);
            }
        }
        retval << res;
    } else {
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
    }
    return retval;
}
//=============================================================================
