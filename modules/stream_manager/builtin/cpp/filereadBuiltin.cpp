//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "filereadBuiltin.hpp"
#include "Error.hpp"
#include "MapFileRead.hpp"
#include "ToCellString.hpp"
#include "characters_encoding.hpp"
#include <boost/filesystem.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
//=============================================================================
using namespace Nelson;
//=============================================================================
// txt = fileread(filename, type)
// txt = fileread(filename, 'char', eol)
// eol == 'native' (system), 'pc' ("\r\n"), 'unix' ("\n")
// eol default is "\n"
//=============================================================================
static std::wifstream&
wsafegetline(std::wifstream& os, std::wstring& line)
{
    if (getline(os, line)) {
        if (line.size() && line[line.size() - 1] == L'\r') {
            line.pop_back();
        }
    }
    return os;
}
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::filereadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 1 || argIn.size() > 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
    if (outputClass == L"char") {
        std::wstring errorMessage;
        ArrayOf res = MapFileRead(fileToRead, eol, errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval.push_back(res);
    } else {
#ifdef _MSC_VER
        std::wifstream wif(fileToRead, std::ios::binary);
#else
        std::wifstream wif(wstring_to_utf8(fileToRead), std::ios::binary);
#endif
        if (!wif.is_open()) {
            Error(_W("Cannot open file."));
        }
        std::wstring line;
        wstringVector lines;
        while (wsafegetline(wif, line)) {
            lines.push_back(line);
        }
        wif.close();
        if (outputClass == L"string") {
            Dimensions dims(lines.size(), 1);
            retval.push_back(ArrayOf::stringArrayConstructor(lines, dims));
        } else {
            retval.push_back(ToCellStringAsColumn(lines));
        }
    }
    return retval;
}
//=============================================================================
