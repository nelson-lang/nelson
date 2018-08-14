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
#include "filewriteBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
//=============================================================================
using namespace Nelson;
//=============================================================================
// filewrite(filename, txt [, eol])
// eol == 'native' (system default), 'pc' ("\r\n"), 'unix' ("\n")
ArrayOfVector
Nelson::StreamGateway::filewriteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || argIn.size() > 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs != 0) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring filename = param1.getContentAsWideString();
    std::wstring eol;
#ifdef _MSC_VER
    eol = L"\r\n";
#else
    eol = L"\n";
#endif
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        std::wstring str = param3.getContentAsWideString();
        if (str == L"native" || str == L"pc" || str == L"unix") {
            if (str == L"pc") {
                eol = L"\r\n";
            }
            if (str == L"unix") {
                eol = L"\n";
            }
        } else {
            Error(_W("Wrong value for #3 argument."));
        }
    }
    wstringVector lines = param2.getContentAsWideStringVector(false);
#ifdef _MSC_VER
    std::wofstream wof(filename, std::ios::trunc | std::ios::binary);
#else
    std::wofstream wof(wstring_to_utf8(filename), std::ios::trunc | std::ios::binary);
#endif
    if (!wof.is_open()) {
        Error(_W("Cannot open file."));
    }
    for (std::wstring line : lines) {
        boost::replace_all(line, L"\r\n", L"\n");
        boost::replace_all(line, L"\n", eol);
        wof << line;
        if (!boost::algorithm::ends_with(line, eol)) {
            wof << eol;
        }
    }
    wof.close();
    return retval;
}
//=============================================================================
