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
// filewrite(filename, txt [, eol, encoding])
// eol == 'native' (system default), 'pc' ("\r\n"), 'unix' ("\n")
ArrayOfVector
Nelson::StreamGateway::filewriteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
            boost::replace_all(line, L"\r\n", L"\n");
            boost::replace_all(line, L"\n", weol);
            wof << line;
            if (!boost::algorithm::ends_with(line, weol)) {
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
            boost::replace_all(asUtf8, "\r\n", "\n");
            boost::replace_all(asUtf8, "\n", eol);
            std::string data;
            if (utf8ToCharsetConverter(asUtf8, data, encoding)) {
                of << data;
                if (!boost::algorithm::ends_with(data, eol)) {
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
