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
#include <sstream>
#include <fstream>
#include<iostream>
#include <boost/filesystem.hpp>
#include "filereadBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::StreamGateway::filereadBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    std::wstring fileToRead = argIn[0].getContentAsWideString();
    bool bIsFile = boost::filesystem::exists(fileToRead) && !boost::filesystem::is_directory(fileToRead);
    if (!bIsFile)
    {
        Error(eval, _W("A filename expected."));
    }
#ifdef _MSC_VER
    std::wifstream wif(fileToRead, std::ios::binary);
#else
    std::wifstream wif(wstring_to_utf8(fileToRead), std::ios::binary);
#endif
    if (!wif.is_open())
    {
        Error(eval, _W("Cannot open file."));
    }
    std::wstringstream wss;
    wss << wif.rdbuf();
    std::wstring content = wss.str();
    retval.push_back(ArrayOf::stringConstructor(content));
    return retval;
}
//=============================================================================
