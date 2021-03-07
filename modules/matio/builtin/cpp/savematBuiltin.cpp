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
#include "savematBuiltin.hpp"
#include "SaveMatioFile.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isOption(const std::wstring& param)
{
    return param.size() > 2 && param[0] == L'-';
}
//=============================================================================
static bool
isValidOption(const std::wstring& option)
{
    return option == L"-append" || option == L"-nocompression" || option == L"-v4"
        || option == L"-v6" || option == L"-v7" || option == L"-v7.3";
}
//=============================================================================
ArrayOfVector
Nelson::MatioGateway::savematBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].getContentAsWideString();
    wstringVector names;
    std::wstring matFileVersion = L"-v7.3";
    bool bAppend = false;
    bool bNoCompression = false;
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf paramK = argIn[k];
        std::wstring param = paramK.getContentAsWideString();
        if (isOption(param)) {
            if (isValidOption(param)) {
                if (param == L"-append") {
                    bAppend = true;
                }
                if (param == L"-nocompression") {
                    bNoCompression = true;
                }
                if (param == L"-v4") {
                    matFileVersion = param;
                }
                if (param == L"-v6") {
                    matFileVersion = param;
                }
                if (param == L"-v7") {
                    matFileVersion = param;
                }
                if (param == L"-v7.3") {
                    matFileVersion = param;
                }
            } else {
                Error(_W("Invalid option:") + param);
            }
        } else {
            names.push_back(param);
        }
    }
    SaveMatioFile(eval, filename, names, matFileVersion, bAppend, bNoCompression);
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
