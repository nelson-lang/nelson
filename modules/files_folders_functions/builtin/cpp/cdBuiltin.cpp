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
#include "cdBuiltin.hpp"
#include "ChangeDirectory.hpp"
#include "Error.hpp"
#include "GetCurrentDirectory.hpp"
#include "NelsonPrint.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::cdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.empty()) {
        std::wstring pwd = GetCurrentDirectory();
        if (pwd.empty()) {
            Error(_W("Impossible to get current directory."));
        } else {
            if (nLhs == 0) {
                NelsonPrint(pwd);
            } else {
                retval.push_back(ArrayOf::characterArrayConstructor(pwd));
            }
        }
    } else // argIn.size() == 1
    {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring wpath = argIn[0].getContentAsWideString();
            ArrayOf res = Cd(wpath);
            if (nLhs == 1) {
                retval.push_back(res);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
