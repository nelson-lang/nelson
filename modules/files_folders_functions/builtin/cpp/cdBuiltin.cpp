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
#include "cdBuiltin.hpp"
#include "ChangeDirectory.hpp"
#include "Error.hpp"
#include "GetCurrentDirectory.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::cdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        std::wstring pwd = GetCurrentDirectory();
        if (pwd == L"") {
            Error(_W("Impossible to get current directory."));
        } else {
            if (nLhs == 0) {
                Interface* io = eval->getInterface();
                if (io) {
                    io->outputMessage(pwd);
                }
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
