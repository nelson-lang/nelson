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
#include "mkdirBuiltin.hpp"
#include "Error.hpp"
#include "MakeDirectory.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::mkdirBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 1 || argIn.size() == 2) {
        if (nLhs > 2) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        std::wstring parentDir;
        std::wstring newDir;
        if (argIn.size() == 2) {
            if (argIn[1].isRowVectorCharacterArray()) {
                newDir = argIn[1].getContentAsWideString();
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED);
            }
        }
        if (argIn[0].isRowVectorCharacterArray()) {
            parentDir = argIn[0].getContentAsWideString();
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring message;
        bool bOK = false;
        if (newDir.empty()) {
            bOK = MakeDirectory(parentDir, message);
        } else {
            bOK = MakeDirectory(parentDir, newDir, message);
        }
        if (nLhs == 0) {
            if (!bOK) {
                Error(message);
            }
        } else {
            if (nLhs > 0) {
                retval.push_back(ArrayOf::logicalConstructor(bOK));
            }
            if (nLhs > 1) {
                retval.push_back(ArrayOf::characterArrayConstructor(message));
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
