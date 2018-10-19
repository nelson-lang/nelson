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
#include "copyfileBuiltin.hpp"
#include "CopyFile.hpp"
#include "Error.hpp"
#include "IsFile.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::copyfileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 2 || argIn.size() == 3) {
        bool bForce = false;
        if (argIn.size() == 3) {
            std::wstring arg3 = argIn[2].getContentAsWideString();
            if ((arg3 == L"f") || (arg3 == L"F")) {
                bForce = true;
            } else {
                Error("'f' expected.");
            }
        }
        bool bRes = false;
        std::wstring errorMessage = L"";
        ArrayOf arg2 = argIn[1];
        std::wstring dest = arg2.getContentAsWideString();
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray()) {
            std::wstring src = arg1.getContentAsWideString();
            if (IsFile(src)) {
                bRes = CopyFile(src, dest, bForce, errorMessage);
            } else {
                bRes = CopyDirectory(src, dest, bForce, errorMessage);
            }
        } else if (arg1.isCell()) {
            wstringVector src = arg1.getContentAsWideStringVector(true);
            bRes = CopyFiles(src, dest, bForce, errorMessage);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
        }
        if (nLhs == 0) {
            if (!bRes) {
                Error(errorMessage);
            }
        } else {
            retval.push_back(ArrayOf::logicalConstructor(bRes));
            if (nLhs > 1) {
                retval.push_back(ArrayOf::characterArrayConstructor(errorMessage));
            }
            if (nLhs > 2) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
