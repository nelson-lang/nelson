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
#include "rmdirBuiltin.hpp"
#include "Error.hpp"
#include "RemoveDirectory.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::rmdirBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 1 || argIn.size() == 2) {
        bool bbSubfolder = false;
        std::wstring arg1 = argIn[0].getContentAsWideString();
        if (argIn.size() == 2) {
            std::wstring arg2 = argIn[1].getContentAsWideString();
            if ((arg2 == L"s") || (arg2 == L"S")) {
                bbSubfolder = true;
            } else {
                Error("'s' expected.");
            }
        }
        std::wstring errorMessage = L"";
        bool res = RemoveDirectory(arg1, bbSubfolder, errorMessage);
        if (nLhs == 0) {
            if (res == false) {
                Error(errorMessage);
            }
        } else {
            retval.push_back(ArrayOf::logicalConstructor(res));
            if (nLhs > 1) {
                retval.push_back(ArrayOf::characterArrayConstructor(errorMessage));
            } else {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
