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
#include "diff_fileBuiltin.hpp"
#include "Error.hpp"
#include "FileDiff.hpp"
#include "IsFile.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::diff_fileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 2 || argIn.size() > 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    logical eolCompare = true;
    std::wstring filename1 = param1.getContentAsWideString();
    std::wstring filename2 = param2.getContentAsWideString();
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        eolCompare = param3.getContentAsLogicalScalar();
    }
    std::wstring res;
    FileDiff(filename1, filename2, eolCompare, res);
    retval.push_back(ArrayOf::characterArrayConstructor(res));
    return retval;
}
//=============================================================================
