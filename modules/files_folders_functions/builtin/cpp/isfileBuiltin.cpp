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
#include "isfileBuiltin.hpp"
#include "Error.hpp"
#include "IsFile.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::isfileBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn[0].isEmpty()) {
        retval.push_back(ArrayOf::logicalConstructor(false));
    } else if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring wpath = argIn[0].getContentAsWideString();
        retval.push_back(ArrayOf::logicalConstructor(IsFile(wpath)));
    } else {
        if (argIn[0].getDataClass() == NLS_CELL_ARRAY) {
            Dimensions dim = argIn[0].getDimensions();
            if (argIn[0].isEmpty()) {
                retval.push_back(ArrayOf::emptyConstructor(dim));
            } else {
                ArrayOf cell(argIn[0]);
                logical* bmat
                    = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, argIn[0].getLength());
                for (indexType k = 0; k < argIn[0].getDimensions().getElementCount(); k++) {
                    ArrayOf cell(argIn[0]);
                    ArrayOf* arg = (ArrayOf*)(cell.getDataPointer());
                    if (arg[k].isRowVectorCharacterArray()) {
                        bmat[k] = IsFile(arg[k].getContentAsWideString());
                    } else {
                        delete[] bmat;
                        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
                    }
                }
                ArrayOf res = ArrayOf(NLS_LOGICAL, argIn[0].getDimensions(), bmat, false);
                retval.push_back(res);
            }
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
        }
    }
    return retval;
}
//=============================================================================
