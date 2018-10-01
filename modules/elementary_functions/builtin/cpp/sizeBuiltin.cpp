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
#include "sizeBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::sizeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bChooseDimension = false;
    indexType dimval = 0;
    switch (argIn.size()) {
    case 1: {
        bChooseDimension = false;
    } break;
    case 2: {
        bChooseDimension = true;
        ArrayOf param1(argIn[1]);
        if (param1.isRowVectorCharacterArray()) {
            std::wstring str = param1.getContentAsWideString();
            if ((str == L"r") || (str == L"c")) {
                if (str == L"r") {
                    dimval = 1;
                } else {
                    dimval = 2;
                }
            } else {
                Error(_W("Wrong value for argument #2. 'r' or 'c' expected"));
            }
        } else {
            dimval = param1.getContentAsScalarIndex(false);
        }
    } break;
    default:
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
        break;
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "size", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "size", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        ArrayOf param1 = argIn[0];
        if (param1.isClassStruct()) {
            Error(_("Undefined function 'size' for input arguments of type") + " '"
                + ClassName(param1) + "'.");
        }
        Dimensions sze(param1.getDimensions());
        sze.simplify();
        if (bChooseDimension) {
            if (nLhs > 1) {
                Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
            }
            if (dimval - 1 >= maxDims) {
                retval.push_back(ArrayOf::doubleConstructor((double)(1.0)));
            } else {
                retval.push_back(ArrayOf::doubleConstructor((double)(sze[dimval - 1])));
            }
        } else {
            if (nLhs > 1) {
                for (int i = 0; i < nLhs; i++) {
                    retval.push_back(ArrayOf::doubleConstructor((double)(sze[i])));
                }
            } else {
                double* dims = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, sze.getLength());
                Dimensions retDim(2);
                if (sze.getLength() == 0) {
                    retDim[0] = 0;
                    retDim[1] = 0;
                } else {
                    for (indexType i = 0; i < sze.getLength(); i++) {
                        dims[i] = (double)sze[i];
                    }
                    retDim[0] = 1;
                    retDim[1] = sze.getLength();
                }
                retval.push_back(ArrayOf(NLS_DOUBLE, retDim, dims));
            }
        }
    }
    return retval;
}
//=============================================================================
