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
#include "doubleBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "ToDouble.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::doubleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->overloadOnBasicTypes) {
        retval = OverloadFunction(eval, nLhs, argIn, "double", bSuccess);
    }
    if (!bSuccess) {
        switch (argIn[0].getDataClass()) {

        case NLS_HANDLE: {
            retval = OverloadFunction(eval, nLhs, argIn, "double", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(_W("Conversion to double from handle is not possible."));
        } break;
        case NLS_CELL_ARRAY: {
            retval = OverloadFunction(eval, nLhs, argIn, "double", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(_W("Conversion to double from cell is not possible."));
        } break;
        case NLS_STRUCT_ARRAY: {
            retval = OverloadFunction(eval, nLhs, argIn, "double", bSuccess);
            if (bSuccess) {
                return retval;
            }
            if (argIn[0].getStructType() != "struct") {
                Error(_("Undefined function 'double' for input arguments of type '")
                        + argIn[0].getStructType() + "'.");
            } else {
                Error(_W("Conversion to double from struct is not possible."));
            }
        } break;
        default:
            retval.push_back(ToDouble(argIn[0]));
            break;
        }
    }
    return retval;
}
//=============================================================================
