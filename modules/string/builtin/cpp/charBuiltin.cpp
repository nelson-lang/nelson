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
#include "charBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "IsCellOfStrings.hpp"
#include "OverloadFunction.hpp"
#include "ToChar.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::charBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->overloadOnBasicTypes) {
        retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse()) {
            retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
            if (!bSuccess) {
                Error(eval, _W("Attempt to convert to unimplemented sparse type."));
            }
            return retval;
        }
        switch (argIn[0].getDataClass()) {
        case NLS_CHAR: {
            if (argIn.size() == 2) {
                retval.push_back(ToChar(argIn[0], argIn[1]));
            } else if (argIn.size() == 1) {
                retval.push_back(ToChar(argIn[0]));
            } else {
                ArrayOf res = argIn[0];
                for (size_t k = 1; k < argIn.size(); ++k) {
                    res = ToChar(res, argIn[k]);
                }
                retval.push_back(res);
            }
        } break;
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX: {
            Error(eval, _W("Conversion to char from complex is not possible."));
        } break;
        default:
        case NLS_STRUCT_ARRAY:
        case NLS_LOGICAL:
        case NLS_HANDLE: {
            retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
            if (!bSuccess) {
                Error(eval,
                    _("Undefined function 'char' for input arguments of type") + " '"
                        + ClassName(argIn[0]) + "'.");
            }
        } break;
        case NLS_CELL_ARRAY: {
            if (!IsCellOfString(argIn[0])) {
                retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
                if (!bSuccess) {
                    Error(eval,
                        _("Undefined function 'char' for input arguments of type") + " '"
                            + ClassName(argIn[0]) + "'.");
                }
            }
        }
        case NLS_UINT8:
        case NLS_INT8:
        case NLS_UINT16:
        case NLS_INT16:
        case NLS_UINT32:
        case NLS_INT32:
        case NLS_UINT64:
        case NLS_INT64:
        case NLS_SINGLE:
        case NLS_DOUBLE: {
            Dimensions dims;
            if (argIn.size() == 2) {
                retval.push_back(ToChar(argIn[0], argIn[1]));
            } else if (argIn.size() == 1) {
                retval.push_back(ToChar(argIn[0]));
            } else {
                ArrayOf res = argIn[0];
                for (size_t k = 1; k < argIn.size(); ++k) {
                    res = ToChar(res, argIn[k]);
                }
                retval.push_back(res);
            }
        } break;
        }
    }
    return retval;
}
//=============================================================================
