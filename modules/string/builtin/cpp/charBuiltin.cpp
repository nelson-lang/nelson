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
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse()) {
            retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
            if (!bSuccess) {
                Error(_W("Attempt to convert to unimplemented sparse type."));
            }
            return retval;
        }
        switch (argIn[0].getDataClass()) {
        case NLS_CHAR: {
            if (argIn.size() == 2) {
                bool needToOverload;
                ArrayOf res = ToChar(argIn[0], argIn[1], needToOverload);
                if (needToOverload) {
                    ArrayOfVector tmp;
                    tmp.push_back(argIn[0]);
                    tmp.push_back(argIn[1]);
                    retval = OverloadFunction(eval, nLhs, tmp, "char", bSuccess);
                    if (!bSuccess) {
                        Error(_("Undefined function 'char' for input arguments."));
                    }
                } else {
                    retval.push_back(res);
                }
            } else if (argIn.size() == 1) {
                bool needToOverload;
                ArrayOf res = ToChar(argIn[0], needToOverload);
                if (needToOverload) {
                    retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
                    if (!bSuccess) {
                        Error(_("Undefined function 'char' for input arguments of type") + " '"
                            + ClassName(argIn[0]) + "'.");
                    }
                } else {
                    retval.push_back(res);
                }
            } else {
                ArrayOf res = argIn[0];
                for (size_t k = 1; k < argIn.size(); ++k) {
                    bool needToOverload;
                    res = ToChar(res, argIn[k], needToOverload);
                    if (needToOverload) {
                        ArrayOfVector tmp;
                        tmp.push_back(argIn[k]);
                        ArrayOfVector tmpRet = OverloadFunction(eval, nLhs, tmp, "char", bSuccess);
                        if (!bSuccess) {
                            Error(_("Undefined function 'char' for input arguments of type") + " '"
                                + ClassName(argIn[k]) + "'.");
                        }
                        res = tmpRet[0];
                    }
                }
                retval.push_back(res);
            }
        } break;
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX: {
            Error(_W("Conversion to char from complex is not possible."));
        } break;
        default:
        case NLS_STRUCT_ARRAY:
        case NLS_LOGICAL:
        case NLS_HANDLE: {
            retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
            if (!bSuccess) {
                Error(_("Undefined function 'char' for input arguments of type") + " '"
                    + ClassName(argIn[0]) + "'.");
            }
        } break;
        case NLS_CELL_ARRAY: {
            if (!IsCellOfString(argIn[0])) {
                retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
                if (!bSuccess) {
                    Error(_("Undefined function 'char' for input arguments of type") + " '"
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
                bool needToOverload;
                ArrayOf r = ToChar(argIn[0], argIn[1], needToOverload);
                if (needToOverload) {
                    ArrayOfVector tmp;
                    tmp.push_back(argIn[0]);
                    tmp.push_back(argIn[1]);
                    retval = OverloadFunction(eval, nLhs, tmp, "char", bSuccess);
                    if (!bSuccess) {
                        Error(_("Undefined function 'char' for input arguments."));
                    }
                } else {
                    retval.push_back(r);
                }
            } else if (argIn.size() == 1) {
                bool needToOverload;
                ArrayOf r = ToChar(argIn[0], needToOverload);
                if (needToOverload) {
                    retval = OverloadFunction(eval, nLhs, argIn, "char", bSuccess);
                    if (!bSuccess) {
                        Error(_("Undefined function 'char' for input arguments."));
                    }
                } else {
                    retval.push_back(r);
                }
            } else {
                ArrayOf res = argIn[0];
                for (size_t k = 1; k < argIn.size(); ++k) {
                    bool needToOverload;
                    ArrayOf r = ToChar(res, argIn[k], needToOverload);
                    if (needToOverload) {
                        ArrayOfVector tmp;
                        tmp.push_back(res);
                        tmp.push_back(argIn[k]);
                        retval = OverloadFunction(eval, nLhs, tmp, "char", bSuccess);
                        if (!bSuccess) {
                            Error(_("Undefined function 'char' for input arguments."));
                        }
                        res = retval[0];
                    } else {
                        res = r;
                    }
                }
                retval.push_back(res);
            }
        } break;
        }
    }
    return retval;
}
//=============================================================================