//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
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
        case NLS_STRING_ARRAY: {
            nargincheck(argIn, 1, 1);
            retval << ArrayOf::stringArrayToCharacterArray(argIn[0], false);
        } break;
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
                    retval << res;
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
                    retval << res;
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
                retval << res;
            }
        } break;
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX: {
            Error(_W("Conversion to char from complex is not possible."));
        } break;
        default:
        case NLS_STRUCT_ARRAY:
        case NLS_LOGICAL:
        case NLS_GO_HANDLE:
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
                    retval << r;
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
                    retval << r;
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
                retval << res;
            }
        } break;
        }
    }
    return retval;
}
//=============================================================================
