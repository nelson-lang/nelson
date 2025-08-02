//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "charBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ToChar.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::charBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
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
                OverloadRequired("char");
            } else {
                retval << res;
            }
        } else if (argIn.size() == 1) {
            bool needToOverload;
            ArrayOf res = ToChar(argIn[0], needToOverload);
            if (needToOverload) {
                OverloadRequired("char");
            } else {
                retval << res;
            }
        } else {
            ArrayOf res = argIn[0];
            for (size_t k = 1; k < argIn.size(); ++k) {
                bool needToOverload;
                res = ToChar(res, argIn[k], needToOverload);
                if (needToOverload) {
                    OverloadRequired("char");
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
    case NLS_CLASS_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_GO_HANDLE:
    case NLS_HANDLE: {
        OverloadRequired("char");
    } break;
    case NLS_CELL_ARRAY: {
        if (!argIn[0].isCellArrayOfCharacterVectors()) {
            OverloadRequired("char");
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
                OverloadRequired("char");
            } else {
                retval << r;
            }
        } else if (argIn.size() == 1) {
            bool needToOverload;
            ArrayOf r = ToChar(argIn[0], needToOverload);
            if (needToOverload) {
                OverloadRequired("char");
            } else {
                retval << r;
            }
        } else {
            ArrayOf res = argIn[0];
            for (size_t k = 1; k < argIn.size(); ++k) {
                bool needToOverload;
                ArrayOf r = ToChar(res, argIn[k], needToOverload);
                if (needToOverload) {
                    OverloadRequired("char");
                } else {
                    res = r;
                }
            }
            retval << res;
        }
    } break;
    }
    return retval;
}
//=============================================================================
