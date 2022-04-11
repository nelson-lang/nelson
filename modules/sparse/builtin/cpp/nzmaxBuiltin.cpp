//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nzmaxBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::nzmaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1); // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "nzmax", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf R(argIn[0]);
        switch (R.getDataClass()) {
        case NLS_LOGICAL:
        case NLS_INT8:
        case NLS_UINT8:
        case NLS_CHAR:
        case NLS_INT16:
        case NLS_UINT16:
        case NLS_INT32:
        case NLS_UINT32:
        case NLS_INT64:
        case NLS_UINT64:
        case NLS_SINGLE:
        case NLS_DOUBLE:
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX:
            retval << ArrayOf::doubleConstructor(static_cast<double>(R.nzmax()));
            break;
        case NLS_STRING_ARRAY:
            retval = OverloadFunction(eval, nLhs, argIn, "nzmax", bSuccess);
            if (!bSuccess) {
                Error(_W("Undefined function 'nzmax' for input arguments of type 'string'."));
            }
            return retval;
        case NLS_CELL_ARRAY:
            retval = OverloadFunction(eval, nLhs, argIn, "nzmax", bSuccess);
            if (!bSuccess) {
                Error(_W("Undefined function 'nzmax' for input arguments of type 'cell'."));
            }
            return retval;
        case NLS_STRUCT_ARRAY:
            retval = OverloadFunction(eval, nLhs, argIn, "nzmax", bSuccess);
            if (!bSuccess) {
                Error(_W("Undefined function 'nzmax' for input arguments of type "
                         "'struct'."));
            }
            return retval;
        default:
            retval = OverloadFunction(eval, nLhs, argIn, "nzmax", bSuccess);
            if (!bSuccess) {
                Error(_W("Undefined function 'nzmax' for input arguments."));
            }
        }
    }
    return retval;
}
//=============================================================================
