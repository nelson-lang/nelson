//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ndimsBuiltin.hpp"
#include "Error.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::ndimsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    switch (param1.getDataClass()) {
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_SCOMPLEX:
    case NLS_DCOMPLEX:
    case NLS_CHAR:
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_FUNCTION_HANDLE:
    case NLS_MISSING_ARRAY:
    case NLS_STRUCT_ARRAY: {
        double ndims = static_cast<double>(param1.nDims());
        if (ndims < 2) {
            ndims = 2;
        }
        retval << ArrayOf::doubleConstructor(ndims);
    } break;
    case NLS_CLASS_ARRAY:
    default: {
        Error(_("Undefined function 'ndims' for input arguments of type") + " '" + ClassName(param1)
            + "'.");
    } break;
    }
    return retval;
}
//=============================================================================
