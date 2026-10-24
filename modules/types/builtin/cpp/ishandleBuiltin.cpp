//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ishandleBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::ishandleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    Dimensions dims = argIn[0].dimensions();
    logical* ptrResult = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, dims.elementCount(), stringVector(), false));
    bool isHandleType
        = (argIn[0].dataClass() == NLS_HANDLE) || (argIn[0].dataClass() == NLS_GO_HANDLE);
    for (indexType k = 0; k < dims.elementCount(); ++k) {
        ptrResult[k] = isHandleType;
    }
    retval << ArrayOf(NLS_LOGICAL, dims, ptrResult);
    return retval;
}
//=============================================================================
