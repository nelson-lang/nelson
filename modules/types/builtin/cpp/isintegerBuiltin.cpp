//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isintegerBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isintegerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isinteger", bSuccess);
    }
    if (!bSuccess) {
        bool bRes = (argIn[0].getDataClass() == NLS_UINT8 || argIn[0].getDataClass() == NLS_INT8
            || argIn[0].getDataClass() == NLS_UINT16 || argIn[0].getDataClass() == NLS_INT16
            || argIn[0].getDataClass() == NLS_UINT32 || argIn[0].getDataClass() == NLS_INT32
            || argIn[0].getDataClass() == NLS_UINT64 || argIn[0].getDataClass() == NLS_INT64);
        retval << ArrayOf::logicalConstructor(bRes);
    }
    return retval;
}
//=============================================================================
