//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "issingleBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::issingleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isint8", bSuccess);
    }
    if (!bSuccess) {
        bool bRes
            = (argIn[0].getDataClass() == NLS_SINGLE || argIn[0].getDataClass() == NLS_SCOMPLEX);
        retval << ArrayOf::logicalConstructor(bRes);
    }
    return retval;
}
//=============================================================================
