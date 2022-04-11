//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isfloatBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isfloatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "isfloat", bSuccess);
    }
    if (argIn[0].isClassStruct() || argIn[0].isHandle()) {
        bool bSuccess = false;
        retval = OverloadFunction(eval, nLhs, argIn, "isfloat", bSuccess);
        if (bSuccess) {
            return retval;
        }
    }
    if (!bSuccess) {
        bool bRes = (argIn[0].getDataClass() == NLS_DOUBLE
            || argIn[0].getDataClass() == NLS_DCOMPLEX || argIn[0].getDataClass() == NLS_SINGLE
            || argIn[0].getDataClass() == NLS_SCOMPLEX);
        retval << ArrayOf::logicalConstructor(bRes);
    }
    return retval;
}
//=============================================================================
