//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isrealBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TypeGateway::isrealBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf arg = argIn[0];
    bool bRes = arg.allReal();
    if (arg.isComplex() && bRes && !arg.isEmpty(true)) {
        bRes = false;
    }
    retval << ArrayOf::logicalConstructor(bRes);
    return retval;
}
//=============================================================================
