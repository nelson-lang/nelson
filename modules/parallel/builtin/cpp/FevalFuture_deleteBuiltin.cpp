//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalFuture_deleteBuiltin.hpp"
#include "DeleteGenericObject.hpp"
#include "FevalFutureObject.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::FevalFuture_deleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (param1.isHandle()) {
        DeleteGenericObject(param1, NLS_HANDLE_FEVALFUTURE_CATEGORY_STR);
    }
    ArrayOfVector retval;
    return retval;
}
//=============================================================================
