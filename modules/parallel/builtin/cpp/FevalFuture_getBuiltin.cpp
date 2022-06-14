//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalFuture_getBuiltin.hpp"
#include "FevalFutureObject.hpp"
#include "Error.hpp"
#include "usedHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::FevalFuture_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring propertyName = param2.getContentAsWideString();
    ArrayOfVector retval(1);
    if (param1.getHandleCategory() != FEVALFUTURE_CATEGORY_STR) {
        Error(_W("FevalFuture handle expected."));
    }
    auto* objFevalFuture = (FevalFutureObject*)param1.getContentAsHandleScalar();
    ArrayOf res;
    if (!objFevalFuture->get(propertyName, res)) {
        Error(ERROR_WRONG_ARGUMENT_2_VALUE + L" " + propertyName);
    }
    retval << res;
    return retval;
}
//=============================================================================
