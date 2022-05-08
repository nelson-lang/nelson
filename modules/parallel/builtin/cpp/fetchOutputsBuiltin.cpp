//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <tuple>
#include "fetchOutputsBuiltin.hpp"
#include "Error.hpp"
#include "FevalFutureObject.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::fetchOutputsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) { 
            Error(_W("fevalFuture handle expected."));
    }
    if (!param1.isScalar()) { 
            Error(_W("fevalFuture handle expected."));
    }
    if (param1.getHandleCategory() != FEVALFUTURE_CATEGORY_STR) {
            Error(_W("fevalFuture handle expected."));
    }
    auto* fevalFutureObject = (FevalFutureObject*)param1.getContentAsHandleScalar();
    bool valid;
    std::tuple<ArrayOfVector, Exception> resultOrFuture = fevalFutureObject->get(valid);
    ArrayOfVector result; 
    result = std::get<0>(resultOrFuture);
    return result;
}
//=============================================================================
