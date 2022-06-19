//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fetchOutputsBuiltin.hpp"
#include "Error.hpp"
#include "FutureFetchOutputs.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::fetchOutputsBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(_W("FevalFuture handle expected."));
    }
    if (!param1.isScalar()) {
        Error(_W("FevalFuture handle expected."));
    }
    if (param1.getHandleCategory() != FEVALFUTURE_CATEGORY_STR) {
        Error(_W("FevalFuture handle expected."));
    }
    auto* fevalFutureObject = (FevalFutureObject*)param1.getContentAsHandleScalar();
    if (fevalFutureObject) {
        retval = FutureFetchOutputs(eval, fevalFutureObject);
    } else {
        Error(_W("FevalFuture not available"));
    }
    return retval;
}
//=============================================================================
