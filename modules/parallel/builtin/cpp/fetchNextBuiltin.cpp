//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fetchNextBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FevalFutureFetchNext.hpp"
#include "FevalFutureObject.hpp"
#include "AfterAllFutureObject.hpp"
#include "AfterEachFutureObject.hpp"
#include "FutureObjectHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::fetchNextBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);

    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(_W("FevalFuture handle expected."));
    }
    bool isSupportedType = (param1.getHandleCategory() == NLS_HANDLE_FEVALFUTURE_CATEGORY_STR);
    if (!isSupportedType) {
        Error(_W("FevalFuture handle expected."));
    }

    double timeout = -1;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        bool isReal = param2.getDataClass() == NLS_DOUBLE || param2.getDataClass() == NLS_SINGLE;
        if (!isReal) {
            Error(_W("a numeric scalar value expected."));
        }
        timeout = param2.getContentAsDoubleScalar();
        if (timeout < 0) {
            Error(_W("non negative value expected."));
        }
    }
    std::vector<FutureObject*> futures = ArrayOfToFutures(param1);
    return FevalFutureFetchNext(eval, futures, nLhs, timeout);
}
//=============================================================================
