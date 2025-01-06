//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FevalQueue_cancelAllBuiltin.hpp"
#include "FevalQueueObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ParallelGateway::FevalQueue_cancelAllBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    ArrayOfVector retval;
    if (param1.getHandleCategory() != NLS_HANDLE_FEVALQUEUE_CATEGORY_STR) {
        Error(_W("FevalQueue handle expected."));
    }
    auto* objFevalQueue = (FevalQueueObject*)param1.getContentAsHandleScalar();
    if (objFevalQueue != nullptr) {
        objFevalQueue->cancelAll();
    }
    return retval;
}
//=============================================================================
