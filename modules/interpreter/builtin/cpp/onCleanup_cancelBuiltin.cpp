//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "onCleanup_cancelBuiltin.hpp"
#include "OnCleanupObjectHandle.hpp"
#include "Error.hpp"
#include "usedHandle.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::InterpreterGateway::onCleanup_cancelBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.getHandleCategory() != NLS_HANDLE_ONCLEANUP_CATEGORY_STR) {
        Error(_W("onCleanup handle expected."));
    }
    auto* obj = (OnCleanupObjectHandle*)param1.getContentAsHandleScalar();
    if (obj) {
        obj->cancel();
    }
    return {};
}
//=============================================================================
