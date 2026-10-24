//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_deleteBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HandleDelete.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_deleteBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
    }
    HandleDelete(eval, param1);
    return retval;
}
//=============================================================================
