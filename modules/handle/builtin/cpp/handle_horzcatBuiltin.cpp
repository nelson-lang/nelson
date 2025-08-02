//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_horzcatBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "HorzCatHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_horzcatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf res = argIn[0];
    for (size_t k = 1; k < argIn.size(); k++) {
        res = HorzCatHandle(res, argIn[k]);
    }
    return res;
}
//=============================================================================
