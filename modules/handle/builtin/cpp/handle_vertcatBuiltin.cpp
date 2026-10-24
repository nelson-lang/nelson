//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_vertcatBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "VertCatHandle.hpp"
#include "ConcatenateDimensions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_vertcatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);

    Dimensions dims;
    bool allEmpty = argIn.size() > 2 && allEmptyOperandsDimensions(argIn, 0, dims);
    if (argIn.size() > 2 && !allEmpty) {
        checkNdOperandsDimensions(argIn, 0);
    }
    ArrayOf res = argIn[0];
    for (size_t k = 1; k < argIn.size(); k++) {
        res = VertCatHandle(res, argIn[k]);
    }
    if (allEmpty) {
        res.reshape(dims);
    }
    return res;
}
//=============================================================================
