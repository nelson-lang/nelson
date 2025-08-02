//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "detBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "DeterminantMatrix.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::detBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool needToOverload;
    ArrayOf param1 = argIn[0];
    ArrayOf res = DeterminantMatrix(param1, needToOverload);
    if (needToOverload) {
        OverloadRequired("det");
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
