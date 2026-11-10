//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "intminBuiltin.hpp"
#include "ConstantClassArguments.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::IntegerGateway::intminBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    ConstantClassRequest request = parseConstantClassArguments(argIn, true);
    ArrayOfVector retval;
    retval << applyConstantClassRequest(integerLimitScalar(request.className, false), request);
    return retval;
}
//=============================================================================
