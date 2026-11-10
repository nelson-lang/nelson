//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cfloat>
#include <limits>
#include "realmaxBuiltin.hpp"
#include "ConstantClassArguments.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::realmaxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    ConstantClassRequest request = parseConstantClassArguments(argIn, false);
    ArrayOf value = request.className == L"single"
        ? ArrayOf::singleScalar(std::numeric_limits<single>::max())
        : ArrayOf::doubleScalar(std::numeric_limits<double>::max());
    ArrayOfVector retval;
    retval << applyConstantClassRequest(value, request);
    return retval;
}
//=============================================================================
