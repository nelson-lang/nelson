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
#include "flintmaxBuiltin.hpp"
#include "ConstantClassArguments.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DoubleGateway::flintmaxBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(nLhs, 0, 1);
    ConstantClassRequest request = parseConstantClassArguments(argIn, false);
    ArrayOf value = request.className == L"single"
        ? ArrayOf::singleScalar(static_cast<single>(1UL << FLT_MANT_DIG))
        : ArrayOf::doubleScalar(static_cast<double>(1ULL << DBL_MANT_DIG));
    ArrayOfVector retval;
    retval << applyConstantClassRequest(value, request);
    return retval;
}
//=============================================================================
