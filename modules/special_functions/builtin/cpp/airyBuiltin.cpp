//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "airyBuiltin.hpp"
#include "BesselFunctions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::airyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // airy(Z), airy(K, Z) or airy(K, Z, SCALE)
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 1);
    int k = 0;
    size_t zIndex = 0;
    if (argIn.size() > 1) {
        zIndex = 1;
        k = getBesselOrderSelector(argIn[0], 0, 3, L"Nelson:special_functions:airyInvalidKind",
            L"K must be equal to 0, 1, 2 or 3.");
    }
    bool scaled = (argIn.size() == 3) ? getBesselScaleArgument(argIn[2]) : false;
    ArrayOfVector retval;
    retval << AiryFunction(k, argIn[zIndex], scaled);
    return retval;
}
//=============================================================================
