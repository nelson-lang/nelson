//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "besselhBuiltin.hpp"
#include "BesselFunctions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::besselhBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // besselh(NU, Z), besselh(NU, K, Z) or besselh(NU, K, Z, SCALE)
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    BesselFunctionKind kind = BesselFunctionKind::H1;
    size_t zIndex = 1;
    if (argIn.size() > 2) {
        zIndex = 2;
        kind = (getBesselOrderSelector(argIn[1], 1, 2,
                    L"Nelson:special_functions:besselhInvalidKind", L"K must be equal to 1 or 2.")
                   == 1)
            ? BesselFunctionKind::H1
            : BesselFunctionKind::H2;
    }
    bool scaled = (argIn.size() == 4) ? getBesselScaleArgument(argIn[3]) : false;
    ArrayOfVector retval;
    retval << BesselFunction(kind, argIn[0], argIn[zIndex], scaled);
    return retval;
}
//=============================================================================
