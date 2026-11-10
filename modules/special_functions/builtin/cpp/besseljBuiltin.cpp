//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "besseljBuiltin.hpp"
#include "BesselFunctions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::besseljBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // besselj(NU, Z) or besselj(NU, Z, SCALE)
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    bool scaled = (argIn.size() == 3) ? getBesselScaleArgument(argIn[2]) : false;
    ArrayOfVector retval;
    retval << BesselFunction(BesselFunctionKind::J, argIn[0], argIn[1], scaled);
    return retval;
}
//=============================================================================
