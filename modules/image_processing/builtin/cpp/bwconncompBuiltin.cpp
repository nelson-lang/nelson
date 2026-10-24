//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bwconncompBuiltin.hpp"
#include "BWConnComp.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
ArrayOfVector
bwconncompBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    double connectivity = argIn[0].dimensions().length() > 2 ? 26 : 8;
    if (argIn.size() == 2) {
        connectivity = argIn[1].asDoubleScalar();
    }
    ArrayOfVector retval;
    retval << BWConnComp(argIn[0], connectivity);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
