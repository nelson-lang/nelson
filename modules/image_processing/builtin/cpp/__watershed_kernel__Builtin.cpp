//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__watershed_kernel__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Watershed.hpp"
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
ArrayOfVector
__watershed_kernel__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    double connectivity = argIn[1].asDoubleScalar();
    ArrayOfVector retval;
    retval << Watershed(argIn[0], connectivity);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
