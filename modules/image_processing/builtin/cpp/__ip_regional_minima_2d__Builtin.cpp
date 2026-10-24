//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__ip_regional_minima_2d__Builtin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RegionalMinima.hpp"
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
ArrayOfVector
__ip_regional_minima_2d__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << RegionalMinima2D(argIn[0], argIn[1].asDoubleScalar());
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
