//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "regionpropsBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RegionProps.hpp"
//=============================================================================
namespace Nelson::ImageProcessingGateway {
//=============================================================================
ArrayOfVector
regionpropsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << RegionProps(argIn);
    return retval;
}
//=============================================================================
} // namespace Nelson::ImageProcessingGateway
//=============================================================================
