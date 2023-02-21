//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "drawnowBuiltin.hpp"
#include "DrawNow.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
drawnowBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval = {};
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 0);

    drawNow();

    return retval;
}
//=============================================================================
}
//=============================================================================
