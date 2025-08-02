//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "grootBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GORoot.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
grootBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval = {};
    retval << ArrayOf::graphicsObjectConstructor(graphicsRootObject());
    return retval;
}
//=============================================================================
}; // namespace Nelson
//=============================================================================
