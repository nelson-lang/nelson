//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcfBuiltin.hpp"
#include "GOFiguresManager.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
gcfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    int64 currentFigureID = getCurrentFigure();
    if (currentFigureID == -1) {
        currentFigureID = createNewFigure();
    }
    retval << ArrayOf::graphicsObjectConstructor(currentFigureID);
    return retval;
}
//=============================================================================
}
//=============================================================================
