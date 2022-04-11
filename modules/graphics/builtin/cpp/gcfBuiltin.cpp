//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "NewFigure.hpp"
#include "gcfBuiltin.hpp"
#include "Error.hpp"
#include "GOWindowManager.hpp"
#include "GOFigure.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::gcfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOf res;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    uint64 currentWindowsID = getCurrentGOWindowID();
    if (currentWindowsID == 0) {
        GOFigure* goPtr = Nelson::newFigure();
        res = ArrayOf::graphicObjectConstructor(goPtr);
    } else {
        GOFigure* goPtr = Nelson::newFigure(currentWindowsID);
        res = ArrayOf::graphicObjectConstructor(goPtr);
    }
    ArrayOfVector retval(1);
    retval << res;
    return retval;
}
//=============================================================================
