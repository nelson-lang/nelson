//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcaBuiltin.hpp"
#include "GOFiguresManager.hpp"
#include "GOPropertyNames.hpp"
#include "axesBuiltin.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
gcaBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    if (getCurrentFigure() == -1) {
        createNewFigure();
    }
    GOFigure* fig = getCurrentGOFigure();
    unsigned current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
    if (current == 0 || isDeletedGraphicsObject(current)) {
        ArrayOfVector arg2;
        axesBuiltin(0, arg2);
        current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
        fig->setRenderingStateInvalid(true);
    }
    retval << ArrayOf::graphicsObjectConstructor(current);
    return retval;
}
//=============================================================================
}
//=============================================================================
