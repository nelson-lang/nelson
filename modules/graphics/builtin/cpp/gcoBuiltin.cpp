//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcoBuiltin.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
gcoBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    int64 currentFigure = getCurrentFigure();
    if (currentFigure == -1) {
        retval << ArrayOf::empty();
        return retval;
    }
    GOFigure* fig = findGOFigure(currentFigure);
    if (!fig) {
        retval << ArrayOf::empty();
        return retval;
    }
    int64 currentObject = fig->findGoProperty(GO_CURRENT_OBJECT_PROPERTY_NAME_STR);
    if (currentObject == 0 || isDeletedGraphicsObject(currentObject)
        || !isVisibleGraphicsHandle(currentObject)) {
        retval << ArrayOf::empty();
        return retval;
    }
    retval << graphicsObjectHandle(currentObject);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
