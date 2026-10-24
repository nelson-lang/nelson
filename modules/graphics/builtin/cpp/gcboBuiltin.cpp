//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcboBuiltin.hpp"
#include "GraphicCallback.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static ArrayOf
callbackFigure(int64 handle)
{
    GraphicsObject* fp = graphicsObjectFromHandle(handle, false);
    if (!fp) {
        return ArrayOf::empty();
    }
    if (fp->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        return graphicsObjectHandle(handle);
    }
    GOFigure* fig = fp->getParentFigure();
    if (!fig) {
        fig = getCurrentGOFigure();
    }
    if (!fig) {
        return ArrayOf::empty();
    }
    int64 figHandle = findGraphicsObjectHandle(fig);
    if (figHandle == -1) {
        return ArrayOf::empty();
    }
    return graphicsObjectHandle(figHandle);
}
//=============================================================================
ArrayOfVector
gcboBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval;
    int64 handle = getCurrentCallbackObject();
    if (handle == HANDLE_UNASSIGNED_OBJECT || isDeletedGraphicsObject(handle)) {
        retval << ArrayOf::empty();
        if (nLhs > 1) {
            retval << ArrayOf::empty();
        }
        return retval;
    }
    retval << graphicsObjectHandle(handle);
    if (nLhs > 1) {
        retval << callbackFigure(handle);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
