//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcbfBuiltin.hpp"
#include "GraphicCallback.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
gcbfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    int64 handle = getCurrentCallbackObject();
    GraphicsObject* fp = graphicsObjectFromHandle(handle, false);
    if (!fp) {
        retval << ArrayOf::empty();
        return retval;
    }
    if (fp->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        retval << graphicsObjectHandle(handle);
        return retval;
    }
    GOFigure* fig = fp->getParentFigure();
    if (!fig) {
        fig = getCurrentGOFigure();
    }
    if (!fig) {
        retval << ArrayOf::empty();
        return retval;
    }
    int64 figHandle = findGraphicsObjectHandle(fig);
    if (figHandle == -1) {
        retval << ArrayOf::empty();
    } else {
        retval << graphicsObjectHandle(figHandle);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
