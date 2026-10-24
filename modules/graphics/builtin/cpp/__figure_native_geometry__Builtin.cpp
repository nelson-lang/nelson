//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__figure_native_geometry__Builtin.hpp"
#include "GOFigureHost.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __figure_native_geometry__(f): test hook returning where the drawing area of
// figure f really is on screen, [left bottom width height] in pixels from the
// bottom-left corner of the primary screen, as the native window reports it
// (not the Position bookkeeping). Returns [] when the figure has no visible
// native window (headless, hidden or minimized).
ArrayOfVector
Nelson::GraphicsGateway::__figure_native_geometry__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isGraphicsObject() || !argIn[0].isScalar()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObject",
            L"Expected graphics object.");
    }
    auto* gobject = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    if (isDeletedGraphicsObject(gobject[0]) || gobject[0] == HANDLE_ROOT_OBJECT
        || gobject[0] >= HANDLE_OFFSET_OBJECT) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }
    GOFigureHost* host = getFigure(gobject[0]);
    double x = 0;
    double y = 0;
    double width = 0;
    double height = 0;
    ArrayOfVector retval;
    if (host == nullptr || !host->hostDrawingAreaScreenRect(x, y, width, height)) {
        retval << ArrayOf::empty();
        return retval;
    }
    double* values = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, 4));
    values[0] = x;
    values[1] = y;
    values[2] = width;
    values[3] = height;
    retval << ArrayOf(NLS_DOUBLE, Dimensions(1, 4), values);
    return retval;
}
//=============================================================================
