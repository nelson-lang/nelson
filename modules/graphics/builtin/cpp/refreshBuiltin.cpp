//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "refreshBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "RefreshFigure.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
refreshBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOfVector retval = {};
    int64 handle = NO_FIGURE;
    switch (argIn.size()) {
    case 0: {
        handle = getCurrentFigure();
        if (handle == NO_FIGURE) {
            handle = createNewFigure();
            return retval;
        }
    } break;
    case 1: {
        bool isScalarGraphicsObject = argIn[0].isGraphicsObject() && argIn[0].isScalar();
        if (!isScalarGraphicsObject) {
            Error(_("figure graphics object expected."));
            return retval;
        }
        handle = argIn[0].getContentAsGraphicsObjectScalar();
    } break;
    default: {
        handle = NO_FIGURE;
    } break;
    }
    if (handle == HANDLE_ROOT_OBJECT || handle >= HANDLE_OFFSET_OBJECT || handle == NO_FIGURE) {
        Error(_("figure graphics object expected."));
    }
    refreshFigure(handle);
    return retval;
}
//=============================================================================
}
//=============================================================================
