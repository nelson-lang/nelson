//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__getframe__Builtin.hpp"
#include "GOWindow.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GetFrame.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__getframe__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);

    if (!argIn[0].isGraphicsObject()) {
        Error(_W("Expected graphics object."));
    }
    nelson_handle* gobject = (nelson_handle*)(argIn[0].getDataPointer());
    if (isDeletedGraphicsObject(gobject[0])) {
        Error(_W("Valid graphics object expected."));
    }

    if (gobject[0] == HANDLE_ROOT_OBJECT || gobject[0] >= HANDLE_OFFSET_OBJECT) {
        Error("figure object expected.");
    }
    GOWindow* goWindow = getFigure(gobject[0]);
    ArrayOfVector retval;
    retval << GetFrame(goWindow);
    return retval;
}
//=============================================================================
}
//=============================================================================
