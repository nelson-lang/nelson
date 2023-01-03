//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_deleteBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "GOFiguresManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphics_object_deleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* fp = nullptr;
    if (handle == HANDLE_ROOT_OBJECT) {
        Error(_W("Cannot delete root graphics object."));
    } else if (handle >= HANDLE_OFFSET_OBJECT) {
        deleteGraphicsObject(handle, true);
    } else {
        closeFigure(handle);
    }
    ArrayOfVector retval = {};
    return retval;
}
//=============================================================================
