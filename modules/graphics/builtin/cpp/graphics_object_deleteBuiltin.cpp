//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_deleteBuiltin.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "GOFiguresManager.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphics_object_deleteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }
    const auto* handles = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    indexType nbElements = argIn[0].elementCount();
    for (indexType k = 0; k < nbElements; k++) {
        int64 handle = handles[k];
        if (handle == HANDLE_ROOT_OBJECT) {
            Error(L"Nelson:delete:cannotDeleteRoot", L"Cannot delete root graphics object.");
        } else if (handle >= HANDLE_OFFSET_OBJECT) {
            if (!isDeletedGraphicsObject(handle)) {
                deleteGraphicsObject(handle, true, true);
            }
        } else {
            deleteFigure(handle);
        }
    }
    ArrayOfVector retval = {};
    return retval;
}
//=============================================================================
