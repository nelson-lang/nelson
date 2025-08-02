//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_getBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphics_object_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    ArrayOf paramGo1 = argIn[0];
    if (paramGo1.getDataClass() != NLS_GO_HANDLE) {
        Error(_W("graphics_object expected."));
    }
    Dimensions dims1 = paramGo1.getDimensions();
    auto* ptrGO1 = (nelson_handle*)paramGo1.getDataPointer();
    std::wstring propname = argIn[1].getContentAsWideString();
    ArrayOfVector retval;
    for (size_t k = 0; k < dims1.getElementCount(); k++) {
        auto handle = ptrGO1[k];
        GraphicsObject* fp = nullptr;
        if (handle == HANDLE_ROOT_OBJECT) {
            fp = getGraphicsRootObject();
            fp->updateState();
        } else if (handle >= HANDLE_OFFSET_OBJECT) {
            fp = findGraphicsObject(handle);
        } else {
            fp = (GraphicsObject*)findGOFigure(handle);
        }
        if (!fp) {
            Error(_W("Invalid handle."));
        }
        retval << fp->findProperty(propname)->get();
    }
    return retval;
}
//=============================================================================
