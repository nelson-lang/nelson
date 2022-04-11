//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphic_object_setBuiltin.hpp"
#include "GraphicObject.hpp"
#include "GraphicObjectSet.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    ArrayOf paramGo = argIn[0];
    ArrayOf paramName = argIn[1];
    auto* go = (GraphicObject*)paramGo.getContentAsGraphicObjectScalar();
    if (go != nullptr) {
        std::string propertyName = paramName.getContentAsCString();
        ArrayOf paramValue = argIn[2];
        graphicObjectSet(go, propertyName, paramValue);
    }
    return retval;
}
//=============================================================================
