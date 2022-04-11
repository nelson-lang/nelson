//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphic_object_getBuiltin.hpp"
#include "GraphicObject.hpp"
#include "GraphicObjectGet.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphic_object_getBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(1);
    ArrayOf param = argIn[0];
    ArrayOf paramName = argIn[1];
    auto* go = (GraphicObject*)param.getContentAsGraphicObjectScalar();
    if (go != nullptr) {
        std::string propertyName = paramName.getContentAsCString();
        retval << graphicObjectGet(go, propertyName);
    }
    return retval;
}
//=============================================================================
