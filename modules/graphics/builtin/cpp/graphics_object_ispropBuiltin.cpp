//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_ispropBuiltin.hpp"
#include "StringHelpers.hpp"
#include "GORoot.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::graphics_object_ispropBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
    std::wstring propname = argIn[1].getContentAsWideString();

    GraphicsObject* fp = nullptr;
    if (handle == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
    } else if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
    } else {
        fp = (GraphicsObject*)findGOFigure(handle);
    }
    if (!fp) {
        Error(_W("Invalid handle."));
    }
    std::vector<std::wstring> fieldnames = fp->getVisibleFieldnames();
    bool isValid = false;
    for (auto& name : fieldnames) {
        if (StringHelpers::iequals(propname, name)) {
            isValid = true;
            break;
        }
    }
    retval << ArrayOf::logicalConstructor(isValid);
    return retval;
}
//=============================================================================
