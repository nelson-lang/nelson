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
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORoot.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isTiledLayoutSubplotTextFontSizeMode(GraphicsObject* fp, const std::wstring& propname)
{
    if (!fp || !fp->isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)
        || !StringHelpers::iequals(propname, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
        return false;
    }
    int64 parentHandle = fp->findGoProperty(GO_PARENT_PROPERTY_NAME_STR);
    GraphicsObject* parent = findGraphicsObject(parentHandle, false);
    return parent && parent->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)
        && fp->findProperty(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR, false);
}
//=============================================================================
static bool
hasProperty(int64 handle, const std::wstring& propname)
{
    GraphicsObject* fp = nullptr;
    if (handle == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
    } else if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle, false);
    } else {
        fp = (GraphicsObject*)findGOFigure(handle);
    }
    if (!fp) {
        return false;
    }
    if (isTiledLayoutSubplotTextFontSizeMode(fp, propname)) {
        return true;
    }
    std::vector<std::wstring> fieldnames = fp->getVisibleFieldnames();
    for (auto& name : fieldnames) {
        if (StringHelpers::iequals(propname, name)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOfVector
GraphicsGateway::graphics_object_ispropBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    std::wstring propname = argIn[1].asWideString();

    // isprop works element-wise and returns a logical array of the same size.
    // An empty array gives a scalar false, so that
    // isprop(obj,"Sub") && isprop(get(obj,"Sub"),"Prop") works when Sub is unset.
    Dimensions dims = argIn[0].dimensions();
    indexType nbElements = dims.elementCount();
    if (nbElements == 0) {
        retval << ArrayOf::logicalScalar(false);
        return retval;
    }
    logical* ptrResult = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, nbElements, stringVector(), false));
    const nelson_handle* handles = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    for (indexType k = 0; k < nbElements; ++k) {
        ptrResult[k] = hasProperty((int64)handles[k], propname) ? 1 : 0;
    }
    retval << ArrayOf(NLS_LOGICAL, dims, ptrResult);
    return retval;
}
//=============================================================================
