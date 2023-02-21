//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_setBuiltin.hpp"
#include "GOPropertyNames.hpp"
#include "GraphicsObject.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static inline void
SetChildren(GraphicsObject* fp, ArrayOf children);
//=============================================================================
ArrayOfVector
graphics_object_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3);
    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();
    GraphicsObject* fp;
    if (handle == HANDLE_ROOT_OBJECT) {
        fp = getGraphicsRootObject();
    } else if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
    } else {
        fp = (GraphicsObject*)findGOFigure(handle);
    }
    int ptr = 1;
    while (argIn.size() >= (ptr + 2)) {
        std::wstring propname = argIn[ptr].getContentAsWideString();
        if (propname == GO_CHILDREN_PROPERTY_NAME_STR) {
            SetChildren(fp, argIn[ptr + 1]);
        } else {
            try {
                GOGenericProperty* hp = fp->findProperty(propname);
                if (hp) {
                    if (!fp->isWritable(propname)) {
                        Error(_W("Property is readable only: ") + propname);
                    }
                    hp->set(argIn[ptr + 1]);
                }
            } catch (const Exception& e) {
                Error(_W("Got error for property:") + L" " + propname + L"\n" + e.what());
            }
        }
        ptr += 2;
    }
    fp->updateState();
    if (!fp->isType(L"figure") && !fp->isType(L"root")) {
        GOFigure* fig = fp->getParentFigure();
        fig->repaint();
    }
    return ArrayOfVector();
}
//=============================================================================void
void
SetChildren(GraphicsObject* fp, ArrayOf children)
{
    const int64* dp = (const int64*)children.getDataPointer();
    for (int i = 0; i < children.getElementCount(); i++) {
        validateGO(dp[i]);
    }
    GraphicsObject* gp;
    GOGObjectsProperty* hp = (GOGObjectsProperty*)fp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> my_children(hp->data());
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            gp = findGraphicsObject(handle);
            gp->dereference();
        }
    }
    for (int i = 0; i < children.getElementCount(); i++) {
        int64 handle = dp[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            gp = findGraphicsObject(handle);
            gp->reference();
        }
    }
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            gp = findGraphicsObject(handle);
            if (gp->referenceCount() <= 0) {
                freeGraphicsObject(handle);
                delete gp;
            }
        }
    }
    hp->set(children);
}
//=============================================================================void
}
//=============================================================================void
