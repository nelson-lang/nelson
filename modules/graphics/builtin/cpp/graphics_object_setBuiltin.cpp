//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "graphics_object_setBuiltin.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GraphicsObject.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOFiguresManager.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static inline void
setChildren(GraphicsObject* fp, ArrayOf childrenToAdd);
//=============================================================================
static inline void
setParent(GraphicsObject* fp, ArrayOf newParent);
//=============================================================================
ArrayOfVector
graphics_object_setBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3);

    if (!argIn[0].isGraphicsObject()) {
        raiseError(
            L"Nelson:graphics:ERROR_EXPECTED_GRAPHICS_OBJECT_S", ERROR_EXPECTED_GRAPHICS_OBJECT_S);
    }
    nelson_handle* gobjects = (nelson_handle*)(argIn[0].getDataPointer());
    indexType nbElements = argIn[0].getElementCount();
    for (indexType k = 0; k < nbElements; k++) {
        GraphicsObject* fp = nullptr;
        if (gobjects[k] == HANDLE_ROOT_OBJECT) {
            fp = getGraphicsRootObject();
        } else if (gobjects[k] >= HANDLE_OFFSET_OBJECT) {
            fp = findGraphicsObject(gobjects[k]);
        } else {
            fp = static_cast<GraphicsObject*>(findGOFigure(gobjects[k]));
        }
        if (!fp) {
            raiseError2(_E("nelson:validators:invalidNelsonHandle"));
        }
        int ptr = 1;
        while (argIn.size() >= (ptr + 2)) {
            std::wstring propname = argIn[ptr].getContentAsWideString();
            if (propname == GO_CHILDREN_PROPERTY_NAME_STR) {
                setChildren(fp, argIn[ptr + 1]);
            } else if (propname == GO_PARENT_PROPERTY_NAME_STR) {
                setParent(fp, argIn[ptr + 1]);
            } else {
                try {
                    GOGenericProperty* hp = fp->findProperty(propname);
                    if (hp) {
                        if (!fp->isWritable(propname) && fp->haveProperty(propname)) {
                            raiseError(L"Nelson:graphics:ERROR_PROPERTY_IS_READABLE_ONLY",
                                ERROR_PROPERTY_IS_READABLE_ONLY, propname);
                        }
                        hp->set(argIn[ptr + 1]);
                    }
                } catch (const Exception& e) {
                    std::wstring msg
                        = ERROR_GOT_ERROR_FOR_PROPERTY + L" " + propname + L"\n" + e.what();
                    Error(msg, L"Nelson:graphics:ERROR_GOT_ERROR_FOR_PROPERTY");
                }
            }
            ptr += 2;
        }
        fp->updateState();
        if (!fp->isType(GO_PROPERTY_VALUE_FIGURE_STR) && !fp->isType(GO_PROPERTY_VALUE_ROOT_STR)
            && !fp->isType(GO_PROPERTY_VALUE_UICONTROL_STR)) {
            GOFigure* fig = fp->getParentFigure();
            if (fig) {
                fig->setRenderingStateInvalid(true);
                fig->repaint();
            }
        }
    }

    return ArrayOfVector();
}
//=============================================================================
void
setParent(GraphicsObject* fp, ArrayOf newParent)
{

    go_handle goNewParent = newParent.getContentAsGraphicsObjectScalar();
    validateGO(goNewParent);

    GOGenericProperty* hParent = fp->findProperty(GO_PARENT_PROPERTY_NAME_STR, false);
    if (!hParent) {
        return;
    }
    hParent->set(newParent);
}
//=============================================================================
void
setChildren(GraphicsObject* fp, ArrayOf childrenToAdd)
{
    const int64* dp = static_cast<const int64*>(childrenToAdd.getDataPointer());
    for (int i = 0; i < childrenToAdd.getElementCount(); i++) {
        validateGO(dp[i]);
    }
    GraphicsObject* gp = nullptr;
    GOGObjectsProperty* hp
        = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> my_children(hp->data());
    for (int i = 0; i < my_children.size(); i++) {
        int64 handle = my_children[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* gp = findGraphicsObject(handle);
            gp->dereference();
        }
    }
    for (int i = 0; i < childrenToAdd.getElementCount(); i++) {
        int64 handle = dp[i];
        if (handle >= HANDLE_OFFSET_OBJECT) {
            GraphicsObject* gp = findGraphicsObject(handle);
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
    hp->set(childrenToAdd);
}
//=============================================================================void
}
//=============================================================================void
