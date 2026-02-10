//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOConstructorHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "GOFiguresManager.hpp"
#include "axesBuiltin.hpp"
#include "GOAxis.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOGroup.hpp"
#include "RefreshFigure.hpp"
#include "GOCallbackProperty.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
processProperties(GraphicsObject* fp, ArrayOfVector& t, bool& isAutoParent, bool& hasGroupAsParent);
static void
attachToCurrentAxis(GraphicsObject* fp, go_handle thisHandle);
static void
attachToGroupParent(GraphicsObject* fp, go_handle thisHandle);
static void
finalizeConstruction(GraphicsObject* fp);
//=============================================================================
go_handle
GOCommonConstructorHelper(GraphicsObject* fp, const ArrayOfVector& arg)
{
    go_handle thisHandle = assignGraphicsObject(fp);
    ArrayOfVector t(arg);
    bool isAutoParent, hasGroupAsParent;

    processProperties(fp, t, isAutoParent, hasGroupAsParent);

    if (isAutoParent) {
        if (!hasGroupAsParent) {
            attachToCurrentAxis(fp, thisHandle);
        } else {
            attachToGroupParent(fp, thisHandle);
        }
    }

    finalizeConstruction(fp);
    GOFigure* fig = getCurrentGOFigure();
    if (fig && !fig->isRenderingStateInvalid()) {
        fig->setRenderingStateInvalid(true);
    }
    return thisHandle;
}
//=============================================================================
void
processProperties(GraphicsObject* fp, ArrayOfVector& t, bool& isAutoParent, bool& hasGroupAsParent)
{
    isAutoParent = true;
    hasGroupAsParent = false;

    while (t.size() >= 2) {
        std::wstring propname(t[0].getContentAsWideString());
        ArrayOf propvalue(t[1]);

        if (propname == GO_AUTO_PARENT_PROPERTY_NAME_STR) {
            isAutoParent = (propvalue.getContentAsWideString() == GO_PROPERTY_VALUE_ON_STR);
        } else {
            try {
                if (propname == GO_PARENT_PROPERTY_NAME_STR) {
                    go_handle hparent = propvalue.getContentAsGraphicsObjectScalar();
                    GraphicsObject* hp = findGraphicsObject(hparent);
                    hasGroupAsParent = hp->isType(GO_PROPERTY_VALUE_HGGROUP_STR);
                }
                if (!fp->isWritable(propname) && fp->haveProperty(propname)) {
                    raiseError(L"Nelson:graphics:ERROR_PROPERTY_IS_READABLE_ONLY",
                        ERROR_PROPERTY_IS_READABLE_ONLY, propname);
                }
                fp->findProperty(propname)->set(propvalue);
            } catch (const Exception& e) {
                std::wstring msg
                    = ERROR_GOT_ERROR_FOR_PROPERTY + L" " + propname + L"\n" + e.what();
                Error(msg, L"Nelson:graphics:ERROR_GOT_ERROR_FOR_PROPERTY");
            }
        }
        t.pop_front();
        t.pop_front();
    }
}
//=============================================================================
void
attachToCurrentAxis(GraphicsObject* fp, go_handle thisHandle)
{
    GOFigure* fig = getCurrentGOFigure();
    int64 current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
    if (current == 0) {
        ArrayOfVector arg2;
        axesBuiltin(0, arg2);
        current = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
    }
    GOAxis* axis = static_cast<GOAxis*>(findGraphicsObject(current));
    GOGObjectsProperty* cp
        = static_cast<GOGObjectsProperty*>(axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));

    std::vector<int64> children(cp->data());
    children.push_back(thisHandle);
    cp->data(children);

    cp = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> parent { current };
    cp->data(parent);

    axis->updateState();
}
//=============================================================================
void
attachToGroupParent(GraphicsObject* fp, go_handle thisHandle)
{
    GOGObjectsProperty* hgGroupParent
        = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> groupParent(hgGroupParent->data());

    if (groupParent.size() == 1) {
        GOGroup* group = static_cast<GOGroup*>(findGraphicsObject(groupParent[0]));
        if (group) {
            GOGObjectsProperty* cp = static_cast<GOGObjectsProperty*>(
                group->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
            std::vector<int64> children(cp->data());
            children.push_back(thisHandle);
            cp->data(children);
        } else {
            raiseError(L"Nelson:graphics:ERROR_HGGROUP_EXPECTED", ERROR_HGGROUP_EXPECTED);
        }
    } else {
        raiseError(L"Nelson:graphics:ERROR_PARENT_SHOULD_HAVE_ONLY_ONE_GRAPHICS_OBJECT",
            ERROR_PARENT_SHOULD_HAVE_ONLY_ONE_GRAPHICS_OBJECT);
    }
}
//=============================================================================
void
finalizeConstruction(GraphicsObject* fp)
{
    fp->updateState();

    if (!fp->isType(GO_PROPERTY_VALUE_FIGURE_STR) && !fp->isType(GO_PROPERTY_VALUE_ROOT_STR)) {
        GOFigure* fig = fp->getParentFigure();
        refreshFigure(fig);
    }

    GOCallbackProperty* goCallback
        = static_cast<GOCallbackProperty*>(fp->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR));
    if (goCallback) {
        goCallback->executeNow(fp);
    }
}
//=============================================================================
}
//=============================================================================
