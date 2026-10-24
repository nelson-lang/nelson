//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cwctype>
#include "GOConstructorHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "GOFiguresManager.hpp"
#include "axesBuiltin.hpp"
#include "GOAxis.hpp"
#include "GOUIComponent.h"
#include "GOUIControl.h"
#include "GOGObjectsProperty.hpp"
#include "GOCallbackProperty.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static bool
isParentPropertyName(const std::wstring& propname);
static void
processProperties(
    GraphicsObject* fp, ArrayOfVector& t, bool& isAutoParent, bool& hasExplicitParent);
static void
attachToCurrentAxis(GraphicsObject* fp, go_handle thisHandle);
static void
attachToExplicitParent(GraphicsObject* fp, go_handle thisHandle);
static void
finalizeConstruction(GraphicsObject* fp);
//=============================================================================
go_handle
GOCommonConstructorHelper(GraphicsObject* fp, const ArrayOfVector& arg)
{
    go_handle thisHandle = assignGraphicsObject(fp);
    ArrayOfVector t(arg);
    bool isAutoParent, hasExplicitParent;

    processProperties(fp, t, isAutoParent, hasExplicitParent);

    if (isAutoParent) {
        // The current axes is only the fallback when no parent is named.
        if (hasExplicitParent) {
            attachToExplicitParent(fp, thisHandle);
        } else {
            attachToCurrentAxis(fp, thisHandle);
        }
    }

    finalizeConstruction(fp);
    // CurrentObject reports what the user clicked, so building an object must not change it.
    GOFigure* fig = getCurrentGOFigure();
    if (fig && !fig->isRenderingStateInvalid()) {
        fig->setRenderingStateInvalid(true);
    }
    return thisHandle;
}
//=============================================================================
bool
isParentPropertyName(const std::wstring& propname)
{
    // findProperty() ignores case, so a lowercase 'parent' counts as explicit too. Compared
    // in place because this runs for every property of every object built.
    const wchar_t* canonical = GO_PARENT_PROPERTY_NAME_STR;
    size_t k = 0;
    for (; k < propname.size(); ++k) {
        if (canonical[k] == L'\0' || towlower(propname[k]) != towlower(canonical[k])) {
            return false;
        }
    }
    return canonical[k] == L'\0';
}
//=============================================================================
void
processProperties(GraphicsObject* fp, ArrayOfVector& t, bool& isAutoParent, bool& hasExplicitParent)
{
    isAutoParent = true;
    hasExplicitParent = false;

    while (t.size() >= 2) {
        std::wstring propname(t[0].asWideString());
        ArrayOf propvalue(t[1]);

        // Accepts the shortest unambiguous leading substring of a property name, e.g.
        // patch('FaceC','r').
        propname = fp->resolvePartialPropertyName(propname);

        if (propname == GO_AUTO_PARENT_PROPERTY_NAME_STR) {
            isAutoParent = (propvalue.asWideString() == GO_PROPERTY_VALUE_ON_STR);
        } else {
            try {
                if (isParentPropertyName(propname)) {
                    // Resolved here so an unusable parent is reported on 'Parent' before the
                    // value is stored. A figure and the root do not resolve; a panel, tab or
                    // widget resolves but does not hold a graphics object of its own.
                    GraphicsObject* declared
                        = findGraphicsObject(propvalue.asGraphicsObjectScalar());
                    if (dynamic_cast<GOUIComponent*>(declared)
                        || dynamic_cast<GOUIControl*>(declared)) {
                        Error(L"Nelson:graphics:parentIsNotAxesOrGroup",
                            L"Parent must be an axes or a group.");
                    }
                    hasExplicitParent = true;
                }
                if (!fp->isWritable(propname) && fp->haveProperty(propname)) {
                    Error(L"Nelson:graphics:readOnlyProperty", L"Property is read-only: %s",
                        propname);
                }
                fp->findProperty(propname)->set(propvalue);
            } catch (const NelsonException& e) {
                RaiseError(formatI18nMessage(
                               Nelson::resolveMessageTemplate(L"Nelson:graphics:gotErrorProperty",
                                   L"Got error for property: %s"),
                               propname)
                        + L"\n" + e.what(),
                    L"Nelson:graphics:propertyError");
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

    // O(1) append. Copying the children vector on every attach made creation O(N^2).
    cp->append(thisHandle);

    cp = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> parent { current };
    cp->data(parent);

    // The full axes recompute rescans every sibling, so it is deferred to the next draw or
    // read. finalizeConstruction() schedules the repaint.
    axis->markStateDirty();
}
//=============================================================================
void
attachToExplicitParent(GraphicsObject* fp, go_handle thisHandle)
{
    GOGObjectsProperty* parentProperty
        = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> declaredParent(parentProperty->data());

    if (declaredParent.size() != 1) {
        Error(
            L"Nelson:graphics:parentSingleChild", L"Parent should have only one graphics object.");
    }
    GraphicsObject* parent = findGraphicsObject(declaredParent[0]);
    GOGObjectsProperty* cp
        = static_cast<GOGObjectsProperty*>(parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    cp->append(thisHandle);

    // A group leaves the axes refresh to finalizeConstruction, which reaches the axes the
    // group lives in. The refresh is deferred so a creation loop stays O(N).
    if (GOAxis* axis = dynamic_cast<GOAxis*>(parent)) {
        axis->markStateDirty();
    }
}
//=============================================================================
void
finalizeConstruction(GraphicsObject* fp)
{
    GOAxis* parentAxis = fp->getParentAxis();
    if (parentAxis && !fp->isType(GO_PROPERTY_VALUE_AXES_STR)
        && !fp->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR)) {
        parentAxis->invalidateViewState();
    }
    fp->updateState();
    if (parentAxis) {
        // Deferred: the recompute runs once at the next draw or read, not per created object.
        parentAxis->markStateDirty();
    }

    if (!fp->isType(GO_PROPERTY_VALUE_FIGURE_STR) && !fp->isType(GO_PROPERTY_VALUE_ROOT_STR)) {
        GOFigure* fig = fp->getParentFigure();
        if (fig) {
            fig->setRenderingStateInvalid(true);
            if (!fig->isDrawLater()) {
                fig->repaint();
            }
        }
    }

    GOCallbackProperty* goCallback
        = static_cast<GOCallbackProperty*>(fp->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR));
    if (goCallback) {
        goCallback->executeNow(fp);
    }
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
