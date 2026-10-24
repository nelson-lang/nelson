//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UIControlBuilder.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
#include "GOUIControl.h"
#include "GOUIContainer.h"
#include "GOUIPanel.h"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOCallbackProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GOFigure*
figureFromHandle(go_handle go)
{
    return go < HANDLE_OFFSET_OBJECT ? findGOFigure(go) : nullptr;
}
//=============================================================================
// A uicontrol can also be held by a uipanel, a uibuttongroup or a uitab.
static GOUIContainer*
containerFromHandle(go_handle go)
{
    auto* container = dynamic_cast<GOUIContainer*>(
        go >= HANDLE_OFFSET_OBJECT ? findGraphicsObject(go, false) : nullptr);
    if (container == nullptr) {
        return nullptr;
    }
    const std::wstring type = container->getType();
    bool accepted = type == GO_PROPERTY_VALUE_UIPANEL_STR
        || type == GO_PROPERTY_VALUE_UIBUTTONGROUP_STR || type == GO_PROPERTY_VALUE_UITAB_STR;
    return accepted ? container : nullptr;
}
//=============================================================================
void
checkUIControlParent(const ArrayOf& newParent)
{
    go_handle go = newParent.asGraphicsObjectScalar();
    if (!figureFromHandle(go) && !containerFromHandle(go)) {
        Error(L"Nelson:uicontrol:expectedFigure",
            L"Figure, uipanel, uibuttongroup or uitab expected.");
    }
}
//=============================================================================
go_handle
UIControlBuilder(const ArrayOfVector& argIn)
{
    ArrayOfVector copyArgIn(argIn);
    int64 currentFigureID = -1;
    GOUIContainer* container = nullptr;
    if (copyArgIn.size() > 0) {
        if (copyArgIn[0].isGraphicsObject()) {
            go_handle go = copyArgIn[0].asGraphicsObjectScalar();
            GOFigure* fig = figureFromHandle(go);
            container = containerFromHandle(go);
            if (fig) {
                currentFigureID = go;
                copyArgIn.pop_front();
            } else if (container) {
                copyArgIn.pop_front();
            } else {
                GraphicsObject* hp = findGraphicsObject(go);
                if (hp->isType(GO_PROPERTY_VALUE_UICONTROL_STR)) {
                    if (copyArgIn.size() == 1) {
                        // uicontrol(h): gives the keyboard focus to h
                        GOUIControl* goUIControl = (GOUIControl*)(hp);
                        goUIControl->setFocus();
                        return go;
                    } else {
                        Error(L"Nelson:uicontrol:expectedUicontrol",
                            L"uicontrol graphic object expected.");
                    }
                } else {
                    Error(L"Nelson:uicontrol:invalidParent",
                        L"Figure, uipanel, uibuttongroup, uitab or uicontrol graphic object "
                        L"expected.");
                }
            }
        }
    }
    for (size_t k = 0; k < copyArgIn.size(); k = k + 2) {
        std::wstring propname(copyArgIn[k].asWideString());
        if (propname == GO_PARENT_PROPERTY_NAME_STR && (k + 1 < copyArgIn.size())) {
            ArrayOf propvalue(copyArgIn[k + 1]);
            checkUIControlParent(propvalue);
            go_handle go = propvalue.asGraphicsObjectScalar();
            GOFigure* fig = figureFromHandle(go);
            container = fig ? nullptr : containerFromHandle(go);
            if (fig) {
                currentFigureID = go;
            }
        }
    }

    if (currentFigureID == -1 && container == nullptr) {
        currentFigureID = getCurrentFigure();
    }
    if (currentFigureID == -1 && container == nullptr) {
        currentFigureID = createNewFigure();
    }
    GOFigureHost* host
        = container ? container->getParentFigureHost() : findGOWindows(currentFigureID);
    if (host == nullptr) {
        Error(L"Nelson:uicontrol:parentNotAttachedToFigure",
            L"Invalid parent: container is not attached to a figure.");
    }

    GOUIControl* go = new GOUIControl;

    go_handle thisHandle = assignGraphicsObject(go);
    go->buildWidget(host);

    GOGObjectsProperty* cp
        = static_cast<GOGObjectsProperty*>(go->findProperty(GO_PARENT_PROPERTY_NAME_STR));
    std::vector<int64> parent;
    parent.push_back(container ? findGraphicsObjectHandle(container) : currentFigureID);
    cp->data(parent);

    GraphicsObject* parentObject
        = container ? static_cast<GraphicsObject*>(container) : host->hostFigure();
    GOGObjectsProperty* hp
        = (GOGObjectsProperty*)parentObject->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(hp->data());
    children.push_back(thisHandle);
    hp->data(children);

    GraphicsObject* fp = (GraphicsObject*)go;
    while (copyArgIn.size() >= 2) {
        std::wstring propname(copyArgIn[0].asWideString());
        ArrayOf propvalue(copyArgIn[1]);
        try {
            GOGenericProperty* goproperty = fp->findProperty(propname);
            if (!fp->isWritable(propname)) {
                Error(L"Nelson:uicontrol:readOnlyProperty", L"Property is read-only: %s", propname);
            }
            goproperty->set(propvalue);
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propname)
                    + L"\n" + e.what(),
                L"Nelson:uicontrol:propertyError");
        }
        copyArgIn.pop_front();
        copyArgIn.pop_front();
    }
    auto* buttonGroup = dynamic_cast<GOUIPanel*>(container);
    if (buttonGroup != nullptr
        && (go->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)
            || go->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR))) {
        buttonGroup->addGroupButton(thisHandle,
            go->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR)
                == go->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR),
            go->hasChanged(GO_VALUE_PROPERTY_NAME_STR));
    }
    fp->updateState();

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)fp->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        goCallback->executeNow(fp);
    }

    return thisHandle;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
