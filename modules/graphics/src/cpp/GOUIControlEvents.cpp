//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIControl.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOUIPanel.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static GOUIPanel*
uiControlButtonGroup(GOUIControl* control);
//=============================================================================
bool
GOUIControl::isEnableInactive()
{
    return stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INACTIVE_STR);
}
//=============================================================================
// Model part of a user click. The widget backend already synced Value/String
// from the widget (UIControlSyncValueFromWidget in graphics_qt) before
// calling this: make the control current and queue its Callback.
void
GOUIControl::clicked()
{
    // An 'inactive' control looks normal but must not run its Action callback.
    if (isEnableInactive()) {
        return;
    }
    bool managedByGroup = onGroupButtonClicked();
    int64 thisHandle = findGraphicsObjectHandle(this);
    if (thisHandle != -1) {
        setCurrentObject(thisHandle);
    }
    // a button group reports selection through its SelectionChangedFcn only
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)findProperty(GO_CALLBACK_PROPERTY_NAME_STR);
    if (goCallback && !managedByGroup) {
        goCallback->pushEvent(this, L"ActionData", L"Action");
    }
}
//=============================================================================
bool
GOUIControl::onGroupButtonClicked()
{
    GOUIPanel* group = uiControlButtonGroup(this);
    if (group == nullptr) {
        return false;
    }
    GOGenericProperty* value = findProperty(GO_VALUE_PROPERTY_NAME_STR);
    if (findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR)
        != findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR)) {
        // the widget toggled the selected button off: it stays selected
        value->set(findProperty(GO_MAX_PROPERTY_NAME_STR)->get());
        value->setModified(true);
        updateState();
        return true;
    }
    group->notifyChildSelected(findGraphicsObjectHandle(this), true);
    return true;
}
//=============================================================================
void
GOUIControl::syncButtonGroupSelection()
{
    if (!hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
        return;
    }
    GOUIPanel* group = uiControlButtonGroup(this);
    if (group != nullptr
        && findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR)
            == findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR)) {
        group->notifyChildSelected(findGraphicsObjectHandle(this), false);
    }
}
//=============================================================================
static GOUIPanel*
uiControlButtonGroup(GOUIControl* control)
{
    if (!control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR)
        && !control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR)) {
        return nullptr;
    }
    auto* parent = dynamic_cast<GOGObjectsProperty*>(
        control->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (parent == nullptr || parent->data().empty() || parent->data()[0] < HANDLE_OFFSET_OBJECT) {
        return nullptr;
    }
    auto* group = dynamic_cast<GOUIPanel*>(findGraphicsObject(parent->data()[0], false));
    return group != nullptr && group->isButtonGroup() ? group : nullptr;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
