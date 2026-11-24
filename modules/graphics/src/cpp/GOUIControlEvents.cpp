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
#include "GOUIControl.h"
#include "GOFigure.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOStringVectorProperty.hpp"
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
// from the widget (UIControlSyncValueFromWidget in the native widget backend)
// before calling this: make the control current and queue its Callback.
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
// Static text, disabled and hidden controls never take the keyboard focus.
static bool
isTabbableControl(GOUIControl* control)
{
    return !control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)
        && !control->stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
        && !control->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
// Flattens the focusable uicontrols under a container in creation order (the stored child
// order; the reported Children list is its reverse), entering each panel where it stands.
static void
collectTabControls(GraphicsObject* node, std::vector<GOUIControl*>& out)
{
    if (node == nullptr) {
        return;
    }
    auto* children = dynamic_cast<GOGObjectsProperty*>(
        node->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children == nullptr) {
        return;
    }
    for (int64 handle : children->data()) {
        if (handle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(handle)) {
            continue;
        }
        GraphicsObject* object = findGraphicsObject(handle, false);
        if (object == nullptr) {
            continue;
        }
        if (auto* control = dynamic_cast<GOUIControl*>(object)) {
            if (isTabbableControl(control)) {
                out.push_back(control);
            }
        } else {
            collectTabControls(object, out);
        }
    }
}
//=============================================================================
// Entry count of a listbox/popupmenu: its String is a GOStringVectorProperty, with the
// '|'-separated single-string form kept for a String set before the Style became a menu.
static int
uiControlItemCount(GOUIControl* control)
{
    auto* stringProp = dynamic_cast<GOStringVectorProperty*>(
        control->findProperty(GO_STRING_PROPERTY_NAME_STR, false));
    if (stringProp == nullptr) {
        return 0;
    }
    const wstringVector lines = stringProp->data();
    if (lines.size() == 1 && lines[0].find(L'|') != std::wstring::npos) {
        return 1 + static_cast<int>(std::count(lines[0].begin(), lines[0].end(), L'|'));
    }
    return static_cast<int>(lines.size());
}
//=============================================================================
bool
deliverUIControlKeyModel(GOUIControl* control, const std::wstring& key, GOUIControl** focused)
{
    if (focused != nullptr) {
        *focused = control;
    }
    if (control == nullptr) {
        return false;
    }
    const std::wstring style = control->findStringProperty(GO_STYLE_PROPERTY_NAME_STR);
    //-------------------------------------------------------------------------
    if (key == L"tab" || key == L"shift+tab") {
        std::vector<GOUIControl*> order;
        collectTabControls(control->getParentFigure(), order);
        if (order.empty()) {
            return true;
        }
        const size_t count = order.size();
        auto it = std::find(order.begin(), order.end(), control);
        size_t index = (key == L"tab") ? 0 : count - 1;
        if (it != order.end()) {
            const size_t current = static_cast<size_t>(it - order.begin());
            index = (key == L"tab") ? (current + 1) % count : (current + count - 1) % count;
        }
        if (focused != nullptr) {
            *focused = order[index];
        }
        return true;
    }
    //-------------------------------------------------------------------------
    // An 'inactive' control still receives the key but ignores it.
    if (control->isEnableInactive()) {
        return true;
    }
    //-------------------------------------------------------------------------
    if (key == L"space") {
        if (style == GO_PROPERTY_VALUE_CHECKBOX_STR || style == GO_PROPERTY_VALUE_RADIOBUTTON_STR
            || style == GO_PROPERTY_VALUE_TOGGLEBUTTON_STR) {
            const double value = control->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
            const double max = control->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR);
            const double min = control->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR);
            control->setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, (value == max) ? min : max);
        }
        control->clicked();
        return true;
    }
    //-------------------------------------------------------------------------
    if (key == L"up" || key == L"down" || key == L"home" || key == L"end") {
        if (style == GO_PROPERTY_VALUE_LISTBOX_STR || style == GO_PROPERTY_VALUE_POPUPMENU_STR) {
            int count = uiControlItemCount(control);
            if (count < 1) {
                count = 1;
            }
            const int value
                = static_cast<int>(control->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
            int next = value;
            if (key == L"down") {
                next = value + 1;
            } else if (key == L"up") {
                next = value - 1;
            } else if (key == L"home") {
                next = 1;
            } else {
                next = count;
            }
            next = std::max(1, std::min(count, next));
            if (next != value) {
                control->setScalarDoubleDefault(
                    GO_VALUE_PROPERTY_NAME_STR, static_cast<double>(next));
                control->clicked();
            }
        }
        return true;
    }
    //-------------------------------------------------------------------------
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
