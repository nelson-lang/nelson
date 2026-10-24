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
#include <cmath>
#include <limits>
#include "GOMenu.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* HANDLE_VISIBILITY_DICT[4]
    = { GO_PROPERTY_VALUE_ON_STR, L"callback", GO_PROPERTY_VALUE_OFF_STR, 0 };
//=============================================================================
static void
rebuildParentFigureMenus(GraphicsObject* object)
{
    GOFigure* figure = object->getParentFigure();
    if (!figure) {
        return;
    }
    go_handle figureHandle = findGraphicsObjectHandle(figure);
    GOFigureHost* window = findGOWindows(figureHandle);
    if (window) {
        window->rebuildHostMenus();
    }
}
//=============================================================================
GOMenu::GOMenu()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOMenu::getType()
{
    return GO_PROPERTY_VALUE_UIMENU_STR;
}
//=============================================================================
void
GOMenu::constructProperties()
{
    registerProperty(new GOCallbackProperty, GO_MENU_SELECTED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TOOLTIP_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ENABLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SEPARATOR_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_ACCELERATOR_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_CHECKED_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TEXT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(1.0, (double)std::numeric_limits<int>::max()),
        GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(HANDLE_VISIBILITY_DICT),
        GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOMenu::setupDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_TOOLTIP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(
        GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, 33.0 / 255.0, 33.0 / 255.0, 33.0 / 255.0);
    setRestrictedStringDefault(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_SEPARATOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_ACCELERATOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_CHECKED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TEXT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setScalarDoubleDefault(GO_POSITION_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    clearAllChanged();
}
//=============================================================================
void
GOMenu::paintMe(RenderInterface& gc)
{
}
//=============================================================================
static double
menuPosition(int64 handle)
{
    GraphicsObject* object = graphicsObjectFromHandle(handle, false);
    if (!object || !object->isType(GO_PROPERTY_VALUE_UIMENU_STR)) {
        return std::numeric_limits<double>::infinity();
    }
    return object->findScalarDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOMenu::reorderParentChildren()
{
    GOGObjectsProperty* parentProperty
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parentProperty || parentProperty->data().empty()) {
        return;
    }
    GraphicsObject* parent = graphicsObjectFromHandle(parentProperty->data()[0], false);
    if (!parent) {
        return;
    }
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    std::vector<int64> values(children->data());
    std::stable_sort(values.begin(), values.end(),
        [](int64 a, int64 b) { return menuPosition(a) > menuPosition(b); });
    children->data(values);
}
//=============================================================================
void
GOMenu::updateState()
{
    bool mustRebuild = hasChanged({ GO_MENU_SELECTED_FCN_PROPERTY_NAME_STR,
        GO_TOOLTIP_PROPERTY_NAME_STR, GO_VISIBLE_PROPERTY_NAME_STR,
        GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, GO_ENABLE_PROPERTY_NAME_STR,
        GO_SEPARATOR_PROPERTY_NAME_STR, GO_ACCELERATOR_PROPERTY_NAME_STR,
        GO_CHECKED_PROPERTY_NAME_STR, GO_TEXT_PROPERTY_NAME_STR, GO_POSITION_PROPERTY_NAME_STR,
        GO_CHILDREN_PROPERTY_NAME_STR, GO_PARENT_PROPERTY_NAME_STR });
    if (hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        reorderParentChildren();
    }
    if (mustRebuild) {
        rebuildParentFigureMenus(this);
        clearChanged({ GO_MENU_SELECTED_FCN_PROPERTY_NAME_STR, GO_TOOLTIP_PROPERTY_NAME_STR,
            GO_VISIBLE_PROPERTY_NAME_STR, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR,
            GO_ENABLE_PROPERTY_NAME_STR, GO_SEPARATOR_PROPERTY_NAME_STR,
            GO_ACCELERATOR_PROPERTY_NAME_STR, GO_CHECKED_PROPERTY_NAME_STR,
            GO_TEXT_PROPERTY_NAME_STR, GO_POSITION_PROPERTY_NAME_STR, GO_CHILDREN_PROPERTY_NAME_STR,
            GO_PARENT_PROPERTY_NAME_STR });
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
