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
#include "UIControlTabOrder.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUIContainer.h"
#include "GOUIControl.h"
#include "GOUITabGroup.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static GraphicsObject*
figureOf(GraphicsObject* object);
static std::vector<int64>
handlesOf(GraphicsObject* object, const std::wstring& name);
static bool
isFocusable(GOUIControl* control);
static void
collectTabOrder(GraphicsObject* parent, bool focusableOnly, std::vector<GOUIControl*>& order);
//=============================================================================
std::vector<GOUIControl*>
uiControlTabOrder(GOUIControl* control, bool focusableOnly)
{
    std::vector<GOUIControl*> order;
    GraphicsObject* figure = figureOf(control);
    if (figure != nullptr) {
        collectTabOrder(figure, focusableOnly, order);
    }
    return order;
}
//=============================================================================
GOUIControl*
nextUIControlInTabOrder(GOUIControl* control, bool forward)
{
    std::vector<GOUIControl*> order = uiControlTabOrder(control, true);
    if (order.empty()) {
        return nullptr;
    }
    auto it = std::find(order.begin(), order.end(), control);
    if (it == order.end()) {
        return forward ? order.front() : order.back();
    }
    size_t index = static_cast<size_t>(it - order.begin());
    size_t count = order.size();
    GOUIControl* next = order[forward ? (index + 1) % count : (index + count - 1) % count];
    return next == control ? nullptr : next;
}
//=============================================================================
static GraphicsObject*
figureOf(GraphicsObject* object)
{
    while (object != nullptr) {
        std::vector<int64> parent = handlesOf(object, GO_PARENT_PROPERTY_NAME_STR);
        if (parent.empty() || parent[0] == HANDLE_ROOT_OBJECT) {
            return nullptr;
        }
        if (parent[0] < HANDLE_OFFSET_OBJECT) {
            return findGOFigure(parent[0]);
        }
        object = findGraphicsObject(parent[0], false);
    }
    return nullptr;
}
//=============================================================================
static std::vector<int64>
handlesOf(GraphicsObject* object, const std::wstring& name)
{
    auto* property = dynamic_cast<GOGObjectsProperty*>(object->findProperty(name, false));
    return property != nullptr ? property->data() : std::vector<int64>();
}
//=============================================================================
static bool
isFocusable(GOUIControl* control)
{
    return !control->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR)
        && control->stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
static void
collectTabOrder(GraphicsObject* parent, bool focusableOnly, std::vector<GOUIControl*>& order)
{
    // stored in creation order (the Children property shows it reversed, most
    // recent first): that is the Tab order
    for (int64 childHandle : handlesOf(parent, GO_CHILDREN_PROPERTY_NAME_STR)) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (child == nullptr || !child->haveProperty(GO_VISIBLE_PROPERTY_NAME_STR)
            || child->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            continue;
        }
        if (auto* control = dynamic_cast<GOUIControl*>(child)) {
            if (!focusableOnly || isFocusable(control)) {
                order.push_back(control);
            }
        } else if (dynamic_cast<GOUITabGroup*>(child) != nullptr) {
            // only the page on show takes part
            std::vector<int64> selected = handlesOf(child, GO_SELECTED_TAB_PROPERTY_NAME_STR);
            GraphicsObject* tab
                = selected.empty() ? nullptr : findGraphicsObject(selected[0], false);
            if (tab != nullptr) {
                collectTabOrder(tab, focusableOnly, order);
            }
        } else if (dynamic_cast<GOUIContainer*>(child) != nullptr) {
            collectTabOrder(child, focusableOnly, order);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
