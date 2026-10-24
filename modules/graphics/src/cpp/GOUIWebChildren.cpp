//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIWebChildren.hpp"
#include "GOAxis.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOUIComponent.h"
#include "GOUIComponentUnits.hpp"
#include "GOUIControl.h"
#include "RenderWeb.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
uiContainerHasAxesChild(GraphicsObject* container)
{
    auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
        container->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return false;
    }
    for (int64 childHandle : childProperty->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (dynamic_cast<GOAxis*>(child) != nullptr) {
            return true;
        }
    }
    return false;
}
//=============================================================================
void
uiDescribeChildrenForWeb(
    GraphicsObject* container, RenderWeb& renderer, const std::vector<double>& position)
{
    auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
        container->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return;
    }
    for (int64 childHandle : childProperty->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        auto* component = dynamic_cast<GOUIComponent*>(child);
        auto* control = dynamic_cast<GOUIControl*>(child);
        if (component == nullptr && control == nullptr) {
            continue;
        }
        std::vector<double> childPosition
            = child->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (childPosition.size() != 4) {
            continue;
        }
        uiComponentPositionInParentFrame(child, childPosition, position[2], position[3]);
        childPosition[0] += position[0];
        childPosition[1] += position[1];
        if (component != nullptr) {
            component->describeForWeb(renderer, childPosition);
        } else {
            control->describeForWeb(renderer, childPosition);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
