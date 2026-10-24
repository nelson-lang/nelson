//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOLayoutOptions.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOLayoutOptions::GOLayoutOptions(LayoutOptionsMode mode) : m_mode(mode)
{
    registerProperties();
    if (m_mode == LayoutOptionsMode::TiledChartLayout) {
        findProperty(GO_TILE_PROPERTY_NAME_STR)->set(ArrayOf::doubleScalar(0.0));
        setTwoVectorDefault(GO_TILE_SPAN_PROPERTY_NAME_STR, 1.0, 1.0);
    } else {
        findProperty(GO_ROW_PROPERTY_NAME_STR)->set(ArrayOf::doubleScalar(0.0));
        findProperty(GO_COLUMN_PROPERTY_NAME_STR)->set(ArrayOf::doubleScalar(0.0));
    }
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
}
//=============================================================================
std::wstring
GOLayoutOptions::getType()
{
    return L"layoutoptions";
}
//=============================================================================
void
GOLayoutOptions::registerProperties()
{
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    if (m_mode == LayoutOptionsMode::TiledChartLayout) {
        // Tile: scalar double (0 = auto/unset) or a string for edge tiles
        registerProperty(new GOArrayOfProperty, GO_TILE_PROPERTY_NAME_STR);
        // TileSpan: [rows cols]
        registerProperty(new GOTwoVectorProperty, GO_TILE_SPAN_PROPERTY_NAME_STR);
    } else {
        // Row / Column: positive integer scalar or [start end] span
        // (0 = auto/unset)
        registerProperty(new GOArrayOfProperty, GO_ROW_PROPERTY_NAME_STR);
        registerProperty(new GOArrayOfProperty, GO_COLUMN_PROPERTY_NAME_STR);
    }
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOLayoutOptions::updateState()
{
    if (m_mode != LayoutOptionsMode::GridLayout) {
        return;
    }
    // Row/Column changed: ask the owning grid layout to recompute.
    GOGObjectsProperty* parentProperty
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parentProperty || parentProperty->data().empty()) {
        return;
    }
    // Parent is the UI component owning this Layout; its own parent may be
    // the grid layout container.
    GraphicsObject* component = findGraphicsObject(parentProperty->data()[0], false);
    if (!component) {
        return;
    }
    GOGObjectsProperty* componentParent = static_cast<GOGObjectsProperty*>(
        component->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!componentParent || componentParent->data().empty()) {
        return;
    }
    GraphicsObject* gridCandidate = findGraphicsObject(componentParent->data()[0], false);
    if (gridCandidate && gridCandidate->getType() == GO_PROPERTY_VALUE_UIGRIDLAYOUT_STR) {
        gridCandidate->updateState();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
