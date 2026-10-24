//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUITree.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOChartDecorators.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOStringOnOffProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUITree::GOUITree(bool isCheckboxTree) : checkboxStyle(isCheckboxTree)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUITree::getType()
{
    return checkboxStyle ? GO_PROPERTY_VALUE_UICHECKBOXTREE_STR : GO_PROPERTY_VALUE_UITREE_STR;
}
//=============================================================================
std::string
GOUITree::getClassName()
{
    return checkboxStyle ? GO_CLASS_NAME_UI_CHECKBOXTREE_STR : GO_CLASS_NAME_UI_TREE_STR;
}
//=============================================================================
void
GOUITree::constructProperties()
{
    registerUIComponentCommonProperties(true, true, true);
    registerProperty(new GOGObjectsProperty, GO_SELECTED_NODES_PROPERTY_NAME_STR);
    if (checkboxStyle) {
        registerProperty(new GOGObjectsProperty, GO_CHECKED_NODES_PROPERTY_NAME_STR);
        registerProperty(new GOCallbackProperty, GO_CHECKED_NODES_CHANGED_FCN_PROPERTY_NAME_STR);
    } else {
        registerProperty(new GOOnOffProperty, GO_MULTISELECT_PROPERTY_NAME_STR);
    }
    registerProperty(new GOOnOffProperty, GO_EDITABLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_NODE_EXPANDED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_NODE_COLLAPSED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_NODE_TEXT_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CLICKED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DOUBLE_CLICKED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOStyleConfigurationsProperty, GO_STYLE_CONFIGURATIONS_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUITree::setupDefaults()
{
    setupUIComponentCommonDefaults(20, 20, 150, 300, true, true);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    if (!checkboxStyle) {
        setRestrictedStringDefault(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    }
    setRestrictedStringDefault(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
void
GOUITree::rebuildTree()
{
    if (binding && binding->exists()) {
        binding->applyState(true);
    }
}
//=============================================================================
void
GOUITree::expandAll(bool expand)
{
    if (binding && binding->exists()) {
        pendingExpandCollapse = expand ? 1 : -1;
        binding->applyState(false);
        pendingExpandCollapse = 0;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
