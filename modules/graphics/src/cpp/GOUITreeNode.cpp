//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUITreeNode.h"
#include "GOUITree.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUITreeNode::GOUITreeNode()
{
    registerProperty(new GOStringProperty, GO_TEXT_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_NODE_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_ICON_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    sortProperties();

    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_TEXT_PROPERTY_NAME_STR, L"Tree Node");
    GOArrayOfProperty* icon
        = static_cast<GOArrayOfProperty*>(findProperty(GO_ICON_PROPERTY_NAME_STR));
    icon->data(ArrayOf::characterArray(L""));
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, L"");
}
//=============================================================================
std::wstring
GOUITreeNode::getType()
{
    return GO_PROPERTY_VALUE_UITREENODE_STR;
}
//=============================================================================
std::string
GOUITreeNode::getClassName()
{
    return GO_CLASS_NAME_UI_TREENODE_STR;
}
//=============================================================================
GOUITree*
GOUITreeNode::getOwningTree()
{
    GraphicsObject* current = this;
    for (int guard = 0; guard < 64 && current; ++guard) {
        GOGObjectsProperty* parentProperty = static_cast<GOGObjectsProperty*>(
            current->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!parentProperty || parentProperty->data().empty()) {
            return nullptr;
        }
        GraphicsObject* parent = findGraphicsObject(parentProperty->data()[0], false);
        GOUITree* tree = dynamic_cast<GOUITree*>(parent);
        if (tree) {
            return tree;
        }
        current = parent;
    }
    return nullptr;
}
//=============================================================================
void
GOUITreeNode::updateState()
{
    GOUITree* tree = getOwningTree();
    if (tree) {
        tree->rebuildTree();
    }
    clearAllChanged();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
