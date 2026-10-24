//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UIParentRules.hpp"
#include "UIControlBuilder.hpp"
#include "GOUIComponent.h"
#include "GOUIContainer.h"
#include "GOUIPanel.h"
#include "GOUIRadioButton.h"
#include "GOUITab.h"
#include "GOUITabGroup.h"
#include "GOUITree.h"
#include "GOUITreeNode.h"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int64
ancestorFigureOf(GraphicsObject* go);
//=============================================================================
ResolvedUIParent
resolveUIParent(const ArrayOf& parentValue)
{
    if (!parentValue.isGraphicsObject() || !parentValue.isScalar()) {
        Error(L"Nelson:uicomponent:invalidParent",
            L"Invalid parent: Figure or UI container graphics object expected.");
    }
    go_handle go = parentValue.asGraphicsObjectScalar();
    GOFigure* fig = (go < HANDLE_OFFSET_OBJECT) ? findGOFigure(go) : nullptr;
    if (fig) {
        ResolvedUIParent resolved;
        resolved.figureID = go;
        resolved.parentHandle = go;
        resolved.isFigure = true;
        return resolved;
    }
    GraphicsObject* object = findGraphicsObject(go, false);
    GOUIContainer* container = dynamic_cast<GOUIContainer*>(object);
    if (!container) {
        Error(L"Nelson:uicomponent:invalidParent",
            L"Invalid parent: Figure or UI container graphics object expected.");
    }
    ResolvedUIParent resolved;
    resolved.figureID = ancestorFigureOf(container);
    resolved.parentHandle = go;
    resolved.isFigure = false;
    if (resolved.figureID == -1) {
        Error(L"Nelson:uicomponent:parentNotAttachedToFigure",
            L"Invalid parent: container is not attached to a figure.");
    }
    return resolved;
}
//=============================================================================
bool
parentIsTabGroup(const ResolvedUIParent& parent)
{
    if (parent.isFigure) {
        return false;
    }
    GraphicsObject* object = findGraphicsObject(parent.parentHandle, false);
    return dynamic_cast<GOUITabGroup*>(object) != nullptr;
}
//=============================================================================
void
checkParentKind(UIParentKind kind, const ResolvedUIParent& parent)
{
    switch (kind) {
    case UIParentKind::TabGroupOnly: {
        if (!parentIsTabGroup(parent)) {
            Error(L"Nelson:uicomponent:parentMustBeTabGroupObject",
                L"Parent must be a TabGroup object.");
        }
        break;
    }
    case UIParentKind::ButtonGroupOnly: {
        GOUIPanel* buttonGroup = parent.isFigure
            ? nullptr
            : dynamic_cast<GOUIPanel*>(findGraphicsObject(parent.parentHandle, false));
        if (!buttonGroup || !buttonGroup->isButtonGroup()) {
            Error(L"Nelson:uicomponent:parentMustBeButtonGroup",
                L"Parent must be a ButtonGroup object.");
        }
        break;
    }
    case UIParentKind::AnyContainer:
    default:
        if (parentIsTabGroup(parent)) {
            Error(L"Nelson:uicomponent:parentMustBeTabGroup",
                L"TabGroup parent can only contain Tab objects.");
        }
        break;
    }
}
//=============================================================================
void
checkUIParentChange(GraphicsObject* child, const ArrayOf& newParent)
{
    if (child->isType(GO_PROPERTY_VALUE_UICONTROL_STR)) {
        checkUIControlParent(newParent);
        return;
    }
    bool isTreeNode = dynamic_cast<GOUITreeNode*>(child) != nullptr;
    GOUIComponent* component = dynamic_cast<GOUIComponent*>(child);
    if (!component && !isTreeNode) {
        return;
    }
    if (isTreeNode) {
        GraphicsObject* parent = newParent.isGraphicsObject() && newParent.isScalar()
            ? findGraphicsObject(newParent.asGraphicsObjectScalar(), false)
            : nullptr;
        if (!dynamic_cast<GOUITree*>(parent) && !dynamic_cast<GOUITreeNode*>(parent)) {
            Error(L"Nelson:uicomponent:parentMustBeTree",
                L"Parent must be a Tree or TreeNode object.");
        }
    } else {
        UIParentKind kind = UIParentKind::AnyContainer;
        if (dynamic_cast<GOUITab*>(component)) {
            kind = UIParentKind::TabGroupOnly;
        } else if (dynamic_cast<GOUIRadioButton*>(component)) {
            kind = UIParentKind::ButtonGroupOnly;
        }
        checkParentKind(kind, resolveUIParent(newParent));
    }
    // a container cannot be moved under itself or one of its descendants
    go_handle childHandle = findGraphicsObjectHandle(child);
    go_handle ancestor = newParent.asGraphicsObjectScalar();
    for (int guard = 0; guard < 64 && ancestor >= HANDLE_OFFSET_OBJECT; ++guard) {
        if (ancestor == childHandle) {
            Error(L"Nelson:uicomponent:invalidParent",
                L"Invalid parent: Figure or UI container graphics object expected.");
        }
        GraphicsObject* object = findGraphicsObject(ancestor, false);
        if (!object) {
            break;
        }
        ancestor = object->findGoProperty(GO_PARENT_PROPERTY_NAME_STR);
    }
}
//=============================================================================
static int64
ancestorFigureOf(GraphicsObject* go)
{
    GraphicsObject* current = go;
    for (int guard = 0; guard < 64 && current; ++guard) {
        GOGObjectsProperty* parentProperty = static_cast<GOGObjectsProperty*>(
            current->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!parentProperty || parentProperty->data().empty()) {
            return -1;
        }
        int64 parentHandle = parentProperty->data()[0];
        if (parentHandle < HANDLE_OFFSET_OBJECT) {
            return parentHandle; // a figure id
        }
        current = findGraphicsObject(parentHandle, false);
    }
    return -1;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
