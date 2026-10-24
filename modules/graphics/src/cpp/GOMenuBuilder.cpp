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
#include <limits>
#include "GOMenuBuilder.hpp"
#include "GOCallbackProperty.hpp"
#include "GOContextMenu.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOMenu.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isFigureHandle(go_handle handle)
{
    return handle >= 0 && handle < HANDLE_OFFSET_OBJECT && findGOFigure(handle) != nullptr;
}
//=============================================================================
static bool
isContextMenuHandle(go_handle handle)
{
    GraphicsObject* object = graphicsObjectFromHandle(handle, false);
    return object && object->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR);
}
//=============================================================================
static bool
isMenuHandle(go_handle handle)
{
    GraphicsObject* object = graphicsObjectFromHandle(handle, false);
    return object && object->isType(GO_PROPERTY_VALUE_UIMENU_STR);
}
//=============================================================================
static bool
isMenuParentHandle(go_handle handle)
{
    return isFigureHandle(handle) || isContextMenuHandle(handle) || isMenuHandle(handle);
}
//=============================================================================
static go_handle
defaultFigureHandle()
{
    go_handle figureHandle = getCurrentFigure();
    if (figureHandle == NO_FIGURE) {
        figureHandle = createNewFigure();
    }
    return figureHandle;
}
//=============================================================================
static GOGObjectsProperty*
childrenProperty(GraphicsObject* object)
{
    if (!object) {
        return nullptr;
    }
    return static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
}
//=============================================================================
static GOGObjectsProperty*
parentProperty(GraphicsObject* object)
{
    if (!object) {
        return nullptr;
    }
    return static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
}
//=============================================================================
static go_handle
currentParentHandle(GraphicsObject* child)
{
    GOGObjectsProperty* parent = parentProperty(child);
    if (!parent || parent->data().empty()) {
        return HANDLE_UNASSIGNED_OBJECT;
    }
    return parent->data()[0];
}
//=============================================================================
static void
removeFromChildren(go_handle parentHandle, go_handle childHandle)
{
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, false);
    GOGObjectsProperty* children = childrenProperty(parent);
    if (!children) {
        return;
    }
    std::vector<int64> values = children->data();
    values.erase(std::remove(values.begin(), values.end(), childHandle), values.end());
    children->data(values);
}
//=============================================================================
static double
menuPosition(go_handle handle)
{
    GraphicsObject* object = graphicsObjectFromHandle(handle, false);
    if (!object || !object->isType(GO_PROPERTY_VALUE_UIMENU_STR)) {
        return std::numeric_limits<double>::infinity();
    }
    return object->findScalarDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
static void
sortMenuChildren(GraphicsObject* parent)
{
    GOGObjectsProperty* children = childrenProperty(parent);
    if (!children) {
        return;
    }
    std::vector<int64> values = children->data();
    std::stable_sort(values.begin(), values.end(),
        [](int64 a, int64 b) { return menuPosition(a) > menuPosition(b); });
    children->data(values);
}
//=============================================================================
static void
rebuildMenusForParent(go_handle parentHandle)
{
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, false);
    if (!parent) {
        return;
    }
    GOFigure* figure = nullptr;
    if (parent->isType(GO_PROPERTY_VALUE_FIGURE_STR)) {
        figure = static_cast<GOFigure*>(parent);
    } else {
        figure = parent->getParentFigure();
    }
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
static void
prependToChildren(go_handle parentHandle, go_handle childHandle)
{
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, true);
    GOGObjectsProperty* children = childrenProperty(parent);
    if (!children) {
        Error(L"Nelson:uimenu:invalidParent", L"Parent cannot contain menu objects.");
    }
    std::vector<int64> values = children->data();
    values.erase(std::remove(values.begin(), values.end(), childHandle), values.end());
    values.insert(values.begin(), childHandle);
    children->data(values);
    sortMenuChildren(parent);
}
//=============================================================================
static double
nextMenuPosition(go_handle parentHandle)
{
    GraphicsObject* parent = graphicsObjectFromHandle(parentHandle, true);
    GOGObjectsProperty* children = childrenProperty(parent);
    if (!children) {
        return 1;
    }
    double maxPosition = 0;
    for (go_handle child : children->data()) {
        GraphicsObject* object = graphicsObjectFromHandle(child, false);
        if (object && object->isType(GO_PROPERTY_VALUE_UIMENU_STR)) {
            maxPosition = std::max(
                maxPosition, object->findScalarDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
        }
    }
    return maxPosition + 1;
}
//=============================================================================
static bool
hasProperty(const ArrayOfVector& args, const std::wstring& propertyName)
{
    for (size_t k = 0; k + 1 < args.size(); k += 2) {
        if (args[k].asWideString() == propertyName) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static go_handle
parentFromProperties(const ArrayOfVector& args, go_handle fallbackParent)
{
    go_handle parentHandle = fallbackParent;
    for (size_t k = 0; k + 1 < args.size(); k += 2) {
        if (args[k].asWideString() == GO_PARENT_PROPERTY_NAME_STR) {
            parentHandle = args[k + 1].asGraphicsObjectScalar();
        }
    }
    return parentHandle;
}
//=============================================================================
static void
validatePropertyArgumentCount(const ArrayOfVector& args)
{
    if (args.size() % 2 != 0) {
        Error(L"Nelson:graphics:pvPairsExpected", L"Property/value pairs expected.");
    }
}
//=============================================================================
void
setGraphicsMenuParent(go_handle childHandle, go_handle parentHandle)
{
    GraphicsObject* child = graphicsObjectFromHandle(childHandle, true);
    bool childIsContextMenu = child->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR);
    bool childIsMenu = child->isType(GO_PROPERTY_VALUE_UIMENU_STR);
    if (!childIsContextMenu && !childIsMenu) {
        Error(L"Nelson:uimenu:expectedMenu", L"Menu or context menu graphics object expected.");
    }
    if (childIsContextMenu && !isFigureHandle(parentHandle)) {
        Error(L"Nelson:uimenu:contextMenuParent", L"Figure parent expected for context menu.");
    }
    if (childIsMenu && !isMenuParentHandle(parentHandle)) {
        Error(
            L"Nelson:uimenu:invalidMenuParent", L"Figure, context menu, or menu parent expected.");
    }

    go_handle oldParentHandle = currentParentHandle(child);
    if (oldParentHandle == parentHandle) {
        prependToChildren(parentHandle, childHandle);
        if (childIsMenu) {
            rebuildMenusForParent(parentHandle);
        }
        return;
    }
    if (!isUnassignedGraphicsObject(oldParentHandle)) {
        removeFromChildren(oldParentHandle, childHandle);
    }

    GOGObjectsProperty* parent = parentProperty(child);
    if (parent) {
        parent->data({ parentHandle });
    }
    prependToChildren(parentHandle, childHandle);
    if (childIsMenu) {
        rebuildMenusForParent(oldParentHandle);
        rebuildMenusForParent(parentHandle);
    }
}
//=============================================================================
static void
applyProperties(GraphicsObject* object, ArrayOfVector args)
{
    while (args.size() >= 2) {
        std::wstring propertyName(args[0].asWideString());
        ArrayOf propertyValue(args[1]);
        if (propertyName != GO_PARENT_PROPERTY_NAME_STR) {
            try {
                if (!object->isWritable(propertyName) && object->haveProperty(propertyName)) {
                    Error(L"Nelson:uimenu:readOnlyProperty", L"Property is read-only: %s",
                        propertyName);
                }
                if (propertyName == GO_CONTEXT_MENU_PROPERTY_NAME_STR && !propertyValue.isEmpty()) {
                    if (!propertyValue.isGraphicsObject() || propertyValue.elementCount() != 1) {
                        Error(L"Nelson:uimenu:expectedContextMenu",
                            L"Context menu graphics object expected.");
                    }
                    go_handle contextMenuHandle = propertyValue.asGraphicsObjectScalar();
                    GraphicsObject* contextMenu = graphicsObjectFromHandle(contextMenuHandle, true);
                    if (!contextMenu->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR)) {
                        Error(L"Nelson:uimenu:expectedContextMenu",
                            L"Context menu graphics object expected.");
                    }
                }
                object->findProperty(propertyName)->set(propertyValue);
            } catch (const NelsonException& e) {
                RaiseError(formatI18nMessage(
                               Nelson::resolveMessageTemplate(L"Nelson:graphics:gotErrorProperty",
                                   L"Got error for property: %s"),
                               propertyName)
                        + L"\n" + e.what(),
                    L"Nelson:uimenu:propertyError");
            }
        }
        args.pop_front();
        args.pop_front();
    }
}
//=============================================================================
static void
executeCreateFunction(GraphicsObject* object)
{
    GOCallbackProperty* callback
        = static_cast<GOCallbackProperty*>(object->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR));
    if (callback) {
        callback->executeNow(object);
    }
}
//=============================================================================
go_handle
ContextMenuBuilder(const ArrayOfVector& argIn)
{
    ArrayOfVector args(argIn);
    go_handle parentHandle = defaultFigureHandle();
    if (!args.empty() && args[0].isGraphicsObject()) {
        parentHandle = args[0].asGraphicsObjectScalar();
        args.pop_front();
    }
    validatePropertyArgumentCount(args);
    parentHandle = parentFromProperties(args, parentHandle);
    if (!isFigureHandle(parentHandle)) {
        Error(L"Nelson:uimenu:contextMenuParent", L"Figure parent expected for context menu.");
    }

    GOContextMenu* contextMenu = new GOContextMenu;
    go_handle handle = assignGraphicsObject(contextMenu);
    setGraphicsMenuParent(handle, parentHandle);
    applyProperties(contextMenu, args);
    contextMenu->updateState();
    executeCreateFunction(contextMenu);
    return handle;
}
//=============================================================================
go_handle
MenuBuilder(const ArrayOfVector& argIn)
{
    ArrayOfVector args(argIn);
    go_handle parentHandle = defaultFigureHandle();
    if (!args.empty() && args[0].isGraphicsObject()) {
        parentHandle = args[0].asGraphicsObjectScalar();
        args.pop_front();
    }
    validatePropertyArgumentCount(args);
    parentHandle = parentFromProperties(args, parentHandle);
    if (!isMenuParentHandle(parentHandle)) {
        Error(
            L"Nelson:uimenu:invalidMenuParent", L"Figure, context menu, or menu parent expected.");
    }

    GOMenu* menu = new GOMenu;
    if (!hasProperty(args, GO_POSITION_PROPERTY_NAME_STR)) {
        menu->setScalarDoubleDefault(GO_POSITION_PROPERTY_NAME_STR, nextMenuPosition(parentHandle));
    }
    go_handle handle = assignGraphicsObject(menu);
    setGraphicsMenuParent(handle, parentHandle);
    applyProperties(menu, args);
    menu->updateState();
    executeCreateFunction(menu);
    return handle;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
