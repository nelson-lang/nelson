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
#include <QtCore/QObject>
#include <QtCore/QPoint>
#include <QtGui/QAction>
#include <QtGui/QColor>
#include <QtGui/QIcon>
#include <QtGui/QKeySequence>
#include <QtGui/QPainter>
#include <QtGui/QPixmap>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QWidget>
#include "GOMenuQt.hpp"
#include "GOCallbackProperty.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOWindow.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static constexpr double DEFAULT_MENU_COLOR_RED = 0.1294;
static constexpr double DEFAULT_MENU_COLOR_GREEN = 0.1294;
static constexpr double DEFAULT_MENU_COLOR_BLUE = 0.1294;
//=============================================================================
static bool
isOn(GraphicsObject* object, const std::wstring& propertyName)
{
    return object->stringCheck(propertyName, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
static QString
menuText(GraphicsObject* object)
{
    return wstringToQString(object->findStringProperty(GO_TEXT_PROPERTY_NAME_STR));
}
//=============================================================================
static std::vector<go_handle>
sortedMenuChildren(GraphicsObject* object)
{
    std::vector<go_handle> result;
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return result;
    }
    for (go_handle child : children->data()) {
        GraphicsObject* childObject = graphicsObjectFromHandle(child, false);
        if (childObject && childObject->isType(GO_PROPERTY_VALUE_UIMENU_STR)
            && isOn(childObject, GO_VISIBLE_PROPERTY_NAME_STR)) {
            result.push_back(child);
        }
    }
    std::stable_sort(result.begin(), result.end(), [](go_handle a, go_handle b) {
        GraphicsObject* menuA = graphicsObjectFromHandle(a, false);
        GraphicsObject* menuB = graphicsObjectFromHandle(b, false);
        double positionA
            = menuA ? menuA->findScalarDoubleProperty(GO_POSITION_PROPERTY_NAME_STR) : 0;
        double positionB
            = menuB ? menuB->findScalarDoubleProperty(GO_POSITION_PROPERTY_NAME_STR) : 0;
        return positionA < positionB;
    });
    return result;
}
//=============================================================================
static bool
hasVisibleMenuChildren(GraphicsObject* object)
{
    return !sortedMenuChildren(object).empty();
}
//=============================================================================
static int
colorByte(double value)
{
    return static_cast<int>(std::clamp(value, 0.0, 1.0) * 255.0);
}
//=============================================================================
static bool
isDefaultMenuColor(const std::vector<double>& rgb)
{
    if (rgb.size() != 3) {
        return true;
    }
    return std::fabs(rgb[0] - DEFAULT_MENU_COLOR_RED) < 0.001
        && std::fabs(rgb[1] - DEFAULT_MENU_COLOR_GREEN) < 0.001
        && std::fabs(rgb[2] - DEFAULT_MENU_COLOR_BLUE) < 0.001;
}
//=============================================================================
static QIcon
foregroundColorIcon(GraphicsObject* object)
{
    std::vector<double> rgb
        = object->findVectorDoubleProperty(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    if (isDefaultMenuColor(rgb)) {
        return QIcon();
    }
    QColor color(colorByte(rgb[0]), colorByte(rgb[1]), colorByte(rgb[2]));
    QPixmap pixmap(12, 12);
    pixmap.fill(Qt::transparent);
    QPainter painter(&pixmap);
    painter.setPen(color.darker(130));
    painter.setBrush(color);
    painter.drawRect(1, 1, 10, 10);
    painter.end();
    return QIcon(pixmap);
}
//=============================================================================
static void
executeMenuCallback(GraphicsObject* object)
{
    GOCallbackProperty* callback = static_cast<GOCallbackProperty*>(
        object->findProperty(GO_MENU_SELECTED_FCN_PROPERTY_NAME_STR, false));
    if (!callback || callback->get().isEmpty()) {
        return;
    }
    setCurrentObject(findGraphicsObjectHandle(object));
    callback->pushEvent(object, L"ActionData", L"Action");
}
//=============================================================================
static void
executeContextOpeningCallback(GraphicsObject* object)
{
    GOCallbackProperty* callback = static_cast<GOCallbackProperty*>(
        object->findProperty(GO_CONTEXT_MENU_OPENING_FCN_PROPERTY_NAME_STR, false));
    if (!callback || callback->get().isEmpty()) {
        return;
    }
    setCurrentObject(findGraphicsObjectHandle(object));
    callback->executeNow(object, L"ContextMenuOpeningData", L"ContextMenuOpening");
}
//=============================================================================
static void
configureAction(QAction* action, GraphicsObject* object, bool allowShortcut)
{
    action->setText(menuText(object));
    action->setEnabled(isOn(object, GO_ENABLE_PROPERTY_NAME_STR));
    action->setVisible(isOn(object, GO_VISIBLE_PROPERTY_NAME_STR));
    action->setToolTip(wstringToQString(object->findStringProperty(GO_TOOLTIP_PROPERTY_NAME_STR)));
    bool checked = isOn(object, GO_CHECKED_PROPERTY_NAME_STR);
    action->setCheckable(checked);
    action->setChecked(checked);
    QIcon colorIcon = foregroundColorIcon(object);
    if (!colorIcon.isNull()) {
        action->setIcon(colorIcon);
    }
    if (allowShortcut) {
        QString accelerator
            = wstringToQString(object->findStringProperty(GO_ACCELERATOR_PROPERTY_NAME_STR))
                  .trimmed();
        if (!accelerator.isEmpty()) {
            if (!accelerator.contains("+")) {
                accelerator = "Ctrl+" + accelerator.toUpper();
            }
            action->setShortcut(QKeySequence(accelerator));
        }
    }
}
//=============================================================================
static QAction*
addMenuAction(QMenu* parentMenu, QWidget* owner, go_handle menuHandle);
//=============================================================================
static QMenu*
createSubMenu(QWidget* owner, go_handle menuHandle)
{
    GraphicsObject* object = graphicsObjectFromHandle(menuHandle, false);
    if (!object) {
        return nullptr;
    }
    QMenu* subMenu = new QMenu(menuText(object), owner);
    configureAction(subMenu->menuAction(), object, false);
    QObject::connect(subMenu, &QMenu::aboutToShow, [menuHandle]() {
        GraphicsObject* object = graphicsObjectFromHandle(menuHandle, false);
        if (object) {
            executeMenuCallback(object);
        }
    });
    for (go_handle childHandle : sortedMenuChildren(object)) {
        addMenuAction(subMenu, subMenu, childHandle);
    }
    return subMenu;
}
//=============================================================================
static QAction*
addLeafAction(QMenu* parentMenu, QWidget* owner, go_handle menuHandle)
{
    GraphicsObject* object = graphicsObjectFromHandle(menuHandle, false);
    if (!object) {
        return nullptr;
    }
    QAction* action = new QAction(parentMenu);
    configureAction(action, object, true);
    QObject::connect(action, &QAction::triggered, [menuHandle]() {
        GraphicsObject* object = graphicsObjectFromHandle(menuHandle, false);
        if (object) {
            executeMenuCallback(object);
        }
    });
    parentMenu->addAction(action);
    return action;
}
//=============================================================================
static QAction*
addMenuAction(QMenu* parentMenu, QWidget* owner, go_handle menuHandle)
{
    GraphicsObject* object = graphicsObjectFromHandle(menuHandle, false);
    if (!object) {
        return nullptr;
    }
    if (isOn(object, GO_SEPARATOR_PROPERTY_NAME_STR)) {
        parentMenu->addSeparator();
    }
    if (hasVisibleMenuChildren(object)) {
        QMenu* subMenu = createSubMenu(owner, menuHandle);
        if (!subMenu) {
            return nullptr;
        }
        parentMenu->addMenu(subMenu);
        return subMenu->menuAction();
    }
    return addLeafAction(parentMenu, owner, menuHandle);
}
//=============================================================================
static QAction*
insertTopLevelMenu(QMenuBar* menuBar, QAction* beforeAction, GOWindow* window, go_handle menuHandle)
{
    GraphicsObject* object = graphicsObjectFromHandle(menuHandle, false);
    if (!object) {
        return nullptr;
    }
    if (isOn(object, GO_SEPARATOR_PROPERTY_NAME_STR)) {
        QAction* separator = menuBar->insertSeparator(beforeAction);
        separator->setProperty("nelsonUserMenu", true);
    }
    QAction* action = nullptr;
    if (hasVisibleMenuChildren(object)) {
        QMenu* menu = createSubMenu(window, menuHandle);
        if (!menu) {
            return nullptr;
        }
        action = beforeAction ? menuBar->insertMenu(beforeAction, menu) : menuBar->addMenu(menu);
    } else {
        action = new QAction(window);
        configureAction(action, object, true);
        QObject::connect(action, &QAction::triggered, [menuHandle]() {
            GraphicsObject* object = graphicsObjectFromHandle(menuHandle, false);
            if (object) {
                executeMenuCallback(object);
            }
        });
        if (beforeAction) {
            menuBar->insertAction(beforeAction, action);
        } else {
            menuBar->addAction(action);
        }
    }
    if (action) {
        action->setProperty("nelsonUserMenu", true);
    }
    return action;
}
//=============================================================================
static void
removeUserMenus(QMenuBar* menuBar)
{
    std::vector<QAction*> toRemove;
    for (QAction* action : menuBar->actions()) {
        if (action->property("nelsonUserMenu").toBool()) {
            toRemove.push_back(action);
        }
    }
    for (QAction* action : toRemove) {
        QMenu* menu = action->menu();
        menuBar->removeAction(action);
        if (menu) {
            menu->deleteLater();
        } else {
            action->deleteLater();
        }
    }
}
//=============================================================================
void
rebuildFigureMenus(GOWindow* window)
{
    // Headless (WEB_GUI): the browser builds the menu bar from the display
    // list; calling menuBar() here would lazily create a QMenuBar.
    if (!window || window->isHeadless() || !window->menuBar()) {
        return;
    }
    removeUserMenus(window->menuBar());
    GOFigure* figure = window->getGOFigure();
    if (!figure) {
        return;
    }
    for (go_handle menuHandle : sortedMenuChildren(figure)) {
        insertTopLevelMenu(window->menuBar(), nullptr, window, menuHandle);
    }
}
//=============================================================================
bool
popupContextMenu(QWidget* parentWidget, go_handle contextMenuHandle, const QPoint& globalPosition)
{
    GraphicsObject* contextMenu = graphicsObjectFromHandle(contextMenuHandle, false);
    if (!contextMenu || !contextMenu->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR)) {
        return false;
    }
    executeContextOpeningCallback(contextMenu);

    QMenu popup(parentWidget);
    for (go_handle menuHandle : sortedMenuChildren(contextMenu)) {
        addMenuAction(&popup, &popup, menuHandle);
    }
    if (popup.actions().isEmpty()) {
        return false;
    }
    popup.exec(globalPosition);
    return true;
}
//=============================================================================
bool
popupContextMenuForObject(
    QWidget* parentWidget, GraphicsObject* object, const QPoint& globalPosition)
{
    if (!object) {
        return false;
    }
    GOGObjectsProperty* contextMenu = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CONTEXT_MENU_PROPERTY_NAME_STR, false));
    if (!contextMenu || contextMenu->data().empty()) {
        return false;
    }
    return popupContextMenu(parentWidget, contextMenu->data()[0], globalPosition);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
