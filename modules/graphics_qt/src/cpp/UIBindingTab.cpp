//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QSignalBlocker>
#include <QtGui/QPainter>
#include <QtGui/QPaintEvent>
#include <QtWidgets/QTabWidget>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUITab.h"
#include "GOUITabGroup.h"
#include "GOUIComponent.h"
#include "GOUIControl.h"
#include "GOFigure.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // tab page: the QTabWidget sizes pages lazily, so children placed with a
    // bottom-left origin must be repositioned every time the page is resized
    class QUITabPageWidget : public QWidget
    {
        GOUITab* goTab;

    public:
        QUITabPageWidget(GOUITab* owner) : QWidget(), goTab(owner)
        {
            setProperty("nelsonPaintsGraphicsChildren", true);
        }

        void
        detachOwner()
        {
            goTab = nullptr;
        }

    protected:
        void
        paintEvent(QPaintEvent* event) override
        {
            QWidget::paintEvent(event);
            paintGraphicsChildren();
        }

        void
        resizeEvent(QResizeEvent* event) override
        {
            QWidget::resizeEvent(event);
            if (goTab) {
                goTab->onPageResized((double)width(), (double)height());
            }
        }

    private:
        void
        paintGraphicsChildren()
        {
            if (!goTab || width() <= 0 || height() <= 0) {
                return;
            }
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            painter.setRenderHint(QPainter::TextAntialiasing);
            RenderQt renderer(&painter, 0, 0, width(), height(), L"Image");
            goTab->paintContentInFrame(renderer);
        }
    };
    //=============================================================================
    static GOUITabGroup*
    findParentTabGroup(GOUIComponent* component)
    {
        GOGObjectsProperty* parentProperty = static_cast<GOGObjectsProperty*>(
            component->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!parentProperty || parentProperty->data().empty()) {
            return nullptr;
        }
        return dynamic_cast<GOUITabGroup*>(findGraphicsObject(parentProperty->data()[0], false));
    }
    //=============================================================================
    static QTabWidget*
    tabWidgetOf(GOUITabGroup* tabGroup)
    {
        if (!tabGroup) {
            return nullptr;
        }
        UIComponentBinding* groupBinding = tabGroup->getBinding();
        if (!groupBinding || !groupBinding->exists()) {
            return nullptr;
        }
        return qobject_cast<QTabWidget*>(static_cast<QWidget*>(groupBinding->nativeWidget()));
    }
    //=============================================================================
    static QTabWidget*
    owningTabWidget(QWidget* page)
    {
        QWidget* parent = page ? page->parentWidget() : nullptr;
        while (parent) {
            QTabWidget* tabWidget = qobject_cast<QTabWidget*>(parent);
            if (tabWidget) {
                return tabWidget;
            }
            parent = parent->parentWidget();
        }
        return nullptr;
    }
    //=============================================================================
    static int64
    tabHandleFromPage(GOUITabGroup* tabGroup, QWidget* pageWidget)
    {
        for (int64 childHandle : tabGroup->getChildrenHandles()) {
            GOUITab* tab = dynamic_cast<GOUITab*>(findGraphicsObject(childHandle, false));
            if (tab && tab->getBinding()
                && static_cast<QWidget*>(tab->getBinding()->nativeWidget()) == pageWidget) {
                return childHandle;
            }
        }
        return -1;
    }
    //=============================================================================
    static GOUITabGroup*
    findTabGroupForWidget(GraphicsObject* object, QTabWidget* target, int depth = 0)
    {
        if (!object || !target || depth >= 64) {
            return nullptr;
        }
        GOUITabGroup* tabGroup = dynamic_cast<GOUITabGroup*>(object);
        if (tabGroup && tabWidgetOf(tabGroup) == target) {
            return tabGroup;
        }
        GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
            object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (!children) {
            return nullptr;
        }
        for (int64 childHandle : children->data()) {
            GOUITabGroup* found
                = findTabGroupForWidget(findGraphicsObject(childHandle, false), target, depth + 1);
            if (found) {
                return found;
            }
        }
        return nullptr;
    }
    //=============================================================================
    static void
    syncSelectedTab(GOUITabGroup* tabGroup, QTabWidget* tabWidget)
    {
        if (!tabGroup || !tabWidget) {
            return;
        }
        GOGObjectsProperty* selectedTab = static_cast<GOGObjectsProperty*>(
            tabGroup->findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
        if (!selectedTab) {
            return;
        }
        int64 selectedHandle = tabHandleFromPage(tabGroup, tabWidget->currentWidget());
        selectedTab->data(
            selectedHandle == -1 ? std::vector<int64> {} : std::vector<int64> { selectedHandle });
    }
    //=============================================================================
    class UITabBinding : public QtUIComponentBinding
    {
    public:
        using QtUIComponentBinding::QtUIComponentBinding;
        //=============================================================================
        // the base class destructor only reaches its own destroy(); run the
        // tab-aware teardown while the derived part is still alive
        ~UITabBinding() override { destroy(); }
        //=============================================================================
        void
        destroy() override
        {
            if (widget) {
                static_cast<QUITabPageWidget*>(widget.data())->detachOwner();
                removeFromTabGroup();
            }
            QtUIComponentBinding::destroy();
        }
        //=============================================================================
    protected:
        //=============================================================================
        void
        applyParentToWidget() override
        {
            GOUITabGroup* tabGroup = findParentTabGroup(model);
            QTabWidget* target = tabWidgetOf(tabGroup);
            if (!target || !widget) {
                return;
            }
            QTabWidget* current = owningTabWidget(widget.data());
            GOUITabGroup* currentGroup = findTabGroupForWidget(model->getParentFigure(), current);
            if (current == target && target->indexOf(widget.data()) != -1) {
                return;
            }
            if (current) {
                int index = current->indexOf(widget.data());
                if (index != -1) {
                    QSignalBlocker blocker(current);
                    current->removeTab(index);
                    syncSelectedTab(currentGroup, current);
                }
            }
            target->addTab(widget.data(),
                wstringToQString(model->findStringProperty(GO_TITLE_PROPERTY_NAME_STR)));
            GOGObjectsProperty* selectedTab = static_cast<GOGObjectsProperty*>(
                tabGroup->findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
            if (selectedTab && selectedTab->data().empty()) {
                selectedTab->data({ findGraphicsObjectHandle(model) });
            }
            static_cast<GOUITab*>(model)->onPageResized(
                static_cast<double>(widget->width()), static_cast<double>(widget->height()));
        }
        //=============================================================================
        void
        createWidget() override
        {
            // the page belongs to the parent tab group's QTabWidget, not to the
            // figure host: resolve (and, if needed, build) the group's widget
            GOUITabGroup* tabGroup = findParentTabGroup(model);
            if (!tabGroup) {
                return;
            }
            QTabWidget* tabWidget = tabWidgetOf(tabGroup);
            if (!tabWidget) {
                tabGroup->updateState();
                tabWidget = tabWidgetOf(tabGroup);
            }
            if (!tabWidget) {
                return;
            }
            QUITabPageWidget* page = new QUITabPageWidget(static_cast<GOUITab*>(model));
            widget = page;
            if (tabWidget->indexOf(page) == -1) {
                tabWidget->addTab(
                    page, wstringToQString(model->findStringProperty(GO_TITLE_PROPERTY_NAME_STR)));
            }
            // first added tab becomes the selected tab
            GOGObjectsProperty* selectedTab = static_cast<GOGObjectsProperty*>(
                tabGroup->findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
            if (selectedTab && selectedTab->data().empty()) {
                std::vector<int64> selection;
                selection.push_back(findGraphicsObjectHandle(model));
                selectedTab->data(selection);
            }
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            if (isNewWidget || model->hasChanged(GO_TITLE_PROPERTY_NAME_STR)) {
                QTabWidget* tabWidget = tabWidgetOf(findParentTabGroup(model));
                if (tabWidget && widget) {
                    int index = tabWidget->indexOf(widget.data());
                    if (index != -1) {
                        tabWidget->setTabText(index,
                            wstringToQString(
                                model->findStringProperty(GO_TITLE_PROPERTY_NAME_STR)));
                    }
                }
            }
            if (isNewWidget || model->hasChanged(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR)) {
                GOColorProperty* backgroundColor = static_cast<GOColorProperty*>(
                    model->findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, false));
                if (backgroundColor && !backgroundColor->isNone() && widget) {
                    std::vector<double> rgb = backgroundColor->data();
                    if (rgb.size() >= 3) {
                        QPalette palette = widget->palette();
                        palette.setColor(QPalette::Window,
                            QColor::fromRgb((int)(rgb[0] * 255.0 + 0.5),
                                (int)(rgb[1] * 255.0 + 0.5), (int)(rgb[2] * 255.0 + 0.5)));
                        widget->setAutoFillBackground(true);
                        widget->setPalette(palette);
                    }
                }
            }
            // Position/InnerPosition/OuterPosition reflect the public tab area.
            if (widget) {
                QRect geometry = widget->geometry();
                static_cast<GOUITab*>(model)->onPageResized(
                    static_cast<double>(geometry.width()), static_cast<double>(geometry.height()));
            }
        }
        //=============================================================================
        void
        applyColorsToWidget() override
        {
            // handled in applyStateToWidget via the palette
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        removeFromTabGroup()
        {
            GOUITabGroup* tabGroup = findParentTabGroup(model);
            QTabWidget* tabWidget = tabWidgetOf(tabGroup);
            if (!tabWidget || !widget) {
                return;
            }
            int index = tabWidget->indexOf(widget.data());
            if (index == -1) {
                return;
            }
            bool removedCurrentTab = tabWidget->currentWidget() == widget.data();
            QSignalBlocker blocker(tabWidget);
            tabWidget->removeTab(index);
            if (removedCurrentTab) {
                GOGObjectsProperty* selectedTab = static_cast<GOGObjectsProperty*>(
                    tabGroup->findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
                if (selectedTab) {
                    int64 selectedHandle = tabHandleFromPage(tabGroup, tabWidget->currentWidget());
                    if (selectedHandle == -1) {
                        selectedTab->data({});
                    } else {
                        selectedTab->data({ selectedHandle });
                    }
                }
            }
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUITabBinding(GOUIComponent* component)
{
    return new UITabBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
