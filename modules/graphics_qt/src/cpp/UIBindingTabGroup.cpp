//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsQtWarnings.h"
#include <QtCore/QCoreApplication>
#include <QtCore/QEvent>
#include <QtGui/QResizeEvent>
#include <QtWidgets/QProxyStyle>
#include <QtWidgets/QTabBar>
#include <QtWidgets/QTabWidget>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUITabGroup.h"
#include "GOUITab.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "SizeChangedDispatcher.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // Reference geometry of a bar on top or at the bottom: a 23 pixel bar, then the page inside
    // a one pixel frame. The native look is kept, only the rectangles are imposed.
    static constexpr int TAB_BAR_HEIGHT = 23;
    static constexpr int TAB_PAGE_FRAME = 1;
    //=============================================================================
    static bool
    isHorizontalTabBar(QTabBar::Shape shape)
    {
        return shape == QTabBar::RoundedNorth || shape == QTabBar::RoundedSouth
            || shape == QTabBar::TriangularNorth || shape == QTabBar::TriangularSouth;
    }
    //=============================================================================
    // A programmatic Position or TabLocation change resizes the pages through a
    // posted layout request that a running script never yields to: deliver it now.
    static void
    flushPageLayout(QTabWidget* tabWidget)
    {
        if (!tabWidget) {
            return;
        }
        // The layout resizes the current page to fit the group.
        QEvent layoutRequest(QEvent::LayoutRequest);
        QCoreApplication::sendEvent(tabWidget, &layoutRequest);
        if (QWidget* page = tabWidget->currentWidget()) {
            // An unmapped page (host window not yet exposed) only gets a posted
            // resize event: deliver the pending one, then a final resize, now.
            QCoreApplication::sendPostedEvents(page, QEvent::Resize);
            QResizeEvent resize(page->size(), QSize(-1, -1));
            QCoreApplication::sendEvent(page, &resize);
        }
    }
    //=============================================================================
    class QUITabBar : public QTabBar
    {
    protected:
        QSize
        tabSizeHint(int index) const override
        {
            QSize hint = QTabBar::tabSizeHint(index);
            if (isHorizontalTabBar(shape())) {
                hint.setHeight(TAB_BAR_HEIGHT);
            }
            return hint;
        }
    };
    //=============================================================================
    class QUITabGroupStyle : public QProxyStyle
    {
    public:
        QRect
        subElementRect(
            SubElement element, const QStyleOption* option, const QWidget* widget) const override
        {
            const QRect native = QProxyStyle::subElementRect(element, option, widget);
            const auto* tabWidget = qobject_cast<const QTabWidget*>(widget);
            if (!tabWidget || tabWidget->count() == 0) {
                return native;
            }
            const bool north = tabWidget->tabPosition() == QTabWidget::North;
            if (!north && tabWidget->tabPosition() != QTabWidget::South) {
                return native;
            }
            const QRect whole = widget->rect();
            const QRect pane = north ? whole.adjusted(0, TAB_BAR_HEIGHT, 0, 0)
                                     : whole.adjusted(0, 0, 0, -TAB_BAR_HEIGHT);
            switch (element) {
            case SE_TabWidgetTabBar:
                return QRect(native.x(), north ? 0 : whole.height() - TAB_BAR_HEIGHT,
                    native.width(), TAB_BAR_HEIGHT);
            case SE_TabWidgetTabPane:
                return pane;
            case SE_TabWidgetTabContents:
                return pane.adjusted(
                    TAB_PAGE_FRAME, TAB_PAGE_FRAME, -TAB_PAGE_FRAME, -TAB_PAGE_FRAME);
            default:
                return native;
            }
        }
    };
    //=============================================================================
    class QUITabWidget : public QTabWidget
    {
    public:
        explicit QUITabWidget(QWidget* parent) : QTabWidget(parent)
        {
            setTabBar(new QUITabBar());
            auto* groupStyle = new QUITabGroupStyle();
            groupStyle->setParent(this);
            setStyle(groupStyle);
        }
    };
    //=============================================================================
    class UITabGroupBinding : public QtUIComponentBinding
    {
    public:
        using QtUIComponentBinding::QtUIComponentBinding;
        //=============================================================================
    protected:
        //=============================================================================
        void
        createWidget() override
        {
            QWidget* parent = getParentQWidget();
            if (!parent) {
                return;
            }
            QTabWidget* tabWidget = new QUITabWidget(parent);
            QObject::connect(tabWidget, &QTabWidget::currentChanged, tabWidget,
                [this](int index) { onCurrentChanged(index); });
            widget = tabWidget;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QTabWidget* tabWidget = qobject_cast<QTabWidget*>(widget.data());
            if (!tabWidget) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_TAB_LOCATION_PROPERTY_NAME_STR)) {
                std::wstring location
                    = model->findStringProperty(GO_TAB_LOCATION_PROPERTY_NAME_STR);
                QTabWidget::TabPosition tabPosition = QTabWidget::North;
                if (location == GO_PROPERTY_VALUE_BOTTOM_STR) {
                    tabPosition = QTabWidget::South;
                } else if (location == GO_PROPERTY_VALUE_LEFT_STR) {
                    tabPosition = QTabWidget::West;
                } else if (location == GO_PROPERTY_VALUE_RIGHT_STR) {
                    tabPosition = QTabWidget::East;
                }
                RequestedTabGeometryScope requested;
                tabWidget->setTabPosition(tabPosition);
                flushPageLayout(tabWidget);
            }
            if (model->hasChanged(GO_SELECTED_TAB_PROPERTY_NAME_STR)) {
                GOGObjectsProperty* selectedTab = static_cast<GOGObjectsProperty*>(
                    model->findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
                if (selectedTab && !selectedTab->data().empty()) {
                    GraphicsObject* tabObject = findGraphicsObject(selectedTab->data()[0], false);
                    GOUITab* tab = dynamic_cast<GOUITab*>(tabObject);
                    if (tab && tab->getBinding() && tab->getBinding()->exists()) {
                        tabWidget->setCurrentWidget(
                            static_cast<QWidget*>(tab->getBinding()->nativeWidget()));
                    }
                }
            }
        }
        //=============================================================================
        void
        applyPositionToWidget() override
        {
            // Resize the page now so its geometry is current within the statement,
            // but a group-driven resize keeps the tab's reported size deferred:
            // the page reports its former size until its own deferred event.
            QtUIComponentBinding::applyPositionToWidget();
            flushPageLayout(qobject_cast<QTabWidget*>(widget.data()));
        }
        //=============================================================================
        void
        applyColorsToWidget() override
        {
            // keep the native tab widget look
        }
        //=============================================================================
    private:
        //=============================================================================
        int64
        tabHandleFromPage(QWidget* pageWidget)
        {
            GOUITabGroup* tabGroup = static_cast<GOUITabGroup*>(model);
            for (int64 childHandle : tabGroup->getChildrenHandles()) {
                GraphicsObject* child = findGraphicsObject(childHandle, false);
                GOUITab* tab = dynamic_cast<GOUITab*>(child);
                if (tab && tab->getBinding()
                    && static_cast<QWidget*>(tab->getBinding()->nativeWidget()) == pageWidget) {
                    return childHandle;
                }
            }
            return -1;
        }
        //=============================================================================
        void
        onCurrentChanged(int index)
        {
            QTabWidget* tabWidget = qobject_cast<QTabWidget*>(widget.data());
            if (!tabWidget || index < 0) {
                return;
            }
            int64 newTabHandle = tabHandleFromPage(tabWidget->widget(index));
            if (newTabHandle == -1) {
                return;
            }
            GOGObjectsProperty* selectedTab = static_cast<GOGObjectsProperty*>(
                model->findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
            if (!selectedTab) {
                return;
            }
            std::vector<int64> previousSelection(selectedTab->data());
            int64 previousHandle = previousSelection.empty() ? -1 : previousSelection[0];
            if (previousHandle == newTabHandle) {
                return;
            }
            std::vector<int64> newSelection;
            newSelection.push_back(newTabHandle);
            selectedTab->data(newSelection);

            if (model->isProgrammaticChange()) {
                return;
            }
            wstringVector fieldNames = { L"OldValue", L"NewValue" };
            ArrayOfVector fieldValues;
            fieldValues << (previousHandle == -1 ? ArrayOf::empty(0, 0)
                                                 : graphicsObjectHandle(previousHandle));
            fieldValues << graphicsObjectHandle(newTabHandle);
            model->emitCallbackEvent(GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR,
                L"SelectionChangedData", L"SelectionChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUITabGroupBinding(GOUIComponent* component)
{
    return new UITabGroupBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
