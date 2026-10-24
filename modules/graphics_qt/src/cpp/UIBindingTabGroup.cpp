//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QTabWidget>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUITabGroup.h"
#include "GOUITab.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
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
            QTabWidget* tabWidget = new QTabWidget(parent);
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
                tabWidget->setTabPosition(tabPosition);
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
