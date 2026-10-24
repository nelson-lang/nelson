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
#include <QtWidgets/QListWidget>
#include <QtWidgets/QStyledItemDelegate>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIListBox.h"
#include "UIItemsHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // Enforce a stable list row height (18 px) whatever the platform style;
    // a larger FontSize grows the rows instead of clipping the text.
    class QUIListRowDelegate : public QStyledItemDelegate
    {
    public:
        QUIListRowDelegate(QObject* parent) : QStyledItemDelegate(parent) { }
        QSize
        sizeHint(const QStyleOptionViewItem& option, const QModelIndex& index) const override
        {
            QSize size = QStyledItemDelegate::sizeHint(option, index);
            size.setHeight(std::max(18, size.height()));
            return size;
        }
    };
    //=============================================================================
    class UIListBoxBinding : public QtUIComponentBinding
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
            QListWidget* listWidget = new QListWidget(parent);
            listWidget->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
            listWidget->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
            listWidget->setUniformItemSizes(true);
            listWidget->setFrameShape(QFrame::Box);
            listWidget->setLineWidth(1);
            // fixed 18 px rows: 4 default items fill the default 74 px height
            listWidget->setItemDelegate(new QUIListRowDelegate(listWidget));
            QObject::connect(listWidget, &QListWidget::itemSelectionChanged, listWidget,
                [this]() { onSelectionChanged(); });
            widget = listWidget;
        }
        //=============================================================================
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QListWidget* listWidget = qobject_cast<QListWidget*>(widget.data());
            if (!listWidget) {
                return;
            }
            bool itemsChanged = model->hasChanged(GO_ITEMS_PROPERTY_NAME_STR)
                || model->hasChanged(GO_ITEMS_DATA_PROPERTY_NAME_STR);
            bool valueChanged = model->hasChanged(GO_VALUE_PROPERTY_NAME_STR)
                || model->hasChanged(GO_VALUE_INDEX_PROPERTY_NAME_STR);
            if (isNewWidget || model->hasChanged(GO_MULTISELECT_PROPERTY_NAME_STR)) {
                listWidget->setSelectionMode(
                    model->stringCheck(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
                        ? QAbstractItemView::ExtendedSelection
                        : QAbstractItemView::SingleSelection);
            }
            if (isNewWidget || itemsChanged || valueChanged
                || model->hasChanged(GO_MULTISELECT_PROPERTY_NAME_STR)) {
                static_cast<GOUIListBox*>(model)->synchronizeValueWithItems();
            }
            if (isNewWidget || itemsChanged) {
                wstringVector items = model->findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
                listWidget->clear();
                for (const std::wstring& item : items) {
                    QListWidgetItem* listItem = new QListWidgetItem(wstringToQString(item));
                    listWidget->addItem(listItem);
                }
            }
            if (isNewWidget || itemsChanged || valueChanged) {
                applySelectionToWidget();
            }
        }
        //=============================================================================
        QString
        additionalStyleSheet() override
        {
            return QStringLiteral("QListWidget {"
                                  " border: 1px solid #8c8c8c;"
                                  " outline: 0;"
                                  "}"
                                  "QListWidget::item {"
                                  " min-height: 17px;"
                                  " padding: 0px 3px;"
                                  "}"
                                  "QListWidget::item:selected,"
                                  "QListWidget::item:selected:active,"
                                  "QListWidget::item:selected:!active {"
                                  " background: #b8b8b8;"
                                  " color: #212121;"
                                  " border: none;"
                                  "}"
                                  "QScrollBar:vertical {"
                                  " width: 16px;"
                                  " background: #eeeeee;"
                                  " border-left: 1px solid #c6c6c6;"
                                  "}"
                                  "QScrollBar:horizontal {"
                                  " height: 16px;"
                                  " background: #eeeeee;"
                                  " border-top: 1px solid #c6c6c6;"
                                  "}"
                                  "QScrollBar::handle {"
                                  " background: #c2c2c2;"
                                  " margin: 2px;"
                                  "}"
                                  "QScrollBar::add-line,"
                                  "QScrollBar::sub-line {"
                                  " background: #eeeeee;"
                                  " border: none;"
                                  "}");
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        applySelectionToWidget()
        {
            QListWidget* listWidget = qobject_cast<QListWidget*>(widget.data());
            if (!listWidget) {
                return;
            }
            listWidget->clearSelection();
            for (int index : static_cast<GOUIListBox*>(model)->currentIndicesFromProperties()) {
                if (index >= 1 && index <= listWidget->count()) {
                    listWidget->item(index - 1)->setSelected(true);
                    listWidget->setCurrentRow(index - 1);
                }
            }
        }
        //=============================================================================
        void
        onSelectionChanged()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            QListWidget* listWidget = qobject_cast<QListWidget*>(widget.data());
            if (!listWidget) {
                return;
            }
            wstringVector items = model->findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
            ArrayOf itemsData = model->findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
            GOArrayOfProperty* valueProperty
                = static_cast<GOArrayOfProperty*>(model->findProperty(GO_VALUE_PROPERTY_NAME_STR));
            GOArrayOfProperty* valueIndexProperty = static_cast<GOArrayOfProperty*>(
                model->findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));

            ArrayOf previousValue = valueProperty->data();
            ArrayOf previousValueIndex = valueIndexProperty->data();

            std::vector<int> indices;
            for (int row = 0; row < listWidget->count(); ++row) {
                if (listWidget->item(row)->isSelected()) {
                    indices.push_back(row + 1);
                }
            }
            ArrayOf newValue;
            ArrayOf newValueIndex;
            if (indices.empty()) {
                newValue = ArrayOf::emptyCell(Dimensions(1, 0));
                newValueIndex = ArrayOf::empty(0, 0);
            } else if (indices.size() == 1) {
                newValue = uiItemsValueAtIndex(indices[0], items, itemsData);
                newValueIndex = ArrayOf::doubleScalar(indices[0]);
            } else {
                ArrayOf* elements
                    = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, indices.size());
                for (size_t k = 0; k < indices.size(); ++k) {
                    elements[k] = uiItemsValueAtIndex(indices[k], items, itemsData);
                }
                newValue = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, indices.size()), elements);
                std::vector<double> asDouble(indices.begin(), indices.end());
                newValueIndex = ArrayOf::doubleRowVector(asDouble);
            }
            valueProperty->data(newValue);
            valueIndexProperty->data(newValueIndex);

            wstringVector fieldNames
                = { L"Value", L"PreviousValue", L"ValueIndex", L"PreviousValueIndex" };
            ArrayOfVector fieldValues;
            fieldValues << newValue;
            fieldValues << previousValue;
            fieldValues << newValueIndex;
            fieldValues << previousValueIndex;
            model->emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData",
                L"ValueChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIListBoxBinding(GOUIComponent* component)
{
    return new UIListBoxBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
