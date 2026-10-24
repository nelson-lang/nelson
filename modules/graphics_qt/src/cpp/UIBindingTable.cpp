//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QTableWidget>
#include <QtWidgets/QHeaderView>
#include <algorithm>
#include <cmath>
#include <vector>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUITable.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static int
    pixelWidth(double value)
    {
        if (!std::isfinite(value) || value <= 0) {
            return 76;
        }
        return std::max(1, (int)std::lround(value));
    }
    //=============================================================================
    static std::vector<int>
    columnPixelWidths(const ArrayOf& columnWidth, indexType columns)
    {
        std::vector<int> widths((size_t)columns, 76);
        if (columns == 0 || columnWidth.isEmpty()) {
            return widths;
        }
        if (columnWidth.isNumeric()) {
            ArrayOf numericWidths(columnWidth);
            numericWidths.promoteType(NLS_DOUBLE);
            const double* values = (const double*)numericWidths.dataPointer();
            if (numericWidths.isScalar()) {
                std::fill(widths.begin(), widths.end(), pixelWidth(values[0]));
            } else {
                const indexType count = std::min(columns, numericWidths.elementCount());
                for (indexType k = 0; k < count; ++k) {
                    widths[(size_t)k] = pixelWidth(values[k]);
                }
            }
            return widths;
        }
        if (!columnWidth.isCell()) {
            return widths;
        }
        const ArrayOf* elements = (const ArrayOf*)columnWidth.dataPointer();
        const indexType count = std::min(columns, columnWidth.elementCount());
        for (indexType k = 0; k < count; ++k) {
            ArrayOf element = elements[k];
            if (element.isNumeric() && element.isScalar()) {
                widths[(size_t)k] = pixelWidth(ArrayOf(element).asDoubleScalar());
            }
        }
        return widths;
    }
    //=============================================================================
    static std::wstring
    cellTextOf(const ArrayOf& data, indexType row, indexType column)
    {
        indexType rows = data.rows();
        if (data.isCell()) {
            const ArrayOf* elements = (const ArrayOf*)data.dataPointer();
            ArrayOf element = elements[row + column * rows];
            if (element.isRowVectorCharacterArray() || element.isScalarStringArray()) {
                return ArrayOf(element).asWideString();
            }
            if (element.isNumeric() && element.isScalar()) {
                wchar_t buffer[64];
                swprintf(buffer, 64, L"%g", ArrayOf(element).asDoubleScalar());
                return buffer;
            }
            if (element.isLogical() && element.isScalar()) {
                return ArrayOf(element).asDoubleScalar() != 0 ? L"true" : L"false";
            }
            return L"";
        }
        if (data.isNumeric() || data.isLogical()) {
            ArrayOf asDouble(data);
            asDouble.promoteType(NLS_DOUBLE);
            const double* values = (const double*)asDouble.dataPointer();
            wchar_t buffer[64];
            swprintf(buffer, 64, L"%g", values[row + column * rows]);
            return buffer;
        }
        return L"";
    }
    //=============================================================================
    class UITableBinding : public QtUIComponentBinding
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
            QTableWidget* table = new QTableWidget(parent);
            table->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
            table->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
            table->setShowGrid(true);
            table->setFrameShape(QFrame::Box);
            table->setLineWidth(1);
            table->horizontalHeader()->setSectionResizeMode(QHeaderView::Fixed);
            table->verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);
            table->horizontalHeader()->setDefaultAlignment(Qt::AlignLeft | Qt::AlignVCenter);
            table->verticalHeader()->setDefaultAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
            table->verticalHeader()->setFixedWidth(40);
            QObject::connect(
                table, &QTableWidget::cellChanged, table, [this, table](int row, int column) {
                    QTableWidgetItem* item = table->item(row, column);
                    if (item) {
                        onCellChanged(row, column, QStringTowstring(item->text()));
                    }
                });
            QObject::connect(table, &QTableWidget::itemSelectionChanged, table,
                [this]() { onSelectionChanged(); });
            widget = table;
        }
        //=============================================================================
        QString
        additionalStyleSheet() override
        {
            return QStringLiteral("QTableWidget {"
                                  " border: 1px solid #7f7f7f;"
                                  " gridline-color: #8c8c8c;"
                                  " background: #ffffff;"
                                  " alternate-background-color: #f2f2f2;"
                                  " outline: 0;"
                                  "}"
                                  "QHeaderView::section {"
                                  " background: #f2f2f2;"
                                  " border: 1px solid #8c8c8c;"
                                  " padding-left: 4px;"
                                  " padding-right: 4px;"
                                  " min-height: 24px;"
                                  "}"
                                  "QTableWidget::item {"
                                  " padding-left: 4px;"
                                  " padding-right: 4px;"
                                  "}"
                                  "QScrollBar:horizontal {"
                                  " height: 16px;"
                                  " background: #eeeeee;"
                                  " border-top: 1px solid #c6c6c6;"
                                  "}"
                                  "QScrollBar::handle:horizontal {"
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
        void
        applyStateToWidget(bool isNewWidget) override
        {
            QTableWidget* table = qobject_cast<QTableWidget*>(widget.data());
            if (!table) {
                return;
            }
            if (isNewWidget || model->hasChanged(GO_DATA_PROPERTY_NAME_STR)
                || model->hasChanged(GO_COLUMN_NAME_PROPERTY_NAME_STR)
                || model->hasChanged(GO_COLUMN_WIDTH_PROPERTY_NAME_STR)
                || model->hasChanged(GO_COLUMN_EDITABLE_PROPERTY_NAME_STR)) {
                refreshTableFromData();
            }
            if (isNewWidget || model->hasChanged(GO_SELECTION_TYPE_PROPERTY_NAME_STR)
                || model->hasChanged(GO_MULTISELECT_PROPERTY_NAME_STR)) {
                std::wstring selectionType
                    = model->findStringProperty(GO_SELECTION_TYPE_PROPERTY_NAME_STR);
                if (selectionType == GO_PROPERTY_VALUE_ROW_STR) {
                    table->setSelectionBehavior(QAbstractItemView::SelectRows);
                } else if (selectionType == GO_PROPERTY_VALUE_COLUMN_STR) {
                    table->setSelectionBehavior(QAbstractItemView::SelectColumns);
                } else {
                    table->setSelectionBehavior(QAbstractItemView::SelectItems);
                }
                table->setSelectionMode(
                    model->stringCheck(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
                        ? QAbstractItemView::ExtendedSelection
                        : QAbstractItemView::SingleSelection);
            }
            if (isNewWidget || model->hasChanged(GO_ROW_STRIPING_PROPERTY_NAME_STR)) {
                table->setAlternatingRowColors(model->stringCheck(
                    GO_ROW_STRIPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            }
        }
        //=============================================================================
    private:
        //=============================================================================
        void
        refreshTableFromData()
        {
            QTableWidget* table = qobject_cast<QTableWidget*>(widget.data());
            if (!table) {
                return;
            }
            ArrayOf data = model->findArrayOfProperty(GO_DATA_PROPERTY_NAME_STR);
            indexType rows = data.isEmpty() ? 0 : data.rows();
            indexType columns = data.isEmpty() ? 0 : data.columns();
            table->setRowCount((int)rows);
            table->setColumnCount((int)columns);
            table->horizontalHeader()->setFixedHeight(27);
            table->verticalHeader()->setFixedWidth(40);
            table->verticalHeader()->setDefaultSectionSize(24);
            std::vector<int> widths = columnPixelWidths(
                model->findArrayOfProperty(GO_COLUMN_WIDTH_PROPERTY_NAME_STR), columns);
            for (indexType column = 0; column < columns; ++column) {
                table->setColumnWidth((int)column, widths[(size_t)column]);
            }
            for (indexType row = 0; row < rows; ++row) {
                table->setRowHeight((int)row, 24);
            }
            ArrayOf columnName = model->findArrayOfProperty(GO_COLUMN_NAME_PROPERTY_NAME_STR);
            QStringList headers;
            bool numberedColumns = columnName.isRowVectorCharacterArray()
                && ArrayOf(columnName).asWideString() == L"numbered";
            if (!numberedColumns && columnName.isCell()) {
                const ArrayOf* elements = (const ArrayOf*)columnName.dataPointer();
                for (indexType k = 0; k < columnName.elementCount(); ++k) {
                    ArrayOf element = elements[k];
                    headers << ((element.isRowVectorCharacterArray()
                                    || element.isScalarStringArray())
                            ? wstringToQString(ArrayOf(element).asWideString())
                            : QString());
                }
                table->setHorizontalHeaderLabels(headers);
            }
            ArrayOf columnEditable
                = model->findArrayOfProperty(GO_COLUMN_EDITABLE_PROPERTY_NAME_STR);
            auto isColumnEditable = [&columnEditable](indexType column) -> bool {
                if (columnEditable.isEmpty() || !columnEditable.isLogical()) {
                    return false;
                }
                const logical* values = (const logical*)columnEditable.dataPointer();
                if (columnEditable.isScalar()) {
                    return values[0] != 0;
                }
                if (column < columnEditable.elementCount()) {
                    return values[column] != 0;
                }
                return false;
            };
            for (indexType row = 0; row < rows; ++row) {
                for (indexType column = 0; column < columns; ++column) {
                    QTableWidgetItem* item
                        = new QTableWidgetItem(wstringToQString(cellTextOf(data, row, column)));
                    item->setTextAlignment((data.isNumeric() || data.isLogical())
                            ? (Qt::AlignRight | Qt::AlignVCenter)
                            : (Qt::AlignLeft | Qt::AlignVCenter));
                    if (!isColumnEditable(column)) {
                        item->setFlags(item->flags() & ~Qt::ItemIsEditable);
                    }
                    table->setItem((int)row, (int)column, item);
                }
            }
            // DisplayData mirrors Data (no sorting support yet)
            static_cast<GOArrayOfProperty*>(model->findProperty(GO_DISPLAY_DATA_PROPERTY_NAME_STR))
                ->data(data);
        }
        //=============================================================================
        void
        onCellChanged(int row, int column, const std::wstring& newText)
        {
            GOUITable* goTable = static_cast<GOUITable*>(model);
            if (!goTable->onUserCellEdited(row, column, newText)) {
                GOUIComponent::ProgrammaticGuard guard(model);
                refreshTableFromData();
            }
        }
        //=============================================================================
        void
        onSelectionChanged()
        {
            if (model->isProgrammaticChange()) {
                return;
            }
            QTableWidget* table = qobject_cast<QTableWidget*>(widget.data());
            if (!table) {
                return;
            }
            GOArrayOfProperty* selectionProperty = static_cast<GOArrayOfProperty*>(
                model->findProperty(GO_SELECTION_PROPERTY_NAME_STR));
            ArrayOf previousSelection = selectionProperty->data();

            QList<QTableWidgetItem*> selectedItems = table->selectedItems();
            std::wstring selectionType
                = model->findStringProperty(GO_SELECTION_TYPE_PROPERTY_NAME_STR);
            ArrayOf newSelection;
            if (selectionType == L"cell") {
                size_t count = (size_t)selectedItems.size();
                ArrayOf matrix = ArrayOf::doubleMatrix2d(count, 2);
                double* values = (double*)matrix.dataPointer();
                for (size_t k = 0; k < count; ++k) {
                    values[k] = (double)(selectedItems[(int)k]->row() + 1);
                    values[k + count] = (double)(selectedItems[(int)k]->column() + 1);
                }
                newSelection = matrix;
            } else {
                std::vector<double> indices;
                QSet<int> seen;
                for (QTableWidgetItem* item : selectedItems) {
                    int index = (selectionType == GO_PROPERTY_VALUE_ROW_STR) ? item->row()
                                                                             : item->column();
                    if (!seen.contains(index)) {
                        seen.insert(index);
                        indices.push_back((double)(index + 1));
                    }
                }
                newSelection = ArrayOf::doubleRowVector(indices);
            }
            selectionProperty->data(newSelection);

            wstringVector fieldNames = { L"Selection", L"PreviousSelection", L"SelectionType" };
            ArrayOfVector fieldValues;
            fieldValues << newSelection;
            fieldValues << previousSelection;
            fieldValues << ArrayOf::characterArray(selectionType);
            model->emitCallbackEvent(GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR,
                L"TableSelectionChangedData", L"SelectionChanged", fieldNames, fieldValues);
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUITableBinding(GOUIComponent* component)
{
    return new UITableBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
