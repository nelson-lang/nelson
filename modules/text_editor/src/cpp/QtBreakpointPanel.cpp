//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QtBreakpointPanel.h"
#include <QtWidgets/QLabel>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QCheckBox>
#include <QtCore/QFileInfo>
#include "StringHelpers.hpp"
//=============================================================================
QtBreakpointPanel::QtBreakpointPanel(Nelson::Evaluator* eval, QWidget* parent)
    : QWidget(parent), nlsEvaluator(eval)
{
    QVBoxLayout* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(2, 2, 2, 2);
    mainLayout->setSpacing(2);

    QLabel* titleLabel = new QLabel(tr("Breakpoints"), this);
    QFont titleFont = titleLabel->font();
    titleFont.setBold(true);
    titleLabel->setFont(titleFont);
    mainLayout->addWidget(titleLabel);

    breakpointTable = new QTableWidget(this);
    breakpointTable->setColumnCount(4);
    breakpointTable->setHorizontalHeaderLabels(
        QStringList() << tr("Enabled") << tr("File") << tr("Function") << tr("Line"));
    breakpointTable->setAlternatingRowColors(true);
    breakpointTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    breakpointTable->setEditTriggers(QAbstractItemView::NoEditTriggers);
    breakpointTable->horizontalHeader()->setStretchLastSection(false);
    breakpointTable->horizontalHeader()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    breakpointTable->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Stretch);
    breakpointTable->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Stretch);
    breakpointTable->horizontalHeader()->setSectionResizeMode(3, QHeaderView::ResizeToContents);
    mainLayout->addWidget(breakpointTable);

    QHBoxLayout* buttonLayout = new QHBoxLayout();
    removeButton = new QPushButton(tr("Remove"), this);
    removeAllButton = new QPushButton(tr("Remove All"), this);
    buttonLayout->addWidget(removeButton);
    buttonLayout->addWidget(removeAllButton);
    buttonLayout->addStretch();
    mainLayout->addLayout(buttonLayout);

    connect(breakpointTable, &QTableWidget::cellClicked, this, &QtBreakpointPanel::onCellClicked);
    connect(breakpointTable, &QTableWidget::cellChanged, this, &QtBreakpointPanel::onCellChanged);
    connect(removeButton, &QPushButton::clicked, this, &QtBreakpointPanel::onRemoveSelected);
    connect(removeAllButton, &QPushButton::clicked, this, &QtBreakpointPanel::onRemoveAll);
}
//=============================================================================
void
QtBreakpointPanel::refreshBreakpoints()
{
    updatingTable = true;
    breakpointTable->setRowCount(0);

    if (!nlsEvaluator) {
        updatingTable = false;
        return;
    }

    // Get all breakpoints from evaluator
    std::vector<Nelson::Breakpoint> breakpoints = nlsEvaluator->getBreakpoints();

    for (const auto& bp : breakpoints) {
        // Skip step mode breakpoints (internal use)
        if (bp.stepMode) {
            continue;
        }

        int row = breakpointTable->rowCount();
        breakpointTable->insertRow(row);

        // Column 0: Enabled checkbox
        QWidget* checkboxWidget = new QWidget();
        QCheckBox* checkbox = new QCheckBox();
        checkbox->setChecked(bp.enabled);
        QHBoxLayout* layoutCheckbox = new QHBoxLayout(checkboxWidget);
        layoutCheckbox->addWidget(checkbox);
        layoutCheckbox->setAlignment(Qt::AlignCenter);
        layoutCheckbox->setContentsMargins(0, 0, 0, 0);
        breakpointTable->setCellWidget(row, 0, checkboxWidget);

        // Column 1: Filename
        QString filename = QString::fromStdWString(bp.filename);
        QTableWidgetItem* fileItem = new QTableWidgetItem(QFileInfo(filename).fileName());
        fileItem->setToolTip(filename);
        fileItem->setData(Qt::UserRole, filename); // Store full path
        breakpointTable->setItem(row, 1, fileItem);

        // Column 2: Function name
        QString functionName = QString::fromStdString(bp.functionName);
        QTableWidgetItem* funcItem = new QTableWidgetItem(functionName);
        breakpointTable->setItem(row, 2, funcItem);

        // Column 3: Line number
        QTableWidgetItem* lineItem = new QTableWidgetItem(QString::number(bp.line));
        lineItem->setData(Qt::UserRole, static_cast<int>(bp.line));
        breakpointTable->setItem(row, 3, lineItem);
    }

    updatingTable = false;
}
//=============================================================================
void
QtBreakpointPanel::clear()
{
    breakpointTable->setRowCount(0);
}
//=============================================================================
void
QtBreakpointPanel::onCellClicked(int row, int column)
{
    if (row < 0 || column < 0) {
        return;
    }

    // If clicking on enabled checkbox, handle it
    if (column == 0) {
        QWidget* widget = breakpointTable->cellWidget(row, 0);
        if (widget) {
            QCheckBox* checkbox = widget->findChild<QCheckBox*>();
            if (checkbox) {
                // Toggle enabled state
                // For now, we just track the UI state
                // In a full implementation, we'd need to extend Breakpoint struct
                // to support enabled/disabled and update the evaluator
            }
        }
        return;
    }

    // Navigate to breakpoint location
    QTableWidgetItem* fileItem = breakpointTable->item(row, 1);
    QTableWidgetItem* lineItem = breakpointTable->item(row, 3);

    if (fileItem && lineItem) {
        QString filename = fileItem->data(Qt::UserRole).toString();
        int line = lineItem->data(Qt::UserRole).toInt();
        emit breakpointClicked(filename, line);
    }
}
//=============================================================================
void
QtBreakpointPanel::onCellChanged(int row, int column)
{
    if (updatingTable) {
        return;
    }
    // Handle cell edits if we add editable columns later
}
//=============================================================================
void
QtBreakpointPanel::onRemoveSelected()
{
    int currentRow = breakpointTable->currentRow();
    if (currentRow < 0) {
        return;
    }

    QTableWidgetItem* fileItem = breakpointTable->item(currentRow, 1);
    QTableWidgetItem* lineItem = breakpointTable->item(currentRow, 3);

    if (fileItem && lineItem && nlsEvaluator) {
        QString filename = fileItem->data(Qt::UserRole).toString();
        int line = lineItem->data(Qt::UserRole).toInt();

        std::wstring wfilename = filename.toStdWString();
        nlsEvaluator->removeBreakpoint(wfilename, line);

        emit breakpointRemoved(filename, line);
        emit breakpointsChanged();
        refreshBreakpoints();
    }
}
//=============================================================================
void
QtBreakpointPanel::onRemoveAll()
{
    if (!nlsEvaluator) {
        return;
    }

    // Get all breakpoints and remove them
    std::vector<Nelson::Breakpoint> breakpoints = nlsEvaluator->getBreakpoints();
    for (const auto& bp : breakpoints) {
        if (!bp.stepMode) {
            nlsEvaluator->removeBreakpoint(bp.filename, bp.line);
        }
    }

    emit breakpointsChanged();
    refreshBreakpoints();
}
//=============================================================================
