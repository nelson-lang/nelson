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
#include <QtWidgets/QMessageBox>
#include <QtCore/QFileInfo>
#include "QStringConverter.hpp"
#include "QtTranslation.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static const int COLUMN_ENABLED = 0;
static const int COLUMN_FILE = 1;
static const int COLUMN_FUNCTION = 2;
static const int COLUMN_LINE = 3;
static const int COLUMN_CONDITION = 4;
//=============================================================================
QtBreakpointPanel::QtBreakpointPanel(Nelson::Evaluator* eval, QWidget* parent)
    : QWidget(parent), nlsEvaluator(eval)
{
    QVBoxLayout* mainLayout = new QVBoxLayout(this);
    mainLayout->setContentsMargins(2, 2, 2, 2);
    mainLayout->setSpacing(2);

    QLabel* titleLabel = new QLabel(TR("Breakpoints"), this);
    QFont titleFont = titleLabel->font();
    titleFont.setBold(true);
    titleLabel->setFont(titleFont);
    mainLayout->addWidget(titleLabel);

    breakpointTable = new QTableWidget(this);
    breakpointTable->setColumnCount(5);
    breakpointTable->setHorizontalHeaderLabels(QStringList()
        << TR("Enabled") << TR("File") << TR("Function") << TR("Line") << TR("Condition"));
    breakpointTable->setAlternatingRowColors(true);
    breakpointTable->setSelectionBehavior(QAbstractItemView::SelectRows);
    breakpointTable->setEditTriggers(QAbstractItemView::DoubleClicked);
    breakpointTable->horizontalHeader()->setStretchLastSection(true);
    breakpointTable->horizontalHeader()->setSectionResizeMode(
        COLUMN_ENABLED, QHeaderView::ResizeToContents);
    breakpointTable->horizontalHeader()->setSectionResizeMode(COLUMN_FILE, QHeaderView::Stretch);
    breakpointTable->horizontalHeader()->setSectionResizeMode(
        COLUMN_FUNCTION, QHeaderView::ResizeToContents);
    breakpointTable->horizontalHeader()->setSectionResizeMode(
        COLUMN_LINE, QHeaderView::ResizeToContents);
    mainLayout->addWidget(breakpointTable);

    QHBoxLayout* buttonLayout = new QHBoxLayout();
    removeButton = new QPushButton(TR("Clear"), this);
    removeAllButton = new QPushButton(TR("Clear All"), this);
    enableAllButton = new QPushButton(TR("Enable All"), this);
    disableAllButton = new QPushButton(TR("Disable All"), this);
    buttonLayout->addWidget(removeButton);
    buttonLayout->addWidget(removeAllButton);
    buttonLayout->addWidget(enableAllButton);
    buttonLayout->addWidget(disableAllButton);
    buttonLayout->addStretch();
    mainLayout->addLayout(buttonLayout);

    QHBoxLayout* errorLayout = new QHBoxLayout();
    pauseOnErrorsCheck = new QCheckBox(TR("Pause on Errors"), this);
    pauseOnErrorsCheck->setToolTip(TR("dbstop if error"));
    pauseOnCaughtErrorsCheck = new QCheckBox(TR("Pause on Caught Errors"), this);
    pauseOnCaughtErrorsCheck->setToolTip(TR("dbstop if caught error"));
    errorLayout->addWidget(pauseOnErrorsCheck);
    errorLayout->addWidget(pauseOnCaughtErrorsCheck);
    errorLayout->addStretch();
    mainLayout->addLayout(errorLayout);

    connect(breakpointTable, &QTableWidget::cellClicked, this, &QtBreakpointPanel::onCellClicked);
    connect(breakpointTable, &QTableWidget::cellChanged, this, &QtBreakpointPanel::onCellChanged);
    connect(removeButton, &QPushButton::clicked, this, &QtBreakpointPanel::onRemoveSelected);
    connect(removeAllButton, &QPushButton::clicked, this, &QtBreakpointPanel::onRemoveAll);
    connect(enableAllButton, &QPushButton::clicked, this, &QtBreakpointPanel::onEnableAll);
    connect(disableAllButton, &QPushButton::clicked, this, &QtBreakpointPanel::onDisableAll);
    connect(
        pauseOnErrorsCheck, &QCheckBox::toggled, this, &QtBreakpointPanel::onPauseOnErrorsToggled);
    connect(pauseOnCaughtErrorsCheck, &QCheckBox::toggled, this,
        &QtBreakpointPanel::onPauseOnCaughtErrorsToggled);
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
    for (const Nelson::Breakpoint& bp : nlsEvaluator->getUserBreakpoints()) {
        int row = breakpointTable->rowCount();
        breakpointTable->insertRow(row);

        QTableWidgetItem* enabledItem = new QTableWidgetItem();
        enabledItem->setFlags(Qt::ItemIsUserCheckable | Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        enabledItem->setCheckState(bp.enabled ? Qt::Checked : Qt::Unchecked);
        breakpointTable->setItem(row, COLUMN_ENABLED, enabledItem);

        QString filename = Nelson::wstringToQString(bp.filename);
        QTableWidgetItem* fileItem = new QTableWidgetItem(QFileInfo(filename).fileName());
        fileItem->setToolTip(filename);
        fileItem->setData(Qt::UserRole, filename);
        fileItem->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        breakpointTable->setItem(row, COLUMN_FILE, fileItem);

        QTableWidgetItem* funcItem = new QTableWidgetItem(QString::fromStdString(bp.functionName));
        funcItem->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        breakpointTable->setItem(row, COLUMN_FUNCTION, funcItem);

        QTableWidgetItem* lineItem = new QTableWidgetItem(QString::number(bp.line));
        lineItem->setData(Qt::UserRole, static_cast<int>(bp.line));
        lineItem->setFlags(Qt::ItemIsEnabled | Qt::ItemIsSelectable);
        breakpointTable->setItem(row, COLUMN_LINE, lineItem);

        QTableWidgetItem* conditionItem
            = new QTableWidgetItem(Nelson::wstringToQString(bp.condition));
        conditionItem->setToolTip(TR("Double-click to set or modify the condition."));
        breakpointTable->setItem(row, COLUMN_CONDITION, conditionItem);
    }
    pauseOnErrorsCheck->setChecked(nlsEvaluator->getStopOnError());
    pauseOnCaughtErrorsCheck->setChecked(nlsEvaluator->getStopOnCaughtError());
    updatingTable = false;
}
//=============================================================================
void
QtBreakpointPanel::clear()
{
    updatingTable = true;
    breakpointTable->setRowCount(0);
    updatingTable = false;
}
//=============================================================================
bool
QtBreakpointPanel::rowLocation(int row, QString& filename, int& line) const
{
    QTableWidgetItem* fileItem = breakpointTable->item(row, COLUMN_FILE);
    QTableWidgetItem* lineItem = breakpointTable->item(row, COLUMN_LINE);
    if (fileItem == nullptr || lineItem == nullptr) {
        return false;
    }
    filename = fileItem->data(Qt::UserRole).toString();
    line = lineItem->data(Qt::UserRole).toInt();
    return true;
}
//=============================================================================
void
QtBreakpointPanel::onCellClicked(int row, int column)
{
    if (row < 0 || column < 0 || column == COLUMN_ENABLED || column == COLUMN_CONDITION) {
        return;
    }
    QString filename;
    int line = 0;
    if (rowLocation(row, filename, line)) {
        emit breakpointClicked(filename, line);
    }
}
//=============================================================================
void
QtBreakpointPanel::onCellChanged(int row, int column)
{
    if (updatingTable || nlsEvaluator == nullptr) {
        return;
    }
    QString filename;
    int line = 0;
    if (!rowLocation(row, filename, line)) {
        return;
    }
    std::wstring wfilename = Nelson::QStringTowstring(filename);
    if (column == COLUMN_ENABLED) {
        QTableWidgetItem* item = breakpointTable->item(row, COLUMN_ENABLED);
        if (item != nullptr) {
            nlsEvaluator->setBreakpointEnabled(
                wfilename, static_cast<size_t>(line), item->checkState() == Qt::Checked);
            emit breakpointsChanged();
        }
        return;
    }
    if (column == COLUMN_CONDITION) {
        QTableWidgetItem* item = breakpointTable->item(row, COLUMN_CONDITION);
        if (item == nullptr) {
            return;
        }
        std::wstring errorMessage;
        if (!nlsEvaluator->setBreakpointCondition(wfilename, static_cast<size_t>(line),
                Nelson::QStringTowstring(item->text()), errorMessage)) {
            QMessageBox::warning(
                this, TR("Set/Modify Condition"), Nelson::wstringToQString(errorMessage));
            refreshBreakpoints();
            return;
        }
        emit breakpointsChanged();
    }
}
//=============================================================================
void
QtBreakpointPanel::onRemoveSelected()
{
    int currentRow = breakpointTable->currentRow();
    if (currentRow < 0 || nlsEvaluator == nullptr) {
        return;
    }
    QString filename;
    int line = 0;
    if (rowLocation(currentRow, filename, line)) {
        nlsEvaluator->removeBreakpoint(Nelson::QStringTowstring(filename), line);
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
    // Clear All Breakpoints also removes the error breakpoints
    nlsEvaluator->clearBreakpoints();
    emit breakpointsChanged();
    refreshBreakpoints();
}
//=============================================================================
void
QtBreakpointPanel::onEnableAll()
{
    if (!nlsEvaluator) {
        return;
    }
    nlsEvaluator->setAllBreakpointsEnabled(true);
    emit breakpointsChanged();
    refreshBreakpoints();
}
//=============================================================================
void
QtBreakpointPanel::onDisableAll()
{
    if (!nlsEvaluator) {
        return;
    }
    nlsEvaluator->setAllBreakpointsEnabled(false);
    emit breakpointsChanged();
    refreshBreakpoints();
}
//=============================================================================
void
QtBreakpointPanel::onPauseOnErrorsToggled(bool checked)
{
    if (updatingTable || !nlsEvaluator) {
        return;
    }
    nlsEvaluator->setStopOnError(checked);
    emit breakpointsChanged();
}
//=============================================================================
void
QtBreakpointPanel::onPauseOnCaughtErrorsToggled(bool checked)
{
    if (updatingTable || !nlsEvaluator) {
        return;
    }
    nlsEvaluator->setStopOnCaughtError(checked);
    emit breakpointsChanged();
}
//=============================================================================
