//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableTypeDefaultTableModel.h"
#include <QtCore/QStringList>
#include <QtCore/QRegularExpression>
#include <QtGui/QColor>
#include <QtCore/QSize>
#include <QtCore/QtGlobal>
#include <algorithm>
#include <cstring>

namespace Nelson {

VariableTypeDefaultTableModel::VariableTypeDefaultTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : QAbstractTableModel(parent)
    , m_array(std::move(array))
    , m_variableName(name)
    , m_evaluator(evaluator)
{
    if (!m_evaluator) {
        qWarning() << "VariableTypeDefaultTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableTypeDefaultTableModel: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
}

int
VariableTypeDefaultTableModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_rows_display;
}

int
VariableTypeDefaultTableModel::columnCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_cols_display;
}

QVariant
VariableTypeDefaultTableModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isValidPosition(index.row(), index.column())) {
        return QVariant();
    }

    const int row = index.row();
    const int col = index.column();

    switch (role) {
    case Qt::DisplayRole:
        return getDisplayValue(row, col);

    case Qt::TextAlignmentRole:
        return static_cast<int>(Qt::AlignCenter);

    case Qt::BackgroundRole:
        // Single cell with light background to indicate read-only text
        return QColor(Qt::lightGray).lighter(140);

    case Qt::ToolTipRole:
        return tr("Text representation of variable '%1' - read-only").arg(m_variableName);

    case Qt::ForegroundRole:
        // Use a slightly muted text color to indicate read-only status
        return QColor(Qt::darkGray);

    default:
        return QVariant();
    }
}

bool
VariableTypeDefaultTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    Q_UNUSED(index)
    Q_UNUSED(value)
    Q_UNUSED(role)

    // Always return false - this model is read-only
    emit errorOccurred(tr("This variable type is read-only and cannot be edited"));
    return false;
}

Qt::ItemFlags
VariableTypeDefaultTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    // Only allow selection and enable, no editing
    return Qt::ItemIsSelectable | Qt::ItemIsEnabled;
}

QVariant
VariableTypeDefaultTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole) {
        return QString::number(section + 1);
    }
    return QVariant();
}

void
VariableTypeDefaultTableModel::refreshFromArray()
{
    beginResetModel();
    initializeDimensions();
    initializeEditData();
    endResetModel();
    emit modelChanged();
}

bool
VariableTypeDefaultTableModel::pasteData(int startRow, int startCol, const QString& text)
{
    Q_UNUSED(startRow)
    Q_UNUSED(startCol)
    Q_UNUSED(text)

    // Paste not supported for read-only model
    emit errorOccurred(tr("Paste operation not supported - this variable is read-only"));
    return false;
}

bool
VariableTypeDefaultTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    Q_UNUSED(startRow)
    Q_UNUSED(startCol)
    Q_UNUSED(text)

    // Never allow paste operations
    return false;
}

QSize
VariableTypeDefaultTableModel::getPasteSize(const QString& text) const
{
    Q_UNUSED(text)

    // No paste operations supported
    return QSize(0, 0);
}

// Private implementation methods

void
VariableTypeDefaultTableModel::initializeDimensions()
{
    // Always display as single cell (1x1) containing text representation
    m_rows = 1;
    m_cols = 1;
    m_rows_display = 1;
    m_cols_display = 1;
}

void
VariableTypeDefaultTableModel::initializeEditData()
{
    // For read-only model, we don't need edit data tracking
    // Keep it minimal
    m_editData.clear();
}

bool
VariableTypeDefaultTableModel::validateArrayState() const
{
    return !m_variableName.isEmpty();
}

QVariant
VariableTypeDefaultTableModel::getDisplayValue(int row, int col) const
{
    // Only one cell (0,0) - display text representation of the entire variable
    if (row != 0 || col != 0) {
        return QVariant();
    }

    // Generate text representation based on array type and content
    QString displayText = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\ndddddddd\nddddddddddddddd\nddd";

    return displayText;
}

bool
VariableTypeDefaultTableModel::hasOriginalData() const
{
    return m_array.getDataPointer() != nullptr;
}

bool
VariableTypeDefaultTableModel::isValidPosition(int row, int col) const
{
    // Only position (0,0) is valid for single-cell display
    return row == 0 && col == 0;
}

bool
VariableTypeDefaultTableModel::expandDisplayIfNeeded(int row, int col)
{
    Q_UNUSED(row)
    Q_UNUSED(col)

    // No expansion needed for read-only model
    return false;
}

bool
VariableTypeDefaultTableModel::resizeArrayIfNeeded(int row, int col)
{
    Q_UNUSED(row)
    Q_UNUSED(col)

    // No resizing for read-only model
    return false;
}

bool
VariableTypeDefaultTableModel::setArrayValue(int row, int col, double value)
{
    Q_UNUSED(row)
    Q_UNUSED(col)
    Q_UNUSED(value)

    // No setting values in read-only model
    return false;
}

bool
VariableTypeDefaultTableModel::setArrayValue(int row, int col, double realpart, double imagpart)
{
    Q_UNUSED(row)
    Q_UNUSED(col)
    Q_UNUSED(realpart)
    Q_UNUSED(imagpart)

    // No setting values in read-only model
    return false;
}

void
VariableTypeDefaultTableModel::updateEditData(int row, int col, const QVariant& value)
{
    Q_UNUSED(row)
    Q_UNUSED(col)
    Q_UNUSED(value)

    // No edit data updates in read-only model
}

bool
VariableTypeDefaultTableModel::parseNumericValue(const QString& text, double& value) const
{
    Q_UNUSED(text)
    Q_UNUSED(value)

    // No parsing needed for read-only model
    return false;
}

QString
VariableTypeDefaultTableModel::formatDisplayValue(std::complex<double> value) const
{
    return QString();
}

QString
VariableTypeDefaultTableModel::formatDisplayValue(double value) const
{
    return QString();
}

void
VariableTypeDefaultTableModel::persistChangesToContext()
{
    // No persistence needed for read-only model
}

int
VariableTypeDefaultTableModel::indexToFlat(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return -1;
    }
    return row + col * m_rows_display;
}

std::pair<int, int>
VariableTypeDefaultTableModel::flatToIndex(int flatIndex) const
{
    if (flatIndex < 0 || m_rows_display <= 0) {
        return { -1, -1 };
    }

    const int row = flatIndex % m_rows_display;
    const int col = flatIndex / m_rows_display;

    return { row, col };
}

VariableTypeDefaultTableModel::PasteData
VariableTypeDefaultTableModel::parsePasteText(const QString& text) const
{
    Q_UNUSED(text)

    // No paste parsing for read-only model
    return PasteData();
}

bool
VariableTypeDefaultTableModel::validatePasteData(
    const PasteData& data, int startRow, int startCol) const
{
    Q_UNUSED(data)
    Q_UNUSED(startRow)
    Q_UNUSED(startCol)

    // No paste validation for read-only model
    return false;
}

bool
VariableTypeDefaultTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
{
    Q_UNUSED(data)
    Q_UNUSED(startRow)
    Q_UNUSED(startCol)

    // No paste application for read-only model
    return false;
}

void
VariableTypeDefaultTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
    emit modelChanged();
}

}
