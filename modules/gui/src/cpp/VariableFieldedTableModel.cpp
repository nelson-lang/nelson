//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableFieldedTableModel.h"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QRegularExpression>
#include <QtCore/QStringList>
#include <QtCore/QDebug>
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
VariableFieldedTableModel::VariableFieldedTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableAbstractTableModel(name, array, evaluator, parent)
{
    // Persistence timer setup is common to both fielded models; the derived
    // constructors only run the field-specific initialization afterwards.
    m_persistenceTimer = new QTimer(this);
    m_persistenceTimer->setSingleShot(true);
    m_persistenceTimer->setInterval(100);
    connect(m_persistenceTimer, &QTimer::timeout, this,
        &VariableFieldedTableModel::performBatchedPersistence);
}
//=============================================================================
void
VariableFieldedTableModel::initializeEditData()
{
    m_editData.clear();
    m_editData.resize(m_rows_display * m_cols_display);
}
//=============================================================================
void
VariableFieldedTableModel::updateFieldNames()
{
    m_fieldNames.clear();

    if (isCompatibleArrayType(m_array) && !m_array.isEmpty()) {
        stringVector fieldNames = rawFieldNames(m_array);
        for (const auto& fieldName : fieldNames) {
            m_fieldNames << QString::fromStdWString(utf8_to_wstring(fieldName));
        }
    }
}
//=============================================================================
int
VariableFieldedTableModel::rowCount(const QModelIndex& parent) const
{
    Q_UNUSED(parent);
    return m_rows_display;
}
//=============================================================================
QVariant
VariableFieldedTableModel::getDisplayValue(int row, int col) const
{
    return getCellDisplayValue(row, col);
}
//=============================================================================
bool
VariableFieldedTableModel::hasOriginalData() const
{
    return !m_array.isEmpty();
}
//=============================================================================
bool
VariableFieldedTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && row < m_rows && col >= 0 && col < m_cols;
}
//=============================================================================
bool
VariableFieldedTableModel::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    return expandArrayIfNeeded(row, col);
}
//=============================================================================
void
VariableFieldedTableModel::updateEditData(int row, int col, const QVariant& value)
{
    if (row >= 0 && row < m_rows_display && col >= 0 && col < m_cols_display) {
        int index = indexToFlat(row, col);
        if (index >= 0 && index < m_editData.size()) {
            m_editData[index] = value;
        }
    }
}
//=============================================================================
QString
VariableFieldedTableModel::fieldName(int fieldIndex) const
{
    if (fieldIndex >= 0 && fieldIndex < m_fieldNames.size()) {
        return m_fieldNames[fieldIndex];
    }
    return QString();
}
//=============================================================================
int
VariableFieldedTableModel::fieldIndex(const QString& fieldName) const
{
    return m_fieldNames.indexOf(fieldName);
}
//=============================================================================
void
VariableFieldedTableModel::persistChangesToContext()
{
    if (m_evaluator && !m_variableName.isEmpty()) {
        try {
            Context* context = m_evaluator->getContext();
            if (context) {
                // Writes target the base scope: the editor reads variables from the
                // base scope, so persisting elsewhere would desynchronize the view.
                context->getCurrentScope()->insertVariable(m_variableName.toStdString(), m_array);
            }
        } catch (const NelsonException& e) {
            emit errorOccurred(QString::fromStdWString(e.message()));
        }
    }
}
//=============================================================================
void
VariableFieldedTableModel::schedulePersistenceUpdate()
{
    m_pendingChanges = true;
    if (m_persistenceTimer && !m_persistenceTimer->isActive()) {
        m_persistenceTimer->start();
    }
}
//=============================================================================
void
VariableFieldedTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
    }
}
//=============================================================================
VariableAbstractTableModel::SelectionBounds
VariableFieldedTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
{
    SelectionBounds bounds;

    if (selectedIndexes.isEmpty()) {
        return bounds;
    }

    bounds.minRow = selectedIndexes.first().row();
    bounds.maxRow = selectedIndexes.first().row();
    bounds.minCol = selectedIndexes.first().column();
    bounds.maxCol = selectedIndexes.first().column();

    for (const QModelIndex& index : selectedIndexes) {
        bounds.minRow = std::min(bounds.minRow, index.row());
        bounds.maxRow = std::max(bounds.maxRow, index.row());
        bounds.minCol = std::min(bounds.minCol, index.column());
        bounds.maxCol = std::max(bounds.maxCol, index.column());
    }

    bounds.isValid = true;
    return bounds;
}
//=============================================================================
VariableAbstractTableModel::PasteData
VariableFieldedTableModel::parsePasteText(const QString& text) const
{
    PasteData data;

    if (text.isEmpty()) {
        return data;
    }

    QStringList lines = text.split(QRegularExpression("[\r\n]"), Qt::SkipEmptyParts);
    if (lines.isEmpty()) {
        return data;
    }

    data.rows = lines.size();
    data.cols = 0;

    for (const QString& line : lines) {
        QStringList cells = line.split('\t');
        data.cols = std::max((int)data.cols, (int)cells.size());
        data.cells.append(QVector<QString>(cells.begin(), cells.end()));
    }
    for (auto& row : data.cells) {
        while (row.size() < data.cols) {
            row.append(QString());
        }
    }

    data.isValid = true;
    return data;
}
//=============================================================================
bool
VariableFieldedTableModel::validatePasteData(
    const VariableAbstractTableModel::PasteData& data, int startRow, int startCol) const
{
    if (!data.isValid) {
        return false;
    }

    if (startRow < 0 || startCol < 0) {
        return false;
    }

    if (startRow + data.rows > MAX_PASTE_SIZE || startCol + data.cols > MAX_PASTE_SIZE) {
        return false;
    }

    return true;
}
//=============================================================================
bool
VariableFieldedTableModel::applyPasteData(
    const VariableAbstractTableModel::PasteData& data, int startRow, int startCol)
{
    if (!validatePasteData(data, startRow, startCol)) {
        return false;
    }

    try {
        expandDisplayIfNeeded(startRow + data.rows - 1, startCol + data.cols - 1);

        if (!expandArrayIfNeeded(startRow + data.rows - 1, startCol + data.cols - 1)) {
            return false;
        }

        for (int row = 0; row < data.rows; ++row) {
            for (int col = 0; col < data.cols; ++col) {
                int targetRow = startRow + row;
                int targetCol = startCol + col;

                if (targetRow < m_rows && targetCol < m_cols) {
                    QString cellText = data.cells[row][col];
                    ArrayOf parsedValue = parseInputToArrayOf(cellText, targetRow, targetCol);

                    if (!writePastedValue(targetRow, targetCol, parsedValue)) {
                        continue;
                    }

                    updateEditData(targetRow, targetCol, QVariant(cellText));
                }
            }
        }

        QModelIndex topLeft = createIndex(startRow, startCol);
        QModelIndex bottomRight = createIndex(startRow + data.rows - 1, startCol + data.cols - 1);
        emit dataChanged(topLeft, bottomRight);

        schedulePersistenceUpdate();
        emit modelChanged();

        return true;
    } catch (const NelsonException& e) {
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
int
VariableFieldedTableModel::indexToFlat(int row, int col) const
{
    return row * m_cols_display + col;
}
//=============================================================================
std::pair<int, int>
VariableFieldedTableModel::flatToIndex(int flatIndex) const
{
    if (m_cols_display == 0) {
        return { 0, 0 };
    }

    int row = flatIndex / m_cols_display;
    int col = flatIndex % m_cols_display;
    return { row, col };
}
//=============================================================================
bool
VariableFieldedTableModel::expandDisplay(int row, int col)
{
    return expandDisplayIfNeeded(row, col);
}
//=============================================================================
bool
VariableFieldedTableModel::resizeArray(int row, int col)
{
    return expandArrayIfNeeded(row, col);
}
//=============================================================================
void
VariableFieldedTableModel::resizeEditData(int oldRows, int oldCols)
{
    QVector<QVariant> newEditData(m_rows_display * m_cols_display);

    for (int row = 0; row < std::min(oldRows, m_rows_display); ++row) {
        for (int col = 0; col < std::min(oldCols, m_cols_display); ++col) {
            int oldIndex = row * oldCols + col;
            int newIndex = row * m_cols_display + col;

            if (oldIndex < m_editData.size() && newIndex < newEditData.size()) {
                newEditData[newIndex] = m_editData[oldIndex];
            }
        }
    }

    m_editData = newEditData;
}
//=============================================================================
QVariant
VariableFieldedTableModel::getCellDisplayValue(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        int index = indexToFlat(row, col);
        if (index >= 0 && index < m_editData.size()) {
            QVariant editValue = m_editData[index];
            if (editValue.isValid() && !editValue.toString().isEmpty()) {
                return editValue;
            }
        }
        return QVariant();
    }

    ArrayOf value = fieldValue(row, col);
    return formatStructForDisplay(value);
}
//=============================================================================
QVariant
VariableFieldedTableModel::getCellEditValue(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        int index = indexToFlat(row, col);
        if (index >= 0 && index < m_editData.size()) {
            return m_editData[index];
        }
        return QVariant();
    }

    ArrayOf value = fieldValue(row, col);
    return formatCellForEdit(value);
}
//=============================================================================
QVariant
VariableFieldedTableModel::formatStructForDisplay(const QVariant& cellData) const
{
    if (!cellData.isValid()) {
        return QString();
    }

    return cellData.toString();
}
//=============================================================================
ArrayOf
VariableFieldedTableModel::parseInputToArrayOf(const QString& input, int row, int col) const
{
    Q_UNUSED(row);
    Q_UNUSED(col);

    if (input.isEmpty()) {
        return ArrayOf::empty();
    }

    try {
        bool ok;
        double numValue = input.toDouble(&ok);
        if (ok) {
            return ArrayOf::doubleScalar(numValue);
        }

        QString lowerInput = input.toLower();
        if (lowerInput == "true" || lowerInput == "1") {
            return ArrayOf::logicalScalar(true);
        } else if (lowerInput == "false" || lowerInput == "0") {
            return ArrayOf::logicalScalar(false);
        }

        std::wstring wstr = utf8_to_wstring(input.toStdString());
        return ArrayOf::characterArray(wstr);
    } catch (const NelsonException&) {
        return ArrayOf::empty();
    }
}
//=============================================================================
QVariant
VariableFieldedTableModel::formatCellForEdit(const QVariant& cellData) const
{
    return cellData;
}
//=============================================================================
bool
VariableFieldedTableModel::isValidStructArray(const ArrayOf& array) const
{
    if (array.isEmpty()) {
        return true;
    }

    return isCompatibleArrayType(array);
}
//=============================================================================
bool
VariableFieldedTableModel::hasConsistentFields(const ArrayOf& array) const
{
    if (!isCompatibleArrayType(array)) {
        return false;
    }
    return true;
}
//=============================================================================
void
VariableFieldedTableModel::refreshFromArray(ArrayOf& value)
{
    if (!isValidStructArray(value)) {
        return;
    }

    beginResetModel();

    m_array = value;
    rebuildModelFromCurrentArray();

    endResetModel();

    emit modelChanged();
}
//=============================================================================
void
VariableFieldedTableModel::rebuildModelFromCurrentArray()
{
    initializeDimensions();
    initializeEditData();
    updateFieldNames();
}
//=============================================================================
bool
VariableFieldedTableModel::isStructureCompatible(const ArrayOf& value)
{
    if (!isValidStructArray(value)) {
        return false;
    }

    if (isCompatibleArrayType(m_array) && isCompatibleArrayType(value)) {
        stringVector currentFields = m_array.fieldNames();
        stringVector newFields = value.fieldNames();

        if (currentFields.size() != newFields.size()) {
            return false;
        }

        for (size_t i = 0; i < currentFields.size(); ++i) {
            if (currentFields[i] != newFields[i]) {
                return false;
            }
        }
    }

    return true;
}
//=============================================================================
void
VariableFieldedTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
}
//=============================================================================
bool
VariableFieldedTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    return pasteDataFromClipboard(startRow, startCol, text);
}
//=============================================================================
bool
VariableFieldedTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    PasteData data = parsePasteText(text);
    if (!data.isValid) {
        return false;
    }

    return validatePasteData(data, startRow, startCol);
}
//=============================================================================
QSize
VariableFieldedTableModel::getPasteSize(const QString& text) const
{
    PasteData data = parsePasteText(text);
    if (!data.isValid) {
        return QSize(0, 0);
    }

    return QSize(data.cols, data.rows);
}
//=============================================================================
bool
VariableFieldedTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return false;
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    return bounds.isValid;
}
//=============================================================================
bool
VariableFieldedTableModel::shouldDisplayAsTable() const
{
    return isCompatibleArrayType(m_array) && !m_array.isEmpty();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
