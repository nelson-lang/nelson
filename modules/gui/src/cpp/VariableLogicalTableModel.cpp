//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableLogicalTableModel.h"
#include "EvaluateCommand.hpp"
#include <QtCore/QStringList>
#include <QtCore/QRegularExpression>
#include <QtGui/QColor>
#include <QtCore/QSize>
#include <QtCore/QDebug>
#include <QtCore/QtGlobal>
#include <algorithm>
#include <cstring>
#include "ClassName.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
VariableLogicalTableModel::VariableLogicalTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableNumericTableModel(name, array, evaluator, parent)
{
    if (!m_evaluator) {
        qWarning() << "VariableLogicalTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableLogicalTableModel: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
    clearUndoState();
}
//=============================================================================
QVariant
VariableLogicalTableModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isValidPosition(index.row(), index.column())) {
        return QVariant();
    }

    const int row = index.row();
    const int col = index.column();

    switch (role) {
    case Qt::DisplayRole:
    case Qt::EditRole:
        return getDisplayValue(row, col);

    case Qt::TextAlignmentRole:
        return static_cast<int>(Qt::AlignRight);

    case Qt::BackgroundRole:
        if (row < m_rows && col < m_cols && hasOriginalData()) {
            return QColor(Qt::lightGray).lighter(120);
        }
        return QVariant();

    case Qt::ToolTipRole:
        if (row >= m_rows || col >= m_cols) {
            return tr("New cell - enter value to expand array");
        }
        return QVariant();

    default:
        return QVariant();
    }
}
//=============================================================================
bool
VariableLogicalTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || role != Qt::EditRole) {
        return false;
    }

    const int row = index.row();
    const int col = index.column();

    if (!isValidPosition(row, col)) {
        return false;
    }

    if (!m_array.isLogical()) {
        emit errorOccurred(tr("Can only edit logical arrays"));
        return false;
    }

    saveCurrentStateForUndo();

    std::wstring command = value.toString().toStdWString();
    ArrayOfVector results;
    try {
        results = EvaluateCommand(m_evaluator, 1, command, L"");
    } catch (NelsonException&) {
    }
    if (results.size() != 1) {
        emit errorOccurred(tr("Invalid command or result type"));
        return false;
    }

    ArrayOf resultArray = results[0];
    if (!resultArray.isScalar() || !(resultArray.isNumeric() || resultArray.isLogical())) {
        emit errorOccurred(tr("Command must return a logical value"));
        return false;
    }

    resultArray.promoteType(NLS_LOGICAL);

    if (m_array.isEmpty()) {
        m_array = ArrayOf::logicalScalar(false);
    }
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    if (!setArrayValue(row, col, resultArray)) {
        return false;
    }

    logical numericValue = resultArray.asLogicalScalar();
    updateEditData(row, col, QVariant(formatDisplayValue(numericValue)));

    m_pendingChanges = true;
    schedulePersistenceUpdate();

    emit dataChanged(index, index);

    return true;
}
//=============================================================================
bool
VariableLogicalTableModel::resizeArray(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const size_t newSize = static_cast<size_t>(newRows) * newCols;

    const size_t MAX_ELEMENTS = 50000000; // ~400MB for logical
    if (newSize > MAX_ELEMENTS) {
        return false;
    }

    try {
        const NelsonType cls = m_array.dataClass();
        if (cls != NLS_LOGICAL) {
            return false;
        }

        const size_t oldSize = static_cast<size_t>(m_rows) * m_cols;
        const size_t elementSize = 1;
        const size_t totalNewSize = newSize * elementSize;

        Dimensions newDims(newRows, newCols);
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(cls, newDims.elementCount());
        ArrayOf newArray(cls, newDims, ptr);
        logical* newData = (logical*)newArray.dataPointer();

        std::fill(newData, newData + totalNewSize, 0.0);

        if (const logical* oldData = (const logical*)m_array.dataPointer()) {
            for (int c = 0; c < m_cols; ++c) {
                const size_t oldColStart = static_cast<size_t>(c) * m_rows * elementSize;
                const size_t newColStart = static_cast<size_t>(c) * newRows * elementSize;
                std::memcpy(newData + newColStart, oldData + oldColStart,
                    sizeof(logical) * m_rows * elementSize);
            }
        }

        m_array = std::move(newArray);
        m_rows = newRows;
        m_cols = newCols;
        return true;

    } catch (const std::exception&) {
        return false;
    }
}
//=============================================================================
Qt::ItemFlags
VariableLogicalTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;

    if (m_array.isLogical()) {
        flags |= Qt::ItemIsEditable;
    }

    return flags;
}
//=============================================================================
bool
VariableLogicalTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    if (text.isEmpty()) {
        return false;
    }

    const PasteData pasteData = parsePasteText(text);
    if (!pasteData.isValid) {
        emit errorOccurred(tr("Invalid paste data format"));
        return false;
    }

    if (!validatePasteData(pasteData, startRow, startCol)) {
        return false;
    }

    return applyPasteData(pasteData, startRow, startCol);
}
//=============================================================================
QVariant
VariableLogicalTableModel::getDisplayValue(int row, int col) const
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return m_editData[idx];
    }

    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const logical* ptr = static_cast<const logical*>(m_array.dataPointer());
        const int arrayIdx = row + col * m_rows;
        if (ptr) {
            return QVariant(formatDisplayValue(ptr[arrayIdx]));
        }
    }
    return QVariant();
}
//=============================================================================
bool
VariableLogicalTableModel::hasOriginalData() const
{
    return m_array.isLogical() && m_array.dataPointer() != nullptr;
}
//=============================================================================
bool
VariableLogicalTableModel::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const int newSize = newRows * newCols;

    try {
        auto newData = std::make_unique<logical[]>(newSize);
        std::fill(newData.get(), newData.get() + newSize, 0.0);

        if (const logical* oldData = static_cast<const logical*>(m_array.dataPointer())) {
            for (int c = 0; c < m_cols; ++c) {
                for (int r = 0; r < m_rows; ++r) {
                    const int oldIdx = r + c * m_rows;
                    const int newIdx = r + c * newRows;
                    newData[newIdx] = oldData[oldIdx];
                }
            }
        }

        Dimensions newDims(newRows, newCols);
        m_array.resize(newDims);

        if (logical* arrayData = (logical*)(m_array.dataPointer())) {
            std::memcpy(arrayData, newData.get(), sizeof(logical) * newSize);
        } else {
            emit errorOccurred(tr("Failed to get array data pointer after resize"));
            return false;
        }

        m_rows = newRows;
        m_cols = newCols;

        return true;

    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to resize array: %1").arg(e.what()));
        return false;
    }
}
//=============================================================================
bool
VariableLogicalTableModel::setArrayValue(int row, int col, ArrayOf& value)
{
    logical* data = (logical*)(m_array.dataPointer());
    if (!data) {
        emit errorOccurred(tr("No array data pointer available"));
        return false;
    }

    const int idx = row + col * m_rows;
    data[idx] = value.asLogicalScalar();
    return true;
}
//=============================================================================
QString
VariableLogicalTableModel::formatDisplayValue(logical value) const
{
    if (value != 0.0) {
        return "true";
    }
    return "false";
}
//=============================================================================
bool
VariableLogicalTableModel::validatePasteData(
    const PasteData& data, int startRow, int startCol) const
{
    if (!data.isValid || startRow < 0 || startCol < 0) {
        return false;
    }

    const int endRow = startRow + data.rows - 1;
    const int endCol = startCol + data.cols - 1;

    if (endRow >= MAX_PASTE_SIZE || endCol >= MAX_PASTE_SIZE) {
        return false;
    }

    for (const auto& row : data.cells) {
        for (const QString& cell : row) {
            std::wstring command = cell.toStdWString();
            ArrayOfVector results;
            try {
                results = EvaluateCommand(m_evaluator, 1, command, L"");
            } catch (NelsonException&) {
            }
            if (results.size() != 1) {
                return false;
            }
            ArrayOf resultArray = results[0];
            if (!resultArray.isScalar()) {
                return false;
            }
            if (!resultArray.isNumeric()) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
bool
VariableLogicalTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
{
    if (!data.isValid) {
        return false;
    }

    saveCurrentStateForUndo();

    const int endRow = startRow + data.rows - 1;
    const int endCol = startCol + data.cols - 1;

    bool needsModelReset = false;
    if (endRow >= m_rows_display || endCol >= m_cols_display || (data.rows * data.cols > 100)) {
        needsModelReset = true;
    }

    if (needsModelReset) {
        beginResetModel();
    }

    if (!expandDisplayIfNeeded(endRow, endCol) || !resizeArrayIfNeeded(endRow, endCol)) {
        if (needsModelReset) {
            endResetModel();
        }
        return false;
    }

    QVector<QModelIndex> changedIndices;
    changedIndices.reserve(qsizetype(data.rows * data.cols));

    QString errorMessage;

    for (int r = 0; r < data.rows; ++r) {
        for (int c = 0; c < data.cols && c < data.cells[r].size(); ++c) {
            const int targetRow = startRow + r;
            const int targetCol = startCol + c;
            const QString& cellText = data.cells[r][c];

            if (cellText.isEmpty()) {
                continue;
            }

            std::wstring command = cellText.toStdWString();
            ArrayOfVector results;
            try {
                results = EvaluateCommand(m_evaluator, 1, command, L"");
            } catch (NelsonException&) {
            }
            if (results.size() != 1) {
                return false;
            }
            ArrayOf value = results[0];
            if (!value.isScalar()) {
                return false;
            }
            if (!value.isNumeric() || !value.isLogical()) {
                return false;
            }
            value.promoteType(NLS_LOGICAL);
            if (setArrayValue(targetRow, targetCol, value)) {
                logical numericValue = value.asLogicalScalar();
                updateEditData(targetRow, targetCol, QVariant(formatDisplayValue(numericValue)));
            } else {
                return false;
            }
        }
    }

    persistChangesToContext();

    if (needsModelReset) {
        endResetModel();
        emit modelChanged();
    } else {
        if (!changedIndices.isEmpty()) {
            int minRow = changedIndices.first().row();
            int maxRow = minRow;
            int minCol = changedIndices.first().column();
            int maxCol = minCol;

            for (const QModelIndex& idx : changedIndices) {
                minRow = std::min(minRow, idx.row());
                maxRow = std::max(maxRow, idx.row());
                minCol = std::min(minCol, idx.column());
                maxCol = std::max(maxCol, idx.column());
            }

            // Use QAbstractTableModel's dataChanged (not our custom signal)
            QAbstractTableModel::dataChanged(index(minRow, minCol), index(maxRow, maxCol));
            emit modelChanged();
        }
    }

    return true;
}
//=============================================================================
bool
VariableLogicalTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.dataClass() == NLS_LOGICAL;
}
//=============================================================================
bool
VariableLogicalTableModel::insertRowAt(int row)
{
    if (row < 0 || row > m_rows) {
        return false;
    }

    saveCurrentStateForUndo();

    int newRows = m_rows + 1;
    int cols = m_cols;
    Dimensions newDims(newRows, cols);

    try {
        size_t total = (size_t)(newRows * cols);
        ArrayOf newArray;
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, total);
        logical* dst = ptr;
        logical* src = (logical*)(m_array.dataPointer());

        for (int c = 0; c < cols; ++c) {
            for (int r = 0; r < row; ++r) {
                dst[r + c * newRows] = src[r + c * m_rows];
            }

            dst[row + c * newRows] = 0.0;

            for (int r = row; r < m_rows; ++r) {
                dst[r + 1 + c * newRows] = src[r + c * m_rows];
            }
        }

        newArray = ArrayOf(NLS_LOGICAL, newDims, ptr);

        m_array = std::move(newArray);
        m_rows = newRows;
        expandDisplayIfNeeded(m_rows - 1, m_cols - 1);
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (...) {
        emit errorOccurred(tr("Failed to insert row."));
        return false;
    }
}
//=============================================================================
bool
VariableLogicalTableModel::insertColumnAt(int col)
{
    if (col < 0 || col > m_cols) {
        return false;
    }
    saveCurrentStateForUndo();

    int newCols = m_cols + 1;
    int rows = m_rows;
    Dimensions newDims(rows, newCols);

    try {
        size_t total = newCols * rows;
        ArrayOf newArray;
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, total);
        logical* dst = ptr;
        logical* src = (logical*)(m_array.dataPointer());

        for (int c = 0; c < col; ++c) {
            for (int r = 0; r < rows; ++r) {
                dst[r + c * rows] = src[r + c * rows];
            }
        }

        for (int r = 0; r < rows; ++r) {
            dst[r + col * rows] = 0.0;
        }

        for (int c = col; c < m_cols; ++c) {
            for (int r = 0; r < rows; ++r) {
                dst[r + (c + 1) * rows] = src[r + c * rows];
            }
        }

        newArray = ArrayOf(NLS_LOGICAL, newDims, ptr);

        m_array = std::move(newArray);
        m_cols = newCols;
        expandDisplayIfNeeded(m_rows - 1, m_cols - 1);
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (...) {
        return false;
    }
}
//=============================================================================
bool
VariableLogicalTableModel::deleteRowAt(int row)
{
    if (row < 0) {
        return false;
    }
    int newRows = m_rows - 1;
    if (newRows < 0) {
        return false;
    }

    saveCurrentStateForUndo();

    int cols = m_cols;
    Dimensions newDims(newRows, cols);

    try {
        size_t total = (size_t)(newRows * cols);
        ArrayOf newArray;
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, total);
        logical* dst = ptr;
        logical* src = (logical*)(m_array.dataPointer());

        for (int c = 0; c < cols; ++c) {
            int k = 0;
            for (int r = 0; r < m_rows; ++r) {
                if (r == row) {
                    continue;
                }
                dst[k++ + c * newRows] = src[r + c * m_rows];
            }
        }

        newArray = ArrayOf(NLS_LOGICAL, newDims, ptr);

        m_array = std::move(newArray);
        m_rows = newRows;
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (...) {
        emit errorOccurred(tr("Failed to delete row."));
        return false;
    }
}
//=============================================================================
bool
VariableLogicalTableModel::deleteColumnAt(int col)
{
    if (col < 0) {
        return false;
    }
    int newCols = m_cols - 1;
    if (newCols < 0) {
        return false;
    }

    saveCurrentStateForUndo();

    int rows = m_rows;
    Dimensions newDims(rows, newCols);

    try {
        size_t total = newCols * rows;
        ArrayOf newArray;
        logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, total);
        logical* dst = ptr;
        logical* src = (logical*)(m_array.dataPointer());

        int k = 0;
        for (int c = 0; c < m_cols; ++c) {
            if (c == col) {
                continue;
            }
            for (int r = 0; r < rows; ++r) {
                dst[r + k * rows] = src[r + c * rows];
            }
            ++k;
        }

        newArray = ArrayOf(NLS_LOGICAL, newDims, ptr);

        m_array = std::move(newArray);
        m_cols = newCols;
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (...) {
        emit errorOccurred(tr("Failed to delete column."));
        return false;
    }
}
//=============================================================================
ArrayOf
VariableLogicalTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
{
    if (!isValidSelectionForExtraction(selectedIndexes)) {
        return ArrayOf();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return ArrayOf();
    }

    Dimensions newDims(bounds.maxRow - bounds.minRow + 1, bounds.maxCol - bounds.minCol + 1);
    ArrayOf newVar;

    logical* ptr = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, newDims.elementCount());
    newVar = ArrayOf(NLS_LOGICAL, newDims, ptr);

    for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
        for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
            QModelIndex idx = index(r, c);
            QVariant val = data(idx, Qt::DisplayRole);
            logical num = val.toBool();
            ptr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.rows()] = num;
        }
    }

    return newVar;
}
//=============================================================================
bool
VariableLogicalTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    return !selectedIndexes.isEmpty() && (m_array.dataClass() == NLS_LOGICAL);
}
//=============================================================================
bool
VariableLogicalTableModel::shouldDisplayAsTable() const
{
    return (m_array.dataClass() == NLS_LOGICAL) && !m_array.isSparse() && m_array.is2D();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
