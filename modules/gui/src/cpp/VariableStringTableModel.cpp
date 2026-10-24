//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableStringTableModel.h"
#include "EvaluateCommand.hpp"
#include "ClassName.hpp"
#include <QtCore/QStringList>
#include <QtCore/QRegularExpression>
#include <QtGui/QColor>
#include <QtGui/QFont>
#include <QtCore/QSize>
#include <QtCore/QDebug>
#include <QtCore/QtGlobal>
#include <algorithm>
#include <cstring>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
VariableStringTableModel::VariableStringTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableNumericTableModel(name, array, evaluator, parent)
{
    if (!m_evaluator) {
        qWarning() << "VariableStringTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableStringTableModel: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
    clearUndoState();
}
//=============================================================================
QVariant
VariableStringTableModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isValidPosition(index.row(), index.column())) {
        return QVariant();
    }

    const int row = index.row();
    const int col = index.column();

    QVariant displayValue = getDisplayValue(row, col);

    switch (role) {
    case Qt::DisplayRole: {
        const char* t = displayValue.typeName();
        if (t) {
            if (strcmp(displayValue.typeName(), "QString") == 0) {
                return displayValue;
            }
            return QVariant(wstringToQString(L"<missing>"));
        }

    } break;
    case Qt::EditRole:
        if (displayValue.canConvert<int>() && displayValue.toInt() == 0) {
            return QVariant(wstringToQString(L""));
        }
        return displayValue;

    case Qt::TextAlignmentRole:
        return static_cast<int>(Qt::AlignCenter);

    case Qt::BackgroundRole:
        if (row < m_rows && col < m_cols && hasOriginalData()) {
            return QColor(Qt::lightGray).lighter(120);
        }
        return QVariant();

    case Qt::FontRole:
        if (displayValue.canConvert<int>() && displayValue.toInt() == 0) {
            QFont italicFont;
            italicFont.setItalic(true);
            return italicFont;
        }
        break;

    case Qt::ToolTipRole:
        if (row >= m_rows || col >= m_cols) {
            return tr("New cell - enter value to expand array");
        }
        return QVariant();

    default:
        return QVariant();
    }
    return QVariant();
}
//=============================================================================
QVariant
VariableStringTableModel::getDisplayValue(int row, int col) const
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return m_editData[idx];
    }

    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const ArrayOf* ptr = static_cast<const ArrayOf*>(m_array.dataPointer());
        const int arrayIdx = row + col * m_rows;
        if (ptr) {
            if (ptr[arrayIdx].isCharacterArray()) {
                return QVariant(wstringToQString(ptr[arrayIdx].asWideString()));
            } else {
                return QVariant::fromValue(int(0));
            }
        }
    }
    return QVariant();
}
//=============================================================================
bool
VariableStringTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || role != Qt::EditRole) {
        return false;
    }
    saveCurrentStateForUndo();

    const int row = index.row();
    const int col = index.column();

    if (!isValidPosition(row, col)) {
        return false;
    }

    if (!m_array.isStringArray()) {
        emit errorOccurred(tr("Can only edit string arrays"));
        return false;
    }

    if (m_array.isEmpty()) {
        Dimensions dims(1, 1);
        m_array.resize(dims);
        ArrayOf* ptr = (ArrayOf*)(m_array.dataPointer());
        ptr[0] = ArrayOf::doubleScalar(std::nan(""));
    }

    std::wstring item;
    bool isMissing = false;
    if (value.canConvert<int>() && value.toInt() == 0) {
        isMissing = true;
    } else {
        item = value.toString().toStdWString();
    }

    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    if (isMissing) {
        if (!setArrayMissingValue(row, col)) {
            return false;
        }
    } else {
        if (!setArrayValue(row, col, item)) {
            return false;
        }
    }

    updateEditData(row, col, value);

    m_pendingChanges = true;
    schedulePersistenceUpdate();

    emit dataChanged(index, index);

    return true;
}
//=============================================================================
bool
VariableStringTableModel::resizeArray(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const size_t newSize = static_cast<size_t>(newRows) * newCols;

    try {
        Dimensions newDims(newRows, newCols);

        const size_t oldSize = static_cast<size_t>(m_rows) * m_cols;

        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, newDims.elementCount());
        ArrayOf newArray = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);
        ArrayOf* newData = (ArrayOf*)newArray.dataPointer();
        for (size_t k = 0; k < newDims.elementCount(); ++k) {
            newData[k] = ArrayOf::doubleScalar(std::nan(""));
        }
        if (const ArrayOf* oldData = (const ArrayOf*)m_array.dataPointer()) {
            for (int c = 0; c < m_cols; ++c) {
                const size_t oldColStart = static_cast<size_t>(c) * m_rows;
                const size_t newColStart = static_cast<size_t>(c) * newRows;
                for (int r = 0; r < m_rows; ++r) {
                    newData[newColStart + r] = oldData[oldColStart + r];
                }
            }
        }

        m_array = std::move(newArray);

        m_rows = newRows;
        m_cols = newCols;

        return true;

    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to resize array: %1").arg(e.what()));
        return false;
    }
}
//=============================================================================
Qt::ItemFlags
VariableStringTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;

    if (m_array.isStringArray()) {
        flags |= Qt::ItemIsEditable;
    }

    return flags;
}
//=============================================================================
bool
VariableStringTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    return pasteDataFromClipboard(startRow, startCol, text);
}
//=============================================================================
bool
VariableStringTableModel::hasOriginalData() const
{
    return m_array.isStringArray() && m_array.dataPointer() != nullptr;
}
//=============================================================================
bool
VariableStringTableModel::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const int newSize = newRows * newCols;

    try {
        auto newData = std::make_unique<double[]>(newSize);
        std::fill(newData.get(), newData.get() + newSize, 0.0);

        if (const double* oldData = static_cast<const double*>(m_array.dataPointer())) {
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

        if (double* arrayData = (double*)(m_array.dataPointer())) {
            std::memcpy(arrayData, newData.get(), sizeof(double) * newSize);
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
VariableStringTableModel::setArrayMissingValue(int row, int col)
{
    ArrayOf* data = (ArrayOf*)(m_array.dataPointer());
    if (!data) {
        return false;
    }

    const int idx = row + col * m_rows;
    data[idx] = ArrayOf::doubleScalar(std::nan(""));
    return true;
}
//=============================================================================
bool
VariableStringTableModel::setArrayValue(int row, int col, const std::wstring& value)
{
    ArrayOf* data = (ArrayOf*)(m_array.dataPointer());
    if (!data) {
        return false;
    }

    const int idx = row + col * m_rows;
    data[idx] = ArrayOf::characterArray(value);
    return true;
}
//=============================================================================
bool
VariableStringTableModel::validatePasteData(const PasteData& data, int startRow, int startCol) const
{
    if (!data.isValid || startRow < 0 || startCol < 0) {
        return false;
    }

    const int endRow = startRow + data.rows - 1;
    const int endCol = startCol + data.cols - 1;

    if (endRow >= MAX_PASTE_SIZE || endCol >= MAX_PASTE_SIZE) {
        return false;
    }

    return true;
}
//=============================================================================
bool
VariableStringTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
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

    bool hasErrors = false;
    QString errorMessage;

    for (int r = 0; r < data.rows; ++r) {
        for (int c = 0; c < data.cols && c < data.cells[r].size(); ++c) {
            const int targetRow = startRow + r;
            const int targetCol = startCol + c;
            const QString& cellText = data.cells[r][c];

            if (cellText.isEmpty()) {
                continue;
            }

            if (setArrayValue(targetRow, targetCol, QStringTowstring(cellText))) {
                updateEditData(targetRow, targetCol, QVariant(cellText));
                if (!needsModelReset) {
                    changedIndices.append(index(targetRow, targetCol));
                }
            } else {
                hasErrors = true;
                if (errorMessage.isEmpty()) {
                    errorMessage = tr("Failed to set value at (%1, %2)")
                                       .arg(targetRow + 1)
                                       .arg(targetCol + 1);
                }
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

    if (hasErrors) {
        emit errorOccurred(errorMessage);
    }

    return !hasErrors;
}
//=============================================================================
bool
VariableStringTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_DCOMPLEX;
}
//=============================================================================
bool
VariableStringTableModel::insertRowAt(int row)
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

        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.dataPointer());

        for (int c = 0; c < cols; ++c) {
            for (int r = 0; r < row; ++r) {
                dst[r + c * newRows] = src[r + c * m_rows];
            }

            dst[row + c * newRows] = ArrayOf::doubleScalar(std::nan(""));

            for (int r = row; r < m_rows; ++r) {
                dst[r + 1 + c * newRows] = src[r + c * m_rows];
            }
        }

        newArray = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);

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
VariableStringTableModel::insertColumnAt(int col)
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
        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.dataPointer());

        for (int c = 0; c < col; ++c) {
            for (int r = 0; r < rows; ++r) {
                dst[r + c * rows] = src[r + c * rows];
            }
        }

        for (int r = 0; r < rows; ++r) {
            dst[r + col * rows] = ArrayOf::doubleScalar(std::nan(""));
        }

        for (int c = col; c < m_cols; ++c) {
            for (int r = 0; r < rows; ++r) {
                dst[r + (c + 1) * rows] = src[r + c * rows];
            }
        }

        newArray = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);

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
VariableStringTableModel::deleteRowAt(int row)
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
        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.dataPointer());

        for (int c = 0; c < cols; ++c) {
            int k = 0;
            for (int r = 0; r < m_rows; ++r) {
                if (r == row) {
                    continue;
                }
                dst[k++ + c * newRows] = src[r + c * m_rows];
            }
        }

        newArray = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);

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
VariableStringTableModel::deleteColumnAt(int col)
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
        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.dataPointer());

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

        newArray = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);

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
VariableStringTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
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

    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, newDims.elementCount());
    newVar = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);

    for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
        for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
            QModelIndex idx = index(r, c);
            QVariant val = data(idx, Qt::DisplayRole);
            ptr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.rows()]
                = ArrayOf::characterArray(QStringTowstring(val.toString()));
        }
    }
    return newVar;
}
//=============================================================================
bool
VariableStringTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    return !selectedIndexes.isEmpty() && (m_array.dataClass() == NLS_STRING_ARRAY);
}
//=============================================================================
bool
VariableStringTableModel::shouldDisplayAsTable() const
{
    return (m_array.dataClass() == NLS_STRING_ARRAY) && !m_array.isSparse() && m_array.is2D();
}
//=============================================================================
bool
VariableStringTableModel::replaceSelectedWithMissing(const QModelIndexList& selectedIndexes)
{
    if (selectedIndexes.isEmpty() || !m_evaluator) {
        return false;
    }

    try {
        ArrayOf* arrayData = (ArrayOf*)(m_array.dataPointer());
        if (!arrayData) {
            return false;
        }

        QVector<QModelIndex> changedIndices;
        changedIndices.reserve(selectedIndexes.size());

        for (const QModelIndex& index : selectedIndexes) {
            if (!index.isValid()) {
                continue;
            }

            int row = index.row();
            int col = index.column();

            if (row >= 0 && row < m_rows && col >= 0 && col < m_cols) {
                const int arrayIdx = row + col * m_rows;
                arrayData[arrayIdx] = ArrayOf::doubleScalar(std::nan(""));

                updateEditData(row, col, QVariant::fromValue(int(0)));
                changedIndices.append(index);
            }
        }

        persistChangesToContext();

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

            emit dataChanged(index(minRow, minCol), index(maxRow, maxCol));
            emit modelChanged();
        }
        return true;

    } catch (const std::exception&) {
        return false;
    } catch (...) {
        return false;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
