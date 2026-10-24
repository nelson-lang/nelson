//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableIntegerTableModel.h"
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
VariableIntegerTableModel::VariableIntegerTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableNumericTableModel(name, array, evaluator, parent)
{
    if (!m_evaluator) {
        qWarning() << "VariableIntegerTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableIntegerTableModel: Invalid array state for variable" << name;
    }
    referenceType = array.dataClass();
    initializeDimensions();
    initializeEditData();
    clearUndoState();
}
//=============================================================================
QVariant
VariableIntegerTableModel::data(const QModelIndex& index, int role) const
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
VariableIntegerTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || role != Qt::EditRole) {
        return false;
    }

    const int row = index.row();
    const int col = index.column();

    if (!isValidPosition(row, col)) {
        return false;
    }

    if (m_array.dataClass() != referenceType) {
        emit errorOccurred(tr("Can only edit integer arrays"));
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
    if (!resultArray.isScalar() || !resultArray.isNumeric()) {
        emit errorOccurred(tr("Command must return a single numeric value"));
        return false;
    }
    resultArray.promoteType(referenceType);

    if (m_array.isEmpty()) {
        void* ptr = (void*)ArrayOf::allocateArrayOf(referenceType, 1, stringVector(), true);
        Dimensions dims(1, 1);
        m_array = ArrayOf(referenceType, dims, ptr);
    }
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    if (!setArrayValue(row, col, resultArray)) {
        return false;
    }

    updateEditData(row, col, QVariant(formatDisplayValue(resultArray)));

    m_pendingChanges = true;
    schedulePersistenceUpdate();

    emit dataChanged(index, index);

    return true;
}
//=============================================================================
bool
VariableIntegerTableModel::resizeArray(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const size_t newSize = static_cast<size_t>(newRows) * newCols;
    const size_t MAX_ELEMENTS = 50000000; // ~400MB for doubles

    if (newSize > MAX_ELEMENTS) {
        return false;
    }

    try {
        const NelsonType cls = m_array.dataClass();

        auto copyTypedArray = [&](auto* newData, const auto* oldData) {
            if (oldData) {
                for (int c = 0; c < m_cols; ++c) {
                    for (int r = 0; r < m_rows; ++r) {
                        newData[c * newRows + r] = oldData[c * m_rows + r];
                    }
                }
            }
        };

        Dimensions newDims(newRows, newCols);
        void* ptr
            = (void*)ArrayOf::allocateArrayOf(cls, newDims.elementCount(), stringVector(), true);
        ArrayOf newArray(cls, newDims, ptr);

        switch (cls) {
        case NLS_INT8:
            copyTypedArray((int8*)(newArray.dataPointer()), (int8*)(m_array.dataPointer()));
            break;
        case NLS_UINT8:
            copyTypedArray((uint8*)(newArray.dataPointer()), (uint8*)(m_array.dataPointer()));
            break;
        case NLS_INT16:
            copyTypedArray((int16*)(newArray.dataPointer()), (int16*)(m_array.dataPointer()));
            break;
        case NLS_UINT16:
            copyTypedArray((uint16*)(newArray.dataPointer()), (uint16*)(m_array.dataPointer()));
            break;
        case NLS_INT32:
            copyTypedArray((int32*)(newArray.dataPointer()), (int32*)(m_array.dataPointer()));
            break;
        case NLS_UINT32:
            copyTypedArray((uint32*)(newArray.dataPointer()), (uint32*)(m_array.dataPointer()));
            break;
        case NLS_INT64:
            copyTypedArray((int64*)(newArray.dataPointer()), (int64*)(m_array.dataPointer()));
            break;
        case NLS_UINT64:
            copyTypedArray((uint64*)(newArray.dataPointer()), (uint64*)(m_array.dataPointer()));
            break;
        default:
            return false;
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
VariableIntegerTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;

    if (m_array.dataClass() == referenceType) {
        flags |= Qt::ItemIsEditable;
    }

    return flags;
}
//=============================================================================
bool
VariableIntegerTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
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
VariableIntegerTableModel::getDisplayValue(int row, int col) const
{
    if (row < 0 || col < 0) {
        return QVariant();
    }

    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < static_cast<int>(m_editData.size()) && m_editData[idx].isValid()) {
        return m_editData[idx];
    }

    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const void* dataPtr = m_array.dataPointer();
        if (dataPtr) {
            const NelsonType cls = m_array.dataClass();
            if (cls == referenceType) {
                const size_t arrayIdx
                    = static_cast<size_t>(row) + static_cast<size_t>(col) * m_rows;

                const size_t totalElements = static_cast<size_t>(m_rows) * m_cols;
                if (arrayIdx < totalElements) {
                    Dimensions scalarDims(1, 1);
                    void* scalarPtr = ArrayOf::allocateArrayOf(cls, 1);
                    ArrayOf scalarArray(cls, scalarDims, scalarPtr);

                    const size_t elementSize = m_array.elementSize();
                    void* scalarData = (void*)scalarArray.dataPointer();
                    if (scalarData) {
                        const char* sourcePtr = static_cast<const char*>(dataPtr);
                        std::memcpy(scalarData, sourcePtr + (arrayIdx * elementSize), elementSize);
                        return QVariant(formatDisplayValue(scalarArray));
                    }
                }
            }
        }
    }
    return QVariant();
}
//=============================================================================
bool
VariableIntegerTableModel::hasOriginalData() const
{
    return m_array.dataClass() == referenceType && m_array.dataPointer() != nullptr;
}
//=============================================================================
bool
VariableIntegerTableModel::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const size_t newSize = static_cast<size_t>(newRows) * newCols;
    const size_t MAX_ELEMENTS = 50000000; // ~400MB for doubles

    if (newSize > MAX_ELEMENTS) {
        emit errorOccurred(tr("Array too large: exceeds maximum size limit"));
        return false;
    }

    try {
        const NelsonType cls = m_array.dataClass();
        if (cls != referenceType) {
            emit errorOccurred(tr("Unsupported data type for resize"));
            return false;
        }

        const size_t elementSize = m_array.elementSize();
        const size_t totalNewSize = newSize * elementSize;

        auto newData = std::make_unique<char[]>(totalNewSize);
        std::memset(newData.get(), 0, totalNewSize);

        if (const void* oldData = m_array.dataPointer()) {
            for (int c = 0; c < m_cols; ++c) {
                const size_t oldColStart = static_cast<size_t>(c) * m_rows * elementSize;
                const size_t newColStart = static_cast<size_t>(c) * newRows * elementSize;
                const size_t copySize = m_rows * elementSize;

                std::memcpy(newData.get() + newColStart,
                    static_cast<const char*>(oldData) + oldColStart, copySize);
            }
        }

        Dimensions newDims(newRows, newCols);
        m_array.resize(newDims);

        void* arrayData = (void*)m_array.dataPointer();
        if (arrayData) {
            std::memcpy(arrayData, newData.get(), totalNewSize);
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
VariableIntegerTableModel::setArrayValue(int row, int col, ArrayOf& value)
{
    void* data = (void*)m_array.dataPointer();
    if (!data) {
        emit errorOccurred(tr("No array data pointer available"));
        return false;
    }

    const int idx = row + col * m_rows;
    NelsonType arrayClass = m_array.dataClass();

    static const std::unordered_map<Nelson::NelsonType, std::function<bool(void*, int, ArrayOf&)>>
        typeHandlers
        = { { Nelson::NLS_INT8,
                [](void* data, int idx, ArrayOf& val) {
                    static_cast<int8*>(data)[idx] = static_cast<int8>(val.asInt32Scalar());
                    return true;
                } },
              { Nelson::NLS_UINT8,
                  [](void* data, int idx, ArrayOf& val) {
                      static_cast<uint8*>(data)[idx] = static_cast<uint8>(val.asInt32Scalar());
                      return true;
                  } },
              { Nelson::NLS_INT16,
                  [](void* data, int idx, ArrayOf& val) {
                      static_cast<int16*>(data)[idx] = static_cast<int16>(val.asInt32Scalar());
                      return true;
                  } },
              { Nelson::NLS_UINT16,
                  [](void* data, int idx, ArrayOf& val) {
                      static_cast<uint16*>(data)[idx] = static_cast<uint16>(val.asInt32Scalar());
                      return true;
                  } },
              { Nelson::NLS_INT32,
                  [](void* data, int idx, ArrayOf& val) {
                      static_cast<int32*>(data)[idx] = val.asInt32Scalar();
                      return true;
                  } },
              { Nelson::NLS_UINT32,
                  [](void* data, int idx, ArrayOf& val) {
                      static_cast<uint32*>(data)[idx] = static_cast<uint32>(val.asInt32Scalar());
                      return true;
                  } },
              { Nelson::NLS_INT64,
                  [](void* data, int idx, ArrayOf& val) {
                      static_cast<int64*>(data)[idx] = static_cast<int64>(val.asInt64Scalar());
                      return true;
                  } },
              { Nelson::NLS_UINT64, [](void* data, int idx, ArrayOf& val) {
                   static_cast<uint64*>(data)[idx] = static_cast<uint64>(val.asInt64Scalar());
                   return true;
               } } };

    auto it = typeHandlers.find(arrayClass);
    if (it != typeHandlers.end()) {
        return it->second(data, idx, value);
    }

    emit errorOccurred(tr("Unsupported integer type"));
    return false;
}
//=============================================================================
QString
VariableIntegerTableModel::formatDisplayValue(ArrayOf& value) const
{
    switch (referenceType) {
    case NLS_INT8: {
        int8 intValue = value.asInt8Scalar();
        return QString::number(intValue);
    } break;
    case NLS_UINT8: {
        uint8 uintValue = value.asUint8Scalar();
        return QString::number(uintValue);
    } break;
    case NLS_INT16: {
        int16 intValue = value.asInt16Scalar();
        return QString::number(intValue);
    } break;
    case NLS_UINT16: {
        uint16 uintValue = value.asUint16Scalar();
        return QString::number(uintValue);
    } break;
    case NLS_INT32: {
        int32 intValue = value.asInt32Scalar();
        return QString::number(intValue);
    } break;
    case NLS_UINT32: {
        uint32 uintValue = value.asUint32Scalar();
        return QString::number(uintValue);
    } break;
    case NLS_INT64: {
        int64 intValue = value.asInt64Scalar();
        return QString::number(intValue);
    } break;
    case NLS_UINT64: {
        uint64 uintValue = value.asUint64Scalar();
        return QString::number(uintValue);
    } break;
    default:
        break;
    }
    return QString("");
}
//=============================================================================
bool
VariableIntegerTableModel::validatePasteData(
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
VariableIntegerTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
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
            if (!value.isNumeric()) {
                return false;
            }
            value.promoteType(referenceType);
            if (setArrayValue(targetRow, targetCol, value)) {
                updateEditData(targetRow, targetCol, QVariant(formatDisplayValue(value)));
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
VariableIntegerTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.dataClass() == referenceType;
}
//=============================================================================
bool
VariableIntegerTableModel::insertRowAt(int row)
{
    if (row < 0 || row > m_rows) {
        return false;
    }

    saveCurrentStateForUndo();

    int newRows = m_rows + 1;
    int cols = m_cols;
    Dimensions newDims(newRows, cols);

    try {
        const NelsonType cls = m_array.dataClass();
        if (cls != referenceType) {
            emit errorOccurred(tr("Unsupported data type for insert row"));
            return false;
        }

        size_t total = static_cast<size_t>(newRows) * cols;
        size_t elementSize = m_array.elementSize();

        void* ptr = ArrayOf::allocateArrayOf(cls, total);
        char* dst = static_cast<char*>(ptr);
        const char* src = static_cast<const char*>(m_array.dataPointer());

        for (int c = 0; c < cols; ++c) {
            for (int r = 0; r < row; ++r) {
                size_t srcIdx = (r + c * m_rows) * elementSize;
                size_t dstIdx = (r + c * newRows) * elementSize;
                std::memcpy(dst + dstIdx, src + srcIdx, elementSize);
            }

            size_t zeroIdx = (row + c * newRows) * elementSize;
            std::memset(dst + zeroIdx, 0, elementSize);

            for (int r = row; r < m_rows; ++r) {
                size_t srcIdx = (r + c * m_rows) * elementSize;
                size_t dstIdx = (r + 1 + c * newRows) * elementSize;
                std::memcpy(dst + dstIdx, src + srcIdx, elementSize);
            }
        }

        ArrayOf newArray(cls, newDims, ptr);
        m_array = std::move(newArray);
        m_rows = newRows;
        expandDisplayIfNeeded(m_rows - 1, m_cols - 1);
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to insert row: %1").arg(e.what()));
        return false;
    }
}
//=============================================================================
bool
VariableIntegerTableModel::insertColumnAt(int col)
{
    if (col < 0 || col > m_cols) {
        return false;
    }

    saveCurrentStateForUndo();

    int newCols = m_cols + 1;
    int rows = m_rows;
    Dimensions newDims(rows, newCols);

    try {
        const NelsonType cls = m_array.dataClass();
        if (cls != referenceType) {
            emit errorOccurred(tr("Unsupported data type for insert column"));
            return false;
        }

        size_t total = static_cast<size_t>(newCols) * rows;
        size_t elementSize = m_array.elementSize();

        void* ptr = ArrayOf::allocateArrayOf(cls, total);
        char* dst = static_cast<char*>(ptr);
        const char* src = static_cast<const char*>(m_array.dataPointer());

        for (int c = 0; c < col; ++c) {
            for (int r = 0; r < rows; ++r) {
                size_t srcIdx = (r + c * rows) * elementSize;
                size_t dstIdx = (r + c * rows) * elementSize;
                std::memcpy(dst + dstIdx, src + srcIdx, elementSize);
            }
        }

        for (int r = 0; r < rows; ++r) {
            size_t zeroIdx = (r + col * rows) * elementSize;
            std::memset(dst + zeroIdx, 0, elementSize);
        }

        for (int c = col; c < m_cols; ++c) {
            for (int r = 0; r < rows; ++r) {
                size_t srcIdx = (r + c * rows) * elementSize;
                size_t dstIdx = (r + (c + 1) * rows) * elementSize;
                std::memcpy(dst + dstIdx, src + srcIdx, elementSize);
            }
        }

        ArrayOf newArray(cls, newDims, ptr);
        m_array = std::move(newArray);
        m_cols = newCols;
        expandDisplayIfNeeded(m_rows - 1, m_cols - 1);
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to insert column: %1").arg(e.what()));
        return false;
    }
}
//=============================================================================
bool
VariableIntegerTableModel::deleteRowAt(int row)
{
    if (row < 0 || row >= m_rows) {
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
        const NelsonType cls = m_array.dataClass();
        if (cls != referenceType) {
            emit errorOccurred(tr("Unsupported data type for delete row"));
            return false;
        }

        size_t total = static_cast<size_t>(newRows) * cols;
        size_t elementSize = m_array.elementSize();

        void* ptr = ArrayOf::allocateArrayOf(cls, total);
        char* dst = static_cast<char*>(ptr);
        const char* src = static_cast<const char*>(m_array.dataPointer());

        for (int c = 0; c < cols; ++c) {
            int k = 0;
            for (int r = 0; r < m_rows; ++r) {
                if (r == row) {
                    continue;
                }

                size_t srcIdx = (r + c * m_rows) * elementSize;
                size_t dstIdx = (k + c * newRows) * elementSize;
                std::memcpy(dst + dstIdx, src + srcIdx, elementSize);
                k++;
            }
        }

        ArrayOf newArray(cls, newDims, ptr);
        m_array = std::move(newArray);
        m_rows = newRows;
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to delete row: %1").arg(e.what()));
        return false;
    }
}
//=============================================================================
bool
VariableIntegerTableModel::deleteColumnAt(int col)
{
    if (col < 0 || col >= m_cols) {
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
        const NelsonType cls = m_array.dataClass();
        if (cls != referenceType) {
            emit errorOccurred(tr("Unsupported data type for delete column"));
            return false;
        }

        size_t total = static_cast<size_t>(newCols) * rows;
        size_t elementSize = m_array.elementSize();

        void* ptr = ArrayOf::allocateArrayOf(cls, total);
        char* dst = static_cast<char*>(ptr);
        const char* src = static_cast<const char*>(m_array.dataPointer());

        int k = 0;
        for (int c = 0; c < m_cols; ++c) {
            if (c == col) {
                continue;
            }

            for (int r = 0; r < rows; ++r) {
                size_t srcIdx = (r + c * rows) * elementSize;
                size_t dstIdx = (r + k * rows) * elementSize;
                std::memcpy(dst + dstIdx, src + srcIdx, elementSize);
            }
            k++;
        }

        ArrayOf newArray(cls, newDims, ptr);
        m_array = std::move(newArray);
        m_cols = newCols;
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to delete column: %1").arg(e.what()));
        return false;
    }
}
//=============================================================================
ArrayOf
VariableIntegerTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
{
    if (!isValidSelectionForExtraction(selectedIndexes)) {
        return ArrayOf();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return ArrayOf();
    }

    const int newRows = bounds.maxRow - bounds.minRow + 1;
    const int newCols = bounds.maxCol - bounds.minCol + 1;
    Dimensions newDims(newRows, newCols);

    try {
        const NelsonType cls = referenceType;
        const size_t elementCount = newDims.elementCount();
        const size_t elementSize = m_array.elementSize();

        void* ptr = ArrayOf::allocateArrayOf(cls, elementCount);
        ArrayOf newVar(cls, newDims, ptr);

        void* dataPtr = (void*)newVar.dataPointer();
        if (dataPtr) {
            std::memset(dataPtr, 0, elementCount * elementSize);
        }

        for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
            for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
                QModelIndex idx = index(r, c);
                QVariant val = data(idx, Qt::DisplayRole);

                if (val.isValid()) {
                    if (r < m_rows && c < m_cols) {
                        const void* srcData = m_array.dataPointer();
                        if (srcData) {
                            const size_t srcIdx = (r + c * m_rows) * elementSize;
                            const size_t dstIdx
                                = ((r - bounds.minRow) + (c - bounds.minCol) * newRows)
                                * elementSize;

                            std::memcpy(static_cast<char*>(dataPtr) + dstIdx,
                                static_cast<const char*>(srcData) + srcIdx, elementSize);
                        }
                    }
                }
            }
        }

        return newVar;

    } catch (const std::exception&) {
        return ArrayOf();
    }
}
//=============================================================================
bool
VariableIntegerTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    return !selectedIndexes.isEmpty() && (m_array.dataClass() == referenceType);
}
//=============================================================================
bool
VariableIntegerTableModel::shouldDisplayAsTable() const
{
    return ((m_array.dataClass() == referenceType) && m_array.is2D());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
