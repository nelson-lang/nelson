//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableCellTableModel.h"
#include "EvaluateCommand.hpp"
#include <QtCore/QStringList>
#include <QtCore/QRegularExpression>
#include <QtGui/QColor>
#include <QtGui/QFont>
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
VariableCellTableModel::VariableCellTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableAbstractTableModel(name, array, evaluator, parent)
{

    initializeDimensions();
    initializeEditData();
    saveCurrentStateForUndo();
}
//=============================================================================
void
VariableCellTableModel::initializeDimensions()
{
    m_rows = static_cast<int>(m_array.getRows());
    m_cols = static_cast<int>(m_array.getColumns());

    // Handle empty arrays
    if (m_rows == 0) {
        m_rows = 1;
        m_rows_display = DEFAULT_EMPTY_SIZE;
    } else {
        m_rows_display = std::max(m_rows * 2, MIN_DISPLAY_SIZE);
    }

    if (m_cols == 0) {
        m_cols = 1;
        m_cols_display = DEFAULT_EMPTY_SIZE;
    } else {
        m_cols_display = std::max(m_cols * 2, MIN_DISPLAY_SIZE);
    }
}
//=============================================================================
void
VariableCellTableModel::initializeEditData()
{
    const int totalSize = m_rows_display * m_cols_display;
    m_editData.clear();
    m_editData.resize(totalSize);
}
//=============================================================================
int
VariableCellTableModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_rows_display;
}
//=============================================================================
int
VariableCellTableModel::columnCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_cols_display;
}
//=============================================================================
QVariant
VariableCellTableModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || !isValidPosition(index.row(), index.column())) {
        return QVariant();
    }

    const int row = index.row();
    const int col = index.column();

    switch (role) {
    case Qt::DisplayRole: {
        return getCellDisplayValue(row, col);
    } break;
    case Qt::EditRole: {
        return getCellEditValue(row, col);
    } break;
    case Qt::TextAlignmentRole: {
        return static_cast<int>(Qt::AlignLeft);
    } break;
    }
    return QVariant();
}
//=============================================================================
QVariant
VariableCellTableModel::getCellDisplayValue(int row, int col) const
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return formatCellForDisplay(m_editData[idx]);
    }

    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const ArrayOf* ptr = static_cast<const ArrayOf*>(m_array.getDataPointer());
        const int arrayIdx = row + col * m_rows;
        if (ptr && arrayIdx < m_array.getElementCount()) {
            return formatCellForDisplay(ptr[arrayIdx]);
        }
    }

    return QVariant();
}
//=============================================================================
QVariant
VariableCellTableModel::getCellEditValue(int row, int col) const
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return formatCellForEdit(m_editData[idx]);
    }

    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const ArrayOf* ptr = static_cast<const ArrayOf*>(m_array.getDataPointer());
        const int arrayIdx = row + col * m_rows;
        if (ptr && arrayIdx < m_array.getElementCount()) {
            return formatCellForEdit(ptr[arrayIdx]);
        }
    }

    return QVariant("");
}
//=============================================================================
QVariant
VariableCellTableModel::formatCellForDisplay(const QVariant& cellData) const
{
    if (!cellData.isValid()) {
        return QVariant(tr("<empty>"));
    }
    return cellData;
}
//=============================================================================
QVariant
VariableCellTableModel::formatCellForDisplay(const ArrayOf& cellArray) const
{

    if (cellArray.isEmpty() && cellArray.isDoubleType()) {
        return QVariant(tr("[]"));
    }
    switch (cellArray.getDataClass()) {
    case NLS_UINT8: {
        if (cellArray.isScalar()) {
            uint8 val = cellArray.getContentAsUnsignedInteger8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT16: {
        if (cellArray.isScalar()) {
            uint16 val = cellArray.getContentAsUnsignedInteger16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT32: {
        if (cellArray.isScalar()) {
            uint32 val = cellArray.getContentAsUnsignedInteger32Scalar();

            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT64: {
        if (cellArray.isScalar()) {
            uint64 val = cellArray.getContentAsUnsignedInteger64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT8: {
        if (cellArray.isScalar()) {
            int8 val = cellArray.getContentAsInteger8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT16: {
        if (cellArray.isScalar()) {
            int16 val = cellArray.getContentAsInteger16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT32: {
        if (cellArray.isScalar()) {
            int32 val = cellArray.getContentAsInteger32Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT64: {
        if (cellArray.isScalar()) {
            int64 val = cellArray.getContentAsInteger64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_SINGLE: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            single val = cellArray.getContentAsSingleScalar();
            if (std::isnan(val)) {
                return QVariant(tr("NaN"));
            } else if (std::isfinite(val)) {
                return QVariant(QString::number(val, 'g', 6));
            } else {
                if (val < 0) {
                    return QVariant(tr("-Inf"));
                } else {
                    return QVariant(tr("Inf"));
                }
            }
        }
    } break;
    case NLS_SCOMPLEX: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            std::complex<single> val = cellArray.getContentAsSingleComplexScalar();
            return QVariant(QString("%1%2%3i")
                                .arg(val.real(), 0, 'g', 6)
                                .arg(val.imag() >= 0 ? "+" : "")
                                .arg(val.imag(), 0, 'g', 6));
        }
    } break;
    case NLS_DOUBLE: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            double val = cellArray.getContentAsDoubleScalar();
            if (std::isnan(val)) {
                return QVariant(tr("NaN"));
            } else if (std::isfinite(val)) {
                return QVariant(QString::number(val, 'g', 6));
            } else {
                if (val < 0) {
                    return QVariant(tr("-Inf"));
                } else {
                    return QVariant(tr("Inf"));
                }
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            std::complex<double> val = cellArray.getContentAsDoubleComplexScalar();
            return QVariant(QString("%1%2%3i")
                                .arg(val.real(), 0, 'g', 6)
                                .arg(val.imag() >= 0 ? "+" : "")
                                .arg(val.imag(), 0, 'g', 6));
        }
    } break;
    case NLS_CHAR: {
        if (cellArray.isRowVectorCharacterArray()) {
            return QVariant(wstringToQString(cellArray.getContentAsWideString()));
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (cellArray.isScalar()) {
            return QVariant(wstringToQString(cellArray.getContentAsWideString()));
        }
    } break;
    case NLS_LOGICAL: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            bool val = cellArray.getContentAsLogicalScalar();
            return QVariant(val ? "true" : "false");
        }
    } break;
    default: {
    } break;
    }
    std::wstring classname;
    ClassName(cellArray, classname);
    return wstringToQString(
        L"[" + cellArray.getDimensions().toWideString() + L" " + classname + L"]");
}
//=============================================================================
QVariant
VariableCellTableModel::formatCellForEdit(const QVariant& cellData) const
{
    if (!cellData.isValid()) {
        return QVariant("");
    }

    return cellData;
}
//=============================================================================
QVariant
VariableCellTableModel::formatCellForEdit(const ArrayOf& cellArray) const
{
    switch (cellArray.getDataClass()) {
    case NLS_UINT8: {
        if (cellArray.isScalar()) {
            uint8 val = cellArray.getContentAsUnsignedInteger8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT16: {
        if (cellArray.isScalar()) {
            uint16 val = cellArray.getContentAsUnsignedInteger16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT32: {
        if (cellArray.isScalar()) {
            uint32 val = cellArray.getContentAsUnsignedInteger32Scalar();

            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT64: {
        if (cellArray.isScalar()) {
            uint64 val = cellArray.getContentAsUnsignedInteger64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT8: {
        if (cellArray.isScalar()) {
            int8 val = cellArray.getContentAsInteger8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT16: {
        if (cellArray.isScalar()) {
            int16 val = cellArray.getContentAsInteger16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT32: {
        if (cellArray.isScalar()) {
            int32 val = cellArray.getContentAsInteger32Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT64: {
        if (cellArray.isScalar()) {
            int64 val = cellArray.getContentAsInteger64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_SINGLE: {
        if (cellArray.isScalar()) {
            single val = cellArray.getContentAsSingleScalar();
            if (std::isnan(val)) {
                return QVariant("");
            }
            return QVariant(QString::number(val, 'g', 15));
        }
    } break;
    case NLS_SCOMPLEX: {
        if (cellArray.isScalar()) {
            std::complex<single> val = cellArray.getContentAsSingleComplexScalar();
            return QVariant(QString("%1%2%3i")
                                .arg(val.real(), 0, 'g', 15)
                                .arg(val.imag() >= 0 ? "+" : "")
                                .arg(val.imag(), 0, 'g', 15));
        }
    } break;
    case NLS_DOUBLE: {
        if (cellArray.isScalar()) {
            double val = cellArray.getContentAsDoubleScalar();
            if (std::isnan(val)) {
                return QVariant("");
            }
            return QVariant(QString::number(val, 'g', 15));
        }
    } break;
    case NLS_DCOMPLEX: {
        if (cellArray.isScalar()) {
            std::complex<double> val = cellArray.getContentAsDoubleComplexScalar();
            return QVariant(QString("%1%2%3i")
                                .arg(val.real(), 0, 'g', 15)
                                .arg(val.imag() >= 0 ? "+" : "")
                                .arg(val.imag(), 0, 'g', 15));
        }
    } break;
    case NLS_CHAR: {
        if (cellArray.isRowVectorCharacterArray()) {
            return QVariant(wstringToQString(cellArray.getContentAsWideString()));
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (cellArray.isScalar()) {
            return QVariant(wstringToQString(cellArray.getContentAsWideString()));
        }
    } break;
    case NLS_LOGICAL: {
        if (cellArray.isScalar()) {
            bool val = cellArray.getContentAsLogicalScalar();
            return QVariant(val ? "true" : "false");
        }
    } break;
    default:
        break;
    }

    // Non-scalar or complex types can't be edited directly
    return QVariant("");
}
//=============================================================================
bool
VariableCellTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
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

    std::wstring command = value.toString().toStdWString();
    ArrayOfVector results;
    bool emitError = false;
    bool asStr = false;
    try {
        results = EvaluateCommand(m_evaluator, 1, command, L"");
    } catch (Exception&) {
        emitError = true;
        const int idx = row + col * m_rows;
        if (idx >= 0 && idx < m_array.getElementCount()) {
            ArrayOf* data = (ArrayOf*)(m_array.getDataPointer());
            if (data) {
                NelsonType destinationType = data[idx].getDataClass();
                emitError = false;
                if (destinationType == NLS_CHAR || destinationType == NLS_STRING_ARRAY) {
                    asStr = true;
                    results.clear();
                    results << ArrayOf::characterArrayConstructor(command);
                }
            }
        }
    }
    if (results.size() != 1) {
        emitError = true;
    }
    if (emitError) {
        emit errorOccurred(tr("Invalid command or result type."));
        return false;
    }

    ArrayOf resultArray = results[0];
    if (asStr) {
        if (!resultArray.isRowVectorCharacterArray()) {
            emit errorOccurred(tr("Command must return a scalar value."));
            return false;
        }
    } else {
        if (!resultArray.isScalar()) {
            emit errorOccurred(tr("Command must return a scalar value."));
            return false;
        }
    }
    ArrayOf* data = (ArrayOf*)(m_array.getDataPointer());
    if (data) {
        const int idx = row + col * m_rows;
        if (idx >= 0 && idx < m_array.getElementCount()) {
            NelsonType destinationType = data[idx].getDataClass();
            if (resultArray.getDataClass() != destinationType) {
                try {
                    resultArray.promoteType(destinationType);
                } catch (const Exception&) {
                    emit errorOccurred(tr("Cannot convert value to the required type."));
                    return false;
                }
            }
        }
    }

    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    // Set the value in the array
    if (!setCellValue(row, col, resultArray)) {
        return false;
    }

    // Update edit tracking
    updateEditData(row, col, value);

    m_pendingChanges = true;
    schedulePeristenceUpdate();

    // Only emit for the specific cell that changed
    emit dataChanged(index, index);

    return true;
}
//=============================================================================
ArrayOf
VariableCellTableModel::parseInputToArrayOf(const QString& input, int row, int col) const
{
    if (input.isEmpty()) {
        Dimensions dims(0, 0);
        return ArrayOf::emptyConstructor(dims);
    }
    QString trimmed = input.trimmed();
    std::wstring command = trimmed.toStdWString();
    ArrayOfVector results;
    try {
        results = EvaluateCommand(m_evaluator, 1, command, L"");
    } catch (Exception&) {
    }
    if (results.size() != 1) {
        return ArrayOf::characterArrayConstructor(QStringTowstring(trimmed));
    }
    ArrayOf value = results[0];
    value.name("");
    return value;
}
//=============================================================================
bool
VariableCellTableModel::setCellValue(int row, int col, const ArrayOf& cellValue)
{
    ArrayOf* data = (ArrayOf*)(m_array.getDataPointer());
    if (!data) {
        return false;
    }

    const int idx = row + col * m_rows;
    if (idx >= 0 && idx < m_array.getElementCount()) {
        data[idx] = cellValue;
        return true;
    }

    return false;
}
//=============================================================================
bool
VariableCellTableModel::expandArrayIfNeeded(int row, int col)
{
    // Check if we need to expand the actual array data
    const bool needsArrayResize = (row >= m_rows || col >= m_cols);

    // Check if we need to expand the display
    const bool needsDisplayExpansion = (row >= m_rows_display || col >= m_cols_display);

    // Handle array resize first (this is the expensive operation)
    if (needsArrayResize) {
        if (!resizeArray(row, col)) {
            return false;
        }
    }

    // Handle display expansion without model reset for small expansions
    if (needsDisplayExpansion) {
        return expandDisplay(row, col);
    }

    return true;
}
//=============================================================================
bool
VariableCellTableModel::expandDisplay(int row, int col)
{
    const int oldRowsDisplay = m_rows_display;
    const int oldColsDisplay = m_cols_display;

    // Calculate new display size
    int newRowsDisplay = m_rows_display;
    int newColsDisplay = m_cols_display;

    if (row >= m_rows_display) {
        newRowsDisplay = std::max((row + 1) * 2, MIN_DISPLAY_SIZE);
    }
    if (col >= m_cols_display) {
        newColsDisplay = std::max((col + 1) * 2, MIN_DISPLAY_SIZE);
    }

    // For reasonable expansions, use insertRows/insertColumns instead of reset
    const int rowsToAdd = newRowsDisplay - oldRowsDisplay;
    const int colsToAdd = newColsDisplay - oldColsDisplay;

    // Threshold for when to use reset vs incremental updates
    const int RESET_THRESHOLD = 1000;

    if (rowsToAdd * newColsDisplay + colsToAdd * oldRowsDisplay > RESET_THRESHOLD) {
        // Large expansion - use reset
        beginResetModel();
        m_rows_display = newRowsDisplay;
        m_cols_display = newColsDisplay;
        resizeEditData(oldRowsDisplay, oldColsDisplay);
        endResetModel();
    } else {
        // Small expansion - use incremental updates

        // Add columns first
        if (colsToAdd > 0) {
            beginInsertColumns(QModelIndex(), oldColsDisplay, newColsDisplay - 1);
            m_cols_display = newColsDisplay;
            endInsertColumns();
        }

        // Add rows
        if (rowsToAdd > 0) {
            beginInsertRows(QModelIndex(), oldRowsDisplay, newRowsDisplay - 1);
            m_rows_display = newRowsDisplay;
            endInsertRows();
        }

        // Resize edit data efficiently
        resizeEditData(oldRowsDisplay, oldColsDisplay);
    }

    return true;
}
//=============================================================================
bool
VariableCellTableModel::resizeArray(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true; // No resize needed
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const size_t newSize = static_cast<size_t>(newRows) * newCols;

    try {
        // Create new dimensions
        Dimensions newDims(newRows, newCols);

        // Create new array with the larger size
        ArrayOf* ptr
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, newDims.getElementCount());
        ArrayOf newArray = ArrayOf(NLS_CELL_ARRAY, newDims, ptr);
        ArrayOf* newData = (ArrayOf*)newArray.getDataPointer();

        // Initialize new cells with empty values
        Dimensions dims(0, 0);
        for (size_t k = 0; k < newDims.getElementCount(); ++k) {
            newData[k] = ArrayOf::emptyConstructor(dims);
        }

        // Copy existing data efficiently
        if (const ArrayOf* oldData = (const ArrayOf*)m_array.getDataPointer()) {
            for (int c = 0; c < m_cols; ++c) {
                const size_t oldColStart = static_cast<size_t>(c) * m_rows;
                const size_t newColStart = static_cast<size_t>(c) * newRows;
                for (int r = 0; r < m_rows; ++r) {
                    newData[newColStart + r] = oldData[oldColStart + r];
                }
            }
        }

        // Replace the array
        m_array = std::move(newArray);

        m_rows = newRows;
        m_cols = newCols;

        return true;

    } catch (const std::exception&) {
        return false;
    }
}
//=============================================================================
void
VariableCellTableModel::resizeEditData(int oldRows, int oldCols)
{
    const int newSize = m_rows_display * m_cols_display;
    const int oldSize = oldRows * oldCols;

    if (newSize <= oldSize) {
        return; // No expansion needed
    }

    // Use reserve and resize for better performance
    QVector<QVariant> newEditData;
    newEditData.reserve(newSize);
    newEditData.resize(newSize);

    // Copy existing data more efficiently
    if (!m_editData.isEmpty() && oldRows > 0 && oldCols > 0) {
        const int copyRows = std::min(oldRows, m_rows_display);
        const int copyCols = std::min(oldCols, m_cols_display);

        for (int c = 0; c < copyCols; ++c) {
            const int oldColStart = c * oldRows;
            const int newColStart = c * m_rows_display;

            // Copy entire columns at once when possible
            for (int r = 0; r < copyRows; ++r) {
                if (oldColStart + r < m_editData.size()) {
                    newEditData[newColStart + r] = std::move(m_editData[oldColStart + r]);
                }
            }
        }
    }

    m_editData = std::move(newEditData);
}
//=============================================================================
void
VariableCellTableModel::schedulePeristenceUpdate()
{
    if (!m_persistenceTimer) {
        m_persistenceTimer = new QTimer(this);
        m_persistenceTimer->setSingleShot(true);
        m_persistenceTimer->setInterval(100); // 100ms delay
        connect(m_persistenceTimer, &QTimer::timeout, this,
            &VariableCellTableModel::performBatchedPersistence);
    }

    m_persistenceTimer->start();
}
//=============================================================================
void
VariableCellTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
        emit modelChanged();
    }
}
//=============================================================================
Qt::ItemFlags
VariableCellTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;
    if (m_array.getDataClass() == NLS_CELL_ARRAY) {
        if (isCellScalarOrEditable(index.row(), index.column())) {
            flags |= Qt::ItemIsEditable;
        }
    }

    return flags;
}
//=============================================================================
bool
VariableCellTableModel::isCellScalarOrEditable(int row, int col) const
{
    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const ArrayOf* ptr = static_cast<const ArrayOf*>(m_array.getDataPointer());
        const int arrayIdx = row + col * m_rows;

        if (ptr && arrayIdx < m_array.getElementCount()) {
            const ArrayOf& cellArray = ptr[arrayIdx];
            if (cellArray.isRowVectorCharacterArray()) {
                return true;
            }
            return cellArray.isScalar() || cellArray.isEmpty();
        }
    }
    return (row >= m_rows || col >= m_cols);
}
//=============================================================================
QVariant
VariableCellTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole) {
        return QString::number(section + 1);
    }
    return QVariant();
}
//=============================================================================
void
VariableCellTableModel::refreshFromArray(ArrayOf& value)
{
    m_array = value;
    beginResetModel();
    initializeDimensions();
    initializeEditData();
    endResetModel();
}
//=============================================================================
bool
VariableCellTableModel::hasOriginalData() const
{
    return m_array.getDataClass() == NLS_CELL_ARRAY && m_array.getDataPointer() != nullptr;
}
//=============================================================================
bool
VariableCellTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && col >= 0 && row < m_rows_display && col < m_cols_display;
}
//=============================================================================
void
VariableCellTableModel::updateEditData(int row, int col, const QVariant& value)
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size()) {
        m_editData[idx] = value;
    }
}
//=============================================================================
void
VariableCellTableModel::persistChangesToContext()
{
    if (!m_evaluator || !m_evaluator->getContext() || m_variableName.isEmpty()) {
        return;
    }

    try {
        m_evaluator->getContext()->getCurrentScope()->insertVariable(
            wstring_to_utf8(QStringTowstring(m_variableName)), m_array);
        WorkspaceBrowser::updateWorkspaceBrowser();
    } catch (const std::exception&) {
    }
}
//=============================================================================
int
VariableCellTableModel::indexToFlat(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return -1;
    }
    return row + col * m_rows_display;
}
//=============================================================================
std::pair<int, int>
VariableCellTableModel::flatToIndex(int flatIndex) const
{
    if (flatIndex < 0 || m_rows_display <= 0) {
        return { -1, -1 };
    }

    const int row = flatIndex % m_rows_display;
    const int col = flatIndex / m_rows_display;

    return { row, col };
}
//=============================================================================
bool
VariableCellTableModel::shouldDisplayAsTable() const
{
    return (m_array.getDataClass() == NLS_CELL_ARRAY) && m_array.is2D();
}
//=============================================================================
VariableCellTableModel::SelectionBounds
VariableCellTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
{
    SelectionBounds bounds;
    if (selectedIndexes.isEmpty()) {
        return bounds;
    }

    bounds.minRow = selectedIndexes.first().row();
    bounds.maxRow = selectedIndexes.first().row();
    bounds.minCol = selectedIndexes.first().column();
    bounds.maxCol = selectedIndexes.first().column();
    bounds.isValid = true;

    for (const QModelIndex& index : selectedIndexes) {
        bounds.minRow = qMin(bounds.minRow, index.row());
        bounds.maxRow = qMax(bounds.maxRow, index.row());
        bounds.minCol = qMin(bounds.minCol, index.column());
        bounds.maxCol = qMax(bounds.maxCol, index.column());
    }

    return bounds;
}
//=============================================================================
void
VariableCellTableModel::redo()
{
    undo();
}
//=============================================================================
void
VariableCellTableModel::undo()
{
    if (!m_hasUndoState) {
        return; // No undo state available
    }

    try {
        m_isPerformingUndo = true;

        // Store current state as redo state (reuse the same variable)
        ArrayOf redoArray = m_array;
        redoArray.ensureSingleOwner();

        // Restore the previous state
        beginResetModel();
        m_array = m_undoArray;
        m_array.ensureSingleOwner();

        // Update dimensions
        initializeDimensions();
        initializeEditData();

        endResetModel();

        // Store the current (now previous) state for redo
        m_undoArray = std::move(redoArray);
        // m_hasUndoState remains true for redo

        // Persist the restored state
        persistChangesToContext();

        // Notify views of the change
        emit modelChanged();

        m_isPerformingUndo = false;

    } catch (const std::exception& e) {
        m_isPerformingUndo = false;
        emit errorOccurred(tr("Failed to undo: %1").arg(e.what()));
    }
}
//=============================================================================
bool
VariableCellTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.getDataClass() == NLS_CELL_ARRAY;
}
//=============================================================================
void
VariableCellTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
    emit modelChanged();
}
//=============================================================================
bool
VariableCellTableModel::pasteDataFromClipboard(int startRow, int startCol, const QString& text)
{
    if (text.isEmpty() || startRow < 0 || startCol < 0) {
        return false;
    }

    // Parse the clipboard text into a 2D structure
    QStringList lines = text.split('\n', Qt::KeepEmptyParts);
    if (lines.isEmpty()) {
        return false;
    }

    // Remove trailing empty line if present (common in clipboard data)
    if (!lines.isEmpty() && lines.last().isEmpty()) {
        lines.removeLast();
    }

    if (lines.isEmpty()) {
        return false;
    }

    // Parse each line into columns (assume tab-separated, fall back to spaces)
    QVector<QStringList> pasteMatrix;
    int maxCols = 0;

    for (const QString& line : lines) {
        QStringList columns;

        // Try tab-separated first (most common for Excel/spreadsheet data)
        if (line.contains('\t')) {
            columns = line.split('\t', Qt::KeepEmptyParts);
        } else {
            // Fall back to space-separated, but be smarter about it
            // Only split on multiple spaces to preserve single spaces in values
            columns = line.split(QRegularExpression("\\s{2,}"), Qt::KeepEmptyParts);

            // If that didn't work well, try single space
            if (columns.size() == 1 && line.contains(' ')) {
                columns = line.split(' ', Qt::KeepEmptyParts);
            }

            // If still just one column, treat the whole line as a single value
            if (columns.size() == 1) {
                columns = QStringList() << line;
            }
        }

        pasteMatrix.append(columns);
        maxCols = qMax(maxCols, columns.size());
    }

    const int pasteRows = pasteMatrix.size();
    const int pasteCols = maxCols;

    // Check if paste would exceed reasonable bounds
    const int endRow = startRow + pasteRows - 1;
    const int endCol = startCol + pasteCols - 1;

    // Reasonable size limits to prevent accidental huge pastes
    const int MAX_PASTE_SIZE = 10000;
    if (pasteRows * pasteCols > MAX_PASTE_SIZE) {
        return false;
    }

    // Save state for undo
    saveCurrentStateForUndo();

    try {
        // Expand array if needed
        if (!expandArrayIfNeeded(endRow, endCol)) {
            return false;
        }

        // Track changes for batch update
        QVector<QPair<QModelIndex, QModelIndex>> changedRanges;

        // Paste the data
        bool anyChanges = false;
        for (int row = 0; row < pasteRows; ++row) {
            const QStringList& rowData = pasteMatrix[row];
            for (int col = 0; col < pasteCols; ++col) {
                const int targetRow = startRow + row;
                const int targetCol = startCol + col;

                QString cellText;
                if (col < rowData.size()) {
                    cellText = rowData[col];
                } else {
                    cellText = ""; // Fill missing columns with empty
                }

                // Parse and set the cell value
                ArrayOf newCellValue = parseInputToArrayOf(cellText, targetRow, targetCol);

                if (setCellValue(targetRow, targetCol, newCellValue)) {
                    updateEditData(targetRow, targetCol, QVariant(cellText));
                    anyChanges = true;
                }
            }
        }

        if (anyChanges) {
            // Batch persistence
            m_pendingChanges = true;
            schedulePeristenceUpdate();

            // Emit dataChanged for the entire pasted region
            QModelIndex topLeft = index(startRow, startCol);
            QModelIndex bottomRight = index(endRow, endCol);
            emit dataChanged(topLeft, bottomRight);

            return true;
        }

    } catch (const std::exception&) {
        // If anything fails, we've already saved undo state, so this is recoverable
        return false;
    }

    return false;
}
//=============================================================================
bool
VariableCellTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    if (text.isEmpty() || startRow < 0 || startCol < 0) {
        return false;
    }

    // Parse the clipboard text into a 2D structure
    QStringList lines = text.split('\n', Qt::KeepEmptyParts);
    if (lines.isEmpty()) {
        return false;
    }

    // Remove trailing empty line if present (common in clipboard data)
    if (!lines.isEmpty() && lines.last().isEmpty()) {
        lines.removeLast();
    }

    if (lines.isEmpty()) {
        return false;
    }

    // Parse each line into columns (assume tab-separated, fall back to spaces)
    QVector<QStringList> pasteMatrix;
    int maxCols = 0;

    for (const QString& line : lines) {
        QStringList columns;

        // Try tab-separated first (most common for Excel/spreadsheet data)
        if (line.contains('\t')) {
            columns = line.split('\t', Qt::KeepEmptyParts);
        } else {
            // Fall back to space-separated, but be smarter about it
            // Only split on multiple spaces to preserve single spaces in values
            columns = line.split(QRegularExpression("\\s{2,}"), Qt::KeepEmptyParts);

            // If that didn't work well, try single space
            if (columns.size() == 1 && line.contains(' ')) {
                columns = line.split(' ', Qt::KeepEmptyParts);
            }

            // If still just one column, treat the whole line as a single value
            if (columns.size() == 1) {
                columns = QStringList() << line;
            }
        }

        pasteMatrix.append(columns);
        maxCols = qMax(maxCols, columns.size());
    }

    const int pasteRows = pasteMatrix.size();
    const int pasteCols = maxCols;

    // Check if paste would exceed reasonable bounds
    const int endRow = startRow + pasteRows - 1;
    const int endCol = startCol + pasteCols - 1;

    // Reasonable size limits to prevent accidental huge pastes
    const int MAX_PASTE_SIZE = 10000;
    if (pasteRows * pasteCols > MAX_PASTE_SIZE) {
        return false;
    }

    // Save state for undo
    saveCurrentStateForUndo();

    try {
        // Expand array if needed
        if (!expandArrayIfNeeded(endRow, endCol)) {
            return false;
        }

        // Track changes for batch update
        QVector<QPair<QModelIndex, QModelIndex>> changedRanges;

        // Paste the data
        bool anyChanges = false;
        for (int row = 0; row < pasteRows; ++row) {
            const QStringList& rowData = pasteMatrix[row];
            for (int col = 0; col < pasteCols; ++col) {
                const int targetRow = startRow + row;
                const int targetCol = startCol + col;

                QString cellText;
                if (col < rowData.size()) {
                    cellText = rowData[col];
                } else {
                    cellText = ""; // Fill missing columns with empty
                }

                // Parse and set the cell value
                ArrayOf newCellValue = parseInputToArrayOf(cellText, targetRow, targetCol);

                if (setCellValue(targetRow, targetCol, newCellValue)) {
                    updateEditData(targetRow, targetCol, QVariant(cellText));
                    anyChanges = true;
                }
            }
        }

        if (anyChanges) {
            // Batch persistence
            m_pendingChanges = true;
            schedulePeristenceUpdate();

            // Emit dataChanged for the entire pasted region
            QModelIndex topLeft = index(startRow, startCol);
            QModelIndex bottomRight = index(endRow, endCol);
            emit dataChanged(topLeft, bottomRight);

            return true;
        }

    } catch (const std::exception&) {
        // If anything fails, we've already saved undo state, so this is recoverable
        return false;
    }

    return false;
}
//=============================================================================

bool
VariableCellTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    return false;
}
//=============================================================================

QSize
VariableCellTableModel::getPasteSize(const QString& text) const
{
    if (text.isEmpty()) {
        return QSize(0, 0);
    }

    QStringList lines = text.split('\n', Qt::KeepEmptyParts);
    if (lines.isEmpty()) {
        return QSize(0, 0);
    }

    // Remove trailing empty line if present
    if (!lines.isEmpty() && lines.last().isEmpty()) {
        lines.removeLast();
    }

    if (lines.isEmpty()) {
        return QSize(0, 0);
    }

    const int rows = lines.size();
    int maxCols = 0;

    // Find the maximum number of columns across all rows
    for (const QString& line : lines) {
        int cols = 1; // At least one column

        if (line.contains('\t')) {
            cols = line.split('\t', Qt::KeepEmptyParts).size();
        } else if (line.contains(' ')) {
            // Try multiple spaces first
            QStringList parts = line.split(QRegularExpression("\\s{2,}"), Qt::KeepEmptyParts);
            if (parts.size() > 1) {
                cols = parts.size();
            } else {
                // Fall back to single space
                parts = line.split(' ', Qt::KeepEmptyParts);
                cols = parts.size();
            }
        }

        maxCols = qMax(maxCols, cols);
    }

    return QSize(maxCols, rows);
}
//=============================================================================
ArrayOf
VariableCellTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
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

    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, newDims.getElementCount());
    newVar = ArrayOf(NLS_CELL_ARRAY, newDims, ptr);

    for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
        for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
            QModelIndex idx = index(r, c);
            QVariant val = data(idx, Qt::DisplayRole);
            // double num = val.toDouble();
            // ptr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.getRows()]
            //     = ArrayOf::characterArrayConstructor(QStringTowstring(val.toString()));
        }
    }
    return newVar;
}
//=============================================================================
bool
VariableCellTableModel::isValidSelectionForExtraction(const QModelIndexList& selectedIndexes) const
{
    return false;
}
//=============================================================================
bool
VariableCellTableModel::insertRowAt(int row)
{
    if (row < 0 || row > m_rows)
        return false;
    saveCurrentStateForUndo();

    int newRows = m_rows + 1;
    int cols = m_cols;
    Dimensions newDims(newRows, cols);

    try {
        size_t total = (size_t)(newRows * cols);

        ArrayOf newArray;

        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        Dimensions dimsEmpty(0, 0);
        for (int c = 0; c < cols; ++c) {
            for (int r = 0; r < row; ++r)
                dst[r + c * newRows] = src[r + c * m_rows];

            dst[row + c * newRows] = ArrayOf::emptyConstructor(dimsEmpty);

            for (int r = row; r < m_rows; ++r)
                dst[r + 1 + c * newRows] = src[r + c * m_rows];
        }

        newArray = ArrayOf(NLS_CELL_ARRAY, newDims, ptr);

        m_array = std::move(newArray);
        m_rows = newRows;
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
VariableCellTableModel::insertColumnAt(int col)
{
    if (col < 0 || col > m_cols)
        return false;
    saveCurrentStateForUndo();

    int newCols = m_cols + 1;
    int rows = m_rows;
    Dimensions newDims(rows, newCols);

    try {
        size_t total = newCols * rows;
        ArrayOf newArray;
        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        for (int c = 0; c < col; ++c)
            for (int r = 0; r < rows; ++r)
                dst[r + c * rows] = src[r + c * rows];

        Dimensions dimsEmpty(0, 0);
        for (int r = 0; r < rows; ++r)
            dst[r + col * rows] = ArrayOf::emptyConstructor(dimsEmpty);

        for (int c = col; c < m_cols; ++c)
            for (int r = 0; r < rows; ++r)
                dst[r + (c + 1) * rows] = src[r + c * rows];

        newArray = ArrayOf(NLS_CELL_ARRAY, newDims, ptr);

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
VariableCellTableModel::deleteRowAt(int row)
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
        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        for (int c = 0; c < cols; ++c) {
            int k = 0;
            for (int r = 0; r < m_rows; ++r) {
                if (r == row)
                    continue;
                dst[k++ + c * newRows] = src[r + c * m_rows];
            }
        }

        newArray = ArrayOf(NLS_CELL_ARRAY, newDims, ptr);

        m_array = std::move(newArray);
        m_rows = newRows;
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (...) {
        return false;
    }
}
//=============================================================================
bool
VariableCellTableModel::deleteColumnAt(int col)
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
        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        int k = 0;
        for (int c = 0; c < m_cols; ++c) {
            if (c == col)
                continue;
            for (int r = 0; r < rows; ++r)
                dst[r + k * rows] = src[r + c * rows];
            ++k;
        }

        newArray = ArrayOf(NLS_CELL_ARRAY, newDims, ptr);

        m_array = std::move(newArray);
        m_cols = newCols;
        forceViewUpdate();
        persistChangesToContext();
        return true;

    } catch (...) {
        return false;
    }
}
//=============================================================================
QString
VariableCellTableModel::getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
{
    return QString();
}
//=============================================================================
bool
VariableCellTableModel::expandDisplayIfNeeded(int row, int col)
{
    bool needsExpansion = false;
    int newRowsDisplay = m_rows_display;
    int newColsDisplay = m_cols_display;

    if (row >= m_rows_display) {
        newRowsDisplay = std::max((row + 1) * 2, MIN_DISPLAY_SIZE);
        needsExpansion = true;
    }

    if (col >= m_cols_display) {
        newColsDisplay = std::max((col + 1) * 2, MIN_DISPLAY_SIZE);
        needsExpansion = true;
    }

    if (needsExpansion) {
        beginResetModel();
        m_rows_display = newRowsDisplay;
        m_cols_display = newColsDisplay;

        // Resize edit data while preserving existing values
        QVector<QVariant> newEditData(m_rows_display * m_cols_display);
        for (int r = 0;
             r < std::min(m_rows_display, static_cast<int>(m_editData.size() / m_cols_display));
             ++r) {
            for (int c = 0; c < std::min(m_cols_display, m_cols_display); ++c) {
                const int oldIdx = r + c * (m_editData.size() / m_cols_display);
                const int newIdx = r + c * m_rows_display;
                if (oldIdx < m_editData.size() && newIdx < newEditData.size()) {
                    newEditData[newIdx] = m_editData[oldIdx];
                }
            }
        }

        m_editData = std::move(newEditData);
        endResetModel();
    }

    return true;
}
//=============================================================================
}
//=============================================================================
