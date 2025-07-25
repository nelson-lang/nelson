//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
    : VariableAbstractTableModel(name, array, evaluator, parent)
{
    if (!m_evaluator) {
        qWarning() << "VariableStringTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableStringTableModel: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
    saveCurrentStateForUndo();
}
//=============================================================================
int
VariableStringTableModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_rows_display;
}
//=============================================================================
int
VariableStringTableModel::columnCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_cols_display;
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
    // Check edit data first
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return m_editData[idx];
    }

    // Check original array data
    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const ArrayOf* ptr = static_cast<const ArrayOf*>(m_array.getDataPointer());
        const int arrayIdx = row + col * m_rows;
        if (ptr) {
            if (ptr[arrayIdx].isCharacterArray()) {
                return QVariant(wstringToQString(ptr[arrayIdx].getContentAsWideString()));
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

    // Only support string arrays for now
    if (!m_array.isStringArray()) {
        emit errorOccurred(tr("Can only edit string arrays"));
        return false;
    }

    if (m_array.isEmpty()) {
        Dimensions dims(1, 1);
        m_array.resize(dims);
        ArrayOf* ptr = (ArrayOf*)(m_array.getDataPointer());
        ptr[0] = ArrayOf::doubleConstructor(std::nan("NaN"));
    }

    std::wstring item;
    bool isMissing = false;
    if (value.canConvert<int>() && value.toInt() == 0) {
        isMissing = true;
    } else {
        item = value.toString().toStdWString();
    }

    // OPTIMIZED: Only expand if absolutely necessary and do it efficiently
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    // Set the value in the array
    if (isMissing) {
        if (!setArrayMissingValue(row, col)) {
            return false;
        }
    } else {
        if (!setArrayValue(row, col, item)) {
            return false;
        }
    }

    // Update edit tracking
    updateEditData(row, col, value);

    // OPTIMIZED: Batch persistence instead of immediate
    m_pendingChanges = true;
    schedulePeristenceUpdate();

    // Only emit for the specific cell that changed
    emit dataChanged(index, index);

    return true;
}
//=============================================================================
bool
VariableStringTableModel::expandArrayIfNeeded(int row, int col)
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
VariableStringTableModel::expandDisplay(int row, int col)
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
void
VariableStringTableModel::resizeEditData(int oldRows, int oldCols)
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
VariableStringTableModel::schedulePeristenceUpdate()
{
    if (!m_persistenceTimer) {
        m_persistenceTimer = new QTimer(this);
        m_persistenceTimer->setSingleShot(true);
        m_persistenceTimer->setInterval(100); // 100ms delay
        connect(m_persistenceTimer, &QTimer::timeout, this,
            &VariableStringTableModel::performBatchedPersistence);
    }

    m_persistenceTimer->start();
}
//=============================================================================
void
VariableStringTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
        emit modelChanged(); // Only emit once per batch
    }
}
//=============================================================================
bool
VariableStringTableModel::resizeArray(int row, int col)
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

        // For large arrays, try to resize in-place if possible
        const size_t oldSize = static_cast<size_t>(m_rows) * m_cols;

        // Create new array with the larger size
        ArrayOf* ptr
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, newDims.getElementCount());
        ArrayOf newArray = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);
        ArrayOf* newData = (ArrayOf*)newArray.getDataPointer();
        for (size_t k = 0; k < newDims.getElementCount(); ++k) {
            newData[k] = ArrayOf::doubleConstructor(std::nan("NaN"));
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

    // Only allow editing of double arrays
    if (m_array.isStringArray()) {
        flags |= Qt::ItemIsEditable;
    }

    return flags;
}
//=============================================================================
QVariant
VariableStringTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole) {
        return QString::number(section + 1);
    }
    return QVariant();
}
//=============================================================================
void
VariableStringTableModel::refreshFromArray(ArrayOf& value)
{
    m_array = value;
    beginResetModel();
    initializeDimensions();
    initializeEditData();
    endResetModel();
}
//=============================================================================
bool
VariableStringTableModel::pasteDataFromClipboard(int startRow, int startCol, const QString& text)
{
    if (text.isEmpty()) {
        return false;
    }

    // Parse the paste text
    const PasteData pasteData = parsePasteText(text);
    if (!pasteData.isValid) {
        emit errorOccurred(tr("Invalid paste data format"));
        return false;
    }

    // Validate the paste operation
    if (!validatePasteData(pasteData, startRow, startCol)) {
        return false;
    }

    // Apply the paste data
    return applyPasteData(pasteData, startRow, startCol);
}
//=============================================================================
bool
VariableStringTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    return pasteDataFromClipboard(startRow, startCol, text);
}
//=============================================================================
bool
VariableStringTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    if (text.isEmpty() || startRow < 0 || startCol < 0) {
        return false;
    }

    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid && validatePasteData(pasteData, startRow, startCol);
}
//=============================================================================
QSize
VariableStringTableModel::getPasteSize(const QString& text) const
{
    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid ? QSize(pasteData.cols, pasteData.rows) : QSize(0, 0);
}
//=============================================================================
void
VariableStringTableModel::initializeDimensions()
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
VariableStringTableModel::initializeEditData()
{
    const int totalSize = m_rows_display * m_cols_display;
    m_editData.clear();
    m_editData.resize(totalSize);
}
//=============================================================================
bool
VariableStringTableModel::validateArrayState() const
{
    return !m_variableName.isEmpty();
}
//=============================================================================
bool
VariableStringTableModel::hasOriginalData() const
{
    return m_array.isStringArray() && m_array.getDataPointer() != nullptr;
}
//=============================================================================
bool
VariableStringTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && col >= 0 && row < m_rows_display && col < m_cols_display;
}
//=============================================================================
bool
VariableStringTableModel::expandDisplayIfNeeded(int row, int col)
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
bool
VariableStringTableModel::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true; // No resize needed
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const int newSize = newRows * newCols;

    try {
        // Create new data array
        auto newData = std::make_unique<double[]>(newSize);
        std::fill(newData.get(), newData.get() + newSize, 0.0);

        // Copy existing data
        if (const double* oldData = static_cast<const double*>(m_array.getDataPointer())) {
            for (int c = 0; c < m_cols; ++c) {
                for (int r = 0; r < m_rows; ++r) {
                    const int oldIdx = r + c * m_rows;
                    const int newIdx = r + c * newRows;
                    newData[newIdx] = oldData[oldIdx];
                }
            }
        }

        // Resize array and copy data
        Dimensions newDims(newRows, newCols);
        m_array.resize(newDims);

        if (double* arrayData = (double*)(m_array.getDataPointer())) {
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
    ArrayOf* data = (ArrayOf*)(m_array.getDataPointer());
    if (!data) {
        return false;
    }

    const int idx = row + col * m_rows;
    data[idx] = ArrayOf::doubleConstructor(std::nan("NaN"));
    return true;
}
//=============================================================================

bool
VariableStringTableModel::setArrayValue(int row, int col, const std::wstring& value)
{
    ArrayOf* data = (ArrayOf*)(m_array.getDataPointer());
    if (!data) {
        return false;
    }

    const int idx = row + col * m_rows;
    data[idx] = ArrayOf::characterArrayConstructor(value);
    return true;
}
//=============================================================================
void
VariableStringTableModel::updateEditData(int row, int col, const QVariant& value)
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size()) {
        m_editData[idx] = value;
    }
}
//=============================================================================
void
VariableStringTableModel::persistChangesToContext()
{
    if (!m_evaluator || !m_evaluator->getContext() || m_variableName.isEmpty()) {
        return;
    }

    try {
        m_evaluator->getContext()->getCurrentScope()->insertVariable(
            wstring_to_utf8(QStringTowstring(m_variableName)), m_array);
        WorkspaceBrowser::updateWorkspaceBrowser();
    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to persist changes: %1").arg(e.what()));
    }
}
//=============================================================================
int
VariableStringTableModel::indexToFlat(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return -1;
    }
    return row + col * m_rows_display;
}

std::pair<int, int>
VariableStringTableModel::flatToIndex(int flatIndex) const
{
    if (flatIndex < 0 || m_rows_display <= 0) {
        return { -1, -1 };
    }

    const int row = flatIndex % m_rows_display;
    const int col = flatIndex / m_rows_display;

    return { row, col };
}
//=============================================================================
VariableStringTableModel::PasteData
VariableStringTableModel::parsePasteText(const QString& text) const
{
    PasteData result;

    if (text.isEmpty()) {
        return result;
    }

    // Split into rows, handling different line endings
    const QStringList rows = text.split(QRegularExpression(R"(\r\n|\r|\n)"), Qt::SkipEmptyParts);

    if (rows.isEmpty()) {
        return result;
    }

    result.rows = rows.size();
    result.cols = 0;
    result.cells.resize(result.rows);

    // Parse each row
    for (int r = 0; r < rows.size(); ++r) {
        // Split by tab or comma, prefer tab
        QStringList columns;
        if (rows[r].contains('\t')) {
            columns = rows[r].split('\t', Qt::KeepEmptyParts);
        } else {
            columns = rows[r].split(',', Qt::KeepEmptyParts);
        }

        result.cols = std::max(result.cols, (int)columns.size());
        result.cells[r].resize(columns.size());

        for (int c = 0; c < columns.size(); ++c) {
            result.cells[r][c] = columns[c].trimmed();
        }
    }

    // Validate size limits
    if (result.rows * result.cols > MAX_PASTE_SIZE) {
        return PasteData(); // Invalid - too large
    }

    result.isValid = true;
    return result;
}
//=============================================================================
bool
VariableStringTableModel::validatePasteData(const PasteData& data, int startRow, int startCol) const
{
    if (!data.isValid || startRow < 0 || startCol < 0) {
        return false;
    }

    // Check if paste would exceed reasonable limits
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

    // Calculate affected range
    const int endRow = startRow + data.rows - 1;
    const int endCol = startCol + data.cols - 1;

    // Check if we need a full model reset (for large changes or dimension changes)
    bool needsModelReset = false;
    if (endRow >= m_rows_display || endCol >= m_cols_display
        || (data.rows * data.cols > 100)) { // Large paste operation
        needsModelReset = true;
    }

    if (needsModelReset) {
        beginResetModel();
    }

    // Expand display and array as needed
    if (!expandDisplayIfNeeded(endRow, endCol) || !resizeArrayIfNeeded(endRow, endCol)) {
        if (needsModelReset) {
            endResetModel();
        }
        return false;
    }

    // Track changes for efficient updates
    QVector<QModelIndex> changedIndices;
    changedIndices.reserve(qsizetype(data.rows * data.cols));

    // Apply the data
    bool hasErrors = false;
    QString errorMessage;

    for (int r = 0; r < data.rows; ++r) {
        for (int c = 0; c < data.cols && c < data.cells[r].size(); ++c) {
            const int targetRow = startRow + r;
            const int targetCol = startCol + c;
            const QString& cellText = data.cells[r][c];

            if (cellText.isEmpty()) {
                continue; // Skip empty cells
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

    // Persist changes
    persistChangesToContext();

    if (needsModelReset) {
        endResetModel();
        // Force view update after model reset
        emit modelChanged();
    } else {
        // Emit change notifications for smaller updates
        if (!changedIndices.isEmpty()) {
            // Find bounding rectangle of changes
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

    // Report errors if any
    if (hasErrors) {
        emit errorOccurred(errorMessage);
    }

    return !hasErrors;
}
//=============================================================================
void
VariableStringTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
    emit modelChanged();
}
//=============================================================================
bool
VariableStringTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.getDataClass() == NLS_DOUBLE || value.getDataClass() == NLS_DCOMPLEX;
}
//=============================================================================
bool
VariableStringTableModel::insertRowAt(int row)
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

        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        for (int c = 0; c < cols; ++c) {
            for (int r = 0; r < row; ++r)
                dst[r + c * newRows] = src[r + c * m_rows];

            dst[row + c * newRows] = ArrayOf::doubleConstructor(std::nan("NaN"));

            for (int r = row; r < m_rows; ++r)
                dst[r + 1 + c * newRows] = src[r + c * m_rows];
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
    if (col < 0 || col > m_cols)
        return false;
    saveCurrentStateForUndo();

    int newCols = m_cols + 1;
    int rows = m_rows;
    Dimensions newDims(rows, newCols);

    try {
        size_t total = newCols * rows;
        ArrayOf newArray;
        ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, total);
        ArrayOf* dst = ptr;
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        for (int c = 0; c < col; ++c)
            for (int r = 0; r < rows; ++r)
                dst[r + c * rows] = src[r + c * rows];

        for (int r = 0; r < rows; ++r)
            dst[r + col * rows] = ArrayOf::doubleConstructor(std::nan("NaN"));

        for (int c = col; c < m_cols; ++c)
            for (int r = 0; r < rows; ++r)
                dst[r + (c + 1) * rows] = src[r + c * rows];

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
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        for (int c = 0; c < cols; ++c) {
            int k = 0;
            for (int r = 0; r < m_rows; ++r) {
                if (r == row)
                    continue;
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
        ArrayOf* src = (ArrayOf*)(m_array.getDataPointer());

        int k = 0;
        for (int c = 0; c < m_cols; ++c) {
            if (c == col)
                continue;
            for (int r = 0; r < rows; ++r)
                dst[r + k * rows] = src[r + c * rows];
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
QString
VariableStringTableModel::getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return QString();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return QString();
    }

    QString clipboardText;
    for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
        for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
            QModelIndex index = this->index(r, c);
            clipboardText += data(index, Qt::DisplayRole).toString();
            if (c < bounds.maxCol) {
                clipboardText += "\t";
            }
        }
        if (r < bounds.maxRow) {
            clipboardText += "\n";
        }
    }
    return clipboardText;
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

    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, newDims.getElementCount());
    newVar = ArrayOf(NLS_STRING_ARRAY, newDims, ptr);

    for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
        for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
            QModelIndex idx = index(r, c);
            QVariant val = data(idx, Qt::DisplayRole);
            ptr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.getRows()]
                = ArrayOf::characterArrayConstructor(QStringTowstring(val.toString()));
        }
    }
    return newVar;
}
//=============================================================================
bool
VariableStringTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    return !selectedIndexes.isEmpty() && (m_array.getDataClass() == NLS_STRING_ARRAY);
}
//=============================================================================
bool
VariableStringTableModel::shouldDisplayAsTable() const
{
    return (m_array.getDataClass() == NLS_STRING_ARRAY) && !m_array.isSparse() && m_array.is2D();
}
//=============================================================================
VariableStringTableModel::SelectionBounds
VariableStringTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
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
bool
VariableStringTableModel::replaceSelectedWithMissing(const QModelIndexList& selectedIndexes)
{
    if (selectedIndexes.isEmpty() || !m_evaluator) {
        return false;
    }

    try {
        ArrayOf* arrayData = (ArrayOf*)(m_array.getDataPointer());
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
                arrayData[arrayIdx] = ArrayOf::doubleConstructor(std::nan("NaN"));

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
void
VariableStringTableModel::redo()
{
    // For single-level undo/redo, redo is the same as undo
    // since we swap the current and stored states
    undo();
}
//=============================================================================
void
VariableStringTableModel::undo()
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
}
//=============================================================================
