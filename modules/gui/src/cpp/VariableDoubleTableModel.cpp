//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableDoubleTableModel.h"
#include "EvaluateCommand.hpp"
#include "ClassName.hpp"
#include <QtCore/QStringList>
#include <QtCore/QRegularExpression>
#include <QtGui/QColor>
#include <QtCore/QSize>
#include <QtCore/QDebug>
#include <QtCore/QtGlobal>
#include <algorithm>
#include <cstring>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
VariableDoubleTableModel::VariableDoubleTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableAbstractTableModel(name, array, evaluator, parent)
{
    if (!m_evaluator) {
        qWarning() << "VariableDoubleTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableDoubleTableModel: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
    saveCurrentStateForUndo();
}
//=============================================================================
int
VariableDoubleTableModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_rows_display;
}
//=============================================================================
int
VariableDoubleTableModel::columnCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_cols_display;
}
//=============================================================================
QVariant
VariableDoubleTableModel::data(const QModelIndex& index, int role) const
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
VariableDoubleTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || role != Qt::EditRole) {
        return false;
    }

    const int row = index.row();
    const int col = index.column();

    if (!isValidPosition(row, col)) {
        return false;
    }

    // Only support double arrays for now
    if (!m_array.isDoubleType(false)) {
        emit errorOccurred(tr("Can only edit double arrays"));
        return false;
    }

    saveCurrentStateForUndo();

    // Parse and validate the input value
    std::wstring command = value.toString().toStdWString();
    ArrayOfVector results;
    try {
        results = EvaluateCommand(m_evaluator, 1, command, L"");
    } catch (Exception&) {
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

    if (resultArray.isComplex() && !m_array.isComplex()) {
        m_array.promoteType(NLS_DCOMPLEX);
    } else if (!resultArray.isComplex() && m_array.isComplex()) {
        resultArray.promoteType(NLS_DCOMPLEX);
    }

    if (m_array.isEmpty()) {
        if (m_array.getDataClass() == NLS_DCOMPLEX) {
            m_array = ArrayOf::dcomplexConstructor(0, 0);
        } else {
            m_array = ArrayOf::doubleConstructor(0);
        }
    }
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    // Set the value in the array
    if (!setArrayValue(row, col, resultArray)) {
        return false;
    }

    // Update edit tracking
    if (resultArray.isComplex()) {
        std::complex<double> cplxValue = resultArray.getContentAsDoubleComplexScalar();
        updateEditData(row, col, QVariant(formatDisplayValue(cplxValue)));
    } else {
        double numericValue = resultArray.getContentAsDoubleScalar();
        updateEditData(row, col, QVariant(formatDisplayValue(numericValue)));
    }

    // OPTIMIZED: Batch persistence instead of immediate
    m_pendingChanges = true;
    schedulePeristenceUpdate();

    // Only emit for the specific cell that changed
    emit dataChanged(index, index);

    return true;
}
//=============================================================================
bool
VariableDoubleTableModel::expandArrayIfNeeded(int row, int col)
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
VariableDoubleTableModel::expandDisplay(int row, int col)
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
VariableDoubleTableModel::resizeEditData(int oldRows, int oldCols)
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
VariableDoubleTableModel::schedulePeristenceUpdate()
{
    if (!m_persistenceTimer) {
        m_persistenceTimer = new QTimer(this);
        m_persistenceTimer->setSingleShot(true);
        m_persistenceTimer->setInterval(100); // 100ms delay
        connect(m_persistenceTimer, &QTimer::timeout, this,
            &VariableDoubleTableModel::performBatchedPersistence);
    }

    m_persistenceTimer->start();
}
//=============================================================================
void
VariableDoubleTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
        emit modelChanged(); // Only emit once per batch
    }
}
//=============================================================================
bool
VariableDoubleTableModel::resizeArray(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true; // No resize needed
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const size_t newSize = static_cast<size_t>(newRows) * newCols;

    const size_t MAX_ELEMENTS = 50000000; // ~400MB for doubles
    if (newSize > MAX_ELEMENTS) {
        return false;
    }

    try {
        const NelsonType cls = m_array.getDataClass();
        if (cls != NLS_DOUBLE && cls != NLS_DCOMPLEX) {
            return false; // Unsupported type
        }

        const size_t oldSize = static_cast<size_t>(m_rows) * m_cols;
        const size_t elementSize = (cls == NLS_DCOMPLEX) ? 2 : 1;
        const size_t totalNewSize = newSize * elementSize;

        Dimensions newDims(newRows, newCols);
        double* ptr = (double*)ArrayOf::allocateArrayOf(cls, newDims.getElementCount());
        ArrayOf newArray(cls, newDims, ptr);
        double* newData = (double*)newArray.getDataPointer();

        // Initialize with zeros
        std::fill(newData, newData + totalNewSize, 0.0);

        // Copy old data column-wise
        if (const double* oldData = (const double*)m_array.getDataPointer()) {
            for (int c = 0; c < m_cols; ++c) {
                const size_t oldColStart = static_cast<size_t>(c) * m_rows * elementSize;
                const size_t newColStart = static_cast<size_t>(c) * newRows * elementSize;
                std::memcpy(newData + newColStart, oldData + oldColStart,
                    sizeof(double) * m_rows * elementSize);
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
VariableDoubleTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;

    // Only allow editing of double arrays
    if (m_array.isDoubleType(false)) {
        flags |= Qt::ItemIsEditable;
    }

    return flags;
}
//=============================================================================
QVariant
VariableDoubleTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole) {
        return QString::number(section + 1);
    }
    return QVariant();
}
//=============================================================================
void
VariableDoubleTableModel::refreshFromArray(ArrayOf& value)
{
    m_array = value;
    beginResetModel();
    initializeDimensions();
    initializeEditData();
    endResetModel();
}
//=============================================================================
bool
VariableDoubleTableModel::pasteDataFromClipboard(int startRow, int startCol, const QString& text)
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
VariableDoubleTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
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
VariableDoubleTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    if (text.isEmpty() || startRow < 0 || startCol < 0) {
        return false;
    }

    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid && validatePasteData(pasteData, startRow, startCol);
}
//=============================================================================
QSize
VariableDoubleTableModel::getPasteSize(const QString& text) const
{
    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid ? QSize(pasteData.cols, pasteData.rows) : QSize(0, 0);
}
//=============================================================================
void
VariableDoubleTableModel::initializeDimensions()
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
VariableDoubleTableModel::initializeEditData()
{
    const int totalSize = m_rows_display * m_cols_display;
    m_editData.clear();
    m_editData.resize(totalSize);
}
//=============================================================================
bool
VariableDoubleTableModel::validateArrayState() const
{
    return !m_variableName.isEmpty();
}
//=============================================================================
QVariant
VariableDoubleTableModel::getDisplayValue(int row, int col) const
{
    // Check edit data first
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return m_editData[idx];
    }

    // Check original array data
    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const double* ptr = static_cast<const double*>(m_array.getDataPointer());
        const int arrayIdx = row + col * m_rows;
        if (ptr) {
            if (m_array.isComplex()) {
                auto* ptrz = reinterpret_cast<std::complex<double>*>((double*)ptr);
                return QVariant(formatDisplayValue(ptrz[arrayIdx]));

            } else {
                return QVariant(formatDisplayValue(ptr[arrayIdx]));
            }
        }
    }
    return QVariant();
}
//=============================================================================
bool
VariableDoubleTableModel::hasOriginalData() const
{
    return m_array.isDoubleType(false) && m_array.getDataPointer() != nullptr;
}
//=============================================================================
bool
VariableDoubleTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && col >= 0 && row < m_rows_display && col < m_cols_display;
}
//=============================================================================
bool
VariableDoubleTableModel::expandDisplayIfNeeded(int row, int col)
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
VariableDoubleTableModel::resizeArrayIfNeeded(int row, int col)
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
VariableDoubleTableModel::setArrayValue(int row, int col, ArrayOf& value)
{
    if (m_array.getDataClass() == NLS_DOUBLE && value.isComplex()) {
        m_array.promoteType(NLS_DCOMPLEX);
    }

    double* data = (double*)(m_array.getDataPointer());
    if (!data) {
        emit errorOccurred(tr("No array data pointer available"));
        return false;
    }

    const int idx = row + col * m_rows;
    if (value.isComplex()) {
        std::complex<double> c = value.getContentAsDoubleComplexScalar();
        data[2 * idx] = c.real();
        data[2 * idx + 1] = c.imag();
    } else {
        data[idx] = value.getContentAsDoubleScalar();
    }
    if (m_array.allReal()) {
        m_array.promoteType(NLS_DOUBLE);
    }
    return true;
}
//=============================================================================
void
VariableDoubleTableModel::updateEditData(int row, int col, const QVariant& value)
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size()) {
        m_editData[idx] = value;
    }
}
//=============================================================================
QString
VariableDoubleTableModel::formatDisplayValue(std::complex<double> value) const
{
    if (std::isnan(value.real()) || std::isnan(value.imag())) {
        return "NaN";
    }
    if (std::isinf(value.real()) || std::isinf(value.imag())) {
        return value.real() > 0 ? "Inf" : "-Inf";
    }
    if (value.imag() == 0.0) {
        return formatDisplayValue(value.real());
    }
    if (value.imag() < 0.0) {
        return formatDisplayValue(value.real()) + " - " + formatDisplayValue(-value.imag()) + "i";
    }
    return formatDisplayValue(value.real()) + " + " + formatDisplayValue(value.imag()) + "i";
}
//=============================================================================
QString
VariableDoubleTableModel::formatDisplayValue(double value) const
{
    // Handle special values
    if (std::isnan(value)) {
        return "NaN";
    }
    if (std::isinf(value)) {
        return value > 0 ? "Inf" : "-Inf";
    }

    // Format normal values
    if (value == 0.0) {
        return "0.";
    } else if (std::abs(value) < 1e-15 && value != 0.0) {
        return QString::number(value, 'e', 6);
    } else if (std::abs(value) > 1e6 || std::abs(value) < 1e-3) {
        return QString::number(value, 'e', 6);
    } else {
        return QString::number(value, 'g', 8);
    }
}
//=============================================================================
void
VariableDoubleTableModel::persistChangesToContext()
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
VariableDoubleTableModel::indexToFlat(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return -1;
    }
    return row + col * m_rows_display;
}

std::pair<int, int>
VariableDoubleTableModel::flatToIndex(int flatIndex) const
{
    if (flatIndex < 0 || m_rows_display <= 0) {
        return { -1, -1 };
    }

    const int row = flatIndex % m_rows_display;
    const int col = flatIndex / m_rows_display;

    return { row, col };
}
//=============================================================================
VariableDoubleTableModel::PasteData
VariableDoubleTableModel::parsePasteText(const QString& text) const
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
VariableDoubleTableModel::validatePasteData(const PasteData& data, int startRow, int startCol) const
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

    for (const auto& row : data.cells) {
        for (const QString& cell : row) {
            std::wstring command = cell.toStdWString();
            ArrayOfVector results;
            try {
                results = EvaluateCommand(m_evaluator, 1, command, L"");
            } catch (Exception&) {
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
VariableDoubleTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
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
            } catch (Exception&) {
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
            if (value.isComplex()) {
                value.promoteType(NLS_DCOMPLEX);
            } else {
                value.promoteType(NLS_DOUBLE);
            }
            if (setArrayValue(targetRow, targetCol, value)) {
                if (value.isComplex()) {
                    std::complex<double> cplxValue = value.getContentAsDoubleComplexScalar();
                    updateEditData(targetRow, targetCol, QVariant(formatDisplayValue(cplxValue)));
                } else {
                    double numericValue = value.getContentAsDoubleScalar();
                    updateEditData(
                        targetRow, targetCol, QVariant(formatDisplayValue(numericValue)));
                }
            } else {
                return false;
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

    return true;
}
//=============================================================================
void
VariableDoubleTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
    emit modelChanged();
}
//=============================================================================
bool
VariableDoubleTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.getDataClass() == NLS_DOUBLE || value.getDataClass() == NLS_DCOMPLEX;
}
//=============================================================================
bool
VariableDoubleTableModel::insertRowAt(int row)
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
        if (m_array.getDataClass() == NLS_DCOMPLEX) {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, total);
            std::complex<double>* dst = reinterpret_cast<std::complex<double>*>(ptr);
            std::complex<double>* src
                = reinterpret_cast<std::complex<double>*>((double*)m_array.getDataPointer());

            for (int c = 0; c < cols; ++c) {
                for (int r = 0; r < row; ++r) {
                    dst[r + c * newRows] = src[r + c * m_rows];
                }

                dst[row + c * newRows] = std::complex<double>(0.0, 0.0);

                for (int r = row; r < m_rows; ++r) {
                    dst[r + 1 + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(NLS_DCOMPLEX, newDims, ptr);
        } else {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, total);
            double* dst = ptr;
            double* src = (double*)(m_array.getDataPointer());

            for (int c = 0; c < cols; ++c) {
                for (int r = 0; r < row; ++r) {
                    dst[r + c * newRows] = src[r + c * m_rows];
                }

                dst[row + c * newRows] = 0.0;

                for (int r = row; r < m_rows; ++r) {
                    dst[r + 1 + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(NLS_DOUBLE, newDims, ptr);
        }

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
VariableDoubleTableModel::insertColumnAt(int col)
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
        if (m_array.getDataClass() == NLS_DCOMPLEX) {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, total);
            std::complex<double>* dst = reinterpret_cast<std::complex<double>*>(ptr);
            std::complex<double>* src
                = reinterpret_cast<std::complex<double>*>((double*)m_array.getDataPointer());

            for (int c = 0; c < col; ++c) {
                for (int r = 0; r < rows; ++r) {
                    dst[r + c * rows] = src[r + c * rows];
                }
            }

            for (int r = 0; r < rows; ++r) {
                dst[r + col * rows] = std::complex<double>(0.0, 0.0);
            }

            for (int c = col; c < m_cols; ++c) {
                for (int r = 0; r < rows; ++r) {
                    dst[r + (c + 1) * rows] = src[r + c * rows];
                }
            }

            newArray = ArrayOf(NLS_DCOMPLEX, newDims, ptr);
        } else {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, total);
            double* dst = ptr;
            double* src = (double*)(m_array.getDataPointer());

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

            newArray = ArrayOf(NLS_DOUBLE, newDims, ptr);
        }

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
VariableDoubleTableModel::deleteRowAt(int row)
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
        if (m_array.getDataClass() == NLS_DCOMPLEX) {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, total);
            std::complex<double>* dst = reinterpret_cast<std::complex<double>*>(ptr);
            std::complex<double>* src
                = reinterpret_cast<std::complex<double>*>((double*)m_array.getDataPointer());

            for (int c = 0; c < cols; ++c) {
                int k = 0;
                for (int r = 0; r < m_rows; ++r) {
                    if (r == row) {
                        continue;
                    }
                    dst[k++ + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(NLS_DCOMPLEX, newDims, ptr);
        } else {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, total);
            double* dst = ptr;
            double* src = (double*)(m_array.getDataPointer());

            for (int c = 0; c < cols; ++c) {
                int k = 0;
                for (int r = 0; r < m_rows; ++r) {
                    if (r == row) {
                        continue;
                    }
                    dst[k++ + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(NLS_DOUBLE, newDims, ptr);
        }

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
VariableDoubleTableModel::deleteColumnAt(int col)
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
        if (m_array.getDataClass() == NLS_DCOMPLEX) {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, total);
            std::complex<double>* dst = reinterpret_cast<std::complex<double>*>(ptr);
            std::complex<double>* src
                = reinterpret_cast<std::complex<double>*>((double*)m_array.getDataPointer());

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

            newArray = ArrayOf(NLS_DCOMPLEX, newDims, ptr);
        } else {
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, total);
            double* dst = ptr;
            double* src = (double*)(m_array.getDataPointer());

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

            newArray = ArrayOf(NLS_DOUBLE, newDims, ptr);
        }

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
VariableDoubleTableModel::getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
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
VariableDoubleTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
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

    if (m_array.getDataClass() == NLS_DOUBLE) {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, newDims.getElementCount());
        newVar = ArrayOf(NLS_DOUBLE, newDims, ptr);

        for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
            for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
                QModelIndex idx = index(r, c);
                QVariant val = data(idx, Qt::DisplayRole);
                double num = val.toDouble();
                ptr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.getRows()] = num;
            }
        }
    } else if (m_array.getDataClass() == NLS_DCOMPLEX) {
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, newDims.getElementCount());
        std::complex<double>* complexPtr = reinterpret_cast<std::complex<double>*>(ptr);
        newVar = ArrayOf(NLS_DCOMPLEX, newDims, ptr);

        for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
            for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
                double* ptrModel = (double*)m_array.getDataPointer();
                std::complex<double>* complexPtrModel
                    = reinterpret_cast<std::complex<double>*>(ptrModel);
                complexPtr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.getRows()]
                    = complexPtrModel[r + c * m_array.getDimensions().getRows()];
            }
        }
    }

    return newVar;
}
//=============================================================================
bool
VariableDoubleTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    return !selectedIndexes.isEmpty()
        && (m_array.getDataClass() == NLS_DOUBLE || m_array.getDataClass() == NLS_DCOMPLEX);
}
//=============================================================================
bool
VariableDoubleTableModel::shouldDisplayAsTable() const
{
    return (m_array.getDataClass() == NLS_DOUBLE || m_array.getDataClass() == NLS_DCOMPLEX)
        && !m_array.isSparse() && m_array.is2D();
}
//=============================================================================
VariableDoubleTableModel::SelectionBounds
VariableDoubleTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
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
VariableDoubleTableModel::redo()
{
    // For single-level undo/redo, redo is the same as undo
    // since we swap the current and stored states
    undo();
}
//=============================================================================
void
VariableDoubleTableModel::undo()
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
