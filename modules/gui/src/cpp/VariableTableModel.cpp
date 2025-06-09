//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableTableModel.h"
#include "EvaluateCommand.hpp"
#include <QtCore/QStringList>
#include <QtCore/QRegularExpression>
#include <QtGui/QColor>
#include <QtCore/QSize>
#include <QtCore/QtGlobal>
#include <algorithm>
#include <cstring>
#include "ClassName.hpp"
#include <cmath>

namespace Nelson {

VariableTableModel::VariableTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : QAbstractTableModel(parent)
    , m_array(std::move(array))
    , m_variableName(name)
    , m_evaluator(evaluator)
{
    if (!m_evaluator) {
        qWarning() << "VariableTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableTableModel: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
}

int
VariableTableModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_rows_display;
}

int
VariableTableModel::columnCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_cols_display;
}

QVariant
VariableTableModel::data(const QModelIndex& index, int role) const
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
        return static_cast<int>(Qt::AlignCenter);

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

bool
VariableTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
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

    // Parse and validate the input value
    std::wstring command = value.toString().toStdWString();
    ArrayOfVector results = EvaluateCommand(m_evaluator, 1, command, L"");
    if (results.size() != 1) {
        emit errorOccurred(tr("Invalid command or result type"));
        return false;
    }

    ArrayOf resultArray = results[0];
    if (!resultArray.isScalar() || !resultArray.isNumeric()) {
        emit errorOccurred(tr("Command must return a single numeric value"));
        return false;
    }

    // Extract the value
    std::complex<double> cplxValue;
    double dValue;
    NelsonType asType;
    QString displayValue;

    if (resultArray.isComplex()) {
        resultArray.promoteType(NLS_DCOMPLEX);
        cplxValue = resultArray.getContentAsDoubleComplexScalar();
        displayValue = formatDisplayValue(cplxValue);
        asType = NLS_DCOMPLEX;
    } else {
        resultArray.promoteType(NLS_DOUBLE);
        dValue = resultArray.getContentAsDoubleScalar();
        displayValue = formatDisplayValue(dValue);
        asType = NLS_DOUBLE;
    }

    // OPTIMIZED: Only expand if absolutely necessary and do it efficiently
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    // Set the value in the array
    if (asType == NLS_DCOMPLEX) {
        if (!setArrayValue(row, col, cplxValue.real(), cplxValue.imag())) {
            return false;
        }
    } else {
        if (!setArrayValue(row, col, dValue)) {
            return false;
        }
    }

    // Update edit tracking
    updateEditData(row, col, QVariant(displayValue));

    // OPTIMIZED: Batch persistence instead of immediate
    m_pendingChanges = true;
    schedulePeristenceUpdate();

    // Only emit for the specific cell that changed
    emit dataChanged(index, index);

    return true;
}

bool
VariableTableModel::expandArrayIfNeeded(int row, int col)
{
    // Check if we need to expand the actual array data
    const bool needsArrayResize = (row >= m_rows || col >= m_cols);

    // Check if we need to expand the display
    const bool needsDisplayExpansion = (row >= m_rows_display || col >= m_cols_display);

    // Handle array resize first (this is the expensive operation)
    if (needsArrayResize) {
        if (!resizeArrayOptimized(row, col)) {
            return false;
        }
    }

    // Handle display expansion without model reset for small expansions
    if (needsDisplayExpansion) {
        return expandDisplayOptimized(row, col);
    }

    return true;
}

// OPTIMIZED: Avoid model resets for reasonable expansions
bool
VariableTableModel::expandDisplayOptimized(int row, int col)
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
        resizeEditDataOptimized(oldRowsDisplay, oldColsDisplay);
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
        resizeEditDataOptimized(oldRowsDisplay, oldColsDisplay);
    }

    return true;
}

// OPTIMIZED: More efficient edit data resizing
void
VariableTableModel::resizeEditDataOptimized(int oldRows, int oldCols)
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

// OPTIMIZED: Batch persistence to avoid frequent workspace updates
void
VariableTableModel::schedulePeristenceUpdate()
{
    if (!m_persistenceTimer) {
        m_persistenceTimer = new QTimer(this);
        m_persistenceTimer->setSingleShot(true);
        m_persistenceTimer->setInterval(100); // 100ms delay
        connect(m_persistenceTimer, &QTimer::timeout, this,
            &VariableTableModel::performBatchedPersistence);
    }

    m_persistenceTimer->start();
}

void
VariableTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
        emit modelChanged(); // Only emit once per batch
    }
}

// OPTIMIZED: More efficient array resizing for large arrays
bool
VariableTableModel::resizeArrayOptimized(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true; // No resize needed
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const size_t newSize = static_cast<size_t>(newRows) * newCols;

    // Check for reasonable size limits
    const size_t MAX_ELEMENTS = 50000000; // ~400MB for doubles
    if (newSize > MAX_ELEMENTS) {
        return false;
    }

    try {
        // Create new dimensions
        Dimensions newDims(newRows, newCols);

        // For large arrays, try to resize in-place if possible
        const size_t oldSize = static_cast<size_t>(m_rows) * m_cols;

        if (m_array.getDataClass() == NLS_DOUBLE) {
            // Create new array with the larger size
            double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, newDims.getElementCount());
            ArrayOf newArray = ArrayOf(NLS_DOUBLE, newDims, ptr);
            double* newData = (double*)newArray.getDataPointer();

            // Initialize with zeros
            std::fill(newData, newData + newSize, 0.0);

            // Copy existing data efficiently
            if (const double* oldData = (const double*)m_array.getDataPointer()) {
                for (int c = 0; c < m_cols; ++c) {
                    const size_t oldColStart = static_cast<size_t>(c) * m_rows;
                    const size_t newColStart = static_cast<size_t>(c) * newRows;

                    // Copy entire columns using memcpy for better performance
                    std::memcpy(
                        newData + newColStart, oldData + oldColStart, sizeof(double) * m_rows);
                }
            }

            // Replace the array
            m_array = std::move(newArray);
        }

        m_rows = newRows;
        m_cols = newCols;

        return true;

    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to resize array: %1").arg(e.what()));
        return false;
    }
}

Qt::ItemFlags
VariableTableModel::flags(const QModelIndex& index) const
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

QVariant
VariableTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole) {
        return QString::number(section + 1);
    }
    return QVariant();
}

QString
VariableTableModel::getClassName() const
{
    std::string classname;
    ClassName(m_array, classname);
    return QString::fromStdString(classname);
}

void
VariableTableModel::refreshFromArray(ArrayOf& value)
{
    // if (!isSame(value)) {
    m_array = value;
    beginResetModel();
    initializeDimensions();
    initializeEditData();
    endResetModel();
    //  emit modelChanged();
    //}
}

bool
VariableTableModel::isSame(ArrayOf& value)
{
    if (!m_array.getDimensions().equals(value.getDimensions())) {
        return false;
    }

    if (m_array.getDataClass() != value.getDataClass()) {
        return false;
    }

    const size_t count = m_array.getElementCount();

    if (m_array.getDataClass() == NLS_DOUBLE) {
        auto compareDouble = [](double a, double b) -> bool {
            if (std::isnan(a) && std::isnan(b))
                return true;
            if (std::isinf(a) && std::isinf(b))
                return std::signbit(a) == std::signbit(b);
            return a == b;
        };

        auto* ptrA = static_cast<const double*>(m_array.getDataPointer());
        auto* ptrB = static_cast<const double*>(value.getDataPointer());

        for (size_t k = 0; k < count; ++k) {
            if (!compareDouble(ptrA[k], ptrB[k])) {
                return false;
            }
        }

        return true;
    }

    if (m_array.getDataClass() == NLS_DCOMPLEX) {
        auto compareComplex
            = [](const std::complex<double>& a, const std::complex<double>& b) -> bool {
            auto eq = [](double x, double y) -> bool {
                if (std::isnan(x) && std::isnan(y))
                    return true;
                if (std::isinf(x) && std::isinf(y))
                    return std::signbit(x) == std::signbit(y);
                return x == y;
            };
            return eq(a.real(), b.real()) && eq(a.imag(), b.imag());
        };
        auto* ptrA = static_cast<const std::complex<double>*>(m_array.getDataPointer());
        auto* ptrB = static_cast<const std::complex<double>*>(value.getDataPointer());

        for (size_t k = 0; k < count; ++k) {
            if (!compareComplex(ptrA[k], ptrB[k])) {
                return false;
            }
        }

        return true;
    }
    return false;
}

bool
VariableTableModel::pasteData(int startRow, int startCol, const QString& text)
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

bool
VariableTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    if (text.isEmpty() || startRow < 0 || startCol < 0) {
        return false;
    }

    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid && validatePasteData(pasteData, startRow, startCol);
}

QSize
VariableTableModel::getPasteSize(const QString& text) const
{
    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid ? QSize(pasteData.cols, pasteData.rows) : QSize(0, 0);
}

// Private implementation methods

void
VariableTableModel::initializeDimensions()
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

void
VariableTableModel::initializeEditData()
{
    const int totalSize = m_rows_display * m_cols_display;
    m_editData.clear();
    m_editData.resize(totalSize);
}

bool
VariableTableModel::validateArrayState() const
{
    return !m_variableName.isEmpty();
}

QVariant
VariableTableModel::getDisplayValue(int row, int col) const
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

bool
VariableTableModel::hasOriginalData() const
{
    return m_array.isDoubleType(false) && m_array.getDataPointer() != nullptr;
}

bool
VariableTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && col >= 0 && row < m_rows_display && col < m_cols_display;
}

bool
VariableTableModel::expandDisplayIfNeeded(int row, int col)
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

bool
VariableTableModel::resizeArrayIfNeeded(int row, int col)
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

bool
VariableTableModel::setArrayValue(int row, int col, double realpart, double imagpart)
{
    if (m_array.getDataClass() == NLS_DOUBLE) {
        m_array.promoteType(NLS_DCOMPLEX);
    }
    double* data = (double*)(m_array.getDataPointer());
    auto* ptrz = reinterpret_cast<std::complex<double>*>((double*)data);

    const int idx = row + col * m_rows;
    ptrz[idx] = std::complex<double>(realpart, imagpart);
    return true;
}

bool
VariableTableModel::setArrayValue(int row, int col, double value)
{
    double* data = (double*)(m_array.getDataPointer());
    if (!data) {
        emit errorOccurred(tr("No array data pointer available"));
        return false;
    }

    const int idx = row + col * m_rows;
    data[idx] = value;
    return true;
}

void
VariableTableModel::updateEditData(int row, int col, const QVariant& value)
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size()) {
        m_editData[idx] = value;
    }
}

bool
VariableTableModel::parseNumericValue(const QString& text, double& value) const
{
    const QString trimmed = text.trimmed();

    if (trimmed.isEmpty()) {
        value = 0.0;
        return true;
    }

    bool ok = false;
    value = trimmed.toDouble(&ok);

    if (!ok) {
        // Try to handle some common formats
        static const QRegularExpression scientificRegex(R"(^[+-]?\d*\.?\d*[eE][+-]?\d+$)");
        if (scientificRegex.match(trimmed).hasMatch()) {
            value = trimmed.toDouble(&ok);
        }
    }

    return ok;
}

QString
VariableTableModel::formatDisplayValue(std::complex<double> value) const
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

QString
VariableTableModel::formatDisplayValue(double value) const
{
    // Handle special values
    if (std::isnan(value))
        return "NaN";
    if (std::isinf(value))
        return value > 0 ? "Inf" : "-Inf";

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

void
VariableTableModel::persistChangesToContext()
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

int
VariableTableModel::indexToFlat(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return -1;
    }
    return row + col * m_rows_display;
}

std::pair<int, int>
VariableTableModel::flatToIndex(int flatIndex) const
{
    if (flatIndex < 0 || m_rows_display <= 0) {
        return { -1, -1 };
    }

    const int row = flatIndex % m_rows_display;
    const int col = flatIndex / m_rows_display;

    return { row, col };
}

VariableTableModel::PasteData
VariableTableModel::parsePasteText(const QString& text) const
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

bool
VariableTableModel::validatePasteData(const PasteData& data, int startRow, int startCol) const
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

    // For double arrays, validate that all cells can be parsed as numbers
    if (m_array.getDataClass() == NLS_DOUBLE) {
        for (const auto& row : data.cells) {
            for (const QString& cell : row) {
                if (!cell.isEmpty()) {
                    double dummy;
                    if (!parseNumericValue(cell, dummy)) {
                        return false;
                    }
                }
            }
        }
    }

    return true;
}

bool
VariableTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
{
    if (!data.isValid) {
        return false;
    }

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
    changedIndices.reserve(data.rows * data.cols);

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

            // Parse and set value
            double value = 0.0;
            if (parseNumericValue(cellText, value)) {
                if (setArrayValue(targetRow, targetCol, value)) {
                    updateEditData(targetRow, targetCol, QVariant(value));
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
            } else {
                hasErrors = true;
                if (errorMessage.isEmpty()) {
                    errorMessage = tr("Invalid value '%1' at position (%2, %3)")
                                       .arg(cellText)
                                       .arg(r + 1)
                                       .arg(c + 1);
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

void
VariableTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
    emit modelChanged();
}

bool
VariableTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.getDataClass() == NLS_DOUBLE || value.getDataClass() == NLS_DCOMPLEX;
}

}
