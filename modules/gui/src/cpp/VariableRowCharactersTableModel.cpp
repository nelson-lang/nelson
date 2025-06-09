//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableRowCharactersTableModel.h"
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
VariableRowCharactersTableModel::VariableRowCharactersTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableAbstractTableModel(name, array, evaluator, parent)
{
    if (!m_evaluator) {
        qWarning() << "VariableRowCharactersTableModel: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableRowCharactersTableModel: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
    saveCurrentStateForUndo();
}
//=============================================================================
int
VariableRowCharactersTableModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_rows_display;
}
//=============================================================================
int
VariableRowCharactersTableModel::columnCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_cols_display;
}
//=============================================================================
QVariant
VariableRowCharactersTableModel::data(const QModelIndex& index, int role) const
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
        return displayValue;

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
    return QVariant();
}
//=============================================================================
QVariant
VariableRowCharactersTableModel::getDisplayValue(int row, int col) const
{
    // Check edit data first
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return m_editData[idx];
    }

    // Check original array data
    if (row < m_rows && col < m_cols && hasOriginalData()) {
        return QVariant(wstringToQString(m_array.getContentAsWideString()));
    }
    return QVariant();
}
//=============================================================================
bool
VariableRowCharactersTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
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

    // Only support double arrays for now
    if (!m_array.isRowVectorCharacterArray()) {
        emit errorOccurred(tr("Can only edit row characters arrays."));
        return false;
    }

    std::wstring item = value.toString().toStdWString();

    // Set the value in the array
    if (!setArrayValue(row, col, item)) {
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
void
VariableRowCharactersTableModel::schedulePeristenceUpdate()
{
    if (!m_persistenceTimer) {
        m_persistenceTimer = new QTimer(this);
        m_persistenceTimer->setSingleShot(true);
        m_persistenceTimer->setInterval(100); // 100ms delay
        connect(m_persistenceTimer, &QTimer::timeout, this,
            &VariableRowCharactersTableModel::performBatchedPersistence);
    }

    m_persistenceTimer->start();
}
//=============================================================================
void
VariableRowCharactersTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
        emit modelChanged(); // Only emit once per batch
    }
}
//=============================================================================
Qt::ItemFlags
VariableRowCharactersTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;

    // Only allow editing of double arrays
    if (m_array.isRowVectorCharacterArray()) {
        flags |= Qt::ItemIsEditable;
    }

    return flags;
}
//=============================================================================
QVariant
VariableRowCharactersTableModel::headerData(
    int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole) {
        return QString::number(section + 1);
    }
    return QVariant();
}
//=============================================================================
void
VariableRowCharactersTableModel::refreshFromArray(ArrayOf& value)
{
    m_array = value;
    beginResetModel();
    initializeDimensions();
    initializeEditData();
    endResetModel();
}
//=============================================================================
bool
VariableRowCharactersTableModel::pasteDataFromClipboard(
    int startRow, int startCol, const QString& text)
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
VariableRowCharactersTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    return pasteDataFromClipboard(startCol, startRow, text);
}
//=============================================================================

bool
VariableRowCharactersTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    if (text.isEmpty() || startRow < 0 || startCol < 0) {
        return false;
    }

    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid && validatePasteData(pasteData, startRow, startCol);
}
//=============================================================================
QSize
VariableRowCharactersTableModel::getPasteSize(const QString& text) const
{
    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid ? QSize(pasteData.cols, pasteData.rows) : QSize(0, 0);
}
//=============================================================================
void
VariableRowCharactersTableModel::initializeDimensions()
{
    m_rows = 1;
    m_cols = 1;
    m_rows_display = 1;
    m_cols_display = 1;
}
//=============================================================================
void
VariableRowCharactersTableModel::initializeEditData()
{
    const int totalSize = m_rows_display * m_cols_display;
    m_editData.clear();
    m_editData.resize(totalSize);
}
//=============================================================================
bool
VariableRowCharactersTableModel::validateArrayState() const
{
    return !m_variableName.isEmpty();
}
//=============================================================================
bool
VariableRowCharactersTableModel::hasOriginalData() const
{
    return m_array.isRowVectorCharacterArray() && m_array.getDataPointer() != nullptr;
}
//=============================================================================
bool
VariableRowCharactersTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && col >= 0 && row < m_rows_display && col < m_cols_display;
}
//=============================================================================
bool
VariableRowCharactersTableModel::setArrayValue(int row, int col, const std::wstring& value)
{
    m_array = ArrayOf::characterArrayConstructor(value);
    return true;
}
//=============================================================================
void
VariableRowCharactersTableModel::updateEditData(int row, int col, const QVariant& value)
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size()) {
        m_editData[idx] = value;
    }
}
//=============================================================================
void
VariableRowCharactersTableModel::persistChangesToContext()
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
VariableRowCharactersTableModel::indexToFlat(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return -1;
    }
    return row + col * m_rows_display;
}

std::pair<int, int>
VariableRowCharactersTableModel::flatToIndex(int flatIndex) const
{
    if (flatIndex < 0 || m_rows_display <= 0) {
        return { -1, -1 };
    }

    const int row = flatIndex % m_rows_display;
    const int col = flatIndex / m_rows_display;

    return { row, col };
}
//=============================================================================
VariableRowCharactersTableModel::PasteData
VariableRowCharactersTableModel::parsePasteText(const QString& text) const
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

    result.rows = 1;
    result.cols = 1;
    result.cells.resize(1);
    result.cells[0].resize(1);

    QString concatenated;
    for (int r = 0; r < rows.size(); ++r) {
        if (rows[r].contains('\t')) {
            concatenated += rows[r].split('\t', Qt::KeepEmptyParts).join("");
        } else {
            concatenated += rows[r].trimmed();
        }
    }

    result.cells[0][0] = concatenated;
    result.isValid = true;

    return result;
}
//=============================================================================
bool
VariableRowCharactersTableModel::validatePasteData(
    const PasteData& data, int startRow, int startCol) const
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
VariableRowCharactersTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
{
    if (!data.isValid || data.rows != 1 || data.cols != 1) {
        return false;
    }

    saveCurrentStateForUndo();

    if (!isValidPosition(startRow, startCol)) {
        return false;
    }

    QModelIndex targetIndex = index(startRow, startCol);
    QString value = data.cells[0][0];

    bool success = setData(targetIndex, value, Qt::EditRole);
    if (success) {
        m_pendingChanges = true;
        schedulePeristenceUpdate();
        emit dataChanged(targetIndex, targetIndex);
    }

    return success;
}
//=============================================================================
void
VariableRowCharactersTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
    emit modelChanged();
}
//=============================================================================
bool
VariableRowCharactersTableModel::isStructureCompatible(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray();
}
//=============================================================================
bool
VariableRowCharactersTableModel::insertRowAt(int row)
{
    return false;
}
//=============================================================================
bool
VariableRowCharactersTableModel::insertColumnAt(int col)
{
    return false;
}
//=============================================================================
bool
VariableRowCharactersTableModel::deleteRowAt(int row)
{
    return false;
}
//=============================================================================
bool
VariableRowCharactersTableModel::deleteColumnAt(int col)
{
    return false;
}
//=============================================================================
QString
VariableRowCharactersTableModel::getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
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
VariableRowCharactersTableModel::createArrayFromSelection(
    const QModelIndexList& selectedIndexes) const
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
            double num = val.toDouble();
            ptr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.getRows()]
                = ArrayOf::characterArrayConstructor(QStringTowstring(val.toString()));
        }
    }
    return newVar;
}
//=============================================================================
bool
VariableRowCharactersTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    return !selectedIndexes.isEmpty() && (m_array.getDataClass() == NLS_STRING_ARRAY);
}
//=============================================================================
bool
VariableRowCharactersTableModel::shouldDisplayAsTable() const
{
    return (m_array.getDataClass() == NLS_STRING_ARRAY) && !m_array.isSparse() && m_array.is2D();
}
//=============================================================================
VariableRowCharactersTableModel::SelectionBounds
VariableRowCharactersTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
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
VariableRowCharactersTableModel::redo()
{
    // For single-level undo/redo, redo is the same as undo
    // since we swap the current and stored states
    undo();
}
//=============================================================================
void
VariableRowCharactersTableModel::undo()
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
