//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableNumericTableModel.hpp"
#include "ClassName.hpp"
#include <QtCore/QStringList>
#include <QtCore/QRegularExpression>
#include <QtCore/QSize>
#include <QtCore/QtGlobal>
#include <algorithm>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
VariableNumericTableModel::VariableNumericTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableAbstractTableModel(name, array, evaluator, parent)
{
}
//=============================================================================
int
VariableNumericTableModel::rowCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_rows_display;
}
//=============================================================================
int
VariableNumericTableModel::columnCount(const QModelIndex& parent) const
{
    return parent.isValid() ? 0 : m_cols_display;
}
//=============================================================================
bool
VariableNumericTableModel::expandArrayIfNeeded(int row, int col)
{
    const bool needsArrayResize = (row >= m_rows || col >= m_cols);

    const bool needsDisplayExpansion = (row >= m_rows_display || col >= m_cols_display);

    if (needsArrayResize) {
        if (!resizeArray(row, col)) {
            return false;
        }
    }

    if (needsDisplayExpansion) {
        return expandDisplay(row, col);
    }

    return true;
}
//=============================================================================
bool
VariableNumericTableModel::expandDisplay(int row, int col)
{
    const int oldRowsDisplay = m_rows_display;
    const int oldColsDisplay = m_cols_display;

    int newRowsDisplay = m_rows_display;
    int newColsDisplay = m_cols_display;

    if (row >= m_rows_display) {
        newRowsDisplay = std::max(row + 1 + DISPLAY_MARGIN, MIN_DISPLAY_SIZE);
    }
    if (col >= m_cols_display) {
        newColsDisplay = std::max(col + 1 + DISPLAY_MARGIN, MIN_DISPLAY_SIZE);
    }

    const int rowsToAdd = newRowsDisplay - oldRowsDisplay;
    const int colsToAdd = newColsDisplay - oldColsDisplay;

    const int RESET_THRESHOLD = 1000;

    if (rowsToAdd * newColsDisplay + colsToAdd * oldRowsDisplay > RESET_THRESHOLD) {
        beginResetModel();
        m_rows_display = newRowsDisplay;
        m_cols_display = newColsDisplay;
        resizeEditData(oldRowsDisplay, oldColsDisplay);
        endResetModel();
    } else {

        if (colsToAdd > 0) {
            beginInsertColumns(QModelIndex(), oldColsDisplay, newColsDisplay - 1);
            m_cols_display = newColsDisplay;
            endInsertColumns();
        }

        if (rowsToAdd > 0) {
            beginInsertRows(QModelIndex(), oldRowsDisplay, newRowsDisplay - 1);
            m_rows_display = newRowsDisplay;
            endInsertRows();
        }

        resizeEditData(oldRowsDisplay, oldColsDisplay);
    }

    return true;
}
//=============================================================================
void
VariableNumericTableModel::resizeEditData(int oldRows, int oldCols)
{
    const int newSize = m_rows_display * m_cols_display;
    const int oldSize = oldRows * oldCols;

    if (newSize <= oldSize) {
        return;
    }

    QVector<QVariant> newEditData;
    newEditData.reserve(newSize);
    newEditData.resize(newSize);

    if (!m_editData.isEmpty() && oldRows > 0 && oldCols > 0) {
        const int copyRows = std::min(oldRows, m_rows_display);
        const int copyCols = std::min(oldCols, m_cols_display);

        for (int c = 0; c < copyCols; ++c) {
            const int oldColStart = c * oldRows;
            const int newColStart = c * m_rows_display;

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
VariableNumericTableModel::schedulePersistenceUpdate()
{
    if (!m_persistenceTimer) {
        m_persistenceTimer = new QTimer(this);
        m_persistenceTimer->setSingleShot(true);
        m_persistenceTimer->setInterval(100);
        connect(m_persistenceTimer, &QTimer::timeout, this,
            &VariableNumericTableModel::performBatchedPersistence);
    }

    m_persistenceTimer->start();
}
//=============================================================================
void
VariableNumericTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
        emit modelChanged();
    }
}
//=============================================================================
QVariant
VariableNumericTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (role == Qt::DisplayRole) {
        return QString::number(section + 1);
    }
    return QVariant();
}
//=============================================================================
void
VariableNumericTableModel::refreshFromArray(ArrayOf& value)
{
    m_array = value;
    beginResetModel();
    rebuildModelFromCurrentArray();
    endResetModel();
}
//=============================================================================
void
VariableNumericTableModel::rebuildModelFromCurrentArray()
{
    initializeDimensions();
    initializeEditData();
}
//=============================================================================
bool
VariableNumericTableModel::pasteDataFromClipboard(int startRow, int startCol, const QString& text)
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
bool
VariableNumericTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    if (text.isEmpty() || startRow < 0 || startCol < 0) {
        return false;
    }

    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid && validatePasteData(pasteData, startRow, startCol);
}
//=============================================================================
QSize
VariableNumericTableModel::getPasteSize(const QString& text) const
{
    const PasteData pasteData = parsePasteText(text);
    return pasteData.isValid ? QSize(pasteData.cols, pasteData.rows) : QSize(0, 0);
}
//=============================================================================
void
VariableNumericTableModel::initializeDimensions()
{
    m_rows = static_cast<int>(m_array.rows());
    m_cols = static_cast<int>(m_array.columns());

    if (m_rows == 0) {
        m_rows = 1;
        m_rows_display = DEFAULT_EMPTY_SIZE;
    } else {
        m_rows_display = std::max(m_rows + DISPLAY_MARGIN, MIN_DISPLAY_SIZE);
    }

    if (m_cols == 0) {
        m_cols = 1;
        m_cols_display = DEFAULT_EMPTY_SIZE;
    } else {
        m_cols_display = std::max(m_cols + DISPLAY_MARGIN, MIN_DISPLAY_SIZE);
    }
}
//=============================================================================
void
VariableNumericTableModel::initializeEditData()
{
    const int totalSize = m_rows_display * m_cols_display;
    m_editData.clear();
    m_editData.resize(totalSize);
}
//=============================================================================
bool
VariableNumericTableModel::validateArrayState() const
{
    return !m_variableName.isEmpty();
}
//=============================================================================
bool
VariableNumericTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && col >= 0 && row < m_rows_display && col < m_cols_display;
}
//=============================================================================
bool
VariableNumericTableModel::expandDisplayIfNeeded(int row, int col)
{
    bool needsExpansion = false;
    int newRowsDisplay = m_rows_display;
    int newColsDisplay = m_cols_display;

    if (row >= m_rows_display) {
        newRowsDisplay = std::max(row + 1 + DISPLAY_MARGIN, MIN_DISPLAY_SIZE);
        needsExpansion = true;
    }

    if (col >= m_cols_display) {
        newColsDisplay = std::max(col + 1 + DISPLAY_MARGIN, MIN_DISPLAY_SIZE);
        needsExpansion = true;
    }

    if (needsExpansion) {
        beginResetModel();
        m_rows_display = newRowsDisplay;
        m_cols_display = newColsDisplay;

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
void
VariableNumericTableModel::updateEditData(int row, int col, const QVariant& value)
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size()) {
        m_editData[idx] = value;
    }
}
//=============================================================================
void
VariableNumericTableModel::persistChangesToContext()
{
    if (!m_evaluator || !m_evaluator->getContext() || m_variableName.isEmpty()) {
        return;
    }

    try {
        // Writes target the base scope: the editor reads variables from the base
        // scope, so persisting elsewhere would silently desynchronize the view.
        m_evaluator->getContext()->getCurrentScope()->insertVariable(
            wstring_to_utf8(QStringTowstring(m_variableName)), m_array);
        WorkspaceBrowser::updateWorkspaceBrowser();
    } catch (const std::exception& e) {
        emit errorOccurred(tr("Failed to persist changes: %1").arg(e.what()));
    }
}
//=============================================================================
int
VariableNumericTableModel::indexToFlat(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return -1;
    }
    return row + col * m_rows_display;
}
//=============================================================================
std::pair<int, int>
VariableNumericTableModel::flatToIndex(int flatIndex) const
{
    if (flatIndex < 0 || m_rows_display <= 0) {
        return { -1, -1 };
    }

    const int row = flatIndex % m_rows_display;
    const int col = flatIndex / m_rows_display;

    return { row, col };
}
//=============================================================================
VariableAbstractTableModel::PasteData
VariableNumericTableModel::parsePasteText(const QString& text) const
{
    PasteData result;

    if (text.isEmpty()) {
        return result;
    }

    const QStringList rows = text.split(QRegularExpression(R"(\r\n|\r|\n)"), Qt::SkipEmptyParts);

    if (rows.isEmpty()) {
        return result;
    }

    result.rows = rows.size();
    result.cols = 0;
    result.cells.resize(result.rows);

    for (int r = 0; r < rows.size(); ++r) {
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

    if (result.rows * result.cols > MAX_PASTE_SIZE) {
        return PasteData();
    }

    result.isValid = true;
    return result;
}
//=============================================================================
void
VariableNumericTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
    emit modelChanged();
}
//=============================================================================
QString
VariableNumericTableModel::getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
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
VariableAbstractTableModel::SelectionBounds
VariableNumericTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
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
} // namespace Nelson
//=============================================================================
