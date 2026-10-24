//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableTypedTableModel.h"
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
template <typename T>
VariableTypedTableModel<T>::VariableTypedTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableNumericTableModel(name, array, evaluator, parent)
{
    if (!m_evaluator) {
        qWarning() << "VariableTypedTableModel<" << VariableTypeTraits<T>::typeName()
                   << ">: Context is null for variable" << name;
    }

    if (!validateArrayState()) {
        qWarning() << "VariableTypedTableModel<" << VariableTypeTraits<T>::typeName()
                   << ">: Invalid array state for variable" << name;
    }

    initializeDimensions();
    initializeEditData();
    clearUndoState();
}
//=============================================================================
template <typename T>
QVariant
VariableTypedTableModel<T>::data(const QModelIndex& index, int role) const
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
template <typename T>
bool
VariableTypedTableModel<T>::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || role != Qt::EditRole) {
        return false;
    }

    const int row = index.row();
    const int col = index.column();

    if (!isValidPosition(row, col)) {
        return false;
    }

    if (!VariableTypeTraits<T>::isType(m_array)) {
        emit errorOccurred(tr("Can only edit %1 arrays").arg(VariableTypeTraits<T>::typeName()));
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

    if (resultArray.isComplex() && !m_array.isComplex()) {
        m_array.promoteType(VariableTypeTraits<T>::ComplexClass);
    } else if (!resultArray.isComplex() && m_array.isComplex()) {
        resultArray.promoteType(VariableTypeTraits<T>::ComplexClass);
    }

    if (m_array.isEmpty()) {
        if (m_array.dataClass() == VariableTypeTraits<T>::ComplexClass) {
            m_array = VariableTypeTraits<T>::makeComplexScalar(0, 0);
        } else {
            m_array = VariableTypeTraits<T>::makeScalar(0);
        }
    }
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    if (!setArrayValue(row, col, resultArray)) {
        return false;
    }

    if (resultArray.isComplex()) {
        std::complex<T> cplxValue = VariableTypeTraits<T>::asComplexScalar(resultArray);
        updateEditData(row, col, QVariant(formatDisplayValue(cplxValue)));
    } else {
        T numericValue = VariableTypeTraits<T>::asScalar(resultArray);
        updateEditData(row, col, QVariant(formatDisplayValue(numericValue)));
    }

    m_pendingChanges = true;
    schedulePersistenceUpdate();

    emit dataChanged(index, index);

    return true;
}
//=============================================================================
template <typename T>
bool
VariableTypedTableModel<T>::resizeArray(int row, int col)
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
        if (cls != VariableTypeTraits<T>::RealClass && cls != VariableTypeTraits<T>::ComplexClass) {
            return false;
        }

        const size_t oldSize = static_cast<size_t>(m_rows) * m_cols;
        const size_t elementSize = (cls == VariableTypeTraits<T>::ComplexClass) ? 2 : 1;
        const size_t totalNewSize = newSize * elementSize;

        Dimensions newDims(newRows, newCols);
        T* ptr = (T*)ArrayOf::allocateArrayOf(cls, newDims.elementCount());
        ArrayOf newArray(cls, newDims, ptr);
        T* newData = (T*)newArray.dataPointer();

        std::fill(newData, newData + totalNewSize, 0.0);

        if (const T* oldData = (const T*)m_array.dataPointer()) {
            for (int c = 0; c < m_cols; ++c) {
                const size_t oldColStart = static_cast<size_t>(c) * m_rows * elementSize;
                const size_t newColStart = static_cast<size_t>(c) * newRows * elementSize;
                std::memcpy(
                    newData + newColStart, oldData + oldColStart, sizeof(T) * m_rows * elementSize);
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
template <typename T>
Qt::ItemFlags
VariableTypedTableModel<T>::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsSelectable | Qt::ItemIsEnabled;

    if (VariableTypeTraits<T>::isType(m_array)) {
        flags |= Qt::ItemIsEditable;
    }

    return flags;
}
//=============================================================================
template <typename T>
bool
VariableTypedTableModel<T>::pasteDataFromExcel(int startRow, int startCol, const QString& text)
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
template <typename T>
QVariant
VariableTypedTableModel<T>::getDisplayValue(int row, int col) const
{
    const int idx = indexToFlat(row, col);
    if (idx >= 0 && idx < m_editData.size() && m_editData[idx].isValid()) {
        return m_editData[idx];
    }

    if (row < m_rows && col < m_cols && hasOriginalData()) {
        const T* ptr = static_cast<const T*>(m_array.dataPointer());
        const int arrayIdx = row + col * m_rows;
        if (ptr) {
            if (m_array.isComplex()) {
                auto* ptrz = reinterpret_cast<std::complex<T>*>((T*)ptr);
                return QVariant(formatDisplayValue(ptrz[arrayIdx]));

            } else {
                return QVariant(formatDisplayValue(ptr[arrayIdx]));
            }
        }
    }
    return QVariant();
}
//=============================================================================
template <typename T>
bool
VariableTypedTableModel<T>::hasOriginalData() const
{
    return VariableTypeTraits<T>::isType(m_array) && m_array.dataPointer() != nullptr;
}
//=============================================================================
template <typename T>
bool
VariableTypedTableModel<T>::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    const int newRows = std::max(m_rows, row + 1);
    const int newCols = std::max(m_cols, col + 1);
    const int newSize = newRows * newCols;

    try {
        auto newData = std::make_unique<T[]>(newSize);
        std::fill(newData.get(), newData.get() + newSize, 0.0);

        if (const T* oldData = static_cast<const T*>(m_array.dataPointer())) {
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

        if (T* arrayData = (T*)(m_array.dataPointer())) {
            std::memcpy(arrayData, newData.get(), sizeof(T) * newSize);
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
template <typename T>
bool
VariableTypedTableModel<T>::setArrayValue(int row, int col, ArrayOf& value)
{
    if (m_array.dataClass() == VariableTypeTraits<T>::RealClass && value.isComplex()) {
        m_array.promoteType(VariableTypeTraits<T>::ComplexClass);
    }

    T* data = (T*)(m_array.dataPointer());
    if (!data) {
        emit errorOccurred(tr("No array data pointer available"));
        return false;
    }

    const int idx = row + col * m_rows;
    if (value.isComplex()) {
        std::complex<T> c = VariableTypeTraits<T>::asComplexScalar(value);
        data[2 * idx] = c.real();
        data[2 * idx + 1] = c.imag();
    } else {
        data[idx] = VariableTypeTraits<T>::asScalar(value);
    }
    if (m_array.allReal()) {
        m_array.promoteType(VariableTypeTraits<T>::RealClass);
    }
    return true;
}
//=============================================================================
template <typename T>
QString
VariableTypedTableModel<T>::formatDisplayValue(std::complex<T> value) const
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
template <typename T>
QString
VariableTypedTableModel<T>::formatDisplayValue(T value) const
{
    if (std::isnan(value)) {
        return "NaN";
    }
    if (std::isinf(value)) {
        return value > 0 ? "Inf" : "-Inf";
    }

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
template <typename T>
bool
VariableTypedTableModel<T>::validatePasteData(
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
template <typename T>
bool
VariableTypedTableModel<T>::applyPasteData(const PasteData& data, int startRow, int startCol)
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
            if (value.isComplex()) {
                value.promoteType(VariableTypeTraits<T>::ComplexClass);
            } else {
                value.promoteType(VariableTypeTraits<T>::RealClass);
            }
            if (setArrayValue(targetRow, targetCol, value)) {
                if (value.isComplex()) {
                    std::complex<T> cplxValue = VariableTypeTraits<T>::asComplexScalar(value);
                    updateEditData(targetRow, targetCol, QVariant(formatDisplayValue(cplxValue)));
                } else {
                    T numericValue = VariableTypeTraits<T>::asScalar(value);
                    updateEditData(
                        targetRow, targetCol, QVariant(formatDisplayValue(numericValue)));
                }
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
template <typename T>
bool
VariableTypedTableModel<T>::isStructureCompatible(const ArrayOf& value)
{
    return value.dataClass() == VariableTypeTraits<T>::RealClass
        || value.dataClass() == VariableTypeTraits<T>::ComplexClass;
}
//=============================================================================
template <typename T>
bool
VariableTypedTableModel<T>::insertRowAt(int row)
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
        if (m_array.dataClass() == VariableTypeTraits<T>::ComplexClass) {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::ComplexClass, total);
            std::complex<T>* dst = reinterpret_cast<std::complex<T>*>(ptr);
            std::complex<T>* src = reinterpret_cast<std::complex<T>*>((T*)m_array.dataPointer());

            for (int c = 0; c < cols; ++c) {
                for (int r = 0; r < row; ++r) {
                    dst[r + c * newRows] = src[r + c * m_rows];
                }

                dst[row + c * newRows] = std::complex<T>(0.0, 0.0);

                for (int r = row; r < m_rows; ++r) {
                    dst[r + 1 + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(VariableTypeTraits<T>::ComplexClass, newDims, ptr);
        } else {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::RealClass, total);
            T* dst = ptr;
            T* src = (T*)(m_array.dataPointer());

            for (int c = 0; c < cols; ++c) {
                for (int r = 0; r < row; ++r) {
                    dst[r + c * newRows] = src[r + c * m_rows];
                }

                dst[row + c * newRows] = 0.0;

                for (int r = row; r < m_rows; ++r) {
                    dst[r + 1 + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(VariableTypeTraits<T>::RealClass, newDims, ptr);
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
template <typename T>
bool
VariableTypedTableModel<T>::insertColumnAt(int col)
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
        if (m_array.dataClass() == VariableTypeTraits<T>::ComplexClass) {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::ComplexClass, total);
            std::complex<T>* dst = reinterpret_cast<std::complex<T>*>(ptr);
            std::complex<T>* src = reinterpret_cast<std::complex<T>*>((T*)m_array.dataPointer());

            for (int c = 0; c < col; ++c) {
                for (int r = 0; r < rows; ++r) {
                    dst[r + c * rows] = src[r + c * rows];
                }
            }

            for (int r = 0; r < rows; ++r) {
                dst[r + col * rows] = std::complex<T>(0.0, 0.0);
            }

            for (int c = col; c < m_cols; ++c) {
                for (int r = 0; r < rows; ++r) {
                    dst[r + (c + 1) * rows] = src[r + c * rows];
                }
            }

            newArray = ArrayOf(VariableTypeTraits<T>::ComplexClass, newDims, ptr);
        } else {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::RealClass, total);
            T* dst = ptr;
            T* src = (T*)(m_array.dataPointer());

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

            newArray = ArrayOf(VariableTypeTraits<T>::RealClass, newDims, ptr);
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
template <typename T>
bool
VariableTypedTableModel<T>::deleteRowAt(int row)
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
        if (m_array.dataClass() == VariableTypeTraits<T>::ComplexClass) {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::ComplexClass, total);
            std::complex<T>* dst = reinterpret_cast<std::complex<T>*>(ptr);
            std::complex<T>* src = reinterpret_cast<std::complex<T>*>((T*)m_array.dataPointer());

            for (int c = 0; c < cols; ++c) {
                int k = 0;
                for (int r = 0; r < m_rows; ++r) {
                    if (r == row) {
                        continue;
                    }
                    dst[k++ + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(VariableTypeTraits<T>::ComplexClass, newDims, ptr);
        } else {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::RealClass, total);
            T* dst = ptr;
            T* src = (T*)(m_array.dataPointer());

            for (int c = 0; c < cols; ++c) {
                int k = 0;
                for (int r = 0; r < m_rows; ++r) {
                    if (r == row) {
                        continue;
                    }
                    dst[k++ + c * newRows] = src[r + c * m_rows];
                }
            }

            newArray = ArrayOf(VariableTypeTraits<T>::RealClass, newDims, ptr);
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
template <typename T>
bool
VariableTypedTableModel<T>::deleteColumnAt(int col)
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
        if (m_array.dataClass() == VariableTypeTraits<T>::ComplexClass) {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::ComplexClass, total);
            std::complex<T>* dst = reinterpret_cast<std::complex<T>*>(ptr);
            std::complex<T>* src = reinterpret_cast<std::complex<T>*>((T*)m_array.dataPointer());

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

            newArray = ArrayOf(VariableTypeTraits<T>::ComplexClass, newDims, ptr);
        } else {
            T* ptr = (T*)ArrayOf::allocateArrayOf(VariableTypeTraits<T>::RealClass, total);
            T* dst = ptr;
            T* src = (T*)(m_array.dataPointer());

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

            newArray = ArrayOf(VariableTypeTraits<T>::RealClass, newDims, ptr);
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
template <typename T>
ArrayOf
VariableTypedTableModel<T>::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
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

    if (m_array.dataClass() == VariableTypeTraits<T>::RealClass) {
        T* ptr = (T*)ArrayOf::allocateArrayOf(
            VariableTypeTraits<T>::RealClass, newDims.elementCount());
        newVar = ArrayOf(VariableTypeTraits<T>::RealClass, newDims, ptr);

        for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
            for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
                QModelIndex idx = index(r, c);
                QVariant val = data(idx, Qt::DisplayRole);
                T num = val.toDouble();
                ptr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.rows()] = num;
            }
        }
    } else if (m_array.dataClass() == VariableTypeTraits<T>::ComplexClass) {
        T* ptr = (T*)ArrayOf::allocateArrayOf(
            VariableTypeTraits<T>::ComplexClass, newDims.elementCount());
        std::complex<T>* complexPtr = reinterpret_cast<std::complex<T>*>(ptr);
        newVar = ArrayOf(VariableTypeTraits<T>::ComplexClass, newDims, ptr);

        for (int r = bounds.minRow; r <= bounds.maxRow; ++r) {
            for (int c = bounds.minCol; c <= bounds.maxCol; ++c) {
                T* ptrModel = (T*)m_array.dataPointer();
                std::complex<T>* complexPtrModel = reinterpret_cast<std::complex<T>*>(ptrModel);
                complexPtr[(r - bounds.minRow) + (c - bounds.minCol) * newDims.rows()]
                    = complexPtrModel[r + c * m_array.dimensions().rows()];
            }
        }
    }

    return newVar;
}
//=============================================================================
template <typename T>
bool
VariableTypedTableModel<T>::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    return !selectedIndexes.isEmpty()
        && (m_array.dataClass() == VariableTypeTraits<T>::RealClass
            || m_array.dataClass() == VariableTypeTraits<T>::ComplexClass);
}
//=============================================================================
template <typename T>
bool
VariableTypedTableModel<T>::shouldDisplayAsTable() const
{
    return (m_array.dataClass() == VariableTypeTraits<T>::RealClass
               || m_array.dataClass() == VariableTypeTraits<T>::ComplexClass)
        && !m_array.isSparse() && m_array.is2D();
}
//=============================================================================
// Explicit instantiations for the two supported element types.
template class VariableTypedTableModel<double>;
template class VariableTypedTableModel<single>;
//=============================================================================
} // namespace Nelson
//=============================================================================
