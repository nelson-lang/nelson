//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableTableTableModel.h"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "ClassName.hpp"
#include "EvaluateCommand.hpp"
#include "QStringConverter.hpp"
#include "characters_encoding.hpp"
#include "MakeValidFieldname.hpp"
#include <QtCore/QMimeData>
#include <QtCore/QRegularExpression>
#include <QtCore/QStringList>
#include <QtCore/QDebug>
#include <QtGui/QClipboard>
#include <QtWidgets/QApplication>
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
VariableTableTableModel::VariableTableTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableAbstractTableModel(name, array, evaluator, parent)
    , m_persistenceTimer(new QTimer(this))
{
    initializeDimensions();
    initializeEditData();
    updateFieldNames();

    m_persistenceTimer->setSingleShot(true);
    m_persistenceTimer->setInterval(100);
    connect(m_persistenceTimer, &QTimer::timeout, this,
        &VariableTableTableModel::performBatchedPersistence);
    saveCurrentStateForUndo();
}
//=============================================================================
void
VariableTableTableModel::initializeDimensions()
{
    if (m_array.isEmpty()) {
        m_rows = 0;
        m_cols = 0;
        m_rows_display = DEFAULT_EMPTY_SIZE;
        m_cols_display = DEFAULT_EMPTY_SIZE;
        return;
    }

    if (m_array.isTable()) {
        m_rows = static_cast<int>(m_array.getTableHeight());
        m_cols = static_cast<int>(m_array.getTableWidth());

        if (m_rows < MIN_DISPLAY_SIZE) {
            m_rows_display = m_rows;
        } else {
            m_rows_display = std::max(m_rows, MIN_DISPLAY_SIZE);
        }
        if (m_cols < MIN_DISPLAY_SIZE) {
            m_cols_display = m_cols;
        } else {
            m_cols_display = std::max(m_cols, MIN_DISPLAY_SIZE);
        }
    } else {
        m_rows = 0;
        m_cols = 0;
        m_rows_display = DEFAULT_EMPTY_SIZE;
        m_cols_display = DEFAULT_EMPTY_SIZE;
    }
}
//=============================================================================
void
VariableTableTableModel::initializeEditData()
{
    m_editData.clear();
    m_editData.resize(m_rows_display * m_cols_display);
}
//=============================================================================
void
VariableTableTableModel::updateFieldNames()
{
    m_fieldNames.clear();

    if (m_array.isTable() && !m_array.isEmpty()) {
        stringVector fieldNames = m_array.getTableVariableNames();
        for (const auto& fieldName : fieldNames) {
            m_fieldNames << QString::fromStdWString(utf8_to_wstring(fieldName));
        }
    }
}
//=============================================================================
int
VariableTableTableModel::rowCount(const QModelIndex& parent) const
{
    Q_UNUSED(parent);
    return m_rows_display;
}
//=============================================================================
int
VariableTableTableModel::columnCount(const QModelIndex& parent) const
{
    Q_UNUSED(parent);
    return m_cols_display;
}
//=============================================================================
QVariant
VariableTableTableModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    const int row = index.row();
    const int col = index.column();

    switch (role) {
    case Qt::DisplayRole:
        return getCellDisplayValue(row, col);
    case Qt::EditRole:
        return getCellEditValue(row, col);
    case Qt::ToolTipRole:
        return getCellDisplayValue(row, col);
    case Qt::TextAlignmentRole:
        return static_cast<int>(Qt::AlignLeft);
    default:
        return QVariant();
    }
}
//=============================================================================
bool
VariableTableTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
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
        results.clear();
        results << ArrayOf::characterArrayConstructor(command);
        emitError = false;
        asStr = true;
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

    if (!setCellValue(row, col, resultArray)) {
        return false;
    }

    updateEditData(row, col, value);

    schedulePeristenceUpdate();

    emit dataChanged(index, index);
    emit modelChanged();

    return true;
}
//=============================================================================
bool
VariableTableTableModel::expandDisplayIfNeeded(int row, int col)
{
    bool expanded = false;

    if (row >= m_rows_display) {
        beginInsertRows(QModelIndex(), m_rows_display, row);
        int oldRows = m_rows_display;
        m_rows_display = std::min(row, MAX_PASTE_SIZE);
        resizeEditData(oldRows, m_cols_display);
        endInsertRows();
        expanded = true;
    }

    if (col >= m_cols_display) {
        beginInsertColumns(QModelIndex(), m_cols_display, col);
        int oldCols = m_cols_display;
        m_cols_display = std::min(col, MAX_PASTE_SIZE);
        resizeEditData(m_rows_display, oldCols);
        endInsertColumns();
        expanded = true;
    }

    return expanded;
}
//=============================================================================
// Private helper methods
//=============================================================================
QVariant
VariableTableTableModel::getDisplayValue(int row, int col) const
{
    return getCellDisplayValue(row, col);
}
//=============================================================================
bool
VariableTableTableModel::hasOriginalData() const
{
    return !m_array.isEmpty();
}
//=============================================================================
bool
VariableTableTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && row < m_rows && col >= 0 && col < m_cols;
}
//=============================================================================
bool
VariableTableTableModel::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    return expandArrayIfNeeded(row, col);
}
//=============================================================================
void
VariableTableTableModel::updateEditData(int row, int col, const QVariant& value)
{
    if (row >= 0 && row < m_rows_display && col >= 0 && col < m_cols_display) {
        int index = indexToFlat(row, col);
        if (index >= 0 && index < m_editData.size()) {
            m_editData[index] = value;
        }
    }
}
//=============================================================================
ArrayOf
VariableTableTableModel::getFieldValue(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return ArrayOf::emptyConstructor();
    }

    try {
        stringVector fieldNames = m_array.getTableVariableNames();
        if (col >= static_cast<int>(fieldNames.size())) {
            return ArrayOf::emptyConstructor();
        }

        ArrayOf column = m_array.getTableColumn(fieldNames[col]);
        if (column.isEmpty() || column.isScalar() || column.isRowVectorCharacterArray()) {
            return column;
        }

        if (column.isCell() || column.isStringArray()) {
            ArrayOf* elements = (ArrayOf*)(column.getDataPointer());
            return elements[row];
        }

        // Lambda for general data extraction
        auto extractRow = [&](auto* src) -> ArrayOf {
            using T = std::decay_t<decltype(*src)>;

            Dimensions dims = column.getDimensions();
            size_t cols = column.isVector() ? 1 : dims.getColumns();
            size_t rows = column.isVector() ? 0 : dims.getRows();

            Dimensions dimsOut(1, cols);
            T* dst = static_cast<T*>(ArrayOf::allocateArrayOf(column.getDataClass(), cols));
            ArrayOf res(column.getDataClass(), dimsOut, dst);

            if (column.isVector()) {
                dst[0] = src[row];
            } else {
                for (size_t c = 0; c < cols; ++c) {
                    dst[c] = src[row + c * rows];
                }
            }

            return res;
        };

        switch (column.getDataClass()) {
        case NLS_DOUBLE:
            return extractRow((double*)(column.getDataPointer()));

        case NLS_SINGLE:
            return extractRow((single*)(column.getDataPointer()));

        case NLS_DCOMPLEX:
            return extractRow(
                reinterpret_cast<std::complex<double>*>((double*)(column.getDataPointer())));

        case NLS_SCOMPLEX:
            return extractRow(
                reinterpret_cast<std::complex<single>*>((single*)(column.getDataPointer())));

        case NLS_INT8:
            return extractRow((int8*)(column.getDataPointer()));

        case NLS_INT16:
            return extractRow((int16*)(column.getDataPointer()));

        case NLS_INT32:
            return extractRow((int32*)(column.getDataPointer()));

        case NLS_INT64:
            return extractRow((int64*)(column.getDataPointer()));

        case NLS_UINT8:
            return extractRow((uint8*)(column.getDataPointer()));

        case NLS_UINT16:
            return extractRow((uint16*)(column.getDataPointer()));

        case NLS_UINT32:
            return extractRow((uint32*)(column.getDataPointer()));

        case NLS_UINT64:
            return extractRow((uint64*)(column.getDataPointer()));

        case NLS_LOGICAL:
            return extractRow((logical*)(column.getDataPointer()));

        default:
            break;
        }

    } catch (const Exception&) {
    }

    return ArrayOf::emptyConstructor();
}
//=============================================================================
bool
VariableTableTableModel::setFieldValue(int row, int col, const ArrayOf& value)
{
    if (!isValidPosition(row, col)) {
        return false;
    }

    try {
        stringVector fieldNames = m_array.getTableVariableNames();
        if (col < static_cast<int>(fieldNames.size())) {
            ArrayOf column = m_array.getTableColumn(fieldNames[col]);
            ArrayOf v = value;
            column.setField(fieldNames[col], v);
            m_array.setTableColumn(fieldNames[col], column);
            return true;
        }
    } catch (const Exception& e) {
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
    }

    return false;
}
//=============================================================================
QString
VariableTableTableModel::getFieldName(int col) const
{
    if (col >= 0 && col < m_fieldNames.size()) {
        return m_fieldNames[col];
    }
    return QString();
}
//=============================================================================
int
VariableTableTableModel::getFieldIndex(const QString& fieldName) const
{
    return m_fieldNames.indexOf(fieldName);
}
//=============================================================================
void
VariableTableTableModel::persistChangesToContext()
{
    if (m_evaluator && !m_variableName.isEmpty()) {
        try {
            Context* context = m_evaluator->getContext();
            if (context) {
                context->insertVariable(m_variableName.toStdString(), m_array);
            }
        } catch (const Exception& e) {
            emit errorOccurred(QString::fromStdWString(e.getMessage()));
        }
    }
}
//=============================================================================
void
VariableTableTableModel::schedulePeristenceUpdate()
{
    m_pendingChanges = true;
    if (m_persistenceTimer && !m_persistenceTimer->isActive()) {
        m_persistenceTimer->start();
    }
}
//=============================================================================
void
VariableTableTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
    }
}
//=============================================================================
VariableTableTableModel::SelectionBounds
VariableTableTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
{
    SelectionBounds bounds;

    if (selectedIndexes.isEmpty()) {
        return bounds;
    }

    bounds.minRow = selectedIndexes.first().row();
    bounds.maxRow = selectedIndexes.first().row();
    bounds.minCol = selectedIndexes.first().column();
    bounds.maxCol = selectedIndexes.first().column();

    for (const QModelIndex& index : selectedIndexes) {
        bounds.minRow = std::min(bounds.minRow, index.row());
        bounds.maxRow = std::max(bounds.maxRow, index.row());
        bounds.minCol = std::min(bounds.minCol, index.column());
        bounds.maxCol = std::max(bounds.maxCol, index.column());
    }

    bounds.isValid = true;
    return bounds;
}
//=============================================================================
VariableTableTableModel::PasteData
VariableTableTableModel::parsePasteText(const QString& text) const
{
    PasteData data;

    if (text.isEmpty()) {
        return data;
    }

    QStringList lines = text.split(QRegularExpression("[\r\n]"), Qt::SkipEmptyParts);
    if (lines.isEmpty()) {
        return data;
    }

    data.rows = lines.size();
    data.cols = 0;

    for (const QString& line : lines) {
        QStringList cells = line.split('\t');
        data.cols = std::max((int)data.cols, (int)cells.size());
        data.cells.append(QVector<QString>(cells.begin(), cells.end()));
    }
    for (auto& row : data.cells) {
        while (row.size() < data.cols) {
            row.append(QString());
        }
    }

    data.isValid = true;
    return data;
}
//=============================================================================
bool
VariableTableTableModel::validatePasteData(const PasteData& data, int startRow, int startCol) const
{
    if (!data.isValid) {
        return false;
    }

    if (startRow < 0 || startCol < 0) {
        return false;
    }

    if (startRow + data.rows > MAX_PASTE_SIZE || startCol + data.cols > MAX_PASTE_SIZE) {
        return false;
    }

    return true;
}
//=============================================================================
bool
VariableTableTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
{
    if (!validatePasteData(data, startRow, startCol)) {
        return false;
    }
    try {
        expandDisplayIfNeeded(startRow + data.rows - 1, startCol + data.cols - 1);

        if (!expandArrayIfNeeded(startRow + data.rows - 1, startCol + data.cols - 1)) {
            return false;
        }

        for (int row = 0; row < data.rows; ++row) {
            for (int col = 0; col < data.cols; ++col) {
                int targetRow = startRow + row;
                int targetCol = startCol + col;

                if (targetRow < m_rows && targetCol < m_cols) {
                    QString cellText = data.cells[row][col];
                    ArrayOf parsedValue = parseInputToArrayOf(cellText, targetRow, targetCol);

                    if (!setCellValue(targetRow, targetCol, parsedValue)) {
                        continue;
                    }

                    updateEditData(targetRow, targetCol, QVariant(cellText));
                }
            }
        }

        QModelIndex topLeft = createIndex(startRow, startCol);
        QModelIndex bottomRight = createIndex(startRow + data.rows - 1, startCol + data.cols - 1);
        emit dataChanged(topLeft, bottomRight);

        schedulePeristenceUpdate();
        emit modelChanged();

        return true;
    } catch (const Exception& e) {
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
        return false;
    }
}
//=============================================================================
int
VariableTableTableModel::indexToFlat(int row, int col) const
{
    return row * m_cols_display + col;
}
//=============================================================================
std::pair<int, int>
VariableTableTableModel::flatToIndex(int flatIndex) const
{
    if (m_cols_display == 0) {
        return { 0, 0 };
    }

    int row = flatIndex / m_cols_display;
    int col = flatIndex % m_cols_display;
    return { row, col };
}
//=============================================================================
bool
VariableTableTableModel::expandArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }
    return false;
}
//=============================================================================
bool
VariableTableTableModel::expandDisplay(int row, int col)
{
    return expandDisplayIfNeeded(row, col);
}
//=============================================================================
bool
VariableTableTableModel::resizeArray(int row, int col)
{
    return expandArrayIfNeeded(row, col);
}
//=============================================================================
void
VariableTableTableModel::resizeEditData(int oldRows, int oldCols)
{
    QVector<QVariant> newEditData(m_rows_display * m_cols_display);

    for (int row = 0; row < std::min(oldRows, m_rows_display); ++row) {
        for (int col = 0; col < std::min(oldCols, m_cols_display); ++col) {
            int oldIndex = row * oldCols + col;
            int newIndex = row * m_cols_display + col;

            if (oldIndex < m_editData.size() && newIndex < newEditData.size()) {
                newEditData[newIndex] = m_editData[oldIndex];
            }
        }
    }

    m_editData = newEditData;
}
//=============================================================================
QVariant
VariableTableTableModel::getCellDisplayValue(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        int index = indexToFlat(row, col);
        if (index >= 0 && index < m_editData.size()) {
            QVariant editValue = m_editData[index];
            if (editValue.isValid() && !editValue.toString().isEmpty()) {
                return editValue;
            }
        }
        return QVariant();
    }

    ArrayOf fieldValue = getFieldValue(row, col);
    return formatStructForDisplay(fieldValue);
}
//=============================================================================
QVariant
VariableTableTableModel::getCellEditValue(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        int index = indexToFlat(row, col);
        if (index >= 0 && index < m_editData.size()) {
            return m_editData[index];
        }
        return QVariant();
    }

    ArrayOf fieldValue = getFieldValue(row, col);
    return formatCellForEdit(fieldValue);
}
//=============================================================================
bool
VariableTableTableModel::setCellValue(int row, int col, const ArrayOf& cellValue)
{
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }
    ArrayOf tableData = m_array.getTableData();
    stringVector fieldnames = tableData.getFieldNames();

    ArrayOf dataCol = tableData.getField(fieldnames[col]);
    switch (dataCol.getDataClass()) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        ArrayOf* elements = (ArrayOf*)dataCol.getDataPointer();
        elements[row] = cellValue;
        return true;
    } break;
    case NLS_CHAR: {
        charType* ptrIn = (charType*)dataCol.getDataPointer();
        std::string val = cellValue.getContentAsCString();
        if (val.length()) {
            ptrIn[row] = val[0];
        }
        return true;
    } break;
    case NLS_DOUBLE: {
        if (cellValue.isSparse()) {
            return false;
        }
        if (cellValue.isComplex()) {
            dataCol.promoteType(NLS_DCOMPLEX);
            auto* ptrZ = reinterpret_cast<std::complex<double>*>((double*)dataCol.getDataPointer());
            std::complex<double> val = cellValue.getContentAsDoubleComplexScalar();
            ptrZ[row].real(val.real());
            ptrZ[row].imag(val.imag());
            tableData.setField(fieldnames[col], dataCol);
            m_array.setField("data", tableData);
        } else {
            double* ptrIn = (double*)dataCol.getDataPointer();
            double val = cellValue.getContentAsDoubleScalar();
            ptrIn[row] = val;
        }
        return true;
    } break;
    case NLS_SINGLE: {
        if (cellValue.isComplex()) {
            dataCol.promoteType(NLS_SCOMPLEX);
            auto* ptrZ = reinterpret_cast<std::complex<single>*>((single*)dataCol.getDataPointer());
            std::complex<double> val = cellValue.getContentAsSingleComplexScalar();
            ptrZ[row].real(val.real());
            ptrZ[row].imag(val.imag());
            tableData.setField(fieldnames[col], dataCol);
            m_array.setField("data", tableData);
        } else {
            single* ptrIn = (single*)dataCol.getDataPointer();
            single val = cellValue.getContentAsSingleScalar();
            ptrIn[row] = val;
        }
        return true;
    } break;
    case NLS_DCOMPLEX: {
        if (cellValue.isSparse()) {
            return false;
        }
        std::complex<double> val = cellValue.getContentAsDoubleComplexScalar();
        auto* ptrZ = reinterpret_cast<std::complex<double>*>((double*)dataCol.getDataPointer());
        ptrZ[row].real(val.real());
        ptrZ[row].imag(val.imag());
        return true;
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single> val = cellValue.getContentAsSingleComplexScalar();
        auto* ptrZ = reinterpret_cast<std::complex<single>*>((single*)dataCol.getDataPointer());
        ptrZ[row].real(val.real());
        ptrZ[row].imag(val.imag());
        return true;
    } break;
    case NLS_INT8: {
        int8 val = cellValue.getContentAsInteger8Scalar();
        int8* ptrIn = (int8*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_INT16: {
        int16 val = cellValue.getContentAsInteger16Scalar();
        int16* ptrIn = (int16*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_INT32: {
        int32 val = cellValue.getContentAsInteger32Scalar();
        int32* ptrIn = (int32*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_INT64: {
        int64 val = cellValue.getContentAsInteger64Scalar();
        int64* ptrIn = (int64*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT8: {
        uint8 val = cellValue.getContentAsUnsignedInteger8Scalar();
        uint8* ptrIn = (uint8*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT16: {
        uint16 val = cellValue.getContentAsUnsignedInteger16Scalar();
        uint16* ptrIn = (uint16*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT32: {
        uint32 val = cellValue.getContentAsUnsignedInteger32Scalar();
        uint32* ptrIn = (uint32*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT64: {
        uint64 val = cellValue.getContentAsUnsignedInteger64Scalar();
        uint64* ptrIn = (uint64*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_LOGICAL: {
        if (cellValue.isSparse()) {
            return false;
        }
        logical val = cellValue.getContentAsLogicalScalar();
        logical* ptrIn = (logical*)dataCol.getDataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    default: {
    } break;
    }
    return false;
}
//=============================================================================
QVariant
VariableTableTableModel::formatStructForDisplay(const QVariant& cellData) const
{
    if (!cellData.isValid()) {
        return QString();
    }
    return cellData.toString();
}
//=============================================================================
QVariant
VariableTableTableModel::formatStructForDisplay(const ArrayOf& cellArray) const
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
            auto formatPart = [](float v, const QString& suffix = "") -> QString {
                if (std::isnan(v))
                    return "NaN" + suffix;
                if (std::isinf(v))
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                return QString::number(v, 'g', 6) + suffix;
            };

            const float real = val.real();
            const float imag = val.imag();

            // Pure real
            if (imag == 0.0f) {
                return QVariant(formatPart(real));
            }

            // Pure imaginary
            if (real == 0.0f) {
                return QVariant(formatPart(imag, "i"));
            }

            // Complex with both parts
            QString realPart = formatPart(real);
            QString imagPart = (imag >= 0 ? "+" : "") + formatPart(imag, "i");

            return QVariant(realPart + imagPart);
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
            auto formatPart = [](double v, const QString& suffix = "") -> QString {
                if (std::isnan(v))
                    return "NaN" + suffix;
                if (std::isinf(v))
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                return QString::number(v, 'g', 6) + suffix;
            };

            const double real = val.real();
            const double imag = val.imag();

            if (imag == 0.0) {
                return QVariant(formatPart(real));
            }
            if (real == 0.0) {
                return QVariant(formatPart(imag, "i"));
            }
            QString realPart = formatPart(real);
            QString imagPart = (imag >= 0 ? "+" : "") + formatPart(imag, "i");

            return QVariant(realPart + imagPart);
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
ArrayOf
VariableTableTableModel::parseInputToArrayOf(const QString& input, int row, int col) const
{
    Q_UNUSED(row);
    Q_UNUSED(col);

    if (input.isEmpty()) {
        return ArrayOf::emptyConstructor();
    }

    try {
        bool ok;
        double numValue = input.toDouble(&ok);
        if (ok) {
            return ArrayOf::doubleConstructor(numValue);
        }

        QString lowerInput = input.toLower();
        if (lowerInput == "true" || lowerInput == "1") {
            return ArrayOf::logicalConstructor(true);
        } else if (lowerInput == "false" || lowerInput == "0") {
            return ArrayOf::logicalConstructor(false);
        }

        std::wstring wstr = utf8_to_wstring(input.toStdString());
        return ArrayOf::characterArrayConstructor(wstr);
    } catch (const Exception&) {
        return ArrayOf::emptyConstructor();
    }
}
//=============================================================================
QVariant
VariableTableTableModel::formatCellForEdit(const ArrayOf& cellArray) const
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
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            std::complex<single> val = cellArray.getContentAsSingleComplexScalar();
            auto formatPart = [](float v, const QString& suffix = "") -> QString {
                if (std::isnan(v))
                    return "NaN" + suffix;
                if (std::isinf(v))
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                return QString::number(v, 'g', 6) + suffix;
            };

            const float real = val.real();
            const float imag = val.imag();

            // Pure real
            if (imag == 0.0f) {
                return QVariant(formatPart(real));
            }

            // Pure imaginary
            if (real == 0.0f) {
                return QVariant(formatPart(imag, "i"));
            }

            // Complex with both parts
            QString realPart = formatPart(real);
            QString imagPart = (imag >= 0 ? "+" : "") + formatPart(imag, "i");

            return QVariant(realPart + imagPart);
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
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            std::complex<double> val = cellArray.getContentAsDoubleComplexScalar();
            auto formatPart = [](double v, const QString& suffix = "") -> QString {
                if (std::isnan(v))
                    return "NaN" + suffix;
                if (std::isinf(v))
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                return QString::number(v, 'g', 6) + suffix;
            };

            const double real = val.real();
            const double imag = val.imag();

            // Pure real
            if (imag == 0.0) {
                return QVariant(formatPart(real));
            }

            // Pure imaginary
            if (real == 0.0) {
                return QVariant(formatPart(imag, "i"));
            }

            // Complex with both parts
            QString realPart = formatPart(real);
            QString imagPart = (imag >= 0 ? "+" : "") + formatPart(imag, "i");
            return QVariant(realPart + imagPart);
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
    return QVariant("");
}
//=============================================================================
QVariant
VariableTableTableModel::formatCellForEdit(const QVariant& cellData) const
{
    return cellData;
}
//=============================================================================
bool
VariableTableTableModel::isValidStructArray(const ArrayOf& array) const
{
    if (array.isEmpty()) {
        return true;
    }

    return array.isTable();
}
//=============================================================================
bool
VariableTableTableModel::hasConsistentFields(const ArrayOf& array) const
{
    if (!array.isTable()) {
        return false;
    }
    return true;
}
//=============================================================================
Qt::ItemFlags
VariableTableTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsEnabled | Qt::ItemIsSelectable;

    const int row = index.row();
    const int col = index.column();

    if (isValidPosition(row, col)) {
        stringVector fieldNames = m_array.getTableVariableNames();
        if (col < static_cast<int>(fieldNames.size())) {
            ArrayOf column = m_array.getTableColumn(fieldNames[col]);
            if ((column.isScalar() && !column.isReferenceType())
                || column.isRowVectorCharacterArray()) {
                flags |= Qt::ItemIsEditable;
            } else {
                if (column.isCell() || column.isStringArray()) {
                    if (row < static_cast<int>(column.getRows())) {
                        flags |= Qt::ItemIsEditable;
                    }
                } else {
                    Dimensions dims = column.getDimensions();
                    switch (column.getDataClass()) {
                    case NLS_INT8:
                    case NLS_INT16:
                    case NLS_INT32:
                    case NLS_INT64:
                    case NLS_UINT8:
                    case NLS_UINT16:
                    case NLS_UINT32:
                    case NLS_UINT64:
                    case NLS_LOGICAL:
                    case NLS_SCOMPLEX:
                    case NLS_SINGLE:
                    case NLS_DCOMPLEX:
                    case NLS_DOUBLE: {
                        if (column.isScalar() || column.isEmpty()) {
                            flags |= Qt::ItemIsEditable;
                        } else if (column.isVector()) {
                            flags |= Qt::ItemIsEditable;
                        }
                    } break;
                    default: {
                    } break;
                    }
                }
            }
        }
    }
    return flags;
}
//=============================================================================
QVariant
VariableTableTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (orientation == Qt::Horizontal) {
        if (role == Qt::DisplayRole || role == Qt::EditRole) {
            ArrayOf tableProperties = m_array.getTableProperties();
            if (tableProperties.isStruct()) {
                ArrayOf variableNames = tableProperties.getField("VariableNames");
                stringVector fieldNames = variableNames.getContentAsCStringVector();
                if (section < static_cast<int>(fieldNames.size())) {
                    return QString::fromStdString(fieldNames[section]);
                }
            }
        }
    } else if (orientation == Qt::Vertical) {
        if (role == Qt::DisplayRole || role == Qt::EditRole) {
            ArrayOf tableProperties = m_array.getTableProperties();
            if (tableProperties.isStruct()) {
                ArrayOf rowNames = tableProperties.getField("RowNames");
                if (rowNames.isEmpty()) {
                    return QString::number(section + 1);
                }
                stringVector fieldNames = rowNames.getContentAsCStringVector();
                if (section < static_cast<int>(fieldNames.size())) {
                    return QString::fromStdString(fieldNames[section]);
                }
            }
        }
    }

    return QVariant();
}
//=============================================================================
bool
VariableTableTableModel::haveRowNames()
{
    ArrayOf tableProperties = m_array.getTableProperties();
    if (tableProperties.isStruct()) {
        ArrayOf rowNames = tableProperties.getField("RowNames");
        stringVector fieldNames = rowNames.getContentAsCStringVector();
        return !fieldNames.empty();
    }
    return false;
}
//=============================================================================
bool
VariableTableTableModel::setHeaderData(
    int section, Qt::Orientation orientation, const QVariant& value, int role)
{
    if (role != Qt::EditRole || section <= 0) {
        return false;
    }
    ArrayOf tableProperties = m_array.getTableProperties();

    if (orientation == Qt::Horizontal) {
        if (tableProperties.isStruct()) {
            ArrayOf variableNames = tableProperties.getField("VariableNames");
            stringVector fieldNames = variableNames.getContentAsCStringVector();
            QString newName = value.toString().trimmed();
            std::string newFieldname = newName.toStdString();
            fieldNames[section] = newFieldname;

            ArrayOf tableData = m_array.getTableData();
            tableData.renameFieldnames(fieldNames);
            m_array.setField("data", tableData);

            ArrayOf newVariableNames = ArrayOf::toCellArrayOfCharacterRowVectors(fieldNames);
            tableProperties.setField("VariableNames", newVariableNames);
            m_array.setField("Properties", tableProperties);
            emit headerDataChanged(Qt::Horizontal, section, section);
        } else {
            return false;
        }

    } else if (orientation == Qt::Vertical) {
        if (tableProperties.isStruct()) {
            ArrayOf rowNames = tableProperties.getField("RowNames");
            stringVector fieldNames = rowNames.getContentAsCStringVector();
            QString newName = value.toString().trimmed();
            std::string newFieldname = newName.toStdString();
            fieldNames[section] = newFieldname;
            ArrayOf newRowNames = ArrayOf::toCellArrayOfCharacterRowVectors(fieldNames);
            tableProperties.setField("RowNames", newRowNames);
            m_array.setField("Properties", tableProperties);
            emit headerDataChanged(Qt::Vertical, section, section);

        } else {
            return false;
        }
    }
    emit modelChanged();
    schedulePeristenceUpdate();
    return true;
}
//=============================================================================
void
VariableTableTableModel::refreshFromArray(ArrayOf& value)
{
    if (!isValidStructArray(value)) {
        return;
    }

    beginResetModel();

    m_array = value;
    initializeDimensions();
    initializeEditData();
    updateFieldNames();

    endResetModel();

    emit modelChanged();
}
//=============================================================================
bool
VariableTableTableModel::isStructureCompatible(const ArrayOf& value)
{
    if (!isValidStructArray(value)) {
        return false;
    }

    if (m_array.isTable() && value.isTable()) {
        stringVector currentFields = m_array.getFieldNames();
        stringVector newFields = value.getFieldNames();

        if (currentFields.size() != newFields.size()) {
            return false;
        }

        for (size_t i = 0; i < currentFields.size(); ++i) {
            if (currentFields[i] != newFields[i]) {
                return false;
            }
        }
    }

    return true;
}
//=============================================================================
void
VariableTableTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
}
//=============================================================================
QString
VariableTableTableModel::getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return QString();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return QString();
    }

    QStringList rows;
    for (int row = bounds.minRow; row <= bounds.maxRow; ++row) {
        QStringList cols;
        for (int col = bounds.minCol; col <= bounds.maxCol; ++col) {
            QVariant cellData = getCellDisplayValue(row, col);
            cols << cellData.toString();
        }
        rows << cols.join("\t");
    }

    return rows.join("\n");
}
//=============================================================================
bool
VariableTableTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    return pasteDataFromClipboard(startRow, startCol, text);
}
//=============================================================================
bool
VariableTableTableModel::pasteDataFromClipboard(int startRow, int startCol, const QString& text)
{
    if (text.isEmpty()) {
        return false;
    }

    PasteData data = parsePasteText(text);
    if (!data.isValid) {
        return false;
    }

    if (!validatePasteData(data, startRow, startCol)) {
        return false;
    }
    saveCurrentStateForUndo();
    return applyPasteData(data, startRow, startCol);
}
//=============================================================================
bool
VariableTableTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    PasteData data = parsePasteText(text);
    if (!data.isValid) {
        return false;
    }

    return validatePasteData(data, startRow, startCol);
}
//=============================================================================
QSize
VariableTableTableModel::getPasteSize(const QString& text) const
{
    PasteData data = parsePasteText(text);
    if (!data.isValid) {
        return QSize(0, 0);
    }

    return QSize(data.cols, data.rows);
}
//=============================================================================
ArrayOf
VariableTableTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return ArrayOf::emptyConstructor();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return ArrayOf::emptyConstructor();
    }

    if (selectedIndexes.size() == 1) {
        const QModelIndex index = selectedIndexes.first();
        if (!index.isValid()) {
            return ArrayOf::emptyConstructor();
        }
        return getFieldValue(index.row(), index.column());
    }

    try {
        int selRows = bounds.maxRow - bounds.minRow + 1;
        int selCols = bounds.maxCol - bounds.minCol + 1;

        if (selCols == 1 && selRows > 1) {
            ArrayOfVector values;
            int col = bounds.minCol;

            NelsonType commonType = NLS_UNKNOWN;
            bool isHomogeneous = true;
            bool allScalar = true;
            for (int row = bounds.minRow; row <= bounds.maxRow; ++row) {
                ArrayOf value = getFieldValue(row, col);
                if (row == bounds.minRow) {
                    commonType = value.getDataClass();
                } else if (isHomogeneous && value.getDataClass() != commonType) {
                    isHomogeneous = false;
                    break;
                }
                if (!value.isScalar() && !value.isRowVectorCharacterArray()) {
                    allScalar = false;
                }
                values.push_back(value);
            }
            if (isHomogeneous && allScalar) {
                switch (commonType) {
                case NLS_SINGLE: {
                    Dimensions dims(values.size(), 1);
                    single* ptr
                        = (single*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsSingleScalar();
                    }
                    return result;
                } break;
                case NLS_DCOMPLEX: {
                    Dimensions dims(values.size(), 1);
                    double* ptr
                        = (double*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    auto* ptrZ
                        = reinterpret_cast<std::complex<double>*>((double*)result.getDataPointer());
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptrZ[k] = values[k].getContentAsDoubleComplexScalar();
                    }
                } break;
                case NLS_SCOMPLEX: {
                    Dimensions dims(values.size(), 1);
                    single* ptr
                        = (single*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    auto* ptrZ
                        = reinterpret_cast<std::complex<single>*>((single*)result.getDataPointer());
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptrZ[k] = values[k].getContentAsSingleComplexScalar();
                    }
                } break;
                case NLS_INT8: {
                    Dimensions dims(values.size(), 1);
                    int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsInteger8Scalar();
                    }
                    return result;
                } break;
                case NLS_INT16: {
                    Dimensions dims(values.size(), 1);
                    int16* ptr
                        = (int16*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsInteger16Scalar();
                    }
                    return result;
                } break;
                case NLS_INT32: {
                    Dimensions dims(values.size(), 1);
                    int32* ptr
                        = (int32*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsInteger32Scalar();
                    }
                    return result;
                } break;
                case NLS_INT64: {
                    Dimensions dims(values.size(), 1);
                    int64* ptr
                        = (int64*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsInteger64Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT8: {
                    Dimensions dims(values.size(), 1);
                    uint8* ptr
                        = (uint8*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsUnsignedInteger8Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT16: {
                    Dimensions dims(values.size(), 1);
                    uint16* ptr
                        = (uint16*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsUnsignedInteger16Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT32: {
                    Dimensions dims(values.size(), 1);
                    uint32* ptr
                        = (uint32*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsUnsignedInteger32Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT64: {
                    Dimensions dims(values.size(), 1);
                    uint64* ptr
                        = (uint64*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsUnsignedInteger64Scalar();
                    }
                    return result;
                } break;
                case NLS_LOGICAL: {
                    Dimensions dims(values.size(), 1);
                    logical* ptr
                        = (logical*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsLogicalScalar();
                    }
                    return result;
                } break;
                case NLS_DOUBLE: {
                    Dimensions dims(values.size(), 1);
                    double* ptr
                        = (double*)ArrayOf::allocateArrayOf(commonType, dims.getElementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k].getContentAsDoubleScalar();
                    }
                    return result;
                } break;
                case NLS_CHAR: {
                    Dimensions dims(values.size(), 1);
                    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(
                        NLS_CELL_ARRAY, dims.getElementCount());
                    ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, ptr);
                    for (indexType k = 0; k < dims.getElementCount(); ++k) {
                        ptr[k] = values[k];
                    }
                    return result;
                } break;
                }
            } else {
                Dimensions dims(values.size(), 1);
                ArrayOf* ptr
                    = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount());
                ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, ptr);
                for (indexType k = 0; k < dims.getElementCount(); ++k) {
                    ptr[k] = values[k];
                }
                return result;
            }
        }

        // Collect valid field names
        stringVector fieldNames;
        for (int col = bounds.minCol; col <= bounds.maxCol; ++col) {
            if (col >= 0 && col < m_fieldNames.size()) {
                fieldNames.push_back(m_fieldNames[col].toStdString());
            } else {
                // Skip invalid columns (outside actual field range)
                return ArrayOf::emptyConstructor();
            }
        }

        Dimensions dims(selRows, 1); // struct array of rows
        indexType elementCount = dims.getElementCount();

        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, elementCount, fieldNames, false));
        ArrayOf result(NLS_STRUCT_ARRAY, dims, ptr, false, fieldNames);

        for (size_t f = 0; f < fieldNames.size(); ++f) {
            ArrayOfVector values;
            values.reserve(selRows);

            for (int row = 0; row < selRows; ++row) {
                int srcRow = bounds.minRow + row;
                int srcCol = bounds.minCol + static_cast<int>(f);
                ArrayOf val = getFieldValue(srcRow, srcCol);
                values.push_back(val);
            }

            result.setFieldAsList(fieldNames[f], values);
        }

        return result;

    } catch (const Exception& e) {
        qDebug() << "Error creating array from selection:"
                 << QString::fromStdWString(e.getMessage());
        return ArrayOf::emptyConstructor();
    }
}
//=============================================================================
bool
VariableTableTableModel::isValidSelectionForExtraction(const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return false;
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    return bounds.isValid;
}
//=============================================================================
bool
VariableTableTableModel::shouldDisplayAsTable() const
{
    return m_array.isTable() && !m_array.isEmpty();
}
//=============================================================================
void
VariableTableTableModel::undo()
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
void
VariableTableTableModel::redo()
{
    undo();
}
//=============================================================================
}
//=============================================================================
