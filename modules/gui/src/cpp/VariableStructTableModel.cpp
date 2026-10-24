//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "VariableStructTableModel.h"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "NelsonException.hpp"
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
VariableStructTableModel::VariableStructTableModel(
    const QString& name, ArrayOf array, Evaluator* evaluator, QObject* parent)
    : VariableFieldedTableModel(name, array, evaluator, parent)
{
    initializeDimensions();
    initializeEditData();
    updateFieldNames();
    clearUndoState();
}
//=============================================================================
void
VariableStructTableModel::initializeDimensions()
{
    if (m_array.isEmpty()) {
        m_rows = 0;
        m_cols = 0;
        m_rows_display = DEFAULT_EMPTY_SIZE;
        m_cols_display = DEFAULT_EMPTY_SIZE;
        return;
    }

    if (m_array.isStruct()) {
        Dimensions dims = m_array.dimensions();
        m_rows = static_cast<int>(dims.elementCount());

        stringVector fieldNames = m_array.fieldNames();
        m_cols = static_cast<int>(fieldNames.size());

        m_rows_display = std::max(m_rows, MIN_DISPLAY_SIZE);
        m_cols_display = std::max(m_cols, MIN_DISPLAY_SIZE);
    } else {
        m_rows = 0;
        m_cols = 0;
        m_rows_display = DEFAULT_EMPTY_SIZE;
        m_cols_display = DEFAULT_EMPTY_SIZE;
    }
}
//=============================================================================
bool
VariableStructTableModel::isCompatibleArrayType(const ArrayOf& array) const
{
    return array.isStruct();
}
//=============================================================================
stringVector
VariableStructTableModel::rawFieldNames(const ArrayOf& array) const
{
    return array.fieldNames();
}
//=============================================================================
int
VariableStructTableModel::columnCount(const QModelIndex& parent) const
{
    Q_UNUSED(parent);
    return m_cols_display + 1;
}
//=============================================================================
QVariant
VariableStructTableModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    const int row = index.row();
    const int col = index.column();

    if (col == 0) {
        // First column shows field names/indices
        switch (role) {
        case Qt::DisplayRole:
            return QString("%1").arg(row + 1);
        case Qt::EditRole:
            return QString("%1").arg(row + 1);
        case Qt::ToolTipRole:
            return QString("Struct element at index %1").arg(row + 1);
        case Qt::TextAlignmentRole:
            return static_cast<int>(Qt::AlignLeft);
        default:
            return QVariant();
        }
    } else {
        int dataCol = col - 1;

        switch (role) {
        case Qt::DisplayRole:
            return getCellDisplayValue(row, dataCol);
        case Qt::EditRole:
            return getCellEditValue(row, dataCol);
        case Qt::ToolTipRole:
            return getCellDisplayValue(row, dataCol);
        case Qt::TextAlignmentRole:
            return static_cast<int>(Qt::AlignLeft);
        default:
            return QVariant();
        }
    }
}
//=============================================================================
bool
VariableStructTableModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || role != Qt::EditRole) {
        return false;
    }

    saveCurrentStateForUndo();

    const int row = index.row();
    const int col = index.column();

    if (col == 0) {
        // First column is not editable
        return false;
    }

    int dataCol = col - 1;

    if (!isValidPosition(row, dataCol)) {
        return false;
    }

    std::wstring command = value.toString().toStdWString();
    ArrayOfVector results;
    bool emitError = false;
    bool asStr = false;
    try {
        results = EvaluateCommand(m_evaluator, 1, command, L"");
    } catch (NelsonException&) {
        emitError = true;
        const int idx = row + col * m_rows;
        if (idx >= 0 && idx < m_array.elementCount()) {
            ArrayOf* data = (ArrayOf*)(m_array.dataPointer());
            if (data) {
                NelsonType destinationType = data[idx].dataClass();
                emitError = false;
                if (destinationType == NLS_CHAR || destinationType == NLS_STRING_ARRAY) {
                    asStr = true;
                    results.clear();
                    results << ArrayOf::characterArray(command);
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

    if (!setCellValue(row, dataCol, resultArray)) {
        return false;
    }

    updateEditData(row, dataCol, value);

    schedulePersistenceUpdate();

    emit dataChanged(index, index);
    emit modelChanged();

    return true;
}
//=============================================================================
bool
VariableStructTableModel::expandDisplayIfNeeded(int row, int col)
{
    bool expanded = false;

    if (row >= m_rows_display) {
        beginInsertRows(QModelIndex(), m_rows_display, row);
        int oldRows = m_rows_display;
        m_rows_display = std::min(row + 1, MAX_PASTE_SIZE);
        resizeEditData(oldRows, m_cols_display);
        endInsertRows();
        expanded = true;
    }

    if (col >= m_cols_display) {
        beginInsertColumns(QModelIndex(), m_cols_display + 1, col + 1); // +1 for Fields column
        int oldCols = m_cols_display;
        m_cols_display = std::min(col + 1, MAX_PASTE_SIZE);
        resizeEditData(m_rows_display, oldCols);
        endInsertColumns();
        expanded = true;
    }

    return expanded;
}
//=============================================================================
// Private helper methods
//=============================================================================
bool
VariableStructTableModel::setArrayMissingValue(int row, int col)
{
    if (!isValidPosition(row, col)) {
        return false;
    }

    return setFieldValue(row, col, ArrayOf::empty());
}
//=============================================================================
bool
VariableStructTableModel::setArrayValue(int row, int col, const std::wstring& value)
{
    if (!isValidPosition(row, col)) {
        return false;
    }

    try {
        ArrayOf parsedValue = parseInputToArrayOf(QString::fromStdWString(value), row, col);
        return setFieldValue(row, col, parsedValue);
    } catch (const NelsonException& e) {
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
ArrayOf
VariableStructTableModel::fieldValue(int structIndex, int fieldIndex) const
{
    if (!isValidPosition(structIndex, fieldIndex)) {
        return ArrayOf::empty();
    }

    try {
        stringVector fieldNames = m_array.fieldNames();
        if (fieldIndex < static_cast<int>(fieldNames.size())) {
            ArrayOfVector results = m_array.fieldAsList(fieldNames[fieldIndex]);
            return results[structIndex];
        }
    } catch (const NelsonException& e) {
        qDebug() << "Error getting field value:" << QString::fromStdWString(e.message());
    }

    return ArrayOf::empty();
}
//=============================================================================
bool
VariableStructTableModel::setFieldValue(int structIndex, int fieldIndex, const ArrayOf& value)
{
    if (!isValidPosition(structIndex, fieldIndex)) {
        return false;
    }

    try {
        stringVector fieldNames = m_array.fieldNames();
        if (fieldIndex < static_cast<int>(fieldNames.size())) {
            ArrayOfVector list = m_array.fieldAsList(fieldNames[fieldIndex]);
            if (static_cast<size_t>(structIndex) < list.size()) {
                list[structIndex] = value;
                m_array.setFieldAsList(fieldNames[fieldIndex], list);
                return true;
            }
        }
    } catch (const NelsonException& e) {
        emit errorOccurred(QString::fromStdWString(e.message()));
    }

    return false;
}
//=============================================================================
bool
VariableStructTableModel::writePastedValue(int row, int col, const ArrayOf& value)
{
    return setFieldValue(row, col, value);
}
//=============================================================================
bool
VariableStructTableModel::expandArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    try {
        int newRows = std::max(m_rows, row + 1);
        int newCols = std::max(m_cols, col + 1);

        stringVector oldFieldNames = m_array.fieldNames();
        stringVector newFieldNames = oldFieldNames;

        while (static_cast<int>(newFieldNames.size()) < newCols) {
            QString generated = QString("Field%1").arg(newFieldNames.size() + 1);
            newFieldNames.push_back(generated.toStdString());
        }

        Dimensions dims(newRows, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), newFieldNames, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, newFieldNames);

        for (size_t j = 0; j < oldFieldNames.size(); ++j) {
            const std::string& field = oldFieldNames[j];
            ArrayOfVector oldList = m_array.fieldAsList(field);
            ArrayOfVector newList;
            newList.reserve(newRows);

            for (int i = 0; i < newRows; ++i) {
                if (i < static_cast<int>(oldList.size())) {
                    newList.push_back(oldList[i]);
                } else {
                    newList.push_back(ArrayOf::empty());
                }
            }

            newArray.setFieldAsList(field, newList);
        }

        for (size_t j = oldFieldNames.size(); j < newFieldNames.size(); ++j) {
            const std::string& field = newFieldNames[j];
            ArrayOfVector list;
            list.reserve(newRows);

            for (int i = 0; i < newRows; ++i) {
                list.push_back(ArrayOf::empty());
            }

            newArray.setFieldAsList(field, list);
        }

        m_array = newArray;
        m_rows = newRows;
        m_cols = newCols;

        updateFieldNames();
        return true;

    } catch (const NelsonException& e) {
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
bool
VariableStructTableModel::setCellValue(int row, int col, const ArrayOf& cellValue)
{
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    return setFieldValue(row, col, cellValue);
}
//=============================================================================
QVariant
VariableStructTableModel::formatStructForDisplay(const ArrayOf& cellArray) const
{
    if (cellArray.isEmpty() && cellArray.isDoubleType()) {
        return QVariant(tr("[]"));
    }
    switch (cellArray.dataClass()) {
    case NLS_UINT8: {
        if (cellArray.isScalar()) {
            uint8 val = cellArray.asUint8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT16: {
        if (cellArray.isScalar()) {
            uint16 val = cellArray.asUint16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT32: {
        if (cellArray.isScalar()) {
            uint32 val = cellArray.asUint32Scalar();

            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT64: {
        if (cellArray.isScalar()) {
            uint64 val = cellArray.asUint64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT8: {
        if (cellArray.isScalar()) {
            int8 val = cellArray.asInt8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT16: {
        if (cellArray.isScalar()) {
            int16 val = cellArray.asInt16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT32: {
        if (cellArray.isScalar()) {
            int32 val = cellArray.asInt32Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT64: {
        if (cellArray.isScalar()) {
            int64 val = cellArray.asInt64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_SINGLE: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            single val = cellArray.asSingleScalar();
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
            std::complex<single> val = cellArray.asSingleComplexScalar();
            return QVariant(QString("%1%2%3i")
                    .arg(val.real(), 0, 'g', 6)
                    .arg(val.imag() >= 0 ? "+" : "")
                    .arg(val.imag(), 0, 'g', 6));
        }
    } break;
    case NLS_DOUBLE: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            double val = cellArray.asDoubleScalar();
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
            std::complex<double> val = cellArray.asDoubleComplexScalar();
            return QVariant(QString("%1%2%3i")
                    .arg(val.real(), 0, 'g', 6)
                    .arg(val.imag() >= 0 ? "+" : "")
                    .arg(val.imag(), 0, 'g', 6));
        }
    } break;
    case NLS_CHAR: {
        if (cellArray.isRowVectorCharacterArray()) {
            return QVariant(wstringToQString(cellArray.asWideString()));
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (cellArray.isScalar()) {
            return QVariant(wstringToQString(cellArray.asWideString()));
        }
    } break;
    case NLS_LOGICAL: {
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            bool val = cellArray.asLogicalScalar();
            return QVariant(val ? "true" : "false");
        }
    } break;
    default: {
    } break;
    }
    std::wstring classname;
    ClassName(cellArray, classname);
    return wstringToQString(L"[" + cellArray.dimensions().toWideString() + L" " + classname + L"]");
}
//=============================================================================
QVariant
VariableStructTableModel::formatCellForEdit(const ArrayOf& cellArray) const
{
    switch (cellArray.dataClass()) {
    case NLS_UINT8: {
        if (cellArray.isScalar()) {
            uint8 val = cellArray.asUint8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT16: {
        if (cellArray.isScalar()) {
            uint16 val = cellArray.asUint16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT32: {
        if (cellArray.isScalar()) {
            uint32 val = cellArray.asUint32Scalar();

            return QVariant(QString::number(val));
        }
    } break;
    case NLS_UINT64: {
        if (cellArray.isScalar()) {
            uint64 val = cellArray.asUint64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT8: {
        if (cellArray.isScalar()) {
            int8 val = cellArray.asInt8Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT16: {
        if (cellArray.isScalar()) {
            int16 val = cellArray.asInt16Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT32: {
        if (cellArray.isScalar()) {
            int32 val = cellArray.asInt32Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_INT64: {
        if (cellArray.isScalar()) {
            int64 val = cellArray.asInt64Scalar();
            return QVariant(QString::number(val));
        }
    } break;
    case NLS_SINGLE: {
        if (cellArray.isScalar()) {
            single val = cellArray.asSingleScalar();
            if (std::isnan(val)) {
                return QVariant("");
            }
            return QVariant(QString::number(val, 'g', 15));
        }
    } break;
    case NLS_SCOMPLEX: {
        if (cellArray.isScalar()) {
            std::complex<single> val = cellArray.asSingleComplexScalar();
            return QVariant(QString("%1%2%3i")
                    .arg(val.real(), 0, 'g', 15)
                    .arg(val.imag() >= 0 ? "+" : "")
                    .arg(val.imag(), 0, 'g', 15));
        }
    } break;
    case NLS_DOUBLE: {
        if (cellArray.isScalar()) {
            double val = cellArray.asDoubleScalar();
            if (std::isnan(val)) {
                return QVariant("");
            }
            return QVariant(QString::number(val, 'g', 15));
        }
    } break;
    case NLS_DCOMPLEX: {
        if (cellArray.isScalar()) {
            std::complex<double> val = cellArray.asDoubleComplexScalar();
            return QVariant(QString("%1%2%3i")
                    .arg(val.real(), 0, 'g', 15)
                    .arg(val.imag() >= 0 ? "+" : "")
                    .arg(val.imag(), 0, 'g', 15));
        }
    } break;
    case NLS_CHAR: {
        if (cellArray.isRowVectorCharacterArray()) {
            return QVariant(wstringToQString(cellArray.asWideString()));
        }
    } break;
    case NLS_STRING_ARRAY: {
        if (cellArray.isScalar()) {
            return QVariant(wstringToQString(cellArray.asWideString()));
        }
    } break;
    case NLS_LOGICAL: {
        if (cellArray.isScalar()) {
            bool val = cellArray.asLogicalScalar();
            return QVariant(val ? "true" : "false");
        }
    } break;
    default:
        break;
    }
    return QVariant("");
}
//=============================================================================
Qt::ItemFlags
VariableStructTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsEnabled | Qt::ItemIsSelectable;

    const int row = index.row();
    const int col = index.column();

    if (col == 0) {
        return flags;
    }

    int dataCol = col - 1;
    if (isValidPosition(row, dataCol)) {

        QString name = m_fieldNames[dataCol];
        const std::string fieldName = name.toStdString();
        ArrayOfVector list = m_array.fieldAsList(fieldName);
        if (row < static_cast<int>(list.size())) {
            const ArrayOf& value = list[row];

            const bool isScalar = value.isScalar() && !value.isReferenceType();
            const bool isCharRowVector = value.isRowVectorCharacterArray();
            if (isScalar || isCharRowVector) {
                flags |= Qt::ItemIsEditable;
            }
        }
    }
    return flags;
}
//=============================================================================
QVariant
VariableStructTableModel::headerData(int section, Qt::Orientation orientation, int role) const
{
    if (orientation == Qt::Horizontal) {
        if (role == Qt::DisplayRole || role == Qt::EditRole) {
            if (section == 0) {
                return tr("Fields");
            } else {
                int fieldIndex = section - 1;
                if (fieldIndex < m_fieldNames.size()) {
                    return m_fieldNames[fieldIndex];
                }
            }
        }
    }

    return QVariant();
}
//=============================================================================
bool
VariableStructTableModel::setHeaderData(
    int section, Qt::Orientation orientation, const QVariant& value, int role)
{
    if (orientation != Qt::Horizontal || role != Qt::EditRole || section <= 0) {
        return false;
    }

    int fieldIndex = section - 1;
    if (fieldIndex >= m_fieldNames.size()) {
        return false;
    }

    QString newName = value.toString().trimmed();
    if (newName.isEmpty() || m_fieldNames.contains(newName)) {
        return false;
    }

    try {
        stringVector currentFields = m_array.fieldNames();
        if (fieldIndex >= static_cast<int>(currentFields.size())) {
            return false;
        }

        std::string newFieldname = newName.toStdString();
        newFieldname = MakeValidFieldname(newFieldname);
        std::string oldFieldname = currentFields[fieldIndex];
        currentFields[fieldIndex] = newFieldname;

        Dimensions dims = m_array.dimensions();
        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), currentFields, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, currentFields);

        for (size_t i = 0; i < currentFields.size(); ++i) {
            const std::string& field = currentFields[i];
            std::string srcField = (i == static_cast<size_t>(fieldIndex)) ? oldFieldname : field;

            int64 idx = m_array.fieldIndex(srcField);
            if (idx != -1) {
                ArrayOfVector list = m_array.fieldAsList(srcField);
                newArray.setFieldAsList(field, list);
            }
        }

        m_array = newArray;
        updateFieldNames();

        emit headerDataChanged(Qt::Horizontal, section, section);
        emit modelChanged();
        schedulePersistenceUpdate();
        return true;

    } catch (const NelsonException& e) {
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
QString
VariableStructTableModel::getSelectedDataAsText(const QModelIndexList& selectedIndexes) const
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
            if (col == 0) {
                cols << QString("%1").arg(row + 1);
            } else {
                QVariant cellData = getCellDisplayValue(row, col - 1);
                cols << cellData.toString();
            }
        }
        rows << cols.join("\t");
    }

    return rows.join("\n");
}
//=============================================================================
bool
VariableStructTableModel::pasteDataFromClipboard(int startRow, int startCol, const QString& text)
{
    startCol = startCol - 1;
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
ArrayOf
VariableStructTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return ArrayOf::empty();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return ArrayOf::empty();
    }

    try {
        int selRows = bounds.maxRow - bounds.minRow + 1;
        stringVector fieldNames;
        for (int col = bounds.minCol; col <= bounds.maxCol; ++col) {
            if (col - 1 < m_fieldNames.size() && col > 0) {
                fieldNames.push_back(m_fieldNames[col - 1].toStdString());
            }
        }

        Dimensions dims(selRows, 1);
        indexType elementCount = dims.elementCount();

        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, elementCount, fieldNames, false));
        ArrayOf result(NLS_STRUCT_ARRAY, dims, ptr, false, fieldNames);

        for (size_t f = 0; f < fieldNames.size(); ++f) {
            ArrayOfVector values;
            values.reserve(selRows);

            for (int row = 0; row < selRows; ++row) {
                int srcRow = bounds.minRow + row;
                int srcCol = bounds.minCol + static_cast<int>(f);
                ArrayOf val = fieldValue(srcRow, srcCol);
                values.push_back(val);
            }

            result.setFieldAsList(fieldNames[f], values);
        }

        return result;

    } catch (const NelsonException& e) {
        qDebug() << "Error creating array from selection:" << QString::fromStdWString(e.message());
        return ArrayOf::empty();
    }
}
//=============================================================================
bool
VariableStructTableModel::insertRowAt(int row)
{
    if (row < 0 || row > m_rows) {
        return false;
    }
    saveCurrentStateForUndo();

    beginInsertRows(QModelIndex(), row, row);

    try {
        stringVector fieldNames = m_array.fieldNames();
        Dimensions newDims(m_rows + 1, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, newDims.elementCount(), fieldNames, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, newDims, ptr, false, fieldNames);

        for (const auto& field : fieldNames) {
            ArrayOfVector list = m_array.fieldAsList(field);
            ArrayOfVector newList;
            newList.reserve(newDims.elementCount());

            for (int i = 0; i < newDims.elementCount(); ++i) {
                if (i == row) {
                    newList.push_back(ArrayOf::empty());
                } else {
                    int srcIndex = (i < row) ? i : i - 1;
                    if (srcIndex < static_cast<int>(list.size())) {
                        newList.push_back(list[srcIndex]);
                    } else {
                        newList.push_back(ArrayOf::empty());
                    }
                }
            }

            newArray.setFieldAsList(field, newList);
        }

        m_array = newArray;
        m_rows++;
        m_rows_display = std::max(m_rows, MIN_DISPLAY_SIZE);

        schedulePersistenceUpdate();

        endInsertRows();
        emit modelChanged();
        return true;

    } catch (const NelsonException& e) {
        endInsertRows();
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
bool
VariableStructTableModel::insertColumnAt(int col)
{
    col = col - 1;

    if (col < 0) {
        return false;
    }
    saveCurrentStateForUndo();

    if (col >= m_cols) {
        if (!expandArrayIfNeeded(m_rows - 1, col)) {
            return false;
        }
        return true;
    }

    beginInsertColumns(QModelIndex(), col, col);

    try {
        QString newFieldName = QString("Field%1").arg(col);
        std::string unewFieldName = newFieldName.toStdString();

        stringVector oldFieldNames = m_array.fieldNames();
        stringVector newFieldNames;

        for (int i = 0; i <= static_cast<int>(oldFieldNames.size()); ++i) {
            if (i == col) {
                newFieldNames.push_back(unewFieldName);
            }
            if (i < static_cast<int>(oldFieldNames.size())) {
                newFieldNames.push_back(oldFieldNames[i]);
            }
        }

        Dimensions dims(m_rows, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), newFieldNames, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, newFieldNames);

        for (int i = 0; i < m_rows; ++i) {
            for (size_t j = 0; j < oldFieldNames.size(); ++j) {
                std::string oldField = oldFieldNames[j];
                ArrayOfVector list = m_array.fieldAsList(oldField);
                if (i < static_cast<int>(list.size())) {
                    size_t targetIndex = (static_cast<int>(j) >= col) ? j + 1 : j;
                    std::string tarfield = newFieldNames[targetIndex];
                    ArrayOfVector newList = newArray.fieldAsList(tarfield);
                    newList[i] = list[i];
                    newArray.setFieldAsList(tarfield, newList);
                }
            }

            ArrayOfVector insertedList = newArray.fieldAsList(unewFieldName);
            insertedList[i] = ArrayOf::empty();
            newArray.setFieldAsList(unewFieldName, insertedList);
        }

        m_array = newArray;
        m_cols++;
        m_cols_display = std::max(m_cols, MIN_DISPLAY_SIZE);

        updateFieldNames();
        schedulePersistenceUpdate();

        endInsertColumns();
        emit modelChanged();
        return true;

    } catch (const NelsonException& e) {
        endInsertColumns();
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
bool
VariableStructTableModel::deleteRowAt(int row)
{
    if (row < 0 || row >= m_rows) {
        return false;
    }
    saveCurrentStateForUndo();

    beginRemoveRows(QModelIndex(), row, row);

    try {
        stringVector fieldNames = m_array.fieldNames();
        Dimensions newDims(m_rows - 1, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, newDims.elementCount(), fieldNames, false));

        ArrayOf newArray(NLS_STRUCT_ARRAY, newDims, ptr, false, fieldNames);

        for (const auto& field : fieldNames) {
            ArrayOfVector list = m_array.fieldAsList(field);
            ArrayOfVector newList;

            for (int i = 0; i < m_rows; ++i) {
                if (i != row && i < static_cast<int>(list.size())) {
                    newList.push_back(list[i]);
                }
            }

            newArray.setFieldAsList(field, newList);
        }

        m_array = newArray;
        m_rows--;
        m_rows_display = std::max(m_rows, MIN_DISPLAY_SIZE);

        schedulePersistenceUpdate();

        endRemoveRows();
        emit modelChanged();
        return true;

    } catch (const NelsonException& e) {
        endRemoveRows();
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
bool
VariableStructTableModel::deleteColumnAt(int col)
{
    col = col - 1;
    if (col < 0 || col >= m_cols) {
        return false;
    }
    saveCurrentStateForUndo();

    beginRemoveColumns(QModelIndex(), col + 1, col + 1);
    try {
        stringVector oldFieldNames = m_array.fieldNames();
        stringVector newFieldNames;

        for (int i = 0; i < static_cast<int>(oldFieldNames.size()); ++i) {
            if (i != col) {
                newFieldNames.push_back(oldFieldNames[i]);
            }
        }

        Dimensions dims(m_rows, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), newFieldNames, false));

        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, newFieldNames);

        for (const auto& field : newFieldNames) {
            ArrayOfVector list = m_array.fieldAsList(field);
            newArray.setFieldAsList(field, list);
        }

        m_array = newArray;
        m_cols--;
        m_cols_display = std::max(m_cols, MIN_DISPLAY_SIZE);

        updateFieldNames();
        schedulePersistenceUpdate();

        endRemoveColumns();
        emit modelChanged();
        return true;

    } catch (const NelsonException& e) {
        endRemoveColumns();
        emit errorOccurred(QString::fromStdWString(e.message()));
        return false;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
