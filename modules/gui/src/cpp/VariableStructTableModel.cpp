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
VariableStructTableModel::VariableStructTableModel(
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
        &VariableStructTableModel::performBatchedPersistence);
    saveCurrentStateForUndo();
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
        Dimensions dims = m_array.getDimensions();
        m_rows = static_cast<int>(dims.getElementCount());

        stringVector fieldNames = m_array.getFieldNames();
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
void
VariableStructTableModel::initializeEditData()
{
    m_editData.clear();
    m_editData.resize(m_rows_display * m_cols_display);
}
//=============================================================================
void
VariableStructTableModel::updateFieldNames()
{
    m_fieldNames.clear();

    if (m_array.isStruct() && !m_array.isEmpty()) {
        stringVector fieldNames = m_array.getFieldNames();
        for (const auto& fieldName : fieldNames) {
            m_fieldNames << QString::fromStdWString(utf8_to_wstring(fieldName));
        }
    }
}
//=============================================================================
int
VariableStructTableModel::rowCount(const QModelIndex& parent) const
{
    Q_UNUSED(parent);
    return m_rows_display;
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
        // Adjust column index for actual data columns
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

    // Adjust column index for actual data columns
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

    if (!setCellValue(row, dataCol, resultArray)) {
        return false;
    }

    updateEditData(row, dataCol, value);

    schedulePeristenceUpdate();

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
QVariant
VariableStructTableModel::getDisplayValue(int row, int col) const
{
    return getCellDisplayValue(row, col);
}
//=============================================================================
bool
VariableStructTableModel::hasOriginalData() const
{
    return !m_array.isEmpty();
}
//=============================================================================
bool
VariableStructTableModel::isValidPosition(int row, int col) const
{
    return row >= 0 && row < m_rows && col >= 0 && col < m_cols;
}
//=============================================================================
bool
VariableStructTableModel::resizeArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    return expandArrayIfNeeded(row, col);
}
//=============================================================================
bool
VariableStructTableModel::setArrayMissingValue(int row, int col)
{
    if (!isValidPosition(row, col)) {
        return false;
    }

    return setFieldValue(row, col, ArrayOf::emptyConstructor());
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
    } catch (const Exception& e) {
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
        return false;
    }
}
//=============================================================================
void
VariableStructTableModel::updateEditData(int row, int col, const QVariant& value)
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
VariableStructTableModel::getFieldValue(int structIndex, int fieldIndex) const
{
    if (!isValidPosition(structIndex, fieldIndex)) {
        return ArrayOf::emptyConstructor();
    }

    try {
        stringVector fieldNames = m_array.getFieldNames();
        if (fieldIndex < static_cast<int>(fieldNames.size())) {
            ArrayOfVector results = m_array.getFieldAsList(fieldNames[fieldIndex]);
            return results[structIndex];
        }
    } catch (const Exception& e) {
        qDebug() << "Error getting field value:" << QString::fromStdWString(e.getMessage());
    }

    return ArrayOf::emptyConstructor();
}
//=============================================================================
bool
VariableStructTableModel::setFieldValue(int structIndex, int fieldIndex, const ArrayOf& value)
{
    if (!isValidPosition(structIndex, fieldIndex)) {
        return false;
    }

    try {
        stringVector fieldNames = m_array.getFieldNames();
        if (fieldIndex < static_cast<int>(fieldNames.size())) {
            ArrayOfVector list = m_array.getFieldAsList(fieldNames[fieldIndex]);
            if (static_cast<size_t>(structIndex) < list.size()) {
                list[structIndex] = value;
                m_array.setFieldAsList(fieldNames[fieldIndex], list);
                return true;
            }
        }
    } catch (const Exception& e) {
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
    }

    return false;
}
//=============================================================================
QString
VariableStructTableModel::getFieldName(int fieldIndex) const
{
    if (fieldIndex >= 0 && fieldIndex < m_fieldNames.size()) {
        return m_fieldNames[fieldIndex];
    }
    return QString();
}
//=============================================================================
int
VariableStructTableModel::getFieldIndex(const QString& fieldName) const
{
    return m_fieldNames.indexOf(fieldName);
}
//=============================================================================
void
VariableStructTableModel::persistChangesToContext()
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
VariableStructTableModel::schedulePeristenceUpdate()
{
    m_pendingChanges = true;
    if (m_persistenceTimer && !m_persistenceTimer->isActive()) {
        m_persistenceTimer->start();
    }
}
//=============================================================================
void
VariableStructTableModel::performBatchedPersistence()
{
    if (m_pendingChanges) {
        persistChangesToContext();
        m_pendingChanges = false;
    }
}
//=============================================================================
VariableStructTableModel::SelectionBounds
VariableStructTableModel::getSelectionBounds(const QModelIndexList& selectedIndexes) const
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
VariableStructTableModel::PasteData
VariableStructTableModel::parsePasteText(const QString& text) const
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
VariableStructTableModel::validatePasteData(const PasteData& data, int startRow, int startCol) const
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
VariableStructTableModel::applyPasteData(const PasteData& data, int startRow, int startCol)
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

                    if (!setFieldValue(targetRow, targetCol, parsedValue)) {
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
VariableStructTableModel::indexToFlat(int row, int col) const
{
    return row * m_cols_display + col;
}
//=============================================================================
std::pair<int, int>
VariableStructTableModel::flatToIndex(int flatIndex) const
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
VariableStructTableModel::expandArrayIfNeeded(int row, int col)
{
    if (row < m_rows && col < m_cols) {
        return true;
    }

    try {
        int newRows = std::max(m_rows, row + 1);
        int newCols = std::max(m_cols, col + 1);

        stringVector oldFieldNames = m_array.getFieldNames();
        stringVector newFieldNames = oldFieldNames;

        while (static_cast<int>(newFieldNames.size()) < newCols) {
            QString generated = QString("Field%1").arg(newFieldNames.size() + 1);
            newFieldNames.push_back(generated.toStdString());
        }

        Dimensions dims(newRows, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dims.getElementCount(), newFieldNames, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, newFieldNames);

        for (size_t j = 0; j < oldFieldNames.size(); ++j) {
            const std::string& field = oldFieldNames[j];
            ArrayOfVector oldList = m_array.getFieldAsList(field);
            ArrayOfVector newList;
            newList.reserve(newRows);

            for (int i = 0; i < newRows; ++i) {
                if (i < static_cast<int>(oldList.size())) {
                    newList.push_back(oldList[i]);
                } else {
                    newList.push_back(ArrayOf::emptyConstructor());
                }
            }

            newArray.setFieldAsList(field, newList);
        }

        for (size_t j = oldFieldNames.size(); j < newFieldNames.size(); ++j) {
            const std::string& field = newFieldNames[j];
            ArrayOfVector list;
            list.reserve(newRows);

            for (int i = 0; i < newRows; ++i) {
                list.push_back(ArrayOf::emptyConstructor());
            }

            newArray.setFieldAsList(field, list);
        }

        m_array = newArray;
        m_rows = newRows;
        m_cols = newCols;

        updateFieldNames();
        return true;

    } catch (const Exception& e) {
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
        return false;
    }
}
//=============================================================================
bool
VariableStructTableModel::expandDisplay(int row, int col)
{
    return expandDisplayIfNeeded(row, col);
}
//=============================================================================
bool
VariableStructTableModel::resizeArray(int row, int col)
{
    return expandArrayIfNeeded(row, col);
}
//=============================================================================
void
VariableStructTableModel::resizeEditData(int oldRows, int oldCols)
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
VariableStructTableModel::getCellDisplayValue(int row, int col) const
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
VariableStructTableModel::getCellEditValue(int row, int col) const
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
VariableStructTableModel::setCellValue(int row, int col, const ArrayOf& cellValue)
{
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }

    return setFieldValue(row, col, cellValue);
}
//=============================================================================
QVariant
VariableStructTableModel::formatStructForDisplay(const QVariant& cellData) const
{
    if (!cellData.isValid()) {
        return QString();
    }

    return cellData.toString();
}
//=============================================================================
QVariant
VariableStructTableModel::formatStructForDisplay(const ArrayOf& cellArray) const
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
ArrayOf
VariableStructTableModel::parseInputToArrayOf(const QString& input, int row, int col) const
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
VariableStructTableModel::formatCellForEdit(const ArrayOf& cellArray) const
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
    return QVariant("");
}
//=============================================================================
QVariant
VariableStructTableModel::formatCellForEdit(const QVariant& cellData) const
{
    return cellData;
}
//=============================================================================
bool
VariableStructTableModel::isValidStructArray(const ArrayOf& array) const
{
    if (array.isEmpty()) {
        return true;
    }

    return array.isStruct();
}
//=============================================================================
bool
VariableStructTableModel::hasConsistentFields(const ArrayOf& array) const
{
    if (!array.isStruct()) {
        return false;
    }
    return true;
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
        ArrayOfVector list = m_array.getFieldAsList(fieldName);
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
        stringVector currentFields = m_array.getFieldNames();
        if (fieldIndex >= static_cast<int>(currentFields.size())) {
            return false;
        }

        std::string newFieldname = newName.toStdString();
        newFieldname = MakeValidFieldname(newFieldname);
        std::string oldFieldname = currentFields[fieldIndex];
        currentFields[fieldIndex] = newFieldname;

        Dimensions dims = m_array.getDimensions();
        ArrayOf* ptr = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dims.getElementCount(), currentFields, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, currentFields);

        for (size_t i = 0; i < currentFields.size(); ++i) {
            const std::string& field = currentFields[i];
            std::string srcField = (i == static_cast<size_t>(fieldIndex)) ? oldFieldname : field;

            int64 idx = m_array.getFieldIndex(srcField);
            if (idx != -1) {
                ArrayOfVector list = m_array.getFieldAsList(srcField);
                newArray.setFieldAsList(field, list);
            }
        }

        m_array = newArray;
        updateFieldNames();

        emit headerDataChanged(Qt::Horizontal, section, section);
        emit modelChanged();
        schedulePeristenceUpdate();
        return true;

    } catch (const Exception& e) {
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
        return false;
    }
}
//=============================================================================
void
VariableStructTableModel::refreshFromArray(ArrayOf& value)
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
VariableStructTableModel::isStructureCompatible(const ArrayOf& value)
{
    if (!isValidStructArray(value)) {
        return false;
    }

    if (m_array.isStruct() && value.isStruct()) {
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
VariableStructTableModel::forceViewUpdate()
{
    beginResetModel();
    endResetModel();
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
VariableStructTableModel::pasteDataFromExcel(int startRow, int startCol, const QString& text)
{
    return pasteDataFromClipboard(startRow, startCol, text);
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
bool
VariableStructTableModel::canPasteAt(int startRow, int startCol, const QString& text) const
{
    PasteData data = parsePasteText(text);
    if (!data.isValid) {
        return false;
    }

    return validatePasteData(data, startRow, startCol);
}
//=============================================================================
QSize
VariableStructTableModel::getPasteSize(const QString& text) const
{
    PasteData data = parsePasteText(text);
    if (!data.isValid) {
        return QSize(0, 0);
    }

    return QSize(data.cols, data.rows);
}
//=============================================================================
ArrayOf
VariableStructTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return ArrayOf::emptyConstructor();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return ArrayOf::emptyConstructor();
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
VariableStructTableModel::isValidSelectionForExtraction(
    const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return false;
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    return bounds.isValid;
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
        stringVector fieldNames = m_array.getFieldNames();
        Dimensions newDims(m_rows + 1, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, newDims.getElementCount(), fieldNames, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, newDims, ptr, false, fieldNames);

        for (const auto& field : fieldNames) {
            ArrayOfVector list = m_array.getFieldAsList(field);
            ArrayOfVector newList;
            newList.reserve(newDims.getElementCount());

            for (int i = 0; i < newDims.getElementCount(); ++i) {
                if (i == row) {
                    newList.push_back(ArrayOf::emptyConstructor());
                } else {
                    int srcIndex = (i < row) ? i : i - 1;
                    if (srcIndex < static_cast<int>(list.size())) {
                        newList.push_back(list[srcIndex]);
                    } else {
                        newList.push_back(ArrayOf::emptyConstructor());
                    }
                }
            }

            newArray.setFieldAsList(field, newList);
        }

        m_array = newArray;
        m_rows++;
        m_rows_display = std::max(m_rows, MIN_DISPLAY_SIZE);

        schedulePeristenceUpdate();

        endInsertRows();
        emit modelChanged();
        return true;

    } catch (const Exception& e) {
        endInsertRows();
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
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

        stringVector oldFieldNames = m_array.getFieldNames();
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

        ArrayOf* ptr = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dims.getElementCount(), newFieldNames, false));
        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, newFieldNames);

        for (int i = 0; i < m_rows; ++i) {
            for (size_t j = 0; j < oldFieldNames.size(); ++j) {
                std::string oldField = oldFieldNames[j];
                ArrayOfVector list = m_array.getFieldAsList(oldField);
                if (i < static_cast<int>(list.size())) {
                    size_t targetIndex = (static_cast<int>(j) >= col) ? j + 1 : j;
                    std::string targetField = newFieldNames[targetIndex];
                    ArrayOfVector newList = newArray.getFieldAsList(targetField);
                    newList[i] = list[i];
                    newArray.setFieldAsList(targetField, newList);
                }
            }

            ArrayOfVector insertedList = newArray.getFieldAsList(unewFieldName);
            insertedList[i] = ArrayOf::emptyConstructor();
            newArray.setFieldAsList(unewFieldName, insertedList);
        }

        m_array = newArray;
        m_cols++;
        m_cols_display = std::max(m_cols, MIN_DISPLAY_SIZE);

        updateFieldNames();
        schedulePeristenceUpdate();

        endInsertColumns();
        emit modelChanged();
        return true;

    } catch (const Exception& e) {
        endInsertColumns();
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
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
        stringVector fieldNames = m_array.getFieldNames();
        Dimensions newDims(m_rows - 1, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, newDims.getElementCount(), fieldNames, false));

        ArrayOf newArray(NLS_STRUCT_ARRAY, newDims, ptr, false, fieldNames);

        for (const auto& field : fieldNames) {
            ArrayOfVector list = m_array.getFieldAsList(field);
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

        schedulePeristenceUpdate();

        endRemoveRows();
        emit modelChanged();
        return true;

    } catch (const Exception& e) {
        endRemoveRows();
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
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
        stringVector oldFieldNames = m_array.getFieldNames();
        stringVector newFieldNames;

        for (int i = 0; i < static_cast<int>(oldFieldNames.size()); ++i) {
            if (i != col) {
                newFieldNames.push_back(oldFieldNames[i]);
            }
        }

        Dimensions dims(m_rows, 1);

        ArrayOf* ptr = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(
            NLS_STRUCT_ARRAY, dims.getElementCount(), newFieldNames, false));

        ArrayOf newArray(NLS_STRUCT_ARRAY, dims, ptr, false, newFieldNames);

        for (const auto& field : newFieldNames) {
            ArrayOfVector list = m_array.getFieldAsList(field);
            newArray.setFieldAsList(field, list);
        }

        m_array = newArray;
        m_cols--;
        m_cols_display = std::max(m_cols, MIN_DISPLAY_SIZE);

        updateFieldNames();
        schedulePeristenceUpdate();

        endRemoveColumns();
        emit modelChanged();
        return true;

    } catch (const Exception& e) {
        endRemoveColumns();
        emit errorOccurred(QString::fromStdWString(e.getMessage()));
        return false;
    }
}
//=============================================================================
bool
VariableStructTableModel::shouldDisplayAsTable() const
{
    return m_array.isStruct() && !m_array.isEmpty();
}
//=============================================================================
void
VariableStructTableModel::undo()
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
VariableStructTableModel::redo()
{
    undo();
}
//=============================================================================
}
//=============================================================================
