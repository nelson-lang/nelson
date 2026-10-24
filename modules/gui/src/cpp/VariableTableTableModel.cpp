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
VariableTableTableModel::VariableTableTableModel(
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
        m_rows = static_cast<int>(m_array.tableHeight());
        m_cols = static_cast<int>(m_array.tableWidth());

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
bool
VariableTableTableModel::isCompatibleArrayType(const ArrayOf& array) const
{
    return array.isTable();
}
//=============================================================================
stringVector
VariableTableTableModel::rawFieldNames(const ArrayOf& array) const
{
    return array.tableVariableNames();
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
    } catch (NelsonException&) {
        results.clear();
        results << ArrayOf::characterArray(command);
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

    schedulePersistenceUpdate();

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
ArrayOf
VariableTableTableModel::fieldValue(int row, int col) const
{
    if (!isValidPosition(row, col)) {
        return ArrayOf::empty();
    }

    try {
        stringVector fieldNames = m_array.tableVariableNames();
        if (col >= static_cast<int>(fieldNames.size())) {
            return ArrayOf::empty();
        }

        ArrayOf column = m_array.tableColumn(fieldNames[col]);
        if (column.isEmpty() || column.isScalar() || column.isRowVectorCharacterArray()) {
            return column;
        }

        if (column.isCell() || column.isStringArray()) {
            ArrayOf* elements = (ArrayOf*)(column.dataPointer());
            return elements[row];
        }

        auto extractRow = [&](auto* src) -> ArrayOf {
            using T = std::decay_t<decltype(*src)>;

            Dimensions dims = column.dimensions();
            size_t cols = column.isVector() ? 1 : dims.columns();
            size_t rows = column.isVector() ? 0 : dims.rows();

            Dimensions dimsOut(1, cols);
            T* dst = static_cast<T*>(ArrayOf::allocateArrayOf(column.dataClass(), cols));
            ArrayOf res(column.dataClass(), dimsOut, dst);

            if (column.isVector()) {
                dst[0] = src[row];
            } else {
                for (size_t c = 0; c < cols; ++c) {
                    dst[c] = src[row + c * rows];
                }
            }

            return res;
        };

        switch (column.dataClass()) {
        case NLS_DOUBLE:
            return extractRow((double*)(column.dataPointer()));

        case NLS_SINGLE:
            return extractRow((single*)(column.dataPointer()));

        case NLS_DCOMPLEX:
            return extractRow(
                reinterpret_cast<std::complex<double>*>((double*)(column.dataPointer())));

        case NLS_SCOMPLEX:
            return extractRow(
                reinterpret_cast<std::complex<single>*>((single*)(column.dataPointer())));

        case NLS_INT8:
            return extractRow((int8*)(column.dataPointer()));

        case NLS_INT16:
            return extractRow((int16*)(column.dataPointer()));

        case NLS_INT32:
            return extractRow((int32*)(column.dataPointer()));

        case NLS_INT64:
            return extractRow((int64*)(column.dataPointer()));

        case NLS_UINT8:
            return extractRow((uint8*)(column.dataPointer()));

        case NLS_UINT16:
            return extractRow((uint16*)(column.dataPointer()));

        case NLS_UINT32:
            return extractRow((uint32*)(column.dataPointer()));

        case NLS_UINT64:
            return extractRow((uint64*)(column.dataPointer()));

        case NLS_LOGICAL:
            return extractRow((logical*)(column.dataPointer()));

        default:
            break;
        }

    } catch (const NelsonException&) {
    }

    return ArrayOf::empty();
}
//=============================================================================
bool
VariableTableTableModel::setFieldValue(int row, int col, const ArrayOf& value)
{
    if (!isValidPosition(row, col)) {
        return false;
    }

    try {
        stringVector fieldNames = m_array.tableVariableNames();
        if (col < static_cast<int>(fieldNames.size())) {
            ArrayOf column = m_array.tableColumn(fieldNames[col]);
            ArrayOf v = value;
            column.setField(fieldNames[col], v);
            m_array.setTableColumn(fieldNames[col], column);
            return true;
        }
    } catch (const NelsonException& e) {
        emit errorOccurred(QString::fromStdWString(e.message()));
    }

    return false;
}
//=============================================================================
bool
VariableTableTableModel::writePastedValue(int row, int col, const ArrayOf& value)
{
    return setCellValue(row, col, value);
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
VariableTableTableModel::setCellValue(int row, int col, const ArrayOf& cellValue)
{
    if (!expandArrayIfNeeded(row, col)) {
        return false;
    }
    ArrayOf tableData = m_array.tableData();
    stringVector fieldnames = tableData.fieldNames();

    ArrayOf dataCol = tableData.field(fieldnames[col]);
    switch (dataCol.dataClass()) {
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY: {
        ArrayOf* elements = (ArrayOf*)dataCol.dataPointer();
        elements[row] = cellValue;
        return true;
    } break;
    case NLS_CHAR: {
        charType* ptrIn = (charType*)dataCol.dataPointer();
        std::string val = cellValue.asUtf8String();
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
            auto* ptrZ = reinterpret_cast<std::complex<double>*>((double*)dataCol.dataPointer());
            std::complex<double> val = cellValue.asDoubleComplexScalar();
            ptrZ[row].real(val.real());
            ptrZ[row].imag(val.imag());
            m_array.setTableColumn(fieldnames[col], dataCol);
        } else {
            double* ptrIn = (double*)dataCol.dataPointer();
            double val = cellValue.asDoubleScalar();
            ptrIn[row] = val;
        }
        return true;
    } break;
    case NLS_SINGLE: {
        if (cellValue.isComplex()) {
            dataCol.promoteType(NLS_SCOMPLEX);
            auto* ptrZ = reinterpret_cast<std::complex<single>*>((single*)dataCol.dataPointer());
            std::complex<double> val = cellValue.asSingleComplexScalar();
            ptrZ[row].real(val.real());
            ptrZ[row].imag(val.imag());
            m_array.setTableColumn(fieldnames[col], dataCol);
        } else {
            single* ptrIn = (single*)dataCol.dataPointer();
            single val = cellValue.asSingleScalar();
            ptrIn[row] = val;
        }
        return true;
    } break;
    case NLS_DCOMPLEX: {
        if (cellValue.isSparse()) {
            return false;
        }
        std::complex<double> val = cellValue.asDoubleComplexScalar();
        auto* ptrZ = reinterpret_cast<std::complex<double>*>((double*)dataCol.dataPointer());
        ptrZ[row].real(val.real());
        ptrZ[row].imag(val.imag());
        return true;
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single> val = cellValue.asSingleComplexScalar();
        auto* ptrZ = reinterpret_cast<std::complex<single>*>((single*)dataCol.dataPointer());
        ptrZ[row].real(val.real());
        ptrZ[row].imag(val.imag());
        return true;
    } break;
    case NLS_INT8: {
        int8 val = cellValue.asInt8Scalar();
        int8* ptrIn = (int8*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_INT16: {
        int16 val = cellValue.asInt16Scalar();
        int16* ptrIn = (int16*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_INT32: {
        int32 val = cellValue.asInt32Scalar();
        int32* ptrIn = (int32*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_INT64: {
        int64 val = cellValue.asInt64Scalar();
        int64* ptrIn = (int64*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT8: {
        uint8 val = cellValue.asUint8Scalar();
        uint8* ptrIn = (uint8*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT16: {
        uint16 val = cellValue.asUint16Scalar();
        uint16* ptrIn = (uint16*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT32: {
        uint32 val = cellValue.asUint32Scalar();
        uint32* ptrIn = (uint32*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_UINT64: {
        uint64 val = cellValue.asUint64Scalar();
        uint64* ptrIn = (uint64*)dataCol.dataPointer();
        ptrIn[row] = val;
        return true;
    } break;
    case NLS_LOGICAL: {
        if (cellValue.isSparse()) {
            return false;
        }
        logical val = cellValue.asLogicalScalar();
        logical* ptrIn = (logical*)dataCol.dataPointer();
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
VariableTableTableModel::formatStructForDisplay(const ArrayOf& cellArray) const
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
            auto formatPart = [](float v, const QString& suffix = "") -> QString {
                if (std::isnan(v)) {
                    return "NaN" + suffix;
                }
                if (std::isinf(v)) {
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                }
                return QString::number(v, 'g', 6) + suffix;
            };

            const float real = val.real();
            const float imag = val.imag();

            if (imag == 0.0f) {
                return QVariant(formatPart(real));
            }

            if (real == 0.0f) {
                return QVariant(formatPart(imag, "i"));
            }

            QString realPart = formatPart(real);
            QString imagPart = (imag >= 0 ? "+" : "") + formatPart(imag, "i");

            return QVariant(realPart + imagPart);
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
            auto formatPart = [](double v, const QString& suffix = "") -> QString {
                if (std::isnan(v)) {
                    return "NaN" + suffix;
                }
                if (std::isinf(v)) {
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                }
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
VariableTableTableModel::formatCellForEdit(const ArrayOf& cellArray) const
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
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            std::complex<single> val = cellArray.asSingleComplexScalar();
            auto formatPart = [](float v, const QString& suffix = "") -> QString {
                if (std::isnan(v)) {
                    return "NaN" + suffix;
                }
                if (std::isinf(v)) {
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                }
                return QString::number(v, 'g', 6) + suffix;
            };

            const float real = val.real();
            const float imag = val.imag();

            if (imag == 0.0f) {
                return QVariant(formatPart(real));
            }

            if (real == 0.0f) {
                return QVariant(formatPart(imag, "i"));
            }

            QString realPart = formatPart(real);
            QString imagPart = (imag >= 0 ? "+" : "") + formatPart(imag, "i");

            return QVariant(realPart + imagPart);
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
        if (cellArray.isScalar() && !cellArray.isSparse()) {
            std::complex<double> val = cellArray.asDoubleComplexScalar();
            auto formatPart = [](double v, const QString& suffix = "") -> QString {
                if (std::isnan(v)) {
                    return "NaN" + suffix;
                }
                if (std::isinf(v)) {
                    return (v > 0 ? "Inf" : "-Inf") + suffix;
                }
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
VariableTableTableModel::flags(const QModelIndex& index) const
{
    if (!index.isValid()) {
        return Qt::NoItemFlags;
    }

    Qt::ItemFlags flags = Qt::ItemIsEnabled | Qt::ItemIsSelectable;

    const int row = index.row();
    const int col = index.column();

    if (isValidPosition(row, col)) {
        stringVector fieldNames = m_array.tableVariableNames();
        if (col < static_cast<int>(fieldNames.size())) {
            ArrayOf column = m_array.tableColumn(fieldNames[col]);
            if ((column.isScalar() && !column.isReferenceType())
                || column.isRowVectorCharacterArray()) {
                flags |= Qt::ItemIsEditable;
            } else {
                if (column.isCell() || column.isStringArray()) {
                    if (row < static_cast<int>(column.rows())) {
                        flags |= Qt::ItemIsEditable;
                    }
                } else {
                    Dimensions dims = column.dimensions();
                    switch (column.dataClass()) {
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
            ArrayOf tableProperties = m_array.tableProperties();
            if (tableProperties.isStruct()) {
                ArrayOf variableNames = tableProperties.field("VariableNames");
                stringVector fieldNames = variableNames.asUtf8StringVector();
                if (section < static_cast<int>(fieldNames.size())) {
                    return QString::fromStdString(fieldNames[section]);
                }
            }
        }
    } else if (orientation == Qt::Vertical) {
        if (role == Qt::DisplayRole || role == Qt::EditRole) {
            ArrayOf tableProperties = m_array.tableProperties();
            if (tableProperties.isStruct()) {
                ArrayOf rowNames = tableProperties.field("RowNames");
                if (rowNames.isEmpty()) {
                    return QString::number(section + 1);
                }
                stringVector fieldNames = rowNames.asUtf8StringVector();
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
    ArrayOf tableProperties = m_array.tableProperties();
    if (tableProperties.isStruct()) {
        ArrayOf rowNames = tableProperties.field("RowNames");
        stringVector fieldNames = rowNames.asUtf8StringVector();
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
    ArrayOf tableProperties = m_array.tableProperties();

    if (orientation == Qt::Horizontal) {
        if (tableProperties.isStruct()) {
            ArrayOf variableNames = tableProperties.field("VariableNames");
            stringVector fieldNames = variableNames.asUtf8StringVector();
            QString newName = value.toString().trimmed();
            std::string newFieldname = newName.toStdString();
            fieldNames[section] = newFieldname;

            ArrayOf tableData = m_array.tableData();
            tableData.renameFieldnames(fieldNames);
            m_array.setTableData(tableData);

            ArrayOf newVariableNames = ArrayOf::cellArrayFromArrayOfCharacterRowVectors(fieldNames);
            tableProperties.setField("VariableNames", newVariableNames);
            m_array.setTableProperties(tableProperties);
            emit headerDataChanged(Qt::Horizontal, section, section);
        } else {
            return false;
        }

    } else if (orientation == Qt::Vertical) {
        if (tableProperties.isStruct()) {
            ArrayOf rowNames = tableProperties.field("RowNames");
            stringVector fieldNames = rowNames.asUtf8StringVector();
            QString newName = value.toString().trimmed();
            std::string newFieldname = newName.toStdString();
            fieldNames[section] = newFieldname;
            ArrayOf newRowNames = ArrayOf::cellArrayFromArrayOfCharacterRowVectors(fieldNames);
            tableProperties.setField("RowNames", newRowNames);
            m_array.setTableProperties(tableProperties);
            emit headerDataChanged(Qt::Vertical, section, section);

        } else {
            return false;
        }
    }
    emit modelChanged();
    schedulePersistenceUpdate();
    return true;
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
ArrayOf
VariableTableTableModel::createArrayFromSelection(const QModelIndexList& selectedIndexes) const
{
    if (selectedIndexes.isEmpty()) {
        return ArrayOf::empty();
    }

    SelectionBounds bounds = getSelectionBounds(selectedIndexes);
    if (!bounds.isValid) {
        return ArrayOf::empty();
    }

    if (selectedIndexes.size() == 1) {
        const QModelIndex index = selectedIndexes.first();
        if (!index.isValid()) {
            return ArrayOf::empty();
        }
        return fieldValue(index.row(), index.column());
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
                ArrayOf value = fieldValue(row, col);
                if (row == bounds.minRow) {
                    commonType = value.dataClass();
                } else if (isHomogeneous && value.dataClass() != commonType) {
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
                        = (single*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asSingleScalar();
                    }
                    return result;
                } break;
                case NLS_DCOMPLEX: {
                    Dimensions dims(values.size(), 1);
                    double* ptr
                        = (double*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    auto* ptrZ
                        = reinterpret_cast<std::complex<double>*>((double*)result.dataPointer());
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptrZ[k] = values[k].asDoubleComplexScalar();
                    }
                } break;
                case NLS_SCOMPLEX: {
                    Dimensions dims(values.size(), 1);
                    single* ptr
                        = (single*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    auto* ptrZ
                        = reinterpret_cast<std::complex<single>*>((single*)result.dataPointer());
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptrZ[k] = values[k].asSingleComplexScalar();
                    }
                } break;
                case NLS_INT8: {
                    Dimensions dims(values.size(), 1);
                    int8* ptr = (int8*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asInt8Scalar();
                    }
                    return result;
                } break;
                case NLS_INT16: {
                    Dimensions dims(values.size(), 1);
                    int16* ptr = (int16*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asInt16Scalar();
                    }
                    return result;
                } break;
                case NLS_INT32: {
                    Dimensions dims(values.size(), 1);
                    int32* ptr = (int32*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asInt32Scalar();
                    }
                    return result;
                } break;
                case NLS_INT64: {
                    Dimensions dims(values.size(), 1);
                    int64* ptr = (int64*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asInt64Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT8: {
                    Dimensions dims(values.size(), 1);
                    uint8* ptr = (uint8*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asUint8Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT16: {
                    Dimensions dims(values.size(), 1);
                    uint16* ptr
                        = (uint16*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asUint16Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT32: {
                    Dimensions dims(values.size(), 1);
                    uint32* ptr
                        = (uint32*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asUint32Scalar();
                    }
                    return result;
                } break;
                case NLS_UINT64: {
                    Dimensions dims(values.size(), 1);
                    uint64* ptr
                        = (uint64*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asUint64Scalar();
                    }
                    return result;
                } break;
                case NLS_LOGICAL: {
                    Dimensions dims(values.size(), 1);
                    logical* ptr
                        = (logical*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asLogicalScalar();
                    }
                    return result;
                } break;
                case NLS_DOUBLE: {
                    Dimensions dims(values.size(), 1);
                    double* ptr
                        = (double*)ArrayOf::allocateArrayOf(commonType, dims.elementCount());
                    ArrayOf result = ArrayOf(commonType, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k].asDoubleScalar();
                    }
                    return result;
                } break;
                case NLS_CHAR: {
                    Dimensions dims(values.size(), 1);
                    ArrayOf* ptr
                        = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount());
                    ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, ptr);
                    for (indexType k = 0; k < dims.elementCount(); ++k) {
                        ptr[k] = values[k];
                    }
                    return result;
                } break;
                default:
                    break;
                }
            } else {
                Dimensions dims(values.size(), 1);
                ArrayOf* ptr
                    = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount());
                ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, ptr);
                for (indexType k = 0; k < dims.elementCount(); ++k) {
                    ptr[k] = values[k];
                }
                return result;
            }
        }

        stringVector fieldNames;
        for (int col = bounds.minCol; col <= bounds.maxCol; ++col) {
            if (col >= 0 && col < m_fieldNames.size()) {
                fieldNames.push_back(m_fieldNames[col].toStdString());
            } else {
                return ArrayOf::empty();
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
} // namespace Nelson
//=============================================================================
