//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include "ErrorID.hpp"
#include <fmt/xchar.h>
#include <memory>
#include <numeric>
#include "TableWriter.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
TableWriter::doubleComplexToString(std::complex<double> value, bool ignoreImagIfZero = false)
{
    if (ignoreImagIfZero && value.imag() == 0.0) {
        return doubleToString(value.real());
    }
    if (value.imag() < 0.0) {
        return fmt::format("{}-{}i", doubleToString(value.real()), doubleToString(-value.imag()));
    }
    return fmt::format("{}+{}i", doubleToString(value.real()), doubleToString(value.imag()));
}
//=============================================================================
std::string
TableWriter::doubleToString(double value)
{
    if (std::isnan(value)) {
        return "NaN";
    }
    if (std::isinf(value)) {
        if (value < 0) {
            return "-Inf";
        } else {
            return "Inf";
        }
    } else if (std::floor(value) == value) {
        return fmt::format("{}", (long long)value);
    } else {
        return fmt::format("{:.15g}", value);
    }
}
//=============================================================================
std::string
TableWriter::duplicateChar(const std::string& str, char ch)
{
    return std::accumulate(
        str.begin(), str.end(), std::string {}, [ch](const std::string& result, char c) {
            return result + c + (c == ch ? std::string(1, ch) : "");
        });
}
//=============================================================================
std::string
TableWriter::formatStringValueQuoteString(const std::string& value)
{
    return "\"" + duplicateChar(value, '\"') + "\"";
}
//=============================================================================
std::string
TableWriter::formatStringValue(const std::string& value, const writeTableOptions& options)
{
    if ((value.find('\"') != std::string::npos)
        || (value.find(options._Delimiter) != std::string::npos)) {
        return formatStringValueQuoteString(value);
    }
    return value;
}
//=============================================================================
void
TableWriter::handleCellColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    ArrayOf* elements = (ArrayOf*)columnData.dataPointer();
    std::vector<std::string> column;
    bool addQuotes = false;
    column.resize(columnData.elementCount());
    OMP_PARALLEL_FOR_LOOP(columnData.elementCount())
    for (ompIndexType j = 0; j < (ompIndexType)columnData.elementCount(); j++) {
        if (elements[j].isRowVectorCharacterArray() || elements[j].isScalarStringArray()) {
            std::string v = elements[j].asUtf8String();
            if (!addQuotes && options._QuoteStrings == "all") {
                addQuotes = true;
            } else if (!addQuotes && options._QuoteStrings == "minimal"
                && (v.find('\"') != std::string::npos
                    || v.find(options._Delimiter) != std::string::npos)) {
                addQuotes = true;
            }
            column[j] = v;
        } else if (elements[j].isNumeric() || elements[j].isLogical()) {
            if (elements[j].isEmpty()) {
                column[j] = "";
            } else if (elements[j].isComplex()) {
                column[j] = doubleComplexToString(elements[j].asDoubleComplexScalar());
            } else {
                column[j] = doubleToString(elements[j].asDoubleScalar());
            }
        } else {
            column[j] = "";
        }
    }
    if (addQuotes) {
        OMP_PARALLEL_FOR_LOOP(column.size())
        for (ompIndexType k = 0; k < (ompIndexType)column.size(); ++k) {
            column[k] = formatStringValueQuoteString(column[k]);
        }
    }

    writer.addColumn(formatStringValue(columnName, options), column);
}
//=============================================================================
void
TableWriter::handleStringColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    ArrayOf* values = (ArrayOf*)columnData.dataPointer();
    Dimensions dimsColumn = columnData.dimensions();

    bool isXml = options._FileType == "xml";
    bool addQuotes = false;

    if (dimsColumn.columns() != 1) {
        for (size_t c = 0; c < dimsColumn.columns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            column.resize(dimsColumn.rows());
            OMP_PARALLEL_FOR_LOOP(dimsColumn.rows())
            for (ompIndexType r = 0; r < (ompIndexType)dimsColumn.rows(); r++) {
                ArrayOf value = values[r + c * dimsColumn.rows()];
                if (value.isRowVectorCharacterArray()) {
                    std::string str = value.asUtf8String();
                    if (!addQuotes && options._QuoteStrings == "all") {
                        addQuotes = true;
                    } else if (!addQuotes && options._QuoteStrings == "minimal"
                        && (str.find('\"') != std::string::npos
                            || str.find(options._Delimiter) != std::string::npos)) {
                        addQuotes = true;
                    }

                    column[r] = str;
                } else {
                    column[r] = "";
                }
            }
            if (addQuotes) {
                OMP_PARALLEL_FOR_LOOP(column.size())
                for (ompIndexType k = 0; k < (ompIndexType)column.size(); ++k) {
                    column[k] = formatStringValueQuoteString(column[k]);
                }
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        column.resize(columnData.elementCount());
        OMP_PARALLEL_FOR_LOOP(columnData.elementCount())
        for (ompIndexType j = 0; j < (ompIndexType)columnData.elementCount(); j++) {
            ArrayOf value = values[j];
            if (value.isRowVectorCharacterArray()) {
                std::string str = value.asUtf8String();
                if (!addQuotes && options._QuoteStrings == "all") {
                    addQuotes = true;
                } else if (!addQuotes && options._QuoteStrings == "minimal"
                    && (str.find('\"') != std::string::npos
                        || str.find(options._Delimiter) != std::string::npos)) {
                    addQuotes = true;
                }
                column[j] = str;
            } else {
                column[j] = "";
            }
        }
        if (addQuotes) {
            OMP_PARALLEL_FOR_LOOP(column.size())
            for (ompIndexType k = 0; k < (ompIndexType)column.size(); ++k) {
                column[k] = formatStringValueQuoteString(column[k]);
            }
        }
        writer.addColumn(formatStringValue(columnName, options), column);
    }
}
//=============================================================================
void
TableWriter::handleDoubleComplexColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    std::complex<double>* values = (std::complex<double>*)columnData.dataPointer();
    Dimensions dimsColumn = columnData.dimensions();
    bool isXml = options._FileType == "xml";

    if (dimsColumn.columns() != 1) {
        for (size_t c = 0; c < dimsColumn.columns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            column.resize(dimsColumn.rows());
            OMP_PARALLEL_FOR_LOOP(dimsColumn.rows())
            for (ompIndexType r = 0; r < (ompIndexType)dimsColumn.rows(); r++) {
                column[r] = doubleComplexToString(values[r + c * dimsColumn.rows()], isXml);
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        column.resize(columnData.elementCount());
        OMP_PARALLEL_FOR_LOOP(columnData.elementCount())
        for (ompIndexType j = 0; j < (ompIndexType)columnData.elementCount(); j++) {
            column[j] = doubleComplexToString(values[j], isXml);
        }
        writer.addColumn(formatStringValue(columnName, options), column);
    }
}
//=============================================================================
void
TableWriter::handleDoubleColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    double* values = (double*)columnData.dataPointer();
    Dimensions dimsColumn = columnData.dimensions();
    bool isXml = options._FileType == "xml";

    if (dimsColumn.columns() != 1) {
        for (size_t c = 0; c < dimsColumn.columns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            column.resize(dimsColumn.rows());
            OMP_PARALLEL_FOR_LOOP(dimsColumn.rows())
            for (ompIndexType r = 0; r < (ompIndexType)dimsColumn.rows(); r++) {
                column[r] = doubleToString(values[r + c * dimsColumn.rows()]);
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        column.resize(columnData.elementCount());
        OMP_PARALLEL_FOR_LOOP(columnData.elementCount())
        for (ompIndexType j = 0; j < (ompIndexType)columnData.elementCount(); j++) {
            column[j] = doubleToString(values[j]);
        }
        writer.addColumn(formatStringValue(columnName, options), column);
    }
}
//=============================================================================
void
TableWriter::handleLogicalColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    logical* values = (logical*)columnData.dataPointer();
    Dimensions dimsColumn = columnData.dimensions();

    bool isXml = options._FileType == "xml";

    if (dimsColumn.columns() != 1) {
        for (size_t c = 0; c < dimsColumn.columns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            column.resize(dimsColumn.rows());
            OMP_PARALLEL_FOR_LOOP(dimsColumn.rows())
            for (ompIndexType r = 0; r < (ompIndexType)dimsColumn.rows(); r++) {
                logical value = values[r + c * dimsColumn.rows()];
                if (options._FileType == "xml") {
                    column[r] = value ? "true" : "false";
                } else {
                    column[r] = value ? "1" : "0";
                }
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        column.resize(columnData.elementCount());
        OMP_PARALLEL_FOR_LOOP(columnData.elementCount())
        for (ompIndexType j = 0; j < (ompIndexType)columnData.elementCount(); j++) {
            logical value = values[j];
            if (options._FileType == "xml") {
                column[j] = value ? "true" : "false";
            } else {
                column[j] = value ? "1" : "0";
            }
        }
        writer.addColumn(formatStringValue(columnName, options), column);
    }
}
//=============================================================================
void
TableWriter::handleCharacterColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    std::vector<std::string> column;
    if (columnData.isVector()) {
        charType* values = (charType*)columnData.dataPointer();
        bool addQuotes = false;
        column.resize(columnData.elementCount());
        OMP_PARALLEL_FOR_LOOP(columnData.elementCount())
        for (ompIndexType j = 0; j < (ompIndexType)columnData.elementCount(); j++) {
            std::wstring str(1, values[j]);
            std::string ustr = wstring_to_utf8(str);
            if (!addQuotes && options._QuoteStrings == "all") {
                addQuotes = true;
            } else if (!addQuotes && options._QuoteStrings == "minimal"
                && (ustr.find('\"') != std::string::npos
                    || ustr.find(options._Delimiter) != std::string::npos)) {
                addQuotes = true;
            }
            column[j] = ustr;
        }

        if (addQuotes) {
            OMP_PARALLEL_FOR_LOOP(column.size())
            for (ompIndexType k = 0; k < (ompIndexType)column.size(); ++k) {
                column[k] = formatStringValueQuoteString(column[k]);
            }
        }

    } else {
        processCharacterMatrix(columnData, column, options);
    }
    writer.addColumn(formatStringValue(columnName, options), column);
}
//=============================================================================
void
TableWriter::processCharacterMatrix(
    const ArrayOf& columnData, std::vector<std::string>& column, const writeTableOptions& options)
{
    Dimensions dims = columnData.dimensions();
    indexType len = dims.elementCount() / dims.rows();
    auto* ptrChar = (charType*)columnData.dataPointer();
    std::wstring str;
    str.reserve(dims.elementCount());

    column.resize(dims.rows());
    for (ompIndexType i = 0; i < (ompIndexType)dims.rows(); i++) {
        for (indexType j = 0; j < len; j++) {
            size_t idx = i + j * dims.rows();
            if (ptrChar[idx] != 0) {
                str.push_back(ptrChar[idx]);
            }
        }
        column[i] = wstring_to_utf8(str);
        str.clear();
    }
    bool addQuotes = false;

    for (auto col : column) {
        if (options._QuoteStrings == "all") {
            addQuotes = true;
            break;
        } else if (options._QuoteStrings == "minimal"
            && (col.find('\"') != std::string::npos
                || col.find(options._Delimiter) != std::string::npos)) {
            addQuotes = true;
            break;
        }
    }

    if (addQuotes) {
        OMP_PARALLEL_FOR_LOOP(column.size())
        for (ompIndexType k = 0; k < (ompIndexType)column.size(); ++k) {
            column[k] = formatStringValueQuoteString(column[k]);
        }
    }
}
//=============================================================================
void
TableWriter::writeRowNames(
    ITableWriter& writer, const wstringVector& rowNames, const writeTableOptions& options)
{
    std::vector<std::string> column;
    for (const auto& name : rowNames) {
        std::string ustr = wstring_to_utf8(name);
        column.push_back(formatStringValue(ustr, options));
    }
    writer.addColumn("Row", column);
}
//=============================================================================
void
TableWriter::writeTableToFile(const ArrayOf& table, const std::wstring& filename,
    const writeTableOptions& options, std::wstring& errorMessage)
{
    ArrayOf columns = table.tableData();
    stringVector columnNames = columns.fieldNames();

    ArrayOf propertiesArrayOf = table.tableProperties();
    ArrayOf rowNamesArrayOf = propertiesArrayOf.field("RowNames");
    wstringVector rowNames = rowNamesArrayOf.asWideStringVector();

    bool withRowNames = !rowNames.empty() && options._WriteRowNames;
    // A table without variables writes no rows: only the header of its row names.
    if (columnNames.empty()) {
        rowNames.clear();
    }

    std::unique_ptr<ITableWriter> writer;
    if (options._FileType == "text") {
        writer = std::make_unique<CSVWriter>(filename, options._Delimiter,
            options._WriteVariableNames, options._WriteMode == "append");
    } else {
        writer = std::make_unique<XMLWriter>(filename, options._TableNodeName, options._RowNodeName,
            options._WriteRowNames, options._AttributeSuffix);
    }

    if (withRowNames) {
        writeRowNames(*writer, rowNames, options);
    }

    for (size_t k = 0; k < columnNames.size(); k++) {
        ArrayOf columnData = columns.field(columnNames[k]);

        switch (columnData.dataClass()) {
        case NLS_CELL_ARRAY: {
            bool isSupportedType = true;
            ArrayOf* elements = (ArrayOf*)columnData.dataPointer();
            for (size_t j = 0; isSupportedType && j < columnData.elementCount(); j++) {
                isSupportedType
                    = (elements[j].dataClass() == NLS_CHAR || elements[j].isScalarStringArray()
                          || elements[j].isNumeric() || elements[j].isLogical())
                    && (!elements[j].isSparse());
            }

            if (!isSupportedType) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:spreadsheet:cellTypeNotSupported", L"Type(s) in cell not supported.");
                return;
            }

            if (!columnData.is2D()) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:spreadsheet:matrix2DExpected", L"Matrix 2D expected.");
                return;
            }
            handleCellColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_STRING_ARRAY: {
            if (!columnData.is2D()) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:spreadsheet:matrix2DExpected", L"Matrix 2D expected.");
                return;
            }
            handleStringColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_LOGICAL: {
            if (!columnData.is2D()) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:spreadsheet:matrix2DExpected", L"Matrix 2D expected.");
                return;
            }
            if (columnData.isSparse()) {
                columnData.makeDense();
            }
            handleLogicalColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX: {
            if (!columnData.is2D()) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:spreadsheet:matrix2DExpected", L"Matrix 2D expected.");
                return;
            }
            if (columnData.isSingleClass()) {
                columnData.promoteType(NLS_DCOMPLEX);
            }
            if (columnData.isSparse()) {
                columnData.makeDense();
            }
            handleDoubleComplexColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_INT8:
        case NLS_INT16:
        case NLS_INT32:
        case NLS_INT64:
        case NLS_UINT8:
        case NLS_UINT16:
        case NLS_UINT32:
        case NLS_UINT64:
        case NLS_SINGLE:
        case NLS_DOUBLE: {
            if (!columnData.is2D()) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:spreadsheet:matrix2DExpected", L"Matrix 2D expected.");
                return;
            }
            if (!columnData.isDoubleType()) {
                columnData.promoteType(NLS_DOUBLE);
            }
            if (columnData.isSparse()) {
                columnData.makeDense();
            }
            handleDoubleColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_CHAR: {
            if (!columnData.is2D()) {
                errorMessage = resolveMessageTemplate(
                    L"Nelson:spreadsheet:matrix2DExpected", L"Matrix 2D expected.");
                return;
            }
            handleCharacterColumn(*writer, columnData, columnNames[k], options);
        } break;
        default: {
            errorMessage = resolveMessageTemplate(
                L"Nelson:spreadsheet:typeNotSupported", L"Type not supported.");
            return;
        } break;
        }
    }
    writer->writeToFile(errorMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
