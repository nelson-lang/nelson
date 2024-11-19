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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <numeric>
#include "TableWriter.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
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
    if (std::floor(value) == value) {
        // It's an integer value
        return fmt::format("{}", (long long)value);
    } else {
        // It's a floating point value
        return fmt::format("{:.6g}", value);
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
    ArrayOf* elements = (ArrayOf*)columnData.getDataPointer();
    std::vector<std::string> column;
    bool addQuotes = false;
    for (size_t j = 0; j < columnData.getElementCount(); j++) {
        if (elements[j].getDataClass() == NLS_CHAR) {
            std::string v = elements[j].getContentAsCString();
            if (!addQuotes && options._QuoteStrings == "all") {
                addQuotes = true;
            } else if (!addQuotes && options._QuoteStrings == "minimal"
                && (v.find('\"') != std::string::npos
                    || v.find(options._Delimiter) != std::string::npos)) {
                addQuotes = true;
            }
            column.push_back(v);
        } else {
            double v = elements[j].getContentAsDoubleScalar();
            column.push_back(doubleToString(v));
        }
    }
    if (addQuotes) {
        for (size_t k = 0; k < column.size(); ++k) {
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
    ArrayOf* values = (ArrayOf*)columnData.getDataPointer();
    Dimensions dimsColumn = columnData.getDimensions();

    bool isXml = options._FileType == "xml";
    bool addQuotes = false;

    if (dimsColumn.getColumns() != 1) {
        for (size_t c = 0; c < dimsColumn.getColumns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            for (size_t r = 0; r < dimsColumn.getRows(); r++) {
                ArrayOf value = values[r + c * dimsColumn.getRows()];
                if (value.isRowVectorCharacterArray()) {
                    std::string str = value.getContentAsCString();
                    if (!addQuotes && options._QuoteStrings == "all") {
                        addQuotes = true;
                    } else if (!addQuotes && options._QuoteStrings == "minimal"
                        && (str.find('\"') != std::string::npos
                            || str.find(options._Delimiter) != std::string::npos)) {
                        addQuotes = true;
                    }

                    column.push_back(str);
                } else {
                    column.push_back("");
                }
            }
            if (addQuotes) {
                for (size_t k = 0; k < column.size(); ++k) {
                    column[k] = formatStringValueQuoteString(column[k]);
                }
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        for (size_t j = 0; j < columnData.getElementCount(); j++) {
            ArrayOf value = values[j];
            if (value.isRowVectorCharacterArray()) {
                std::string str = value.getContentAsCString();
                if (!addQuotes && options._QuoteStrings == "all") {
                    addQuotes = true;
                } else if (!addQuotes && options._QuoteStrings == "minimal"
                    && (str.find('\"') != std::string::npos
                        || str.find(options._Delimiter) != std::string::npos)) {
                    addQuotes = true;
                }
                column.push_back(str);
            } else {
                column.push_back("");
            }
        }
        if (addQuotes) {
            for (size_t k = 0; k < column.size(); ++k) {
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
    std::complex<double>* values = (std::complex<double>*)columnData.getDataPointer();
    Dimensions dimsColumn = columnData.getDimensions();
    bool isXml = options._FileType == "xml";

    if (dimsColumn.getColumns() != 1) {
        for (size_t c = 0; c < dimsColumn.getColumns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            for (size_t r = 0; r < dimsColumn.getRows(); r++) {
                column.push_back(
                    doubleComplexToString(values[r + c * dimsColumn.getRows()], isXml));
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        for (size_t j = 0; j < columnData.getElementCount(); j++) {
            column.push_back(doubleComplexToString(values[j], isXml));
        }
        writer.addColumn(formatStringValue(columnName, options), column);
    }
}
//=============================================================================
void
TableWriter::handleDoubleColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    double* values = (double*)columnData.getDataPointer();
    Dimensions dimsColumn = columnData.getDimensions();
    bool isXml = options._FileType == "xml";

    if (dimsColumn.getColumns() != 1) {
        for (size_t c = 0; c < dimsColumn.getColumns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            for (size_t r = 0; r < dimsColumn.getRows(); r++) {
                column.push_back(doubleToString(values[r + c * dimsColumn.getRows()]));
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        for (size_t j = 0; j < columnData.getElementCount(); j++) {
            column.push_back(doubleToString(values[j]));
        }
        writer.addColumn(formatStringValue(columnName, options), column);
    }
}
//=============================================================================
void
TableWriter::handleLogicalColumn(ITableWriter& writer, const ArrayOf& columnData,
    const std::string& columnName, const writeTableOptions& options)
{
    logical* values = (logical*)columnData.getDataPointer();
    Dimensions dimsColumn = columnData.getDimensions();

    bool isXml = options._FileType == "xml";

    if (dimsColumn.getColumns() != 1) {
        for (size_t c = 0; c < dimsColumn.getColumns(); c++) {
            std::string colName
                = isXml ? columnName : columnName + "_" + std::to_string((int)c + 1);
            std::vector<std::string> column;
            for (size_t r = 0; r < dimsColumn.getRows(); r++) {
                logical value = values[r + c * dimsColumn.getRows()];
                if (options._FileType == "xml") {
                    column.push_back(value ? "true" : "false");
                } else {
                    column.push_back(value ? "1" : "0");
                }
            }
            writer.addColumn(formatStringValue(colName, options), column);
        }
    } else {
        std::vector<std::string> column;
        for (size_t j = 0; j < columnData.getElementCount(); j++) {
            logical value = values[j];
            if (options._FileType == "xml") {
                column.push_back(value ? "true" : "false");
            } else {
                column.push_back(value ? "1" : "0");
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
        charType* values = (charType*)columnData.getDataPointer();
        bool addQuotes = false;
        for (size_t j = 0; j < columnData.getElementCount(); j++) {
            std::wstring str(1, values[j]);
            std::string ustr = wstring_to_utf8(str);
            if (!addQuotes && options._QuoteStrings == "all") {
                addQuotes = true;
            } else if (!addQuotes && options._QuoteStrings == "minimal"
                && (ustr.find('\"') != std::string::npos
                    || ustr.find(options._Delimiter) != std::string::npos)) {
                addQuotes = true;
            }
            column.push_back(ustr);
        }

        if (addQuotes) {
            for (size_t k = 0; k < column.size(); ++k) {
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
    Dimensions dims = columnData.getDimensions();
    indexType len = dims.getElementCount() / dims.getRows();
    auto* ptrChar = (charType*)columnData.getDataPointer();
    std::wstring str;
    str.reserve(dims.getElementCount());

    for (indexType i = 0; i < dims.getRows(); i++) {
        for (indexType j = 0; j < len; j++) {
            size_t idx = i + j * dims.getRows();
            if (ptrChar[idx] != 0) {
                str.push_back(ptrChar[idx]);
            }
        }
        column.push_back(wstring_to_utf8(str));
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
        for (size_t k = 0; k < column.size(); ++k) {
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
    ArrayOf columns = table.getField("data");
    stringVector columnNames = columns.getFieldNames();

    ArrayOf propertiesArrayOf = table.getField("Properties");
    ArrayOf rowNamesArrayOf = propertiesArrayOf.getField("RowNames");
    wstringVector rowNames = rowNamesArrayOf.getContentAsWideStringVector();

    ITableWriter* writer;
    if (options._FileType == "text") {
        writer = new CSVWriter(filename, options._Delimiter, options._WriteVariableNames,
            options._WriteMode == "append");
    } else {
        writer = new XMLWriter(filename, options._TableNodeName, options._RowNodeName,
            options._WriteRowNames, options._AttributeSuffix);
    }

    if (!rowNames.empty() && options._WriteRowNames) {
        writeRowNames(*writer, rowNames, options);
    }

    for (size_t k = 0; k < columnNames.size(); k++) {
        ArrayOf columnData = columns.getField(columnNames[k]);

        switch (columnData.getDataClass()) {
        case NLS_CELL_ARRAY: {
            handleCellColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_STRING_ARRAY: {
            handleStringColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_LOGICAL: {
            if (columnData.isSparse()) {
                columnData.makeDense();
            }
            handleLogicalColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_SCOMPLEX:
        case NLS_DCOMPLEX: {
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
            if (!columnData.isDoubleType()) {
                columnData.promoteType(NLS_DOUBLE);
            }
            if (columnData.isSparse()) {
                columnData.makeDense();
            }
            handleDoubleColumn(*writer, columnData, columnNames[k], options);
        } break;
        case NLS_CHAR: {
            handleCharacterColumn(*writer, columnData, columnNames[k], options);
        } break;
        default: {
            delete writer;
            errorMessage = _W("Type not managed.");
            return;
        } break;
        }
    }
    writer->writeToFile(errorMessage);
    delete writer;
}
//=============================================================================
}
//=============================================================================
