//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ExcelWorkbookIO.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cwctype>
#include <fstream>
#include <limits>
#include <sstream>
#include <xlnt/xlnt.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    struct CellRange
    {
        indexType firstRow = 1;
        indexType firstColumn = 1;
        indexType lastRow = 0;
        indexType lastColumn = 0;
    };
    //=============================================================================
    static std::wstring
    lowerWide(std::wstring value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](wchar_t c) { return (wchar_t)std::towlower(c); });
        return value;
    }
    //=============================================================================
    static std::wstring
    fileExtension(const std::wstring& filename)
    {
        size_t dot = filename.find_last_of(L'.');
        if (dot == std::wstring::npos) {
            return L"";
        }
        return lowerWide(filename.substr(dot));
    }
    //=============================================================================
    static bool
    fileExists(const std::wstring& filename)
    {
        std::ifstream stream(wstring_to_utf8(filename), std::ios::binary);
        return stream.good();
    }
    //=============================================================================
    static void
    validateWorkbookFilename(const std::wstring& filename)
    {
        if (!isExcelWorkbookExtension(filename)) {
            throw std::runtime_error("Only .xlsx spreadsheet files are supported by this backend.");
        }
    }
    //=============================================================================
    static indexType
    columnNameToIndex(const std::wstring& text)
    {
        if (text.empty()) {
            throw std::runtime_error("Invalid range.");
        }
        indexType value = 0;
        for (wchar_t c : text) {
            wchar_t upper = (wchar_t)std::towupper(c);
            if (upper < L'A' || upper > L'Z') {
                throw std::runtime_error("Invalid range.");
            }
            value = value * 26 + (indexType)(upper - L'A' + 1);
        }
        return value;
    }
    //=============================================================================
    static void
    parseCellReference(const std::wstring& text, indexType& row, indexType& column)
    {
        std::wstring colText;
        std::wstring rowText;
        for (wchar_t c : text) {
            if (c == L'$') {
                continue;
            }
            if ((c >= L'A' && c <= L'Z') || (c >= L'a' && c <= L'z')) {
                if (!rowText.empty()) {
                    throw std::runtime_error("Invalid range.");
                }
                colText.push_back(c);
            } else if (c >= L'0' && c <= L'9') {
                rowText.push_back(c);
            } else {
                throw std::runtime_error("Invalid range.");
            }
        }
        if (colText.empty() || rowText.empty()) {
            throw std::runtime_error("Invalid range.");
        }
        column = columnNameToIndex(colText);
        row = (indexType)std::stoull(rowText);
        if (row == 0 || column == 0) {
            throw std::runtime_error("Invalid range.");
        }
    }
    //=============================================================================
    static CellRange
    usedRange(const xlnt::worksheet& sheet)
    {
        CellRange range;
        if (sheet.is_empty()) {
            range.lastRow = 0;
            range.lastColumn = 0;
            return range;
        }
        xlnt::range_reference dimension = sheet.calculate_dimension();
        range.firstRow = (indexType)dimension.top_left().row();
        range.firstColumn = (indexType)dimension.top_left().column_index();
        range.lastRow = (indexType)dimension.bottom_right().row();
        range.lastColumn = (indexType)dimension.bottom_right().column_index();
        return range;
    }
    //=============================================================================
    static CellRange
    resolveRange(const xlnt::worksheet& sheet, const excelWorkbookOptions& options)
    {
        CellRange range = usedRange(sheet);
        if (!options.hasRange || options.range.empty()) {
            return range;
        }

        size_t colon = options.range.find(L':');
        if (colon == std::wstring::npos) {
            indexType row = 1;
            indexType column = 1;
            parseCellReference(options.range, row, column);
            range.firstRow = row;
            range.firstColumn = column;
            if (range.lastRow < row) {
                range.lastRow = row;
            }
            if (range.lastColumn < column) {
                range.lastColumn = column;
            }
            return range;
        }

        parseCellReference(options.range.substr(0, colon), range.firstRow, range.firstColumn);
        parseCellReference(options.range.substr(colon + 1), range.lastRow, range.lastColumn);
        if (range.lastRow < range.firstRow || range.lastColumn < range.firstColumn) {
            throw std::runtime_error("Invalid range.");
        }
        return range;
    }
    //=============================================================================
    static xlnt::worksheet
    selectSheet(xlnt::workbook& workbook, const excelWorkbookOptions& options, bool createMissing)
    {
        if (options.hasSheet && options.sheetIsIndex) {
            if (options.sheetIndex < 1) {
                throw std::runtime_error("Sheet index must be positive.");
            }
            while (createMissing && (indexType)workbook.sheet_count() < options.sheetIndex) {
                workbook.create_sheet();
            }
            if ((indexType)workbook.sheet_count() < options.sheetIndex) {
                throw std::runtime_error("Sheet not found.");
            }
            return workbook.sheet_by_index((std::size_t)(options.sheetIndex - 1));
        }
        if (options.hasSheet && !options.sheetName.empty()) {
            std::string name = wstring_to_utf8(options.sheetName);
            if (!workbook.contains(name)) {
                if (!createMissing) {
                    throw std::runtime_error("Sheet not found.");
                }
                xlnt::worksheet sheet = workbook.create_sheet();
                sheet.title(name);
                return sheet;
            }
            return workbook.sheet_by_title(name);
        }
        return workbook.sheet_by_index(0);
    }
    //=============================================================================
    static ArrayOf
    cellValueToArrayOf(const xlnt::cell& cell, const excelWorkbookOptions& options)
    {
        if (!cell.has_value() || cell.data_type() == xlnt::cell_type::empty) {
            return ArrayOf::empty(0, 0);
        }
        switch (cell.data_type()) {
        case xlnt::cell_type::boolean:
            return ArrayOf::logicalScalar(cell.value<bool>());
        case xlnt::cell_type::number:
        case xlnt::cell_type::date:
            return ArrayOf::doubleScalar(cell.value<double>());
        case xlnt::cell_type::error:
            return ArrayOf::characterArray(cell.error());
        case xlnt::cell_type::inline_string:
        case xlnt::cell_type::shared_string:
        case xlnt::cell_type::formula_string:
        default: {
            std::string value = cell.to_string();
            if (options.textType == "string") {
                return ArrayOf::stringArray(value);
            }
            return ArrayOf::characterArray(value);
        } break;
        }
    }
    //=============================================================================
    static ArrayOf
    readCellRange(xlnt::worksheet& sheet, const excelWorkbookOptions& options)
    {
        CellRange range = resolveRange(sheet, options);
        if (range.lastRow == 0 || range.lastColumn == 0) {
            return ArrayOf(NLS_CELL_ARRAY, Dimensions(0, 0), nullptr);
        }
        indexType rows = range.lastRow - range.firstRow + 1;
        indexType columns = range.lastColumn - range.firstColumn + 1;
        Dimensions dims(rows, columns);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount());
        for (indexType c = 0; c < columns; ++c) {
            for (indexType r = 0; r < rows; ++r) {
                xlnt::cell_reference ref(
                    xlnt::column_t((xlnt::column_t::index_t)(range.firstColumn + c)),
                    (xlnt::row_t)(range.firstRow + r));
                indexType index = r + c * rows;
                if (sheet.has_cell(ref)) {
                    elements[index] = cellValueToArrayOf(sheet.cell(ref), options);
                } else {
                    elements[index] = ArrayOf::empty(0, 0);
                }
            }
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    //=============================================================================
    static bool
    isTextValue(const ArrayOf& value)
    {
        return value.isRowVectorCharacterArray() || value.isScalarStringArray();
    }
    //=============================================================================
    static bool
    isEmptyValue(const ArrayOf& value)
    {
        return value.isEmpty() || value.elementCount() == 0;
    }
    //=============================================================================
    static double
    scalarNumericValue(ArrayOf value)
    {
        if (value.isSparse()) {
            value.makeDense();
        }
        if (!value.isDoubleType()) {
            value.promoteType(NLS_DOUBLE);
        }
        return value.asDoubleScalar();
    }
    //=============================================================================
    static void
    setCellValue(xlnt::cell cell, const ArrayOf& value)
    {
        if (isEmptyValue(value)) {
            cell.clear_value();
        } else if (value.isLogical()) {
            cell.value(value.asLogicalScalar());
        } else if (value.isNumeric() && !value.isComplex() && value.isScalar()) {
            cell.value(scalarNumericValue(value));
        } else if (isTextValue(value)) {
            cell.value(wstring_to_utf8(value.asWideString()));
        } else {
            throw std::runtime_error("Type not supported for spreadsheet file.");
        }
    }
    //=============================================================================
    static void
    clearSheet(xlnt::worksheet& sheet)
    {
        CellRange range = usedRange(sheet);
        for (indexType c = range.firstColumn; c <= range.lastColumn; ++c) {
            for (indexType r = range.firstRow; r <= range.lastRow; ++r) {
                xlnt::cell_reference ref(
                    xlnt::column_t((xlnt::column_t::index_t)c), (xlnt::row_t)r);
                if (sheet.has_cell(ref)) {
                    sheet.clear_cell(ref);
                }
            }
        }
    }
    //=============================================================================
    static indexType
    appendStartRow(xlnt::worksheet& sheet)
    {
        CellRange range = usedRange(sheet);
        return range.lastRow == 0 ? 1 : range.lastRow + 1;
    }
    //=============================================================================
    static CellRange
    writeStartRange(xlnt::worksheet& sheet, const excelWorkbookOptions& options)
    {
        CellRange range;
        range.firstRow = 1;
        range.firstColumn = 1;
        if (options.writeMode == "append") {
            range.firstRow = appendStartRow(sheet);
            return range;
        }
        if (options.hasRange && !options.range.empty()) {
            size_t colon = options.range.find(L':');
            std::wstring first
                = colon == std::wstring::npos ? options.range : options.range.substr(0, colon);
            parseCellReference(first, range.firstRow, range.firstColumn);
        }
        return range;
    }
    //=============================================================================
    static xlnt::workbook
    openWorkbookForWrite(const std::wstring& filename, const excelWorkbookOptions& options)
    {
        if (fileExists(filename) && options.writeMode != "replacefile") {
            return xlnt::workbook(xlnt::path(wstring_to_utf8(filename)));
        }
        return xlnt::workbook();
    }
    //=============================================================================
    static void
    applyWriteMode(
        xlnt::workbook& workbook, xlnt::worksheet& sheet, const excelWorkbookOptions& options)
    {
        if (options.writeMode == "overwritesheet") {
            clearSheet(sheet);
        } else if (options.writeMode == "replacefile") {
            while (workbook.sheet_count() > 1) {
                xlnt::worksheet current = workbook.sheet_by_index(workbook.sheet_count() - 1);
                workbook.remove_sheet(current);
            }
            sheet = selectSheet(workbook, options, true);
            clearSheet(sheet);
        } else if (options.writeMode != "inplace" && options.writeMode != "append") {
            throw std::runtime_error("WriteMode not supported.");
        }
    }
    //=============================================================================
    static void
    saveWorkbook(const xlnt::workbook& workbook, const std::wstring& filename)
    {
        workbook.save(xlnt::path(wstring_to_utf8(filename)));
    }
    //=============================================================================
    static ArrayOf
    readWorkbookCells(const std::wstring& filename, const excelWorkbookOptions& options)
    {
        validateWorkbookFilename(filename);
        xlnt::workbook workbook(xlnt::path(wstring_to_utf8(filename)));
        xlnt::worksheet sheet = selectSheet(workbook, options, false);
        return readCellRange(sheet, options);
    }
    //=============================================================================
    static stringVector
    defaultVariableNames(indexType count)
    {
        stringVector names;
        names.reserve(count);
        for (indexType k = 0; k < count; ++k) {
            names.push_back("Var" + std::to_string((long long)k + 1));
        }
        return names;
    }
    //=============================================================================
    static bool
    isValidVariableNameText(const ArrayOf& value)
    {
        return value.isRowVectorCharacterArray() || value.isScalarStringArray();
    }
    //=============================================================================
    static bool
    isAsciiLetter(char c)
    {
        return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
    }
    //=============================================================================
    static bool
    isAsciiDigit(char c)
    {
        return c >= '0' && c <= '9';
    }
    //=============================================================================
    static std::string
    makeValidVariableName(const std::string& text, indexType index)
    {
        std::string name;
        bool capitalizeNext = false;
        for (char c : text) {
            if (isAsciiLetter(c) || isAsciiDigit(c) || c == '_') {
                if (name.empty() && isAsciiDigit(c)) {
                    name.push_back('x');
                }
                if (capitalizeNext && isAsciiLetter(c)) {
                    name.push_back((char)std::toupper((unsigned char)c));
                } else {
                    name.push_back(c);
                }
                capitalizeNext = false;
            } else if (c == '%') {
                if (name.empty()) {
                    name = "Var" + std::to_string((long long)index + 1);
                }
                if (name.back() != '_') {
                    name.push_back('_');
                }
                capitalizeNext = false;
            } else {
                capitalizeNext = !name.empty();
            }
        }
        if (name.empty()) {
            name = "Var" + std::to_string((long long)index + 1);
        }
        return name;
    }
    //=============================================================================
    static void
    makeNamesUnique(stringVector& names)
    {
        for (indexType k = 0; k < (indexType)names.size(); ++k) {
            std::string base = names[k];
            indexType suffix = 1;
            bool unique = false;
            while (!unique) {
                unique = true;
                for (indexType j = 0; j < k; ++j) {
                    if (names[j] == names[k]) {
                        unique = false;
                        break;
                    }
                }
                if (!unique) {
                    names[k] = base + "_" + std::to_string((long long)suffix);
                    ++suffix;
                }
            }
        }
    }
    //=============================================================================
    static bool
    isScalarNumericValue(const ArrayOf& value)
    {
        return value.isNumeric() && !value.isComplex() && value.isScalar();
    }
    //=============================================================================
    static bool
    isTextCellValue(const ArrayOf& value)
    {
        return isTextValue(value);
    }
    //=============================================================================
    static ArrayOf
    numericColumnFromCellRange(
        ArrayOf* rawElements, indexType rows, indexType column, indexType firstRow, indexType count)
    {
        double* data = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, count);
        for (indexType r = 0; r < count; ++r) {
            ArrayOf value = rawElements[(firstRow - 1 + r) + column * rows];
            data[r] = isEmptyValue(value) ? std::nan("") : scalarNumericValue(value);
        }
        return ArrayOf(NLS_DOUBLE, Dimensions(count, 1), data);
    }
    //=============================================================================
    static ArrayOf
    logicalColumnFromCellRange(
        ArrayOf* rawElements, indexType rows, indexType column, indexType firstRow, indexType count)
    {
        logical* data = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, count);
        for (indexType r = 0; r < count; ++r) {
            ArrayOf value = rawElements[(firstRow - 1 + r) + column * rows];
            data[r] = (!isEmptyValue(value) && value.asLogicalScalar()) ? 1 : 0;
        }
        return ArrayOf(NLS_LOGICAL, Dimensions(count, 1), data);
    }
    //=============================================================================
    static ArrayOf
    cellColumnFromCellRange(
        ArrayOf* rawElements, indexType rows, indexType column, indexType firstRow, indexType count)
    {
        Dimensions dims(count, 1);
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount());
        for (indexType r = 0; r < count; ++r) {
            elements[r] = rawElements[(firstRow - 1 + r) + column * rows];
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    //=============================================================================
    static ArrayOf
    columnFromCellRange(const ArrayOf& raw, indexType column, indexType firstRow)
    {
        indexType rows = raw.dimensions().rows();
        indexType count = rows >= firstRow ? rows - firstRow + 1 : 0;
        ArrayOf* rawElements = (ArrayOf*)raw.dataPointer();

        bool hasNumeric = false;
        bool hasLogical = false;
        bool hasText = false;
        bool hasOther = false;
        for (indexType r = 0; r < count; ++r) {
            ArrayOf value = rawElements[(firstRow - 1 + r) + column * rows];
            if (isEmptyValue(value)) {
                continue;
            }
            if (value.isLogical() && value.isScalar()) {
                hasLogical = true;
            } else if (isScalarNumericValue(value)) {
                hasNumeric = true;
            } else if (isTextCellValue(value)) {
                hasText = true;
            } else {
                hasOther = true;
            }
        }

        if (!hasText && !hasOther && hasNumeric && !hasLogical) {
            return numericColumnFromCellRange(rawElements, rows, column, firstRow, count);
        }
        if (!hasText && !hasOther && hasLogical && !hasNumeric) {
            return logicalColumnFromCellRange(rawElements, rows, column, firstRow, count);
        }
        return cellColumnFromCellRange(rawElements, rows, column, firstRow, count);
    }
    //=============================================================================
    static stringVector
    variableNamesFromFirstRow(const ArrayOf& raw)
    {
        indexType columns = raw.dimensions().columns();
        indexType rows = raw.dimensions().rows();
        stringVector names;
        names.reserve(columns);
        ArrayOf* elements = (ArrayOf*)raw.dataPointer();
        bool allText = rows > 0;
        for (indexType c = 0; c < columns; ++c) {
            allText = allText && isValidVariableNameText(elements[c * rows]);
        }
        if (!allText) {
            return defaultVariableNames(columns);
        }
        for (indexType c = 0; c < columns; ++c) {
            std::string name = elements[c * rows].asUtf8String();
            names.push_back(name.empty() ? ("Var" + std::to_string((long long)c + 1))
                                         : makeValidVariableName(name, c));
        }
        makeNamesUnique(names);
        return names;
    }
    //=============================================================================
    static ArrayOf
    readTableFromRaw(const ArrayOf& raw, const excelWorkbookOptions& options)
    {
        indexType columns = raw.dimensions().columns();
        indexType firstDataRow = options.readVariableNames ? 2 : 1;
        stringVector names = options.readVariableNames ? variableNamesFromFirstRow(raw)
                                                       : defaultVariableNames(columns);
        ArrayOfVector values;
        values.reserve(columns);
        for (indexType c = 0; c < columns; ++c) {
            values.push_back(columnFromCellRange(raw, c, firstDataRow));
        }
        return ArrayOf::table(values, names, stringVector {});
    }
    //=============================================================================
    static void
    writeArrayBlock(
        xlnt::worksheet& sheet, const ArrayOf& value, const CellRange& start, bool expandCellRows)
    {
        if (value.isCell()) {
            ArrayOf* elements = (ArrayOf*)value.dataPointer();
            Dimensions dims = value.dimensions();
            for (indexType c = 0; c < dims.columns(); ++c) {
                for (indexType r = 0; r < dims.rows(); ++r) {
                    indexType index = r + c * dims.rows();
                    xlnt::cell_reference ref(
                        xlnt::column_t((xlnt::column_t::index_t)(start.firstColumn + c)),
                        (xlnt::row_t)(start.firstRow + r));
                    setCellValue(sheet.cell(ref), elements[index]);
                }
            }
            return;
        }
        if (value.is2D()
            && (value.isNumeric() || value.isLogical() || value.isStringArray()
                || value.isCharacterArray())) {
            Dimensions dims = value.dimensions();
            if (value.isCharacterArray() && !value.isRowVectorCharacterArray()) {
                wstringVector rows = value.asWideStringVector();
                for (indexType r = 0; r < (indexType)rows.size(); ++r) {
                    xlnt::cell_reference ref(
                        xlnt::column_t((xlnt::column_t::index_t)start.firstColumn),
                        (xlnt::row_t)(start.firstRow + r));
                    sheet.cell(ref).value(wstring_to_utf8(rows[r]));
                }
                return;
            }
            ArrayOf copy = value;
            if (copy.isSparse()) {
                copy.makeDense();
            }
            for (indexType c = 0; c < dims.columns(); ++c) {
                for (indexType r = 0; r < dims.rows(); ++r) {
                    ArrayOf scalar = copy.valueAtIndex(r + c * dims.rows());
                    xlnt::cell_reference ref(
                        xlnt::column_t((xlnt::column_t::index_t)(start.firstColumn + c)),
                        (xlnt::row_t)(start.firstRow + r));
                    setCellValue(sheet.cell(ref), scalar);
                }
            }
            return;
        }
        if (!expandCellRows) {
            throw std::runtime_error("Type not supported for spreadsheet file.");
        }
        setCellValue(sheet.cell(xlnt::cell_reference(
                         xlnt::column_t((xlnt::column_t::index_t)start.firstColumn),
                         (xlnt::row_t)start.firstRow)),
            value);
    }
    //=============================================================================
    static void
    writeTableBlock(xlnt::worksheet& sheet, const ArrayOf& table, const CellRange& start,
        const excelWorkbookOptions& options)
    {
        stringVector names = table.tableVariableNames();
        indexType currentColumn = start.firstColumn;
        indexType startRow = start.firstRow;

        if (options.writeVariableNames) {
            for (size_t k = 0; k < names.size(); ++k) {
                xlnt::cell_reference ref(
                    xlnt::column_t((xlnt::column_t::index_t)(currentColumn + (indexType)k)),
                    (xlnt::row_t)startRow);
                sheet.cell(ref).value(names[k]);
            }
            ++startRow;
        }

        for (size_t k = 0; k < names.size(); ++k) {
            ArrayOf column = table.tableColumn(names[k]);
            CellRange columnStart;
            columnStart.firstRow = startRow;
            columnStart.firstColumn = currentColumn + (indexType)k;
            writeArrayBlock(sheet, column, columnStart, true);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
isExcelWorkbookExtension(const std::wstring& filename)
{
    return fileExtension(filename) == L".xlsx";
}
//=============================================================================
ArrayOf
ReadExcelCell(
    const std::wstring& filename, const excelWorkbookOptions& options, std::wstring& errorMessage)
{
    try {
        if (options.useExcel) {
            errorMessage = L"UseExcel is not supported by this backend.";
            return {};
        }
        return readWorkbookCells(filename, options);
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    }
    return {};
}
//=============================================================================
ArrayOf
ReadExcelMatrix(const std::wstring& filename, const excelWorkbookOptions& options,
    NelsonType outputType, std::wstring& errorMessage)
{
    try {
        ArrayOf raw = readWorkbookCells(filename, options);
        Dimensions dims = raw.dimensions();
        ArrayOf* cells = (ArrayOf*)raw.dataPointer();
        if (outputType == NLS_CELL_ARRAY) {
            return raw;
        }
        if (outputType == NLS_STRING_ARRAY) {
            wstringVector text;
            text.reserve(dims.elementCount());
            for (indexType k = 0; k < dims.elementCount(); ++k) {
                if (isEmptyValue(cells[k])) {
                    text.push_back(L"");
                } else {
                    text.push_back(cells[k].asWideString());
                }
            }
            return ArrayOf::stringArray(text, dims);
        }
        double* values = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount());
        for (indexType k = 0; k < dims.elementCount(); ++k) {
            if (isEmptyValue(cells[k]) || !cells[k].isNumeric() || cells[k].isComplex()) {
                values[k] = std::numeric_limits<double>::quiet_NaN();
            } else {
                values[k] = scalarNumericValue(cells[k]);
            }
        }
        ArrayOf result(NLS_DOUBLE, dims, values);
        if (outputType != NLS_DOUBLE) {
            result.promoteType(outputType);
        }
        return result;
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    }
    return {};
}
//=============================================================================
ArrayOf
ReadExcelTable(
    const std::wstring& filename, const excelWorkbookOptions& options, std::wstring& errorMessage)
{
    try {
        return readTableFromRaw(readWorkbookCells(filename, options), options);
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    }
    return {};
}
//=============================================================================
bool
WriteExcelArray(const ArrayOf& value, const std::wstring& filename,
    const excelWorkbookOptions& options, std::wstring& errorMessage)
{
    try {
        validateWorkbookFilename(filename);
        if (options.useExcel) {
            errorMessage = L"UseExcel is not supported by this backend.";
            return false;
        }
        xlnt::workbook workbook = openWorkbookForWrite(filename, options);
        xlnt::worksheet sheet = selectSheet(workbook, options, true);
        applyWriteMode(workbook, sheet, options);
        writeArrayBlock(sheet, value, writeStartRange(sheet, options), false);
        saveWorkbook(workbook, filename);
        return true;
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    }
    return false;
}
//=============================================================================
bool
WriteExcelTable(const ArrayOf& table, const std::wstring& filename,
    const excelWorkbookOptions& options, std::wstring& errorMessage)
{
    try {
        validateWorkbookFilename(filename);
        if (options.useExcel) {
            errorMessage = L"UseExcel is not supported by this backend.";
            return false;
        }
        xlnt::workbook workbook = openWorkbookForWrite(filename, options);
        xlnt::worksheet sheet = selectSheet(workbook, options, true);
        applyWriteMode(workbook, sheet, options);
        writeTableBlock(sheet, table, writeStartRange(sheet, options), options);
        saveWorkbook(workbook, filename);
        return true;
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    }
    return false;
}
//=============================================================================
bool
ExcelWorkbookInfo(const std::wstring& filename, stringVector& sheetNames, std::wstring& format,
    std::wstring& errorMessage)
{
    try {
        validateWorkbookFilename(filename);
        xlnt::workbook workbook(xlnt::path(wstring_to_utf8(filename)));
        sheetNames = workbook.sheet_titles();
        format = L"Open XML Spreadsheet";
        return true;
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
