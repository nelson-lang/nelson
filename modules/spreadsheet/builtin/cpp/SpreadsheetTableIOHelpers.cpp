//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "SpreadsheetTableIOHelpers.hpp"
#include "DelimitedTextImportOptionsHelpers.hpp"
#include "DateNumber.hpp"
#include "DateToFormattedString.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ExcelWorkbookIO.hpp"
#include "Evaluator.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
#include "i18n.hpp"
#include "ReadTable.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cctype>
#include <cwctype>
#include <cmath>
#include <iomanip>
#include <regex>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
validateDelimiter(const std::wstring& delimiter, writeTableOptions& options);
//=============================================================================
static std::string
lowerString(std::string value);
//=============================================================================
static std::wstring
lowerWideString(std::wstring value);
//=============================================================================
static bool
isSpreadsheetFileType(const std::wstring& filename, const std::string& fileType);
//=============================================================================
static void
parseExcelSheetOption(const ArrayOf& value, excelWorkbookOptions& options);
//=============================================================================
static void
parseExcelReadOptions(const std::wstring& filename, const ArrayOfVector& argIn, size_t first,
    excelWorkbookOptions& options, std::string& fileType, size_t& firstUnknown);
//=============================================================================
static std::wstring
numericRangeToRangeString(const ArrayOf& value);
//=============================================================================
static bool
parseDurationText(const std::wstring& value, double& seconds);
//=============================================================================
static bool
parseCommonDatetimeText(const std::wstring& value, double& serial);
//=============================================================================
static bool
looksLikeDatetimeText(const std::wstring& value);
//=============================================================================
static ArrayOf
normalizeImportedTimeTextColumn(
    const ArrayOf& column, const std::wstring& inputFormat, bool& columnChanged);
//=============================================================================
static std::wstring
formatDuration(double totalSeconds);
//=============================================================================
static ArrayOf
datetimeToText(const ArrayOf& value);
//=============================================================================
static ArrayOf
durationToText(const ArrayOf& value);
//=============================================================================
static ArrayOf
convertColumnToDeclaredType(Evaluator* eval, const ArrayOf& column, const std::string& declaredType,
    const std::wstring& inputFormat);
//=============================================================================
static bool
hasExplicitVariableTypes(const detectImportOptions& options);
//=============================================================================
static ArrayOf
applyImportOptionsColumnTypes(
    Evaluator* eval, const ArrayOf& table, const detectImportOptions& options);
//=============================================================================
std::string
normalizedNameValueName(const ArrayOf& name)
{
    bool isName = name.isScalarStringArray() || name.isRowVectorCharacterArray();
    if (!isName) {
        Error(L"Nelson:spreadsheet:invalidParameterName",
            L"Invalid parameter name. The parameter name must be a nonempty string or character "
            L"array.");
    }
    std::string value = wstring_to_utf8(name.asWideString());
    std::transform(
        value.begin(), value.end(), value.begin(), [](unsigned char c) { return std::tolower(c); });
    return value;
}
//=============================================================================
ArrayOf
readTableWithImportOptions(Evaluator* eval, const std::wstring& filename,
    const ArrayOfVector& argIn, size_t optionsIndex, size_t& firstNameValue)
{
    excelWorkbookOptions excelOptions;
    std::string fileType;
    parseExcelReadOptions(filename, argIn, optionsIndex, excelOptions, fileType, firstNameValue);
    if (isSpreadsheetFileType(filename, fileType)) {
        bool hasImportOptions = argIn.size() > optionsIndex
            && SpreadsheetGateway::isImportOptions(argIn[optionsIndex]);
        detectImportOptions options;
        initializeDetectImportOptions(options);
        if (hasImportOptions) {
            SpreadsheetGateway::populateVariableImportOptions(argIn[optionsIndex], options);
        }
        std::wstring errorMessage;
        ArrayOf table = ReadExcelTable(filename, excelOptions, errorMessage);
        if (!errorMessage.empty()) {
            Error(L"Nelson:spreadsheet:tableIOFailed", errorMessage);
        }
        if (!hasImportOptions) {
            return table;
        }
        if (!options.VariableNames.empty()) {
            stringVector current = table.tableVariableNames();
            if (current.size() == options.VariableNames.size()) {
                ArrayOfVector columns;
                columns.reserve(current.size());
                for (const std::string& name : current) {
                    columns.push_back(table.tableColumn(name));
                }
                table = ArrayOf::table(columns, options.VariableNames, table.tableRowNames());
            }
        }
        return applyImportOptionsColumnTypes(eval, table, options);
    }

    // 'Sheet' only applies to spreadsheet files; delimited text rejects it.
    if (excelOptions.hasSheet) {
        Error(L"Nelson:spreadsheet:parameterNotSupported",
            L"Unsupported parameter 'Sheet' for FileType 'text'.");
    }

    std::string errorMessage;
    detectImportOptions options;
    initializeDetectImportOptions(options);
    if (argIn.size() > optionsIndex
        && SpreadsheetGateway::isDelimitedTextImportOptions(argIn[optionsIndex])) {
        SpreadsheetGateway::populateImportOptions(argIn[optionsIndex], options);
        firstNameValue = optionsIndex + 1;
    } else {
        analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
        if (!errorMessage.empty()) {
            Error("Nelson:spreadsheet:tableIOFailed", errorMessage);
        }
    }

    if (!excelOptions.textType.empty()) {
        options.TextType = excelOptions.textType;
    }

    if (excelOptions.hasRange && !excelOptions.range.empty()) {
        int readVariableNames = -1;
        if (excelOptions.readVariableNamesSet) {
            readVariableNames = excelOptions.readVariableNames ? 1 : 0;
        }
        ArrayOf ranged = ReadTableRange(
            filename, options, excelOptions.range, readVariableNames, errorMessage);
        if (!errorMessage.empty()) {
            Error("Nelson:spreadsheet:tableIOFailed", errorMessage);
        }
        return normalizeImportedTimeTextColumns(ranged);
    }

    ArrayOf table = ReadTable(filename, options, errorMessage);
    if (!errorMessage.empty()) {
        Error("Nelson:spreadsheet:tableIOFailed", errorMessage);
    }
    return applyImportOptionsColumnTypes(eval, table, options);
}
//=============================================================================
bool
hasExplicitVariableTypes(const detectImportOptions& options)
{
    for (const std::string& type : options.VariableTypes) {
        std::string lowered = lowerString(type);
        if (!lowered.empty() && lowered != "char" && lowered != "auto") {
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOf
convertColumnToDeclaredType(Evaluator* eval, const ArrayOf& column, const std::string& declaredType,
    const std::wstring& inputFormat)
{
    std::string type = lowerString(declaredType);
    if (type == "string" && column.isStringArray()) {
        // Already a string array from the delimited-text reader.
        return column;
    }
    Context* context = eval->getContext();
    FunctionDef* functionDef = nullptr;
    if (!context->lookupFunction(type, functionDef)) {
        Error(L"Nelson:spreadsheet:unknownVariableType",
            L"Unable to convert a column to type '" + utf8_to_wstring(type) + L"'.");
    }
    ArrayOfVector inputs;
    inputs.push_back(column);
    if (type == "datetime") {
        if (column.isNumeric()) {
            // Numeric spreadsheet dates are serial values on the 1900 epoch.
            inputs.push_back(ArrayOf::characterArray("ConvertFrom"));
            inputs.push_back(ArrayOf::characterArray("excel"));
        } else if (!inputFormat.empty()) {
            inputs.push_back(ArrayOf::characterArray("InputFormat"));
            inputs.push_back(ArrayOf::characterArray(wstring_to_utf8(inputFormat)));
        }
    }
    ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, 1);
    if (result.empty()) {
        Error(L"Nelson:spreadsheet:variableTypeConversionFailed",
            L"Unable to convert a column to its declared type.");
    }
    return result[0];
}
//=============================================================================
ArrayOf
applyImportOptionsColumnTypes(
    Evaluator* eval, const ArrayOf& table, const detectImportOptions& options)
{
    if (!hasExplicitVariableTypes(options)) {
        // No explicit non-char type declared: keep automatic detection.
        return normalizeImportedTimeTextColumns(table);
    }
    stringVector names = table.tableVariableNames();
    stringVector rowNames = table.tableRowNames();
    ArrayOfVector values;
    values.reserve(names.size());
    for (size_t i = 0; i < names.size(); ++i) {
        ArrayOf column = table.tableColumn(names[i]);
        std::string declaredType
            = (i < options.VariableTypes.size()) ? options.VariableTypes[i] : "";
        std::string lowered = lowerString(declaredType);
        if (lowered.empty() || lowered == "char" || lowered == "auto") {
            bool columnChanged = false;
            column = normalizeImportedTimeTextColumn(column, L"", columnChanged);
        } else {
            std::wstring inputFormat = (i < options.VariableInputFormats.size())
                ? utf8_to_wstring(options.VariableInputFormats[i])
                : L"";
            column = convertColumnToDeclaredType(eval, column, declaredType, inputFormat);
        }
        values.push_back(column);
    }
    return ArrayOf::table(values, names, rowNames);
}
//=============================================================================
ArrayOf
normalizeImportedTimeTextColumns(const ArrayOf& table, const std::wstring& inputFormat)
{
    stringVector names = table.tableVariableNames();
    ArrayOfVector values;
    values.reserve(names.size());
    bool changed = false;
    for (const std::string& name : names) {
        ArrayOf column = table.tableColumn(name);
        bool columnChanged = false;
        ArrayOf normalized = normalizeImportedTimeTextColumn(column, inputFormat, columnChanged);
        changed = changed || columnChanged;
        values.push_back(normalized);
    }
    if (!changed) {
        return table;
    }
    return ArrayOf::table(values, names, stringVector {});
}
//=============================================================================
void
initializeWriteTableOptions(writeTableOptions& options)
{
    options._Delimiter = L'\0';
    options._FileType = "";
    options._WriteMode = "";
    options._QuoteStrings = "";
    options._WriteVariableNames = true;
    options._WriteRowNames = false;
    options._RowNodeName = "";
    options._TableNodeName = "";
    options._ExcelOptions = excelWorkbookOptions();
}
//=============================================================================
void
parseWriteTableOptions(const ArrayOfVector& argIn, size_t startIdx, writeTableOptions& options)
{
    if (((argIn.size() - startIdx) % 2) != 0) {
        Error(L"Nelson:spreadsheet:nameValueNotPaired", L"Name-value arguments must be paired.");
    }
    for (size_t k = startIdx; k < argIn.size(); k += 2) {
        std::string name = normalizedNameValueName(argIn[k]);
        if (name == "delimiter") {
            if (!validateDelimiter(argIn[k + 1].asWideString(), options)) {
                Error(L"Nelson:spreadsheet:delimiterNotSupported", L"Delimiter not supported.");
            }
        } else if (name == "writevariablenames") {
            options._WriteVariableNames = argIn[k + 1].asLogicalScalar();
            options._ExcelOptions.writeVariableNames = options._WriteVariableNames;
        } else if (name == "writerownames") {
            options._WriteRowNames = argIn[k + 1].asLogicalScalar();
            options._ExcelOptions.writeRowNames = options._WriteRowNames;
        } else if (name == "quotestrings") {
            std::wstring value = argIn[k + 1].asWideString();
            value = lowerWideString(value);
            if (value == L"all" || value == L"none" || value == L"minimal") {
                options._QuoteStrings = wstring_to_utf8(value);
            } else {
                Error(
                    L"Nelson:spreadsheet:quoteStringsNotSupported", L"QuoteStrings not supported.");
            }
        } else if (name == "filetype") {
            std::wstring value = argIn[k + 1].asWideString();
            value = lowerWideString(value);
            if (value == L"text" || value == L"xml" || value == L"spreadsheet") {
                options._FileType = wstring_to_utf8(value);
            } else {
                Error(L"Nelson:spreadsheet:fileTypeNotSupported", L"FileType not supported.");
            }
        } else if (name == "writemode") {
            std::wstring value = argIn[k + 1].asWideString();
            value = lowerWideString(value);
            if (value == L"append" || value == L"overwrite" || value == L"inplace"
                || value == L"overwritesheet" || value == L"replacefile") {
                options._WriteMode = wstring_to_utf8(value);
                options._ExcelOptions.writeMode
                    = options._WriteMode == "overwrite" ? "replacefile" : options._WriteMode;
            } else {
                Error(L"Nelson:spreadsheet:writeModeNotSupported", L"WriteMode not supported.");
            }
        } else if (name == "sheet") {
            parseExcelSheetOption(argIn[k + 1], options._ExcelOptions);
        } else if (name == "range") {
            options._ExcelOptions.range = argIn[k + 1].asWideString();
            options._ExcelOptions.hasRange = true;
        } else if (name == "useexcel") {
            options._ExcelOptions.useExcel = argIn[k + 1].asLogicalScalar();
        } else if (name == "autofitwidth" || name == "preserveformat") {
            Error(L"Nelson:spreadsheet:optionNotSupportedByBackend",
                L"This spreadsheet option is not supported by this backend.");
        } else if (name == "rownodename") {
            options._RowNodeName = argIn[k + 1].asUtf8String();
        } else if (name == "tablenodename") {
            options._TableNodeName = argIn[k + 1].asUtf8String();
        } else if (name == "attributesuffix") {
            options._AttributeSuffix = argIn[k + 1].asUtf8String();
        } else {
            Error(L"Nelson:spreadsheet:propertyNotSupported", L"Property name not supported.");
        }
    }
}
//=============================================================================
std::string
determineWriteFileType(const std::wstring& filename, const std::string& specifiedType)
{
    if (!specifiedType.empty()) {
        return specifiedType;
    }
    std::wstring extension
        = filename.rfind(L'.') != std::wstring::npos ? filename.substr(filename.rfind(L'.')) : L"";
    if (extension == L".xml") {
        return "xml";
    }
    if (extension == L".xlsx") {
        return "spreadsheet";
    }
    if (extension == L".txt" || extension == L".csv" || extension == L".dat") {
        return "text";
    }
    Error(L"Nelson:spreadsheet:unrecognizedFileExtension", L"Unrecognized file extension.");
    return "";
}
//=============================================================================
void
validateWriteTableOptions(writeTableOptions& options)
{
    if (options._FileType == "xml" && !options._QuoteStrings.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:quoteStringsNotAllowedXmlFileType",
            L"QuoteStrings not allowed for xml file type.");
    } else if (options._FileType == "spreadsheet" && !options._QuoteStrings.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:"
              L"quoteStringsNotAllowedSpreadsheetFileType",
            L"QuoteStrings not allowed for spreadsheet file type.");
    } else if (options._FileType == "text" && options._QuoteStrings.empty()) {
        options._QuoteStrings = "minimal";
    }
    if (options._FileType == "xml" && options._Delimiter != L'\0') {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType",
            L"Delimiter not allowed for xml file type.");
    } else if (options._FileType == "spreadsheet" && options._Delimiter != L'\0') {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:"
              L"delimiterNotAllowedSpreadsheetFileType",
            L"Delimiter not allowed for spreadsheet file type.");
    } else if (options._FileType == "text" && options._Delimiter == L'\0') {
        options._Delimiter = ',';
    }
    if (options._FileType == "xml" && !options._WriteMode.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:writeModeNotAllowedXmlFileType",
            L"WriteMode not allowed for xml file type.");
    } else if (options._FileType == "spreadsheet" && options._WriteMode.empty()) {
        options._WriteMode = "inplace";
        options._ExcelOptions.writeMode = "inplace";
    } else if (options._FileType == "text" && options._WriteMode.empty()) {
        options._WriteMode = "overwrite";
    }
    if (options._FileType == "text" && !options._AttributeSuffix.empty()) {
        Error(
            L"Nelson:spreadsheet:optionNotAllowedForFileType:attributeSuffixNotAllowedTextFileType",
            L"AttributeSuffix not allowed for text file type.");
    } else if (options._FileType == "spreadsheet" && !options._AttributeSuffix.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:"
              L"attributeSuffixNotAllowedSpreadsheetFileType",
            L"AttributeSuffix not allowed for spreadsheet file type.");
    } else if (options._FileType == "xml" && options._AttributeSuffix.empty()) {
        options._AttributeSuffix = "Attribute";
    }
    if (options._FileType == "text" && !options._RowNodeName.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:rowNodeNameAllowedOnlyXmlFileType",
            L"RowNodeName allowed only for xml file type.");
    } else if (options._FileType == "spreadsheet" && !options._RowNodeName.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:rowNodeNameAllowedOnlyXmlFileType",
            L"RowNodeName allowed only for xml file type.");
    } else if (options._FileType == "xml" && options._RowNodeName.empty()) {
        options._RowNodeName = "row";
    }
    if (options._FileType == "text" && !options._TableNodeName.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:tableNodeNameAllowedOnlyXmlFileType",
            L"TableNodeName allowed only for xml file type.");
    } else if (options._FileType == "spreadsheet" && !options._TableNodeName.empty()) {
        Error(L"Nelson:spreadsheet:optionNotAllowedForFileType:tableNodeNameAllowedOnlyXmlFileType",
            L"TableNodeName allowed only for xml file type.");
    } else if (options._FileType == "xml" && options._TableNodeName.empty()) {
        options._TableNodeName = "table";
    }
}
//=============================================================================
ArrayOf
makeDatetime(const std::vector<double>& serial)
{
    Dimensions dims(serial.size(), 1);
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, serial.size());
    std::copy(serial.begin(), serial.end(), ptr);
    ArrayOf values(NLS_DOUBLE, dims, ptr);
    stringVector names = { "SerialValue", "FormatValue", "TimeZoneValue" };
    ArrayOfVector fields;
    fields.push_back(values);
    fields.push_back(ArrayOf::characterArray("dd-MMM-yyyy"));
    fields.push_back(ArrayOf::characterArray(""));
    return ArrayOf::classdefArray("datetime", names, fields);
}
//=============================================================================
ArrayOf
makeDuration(const std::vector<double>& seconds)
{
    Dimensions dims(seconds.size(), 1);
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, seconds.size());
    std::copy(seconds.begin(), seconds.end(), ptr);
    ArrayOf values(NLS_DOUBLE, dims, ptr);
    stringVector names = { "SecondsValue", "FormatValue" };
    ArrayOfVector fields;
    fields.push_back(values);
    fields.push_back(ArrayOf::characterArray("hh:mm:ss"));
    return ArrayOf::classdefArray("duration", names, fields);
}
//=============================================================================
bool
cellText(const ArrayOf& column, wstringVector& text)
{
    if (column.isCell()) {
        auto* cells = (ArrayOf*)column.dataPointer();
        text.reserve(column.elementCount());
        for (indexType k = 0; k < column.elementCount(); ++k) {
            if (!cells[k].isRowVectorCharacterArray() && !cells[k].isScalarStringArray()) {
                return false;
            }
            text.push_back(cells[k].asWideString());
        }
        return true;
    }
    if (column.isStringArray() || column.isCellArrayOfCharacterVectors()) {
        text = column.asWideStringVector();
        return true;
    }
    return false;
}
//=============================================================================
bool
parseTextAsDatetime(const wstringVector& text, const std::wstring& inputFormat, ArrayOf& rowTimes)
{
    std::vector<double> serial(text.size());
    for (size_t k = 0; k < text.size(); ++k) {
        bool parsed = false;
        if (inputFormat.empty()) {
            if (!looksLikeDatetimeText(text[k])) {
                return false;
            }
            parsed = parseCommonDatetimeText(text[k], serial[k]);
            if (!parsed) {
                serial[k] = DateNumber(text[k], false, 0, parsed);
            }
        } else {
            serial[k] = DateNumber(text[k], inputFormat, false, 0, parsed);
        }
        if (!parsed) {
            return false;
        }
    }
    rowTimes = makeDatetime(serial);
    return true;
}
//=============================================================================
bool
parseTextAsDuration(const wstringVector& text, ArrayOf& rowTimes)
{
    std::vector<double> seconds(text.size());
    for (size_t k = 0; k < text.size(); ++k) {
        if (!parseDurationText(text[k], seconds[k])) {
            return false;
        }
    }
    rowTimes = makeDuration(seconds);
    return true;
}
//=============================================================================
bool
isTimeObject(const ArrayOf& value)
{
    return value.isClassdefType()
        && (value.classType() == "datetime" || value.classType() == "duration");
}
//=============================================================================
indexType
timeLength(const ArrayOf& value)
{
    if (value.isClassdefType() && value.classType() == "datetime") {
        return value.field("SerialValue").elementCount();
    }
    if (value.isClassdefType() && value.classType() == "duration") {
        return value.field("SecondsValue").elementCount();
    }
    return value.elementCount();
}
//=============================================================================
double
scalarTimeStorage(const ArrayOf& value, const std::string& field)
{
    ArrayOf storage = value.field(field);
    if (!storage.isScalar()) {
        Error(L"Nelson:spreadsheet:timeValueMustBeScalar", L"Time value must be scalar.");
    }
    return storage.asDoubleScalar();
}
//=============================================================================
ArrayOf
timeObjectToText(const ArrayOf& value)
{
    if (value.isClassdefType() && value.classType() == "datetime") {
        return datetimeToText(value);
    }
    if (value.isClassdefType() && value.classType() == "duration") {
        return durationToText(value);
    }
    return value;
}
//=============================================================================
bool
validateDelimiter(const std::wstring& delimiter, writeTableOptions& options)
{
    if (delimiter == L"comma" || delimiter == L",") {
        options._Delimiter = ',';
    } else if (delimiter == L"space" || delimiter == L" ") {
        options._Delimiter = ' ';
    } else if (delimiter == L"tab" || delimiter == L"\\t") {
        options._Delimiter = '\t';
    } else if (delimiter == L"semi" || delimiter == L";") {
        options._Delimiter = ';';
    } else if (delimiter == L"bar" || delimiter == L"|") {
        options._Delimiter = '|';
    } else {
        return false;
    }
    return true;
}
//=============================================================================
std::string
lowerString(std::string value)
{
    std::transform(
        value.begin(), value.end(), value.begin(), [](unsigned char c) { return std::tolower(c); });
    return value;
}
//=============================================================================
std::wstring
lowerWideString(std::wstring value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](wchar_t c) { return (wchar_t)std::towlower(c); });
    return value;
}
//=============================================================================
std::wstring
numericRangeToRangeString(const ArrayOf& value)
{
    ArrayOf numeric(value);
    if (!numeric.isDoubleType()) {
        numeric.promoteType(NLS_DOUBLE);
    }
    indexType n = numeric.elementCount();
    const double* p = (const double*)numeric.dataPointer();
    if ((n != 2 && n != 4) || p == nullptr) {
        Error(L"Nelson:spreadsheet:invalidRange", L"Invalid Range value.");
    }
    auto columnLetters = [](long long col) -> std::wstring {
        std::wstring letters;
        while (col > 0) {
            long long rem = (col - 1) % 26;
            letters.insert(letters.begin(), (wchar_t)(L'A' + rem));
            col = (col - 1) / 26;
        }
        return letters.empty() ? std::wstring(L"A") : letters;
    };
    auto cellRef = [&](long long row, long long col) -> std::wstring {
        return columnLetters(col) + std::to_wstring(row);
    };
    if (n == 4) {
        return cellRef((long long)p[0], (long long)p[1]) + L":"
            + cellRef((long long)p[2], (long long)p[3]);
    }
    return cellRef((long long)p[0], (long long)p[1]);
}
//=============================================================================
bool
isSpreadsheetFileType(const std::wstring& filename, const std::string& fileType)
{
    if (fileType == "spreadsheet") {
        return true;
    }
    std::wstring extension = filename.rfind(L'.') != std::wstring::npos
        ? lowerWideString(filename.substr(filename.rfind(L'.')))
        : L"";
    return extension == L".xlsx";
}
//=============================================================================
void
parseExcelSheetOption(const ArrayOf& value, excelWorkbookOptions& options)
{
    options.hasSheet = true;
    if (value.isNumeric() && value.isScalar()) {
        double sheet = value.asDoubleScalar();
        options.sheetIndex = (indexType)sheet;
        options.sheetIsIndex = true;
    } else {
        options.sheetName = value.asWideString();
        options.sheetIsIndex = false;
    }
}
//=============================================================================
void
parseExcelReadOptions(const std::wstring& filename, const ArrayOfVector& argIn, size_t first,
    excelWorkbookOptions& options, std::string& fileType, size_t& firstUnknown)
{
    firstUnknown = first;
    // An import-options object may precede the name-value pairs (e.g. 'UseExcel').
    size_t parseStart = first;
    if (argIn.size() > first && SpreadsheetGateway::isImportOptions(argIn[first])) {
        parseStart = first + 1;
        firstUnknown = parseStart;
    }
    if (((argIn.size() - parseStart) % 2) != 0) {
        return;
    }
    for (size_t k = parseStart; k < argIn.size(); k += 2) {
        std::string name = normalizedNameValueName(argIn[k]);
        if (name == "filetype") {
            fileType = lowerString(argIn[k + 1].asUtf8String());
        } else if (name == "sheet") {
            parseExcelSheetOption(argIn[k + 1], options);
        } else if (name == "range") {
            if (argIn[k + 1].isNumeric()) {
                options.range = numericRangeToRangeString(argIn[k + 1]);
            } else {
                options.range = argIn[k + 1].asWideString();
            }
            options.hasRange = true;
        } else if (name == "useexcel") {
            options.useExcel = argIn[k + 1].asLogicalScalar();
        } else if (name == "texttype") {
            options.textType = lowerString(argIn[k + 1].asUtf8String());
            if (options.textType != "char" && options.textType != "string") {
                Error(L"Nelson:spreadsheet:textTypeNotSupported", L"TextType not supported.");
            }
        } else if (name == "readvariablenames") {
            options.readVariableNames = argIn[k + 1].asLogicalScalar();
            options.readVariableNamesSet = true;
        } else if (name == "datetimetype" || name == "durationtype" || name == "numheaderlines"
            || name == "expectednumvariables" || name == "datelocale") {
            // Accepted for syntax compatibility; workbook values are kept as stored.
        } else if (name == "mergedcellcolumnrule" || name == "mergedcellrowrule"
            || name == "weboptions") {
            Error(L"Nelson:spreadsheet:optionNotSupportedByBackend",
                L"This spreadsheet option is not supported by this backend.");
        } else {
            firstUnknown = k;
            return;
        }
        firstUnknown = k + 2;
    }
}
//=============================================================================
bool
parseDurationText(const std::wstring& value, double& seconds)
{
    std::wregex rx(LR"(^\s*(-)?(\d+):(\d{1,2})(?::(\d{1,2}(?:\.\d+)?))?\s*$)");
    std::wsmatch m;
    if (!std::regex_match(value, m, rx)) {
        return false;
    }
    double first = std::stod(m[2].str());
    double second = std::stod(m[3].str());
    double third = m[4].matched ? std::stod(m[4].str()) : 0;
    seconds = m[4].matched ? first * 3600 + second * 60 + third : first * 60 + second;
    if (m[1].matched) {
        seconds = -seconds;
    }
    return true;
}
//=============================================================================
bool
parseCommonDatetimeText(const std::wstring& value, double& serial)
{
    std::wregex rx(
        LR"(^\s*(\d{4})-(\d{1,2})-(\d{1,2})(?:[ T](\d{1,2}):(\d{2})(?::(\d{2}(?:\.\d+)?))?)?\s*$)");
    std::wsmatch m;
    if (!std::regex_match(value, m, rx)) {
        return false;
    }
    int year = std::stoi(m[1].str());
    int month = std::stoi(m[2].str());
    int day = std::stoi(m[3].str());
    int hour = m[4].matched ? std::stoi(m[4].str()) : 0;
    int minute = m[5].matched ? std::stoi(m[5].str()) : 0;
    double second = m[6].matched ? std::stod(m[6].str()) : 0;
    serial = DateNumber(year, month, day, hour, minute, second);
    return true;
}
//=============================================================================
bool
looksLikeDatetimeText(const std::wstring& value)
{
    for (wchar_t c : value) {
        if (c == L'-' || c == L'/' || c == L'.' || c == L'T') {
            return true;
        }
        if ((c >= L'A' && c <= L'Z') || (c >= L'a' && c <= L'z')) {
            return true;
        }
    }
    return false;
}
//=============================================================================
ArrayOf
normalizeImportedTimeTextColumn(
    const ArrayOf& column, const std::wstring& inputFormat, bool& columnChanged)
{
    wstringVector text;
    if (!cellText(column, text)) {
        return column;
    }
    ArrayOf converted;
    if (parseTextAsDuration(text, converted)) {
        columnChanged = true;
        return converted;
    }
    if (parseTextAsDatetime(text, inputFormat, converted)) {
        columnChanged = true;
        return converted;
    }
    return column;
}
//=============================================================================
std::wstring
formatDuration(double totalSeconds)
{
    bool negative = totalSeconds < 0;
    totalSeconds = std::fabs(totalSeconds);
    long long whole = (long long)std::floor(totalSeconds);
    double fraction = totalSeconds - (double)whole;
    long long hours = whole / 3600;
    long long minutes = (whole % 3600) / 60;
    double seconds = (double)(whole % 60) + fraction;
    std::wstringstream ss;
    if (negative) {
        ss << L"-";
    }
    ss << std::setfill(L'0') << std::setw(2) << hours << L":" << std::setw(2) << minutes << L":";
    ss << std::fixed << std::setprecision(fraction == 0 ? 0 : 3) << std::setw(2) << seconds;
    return ss.str();
}
//=============================================================================
ArrayOf
datetimeToText(const ArrayOf& value)
{
    ArrayOf serial = value.field("SerialValue");
    auto* ptr = (double*)serial.dataPointer();
    std::vector<double> values(ptr, ptr + serial.elementCount());
    wstringVector text
        = dateToUserFormatString(values, serial.dimensions(), L"yyyy-mm-ddTHH:MM:SSZ", false);
    return ArrayOf::stringArray(text, serial.dimensions());
}
//=============================================================================
ArrayOf
durationToText(const ArrayOf& value)
{
    ArrayOf seconds = value.field("SecondsValue");
    auto* ptr = (double*)seconds.dataPointer();
    wstringVector text;
    text.reserve(seconds.elementCount());
    for (indexType k = 0; k < seconds.elementCount(); ++k) {
        text.push_back(formatDuration(ptr[k]));
    }
    return ArrayOf::stringArray(text, seconds.dimensions());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
