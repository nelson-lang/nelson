//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include <algorithm>
#include <cctype>
#include "readmatrixBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ReadMatrix.hpp"
#include "ExcelWorkbookIO.hpp"
#include "DetectImportOptions.hpp"
#include "DelimitedTextImportOptionsHelpers.hpp"
#include "SpreadsheetTableIOHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static NelsonType
convertToNelsonType(const std::wstring& typeStr);
//=============================================================================
static std::string
lowerString(std::string value);
//=============================================================================
static void
parseSheetOption(const ArrayOf& value, excelWorkbookOptions& options);
//=============================================================================
static bool
tryParseExcelReadMatrixOptions(const ArrayOfVector& argIn, excelWorkbookOptions& options,
    NelsonType& outputType, std::wstring& errorMessage);
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::readmatrixBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 12);
    std::wstring filename = argIn[0].asWideString();
    std::string errorMessage;
    detectImportOptions options;
    NelsonType OutputType = NLS_DOUBLE;
    initializeDetectImportOptions(options);

    excelWorkbookOptions excelOptions;
    std::wstring excelError;
    if (tryParseExcelReadMatrixOptions(argIn, excelOptions, OutputType, excelError)) {
        retval << ReadExcelMatrix(filename, excelOptions, OutputType, excelError);
        if (!excelError.empty()) {
            Error(L"Nelson:readmatrix:excelError", excelError);
        }
        return retval;
    }
    if (!excelError.empty()) {
        Error(L"Nelson:readmatrix:excelError", excelError);
    }
    if (argIn.size() > 4) {
        Error(L"Nelson:spreadsheet:propertyNotSupported", L"Property name not supported.");
    }

    switch (argIn.size()) {
    case 1: {
        // readmatrix(filename)
        analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
        options.CommentStyle.clear();
        if (!errorMessage.empty()) {
            Error("Nelson:readmatrix:readFailed", errorMessage);
        }
    } break;
    case 2: {
        // readmatrix(filename, options)
        if (isDelimitedTextImportOptions(argIn[1])) {
            populateImportOptions(argIn[1], options);
        } else {
            Error(L"Nelson:spreadsheet:importOptionsExpected", L"Import options object expected.");
        }
    } break;
    case 3: {
        // readmatrix(filename, 'fieldname', fieldvalue)
        std::wstring fieldname = argIn[1].asWideString();
        if (fieldname != L"OutputType") {
            Error(L"Nelson:readmatrix:outputTypeExpected", L"OutputType name expected.");
        }
        OutputType = convertToNelsonType(argIn[2].asWideString());
        analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
        options.CommentStyle.clear();
        if (!errorMessage.empty()) {
            Error("Nelson:readmatrix:readFailed", errorMessage);
        }
    } break;
    case 4: {
        // readmatrix(filename, options, 'fieldname', fieldvalue)
        if (isDelimitedTextImportOptions(argIn[1])) {
            populateImportOptions(argIn[1], options);
        } else {
            Error(L"Nelson:spreadsheet:importOptionsExpected", L"Import options object expected.");
        }
        std::wstring fieldname = argIn[2].asWideString();
        if (fieldname != L"OutputType") {
            Error(L"Nelson:readmatrix:outputTypeExpected", L"OutputType name expected.");
        }
        OutputType = convertToNelsonType(argIn[3].asWideString());
    } break;
    default: {
    } break;
    }

    retval << ReadMatrix(filename, options, OutputType, errorMessage);
    if (!errorMessage.empty()) {
        Error("Nelson:readmatrix:readFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
NelsonType
convertToNelsonType(const std::wstring& typeStr)
{
    static const std::map<std::wstring, NelsonType> typeMap = { { L"double", NLS_DOUBLE },
        { L"single", NLS_SINGLE }, { L"string", NLS_STRING_ARRAY }, { L"char", NLS_CELL_ARRAY },
        { L"cell", NLS_CELL_ARRAY }, { L"int8", NLS_INT8 }, { L"int16", NLS_INT16 },
        { L"int32", NLS_INT32 }, { L"int64", NLS_INT64 }, { L"uint8", NLS_UINT8 },
        { L"uint16", NLS_UINT16 }, { L"uint32", NLS_UINT32 }, { L"uint64", NLS_UINT64 } };

    auto it = typeMap.find(typeStr);
    if (it != typeMap.end()) {
        return it->second;
    }

    Error(L"Nelson:readmatrix:unsupportedType", L"Type not supported.");
    return NLS_DOUBLE;
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
void
parseSheetOption(const ArrayOf& value, excelWorkbookOptions& options)
{
    options.hasSheet = true;
    if (value.isNumeric() && value.isScalar()) {
        options.sheetIndex = (indexType)value.asDoubleScalar();
        options.sheetIsIndex = true;
    } else {
        options.sheetName = value.asWideString();
        options.sheetIsIndex = false;
    }
}
//=============================================================================
bool
tryParseExcelReadMatrixOptions(const ArrayOfVector& argIn, excelWorkbookOptions& options,
    NelsonType& outputType, std::wstring& errorMessage)
{
    std::wstring filename = argIn[0].asWideString();
    bool spreadsheet = isExcelWorkbookExtension(filename);
    if (argIn.size() == 2 && SpreadsheetGateway::isDelimitedTextImportOptions(argIn[1])) {
        if (spreadsheet) {
            errorMessage = L"Import options object is not supported for spreadsheet files.";
        }
        return false;
    }
    if (argIn.size() == 1) {
        return spreadsheet;
    }
    if (((argIn.size() - 1) % 2) != 0) {
        return false;
    }
    for (size_t k = 1; k < argIn.size(); k += 2) {
        std::string name = normalizedNameValueName(argIn[k]);
        if (name == "filetype") {
            std::string value = lowerString(argIn[k + 1].asUtf8String());
            if (value == "spreadsheet") {
                spreadsheet = true;
            } else if (value == "text") {
                spreadsheet = false;
            } else {
                errorMessage = L"FileType not supported.";
                return false;
            }
        } else if (name == "sheet") {
            parseSheetOption(argIn[k + 1], options);
            spreadsheet = true;
        } else if (name == "range") {
            options.range = argIn[k + 1].asWideString();
            options.hasRange = true;
            spreadsheet = true;
        } else if (name == "useexcel") {
            options.useExcel = argIn[k + 1].asLogicalScalar();
            spreadsheet = true;
        } else if (name == "outputtype") {
            outputType = convertToNelsonType(argIn[k + 1].asWideString());
        } else if (name == "datetimetype" || name == "durationtype" || name == "numheaderlines"
            || name == "expectednumvariables" || name == "datelocale") {
            spreadsheet = true;
        } else if (name == "mergedcellcolumnrule" || name == "mergedcellrowrule"
            || name == "weboptions") {
            errorMessage = L"This spreadsheet option is not supported by this backend.";
            return false;
        } else {
            return false;
        }
    }
    return spreadsheet;
}
//=============================================================================
