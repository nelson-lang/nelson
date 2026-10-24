//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "readcellBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ReadCell.hpp"
#include "ExcelWorkbookIO.hpp"
#include "DetectImportOptions.hpp"
#include "DelimitedTextImportOptionsHelpers.hpp"
#include "SpreadsheetTableIOHelpers.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
static std::string
lowerString(std::string value)
{
    std::transform(
        value.begin(), value.end(), value.begin(), [](unsigned char c) { return std::tolower(c); });
    return value;
}
//=============================================================================
static void
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
static bool
tryParseExcelReadCellOptions(
    const ArrayOfVector& argIn, excelWorkbookOptions& options, std::wstring& errorMessage)
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
        } else if (name == "texttype") {
            options.textType = lowerString(argIn[k + 1].asUtf8String());
            if (options.textType != "char" && options.textType != "string") {
                errorMessage = L"TextType not supported.";
                return false;
            }
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
} // namespace
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::readcellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 12);
    std::wstring filename = argIn[0].asWideString();
    std::string errorMessage;
    detectImportOptions options;

    initializeDetectImportOptions(options);

    excelWorkbookOptions excelOptions;
    std::wstring excelError;
    if (tryParseExcelReadCellOptions(argIn, excelOptions, excelError)) {
        retval << ReadExcelCell(filename, excelOptions, excelError);
        if (!excelError.empty()) {
            Error(L"Nelson:readcell:excelError", excelError);
        }
        return retval;
    }
    if (!excelError.empty()) {
        Error(L"Nelson:readcell:excelError", excelError);
    }
    if (argIn.size() > 2) {
        Error(L"Nelson:spreadsheet:propertyNotSupported", L"Property name not supported.");
    }

    if (argIn.size() > 1) {
        if (!isDelimitedTextImportOptions(argIn[1])) {
            Error(L"Nelson:spreadsheet:importOptionsExpected", L"Import options object expected.");
        }
        populateImportOptions(argIn[1], options);
    } else {
        analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
        options.CommentStyle.clear();
        options.DataLines.clear();
        options.DataLines.push_back(1);
        options.DataLines.push_back(std::numeric_limits<double>::infinity());
        if (!errorMessage.empty()) {
            Error("Nelson:readcell:readFailed", errorMessage);
        }
    }

    retval << ReadCell(filename, options, errorMessage);
    if (!errorMessage.empty()) {
        Error("Nelson:readcell:readFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
