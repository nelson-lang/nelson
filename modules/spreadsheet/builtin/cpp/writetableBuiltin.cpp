//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "writetableBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WriteTable.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
initializeDefaultOptions(writeTableOptions& options)
{
    options._Delimiter = L'\0';
    options._FileType = "";
    options._WriteMode = "";
    options._QuoteStrings = "";
    options._WriteVariableNames = true;
    options._WriteRowNames = false;
    options._RowNodeName = "";
    options._TableNodeName = "";
}
//=============================================================================
static bool
validateDelimiter(const std::wstring& delimiter, writeTableOptions& options)
{
    wstringVector supportedDelimiters
        = { L",", L"comma", L" ", L"space", L"\\t", L"tab", L";", L"semi", L"|", L"bar" };
    auto it = std::find(supportedDelimiters.begin(), supportedDelimiters.end(), delimiter);
    if (it == supportedDelimiters.end()) {
        return false;
    }

    if (delimiter == L"comma" || delimiter == L",")
        options._Delimiter = ',';
    else if (delimiter == L"space" || delimiter == L" ")
        options._Delimiter = ' ';
    else if (delimiter == L"tab" || delimiter == L"\\t")
        options._Delimiter = '\t';
    else if (delimiter == L"semi" || delimiter == L";")
        options._Delimiter = ';';
    else if (delimiter == L"bar" || delimiter == L"|")
        options._Delimiter = ';';

    return true;
}
//=============================================================================
static std::string
determineFileType(const std::wstring& filename, const std::string& specifiedType)
{
    std::wstring extension = (filename.rfind(L'.') != std::wstring::npos)
        ? filename.substr(filename.rfind(L'.'))
        : L"";

    if (extension == L".xml")
        return "xml";
    if (extension == L".txt" || extension == L".csv" || extension == L".dat")
        return "text";
    if (!specifiedType.empty())
        return specifiedType;

    Error(_W("Unrecognized file extension."));
    return "";
}
//=============================================================================
static void
validateOptionCombinations(writeTableOptions& options)
{
    if (options._FileType == "xml" && !options._QuoteStrings.empty()) {
        Error(_W("QuoteStrings not allowed for xml file type."));
    } else if (options._FileType == "text" && options._QuoteStrings.empty()) {
        options._QuoteStrings = "minimal";
    }

    if (options._FileType == "xml" && options._Delimiter != L'\0') {
        Error(_W("Delimiter not allowed for xml file type."));
    } else if (options._FileType == "text" && options._Delimiter == L'\0') {
        options._Delimiter = ',';
    }

    if (options._FileType == "xml" && !options._WriteMode.empty()) {
        Error(_W("WriteMode not allowed for xml file type."));
    } else if (options._FileType == "text" && options._WriteMode.empty()) {
        options._WriteMode = "overwrite";
    }

    if (options._FileType == "text" && !options._AttributeSuffix.empty()) {
        Error(_W("AttributeSuffix not allowed for text file type."));
    } else if (options._FileType == "xml" && options._AttributeSuffix.empty()) {
        options._AttributeSuffix = "Attribute";
    }

    if (options._FileType == "text" && !options._RowNodeName.empty()) {
        Error(_W("RowNodeName allowed only for xml file type."));
    } else if (options._FileType == "xml" && options._RowNodeName.empty()) {
        options._RowNodeName = "row";
    }

    if (options._FileType == "text" && !options._TableNodeName.empty()) {
        Error(_W("TableNodeName allowed only for xml file type."));
    } else if (options._FileType == "xml" && options._TableNodeName.empty()) {
        options._TableNodeName = "table";
    }
}
//=============================================================================
static void
parseWriteTableOptions(const ArrayOfVector& argIn, size_t startIdx, writeTableOptions& options)
{
    for (size_t k = startIdx; k < argIn.size(); k = k + 2) {
        ArrayOf nameArrayOf = argIn[k];
        bool isName
            = nameArrayOf.isScalarStringArray() || (nameArrayOf.isRowVectorCharacterArray());
        if (!isName) {
            Error(_W("Invalid parameter name. The parameter name must be a non-empty string or "
                     "character array."));
        }
        std::wstring name = argIn[k].getContentAsWideString();
        if (name == L"Delimiter") {
            std::wstring delimiter = argIn[k + 1].getContentAsWideString();
            if (!validateDelimiter(delimiter, options)) {
                Error(_("delimiter not supported."));
            }
        } else if (name == L"WriteRowNames") {
            options._WriteRowNames = argIn[k + 1].getContentAsLogicalScalar();
        } else if (name == L"QuoteStrings") {
            std::wstring quoteStrings = argIn[k + 1].getContentAsWideString();
            if (quoteStrings == L"all") {
                options._QuoteStrings = "all";
            } else if (quoteStrings == L"none") {
                options._QuoteStrings = "none";
            } else if (quoteStrings == L"minimal") {
                options._QuoteStrings = "minimal";
            } else {
                Error(_("Quote strings not supported."));
            }
        } else if (name == L"WriteVariableNames") {
            options._WriteVariableNames = argIn[k + 1].getContentAsLogicalScalar();
        } else if (name == L"FileType") {
            std::wstring fileType = argIn[k + 1].getContentAsWideString();
            bool isSupportedFileType = (fileType == L"text") || (fileType == L"xml");
            if (!isSupportedFileType) {
                Error(_("Only text currently supported."));
            }
        } else if (name == L"WriteMode") {
            std::wstring writeMode = argIn[k + 1].getContentAsWideString();
            if (writeMode == L"append") {
                options._WriteMode = "append";
            } else if (writeMode == L"overwrite") {
                options._WriteMode = "overwrite";
            } else {
                Error(_("'append' or 'overwrite' expected."));
            }
        } else if (name == L"RowNodeName") {
            options._RowNodeName = argIn[k + 1].getContentAsCString();
        } else if (name == L"TableNodeName") {
            options._TableNodeName = argIn[k + 1].getContentAsCString();
        } else if (name == L"AttributeSuffix") {
            options._AttributeSuffix = argIn[k + 1].getContentAsCString();
        } else {
            Error(_("Property name not supported."));
        }
    }
}
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::writetableBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // writetable(T)
    // writetable(T, filename)
    // writetable(T, filename, Name1, Value1, ... , Name2, Value2)
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 11);

    writeTableOptions options;
    initializeDefaultOptions(options);

    ArrayOf T = argIn[0];
    bool isTable = (T.isClassType() && T.getClassType() == "table");
    if (!isTable) {
        Error(_W("Wrong type for argument #1. table expected."));
    }
    std::wstring filename;
    if (argIn.size() > 1) {
        filename = argIn[1].getContentAsWideString();
    }
    if (filename.empty()) {
        filename = argIn[1].wname();
        if (filename.empty()) {
            filename = L"table";
        }
        filename = filename + L".txt";
    }

    options._FileType = determineFileType(filename, options._FileType);
    validateOptionCombinations(options);
    parseWriteTableOptions(argIn, 2, options);

    std::wstring errorMessage;

    WriteTable(T, filename, options, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
