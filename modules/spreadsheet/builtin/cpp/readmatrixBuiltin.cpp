//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <map>
#include "readmatrixBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ReadMatrix.hpp"
#include "DetectImportOptions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static NelsonType
convertToNelsonType(const std::wstring& typeStr);
static void
populateImportOptions(const ArrayOf& importOptionsObj, detectImportOptions& options);
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::readmatrixBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 4);
    std::wstring filename = argIn[0].getContentAsWideString();
    std::string errorMessage;
    detectImportOptions options;
    NelsonType OutputType = NLS_DOUBLE;
    initializeDetectImportOptions(options);

    switch (argIn.size()) {
    case 1: {
        // readmatrix(filename)
        analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
        options.CommentStyle.clear();
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
    } break;
    case 2: {
        // readmatrix(filename, options)
        if (argIn[1].isClassType() && argIn[1].getClassType() == "DelimitedTextImportOptions") {
            populateImportOptions(argIn[1], options);
        } else {
            Error(_W("Import options object expected."));
        }
    } break;
    case 3: {
        // readmatrix(filename, 'fieldname', fieldvalue)
        std::wstring fieldname = argIn[1].getContentAsWideString();
        if (fieldname != L"OutputType") {
            Error(_W("OutputType name expected."));
        }
        OutputType = convertToNelsonType(argIn[2].getContentAsWideString());
        analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
        options.CommentStyle.clear();
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
    } break;
    case 4: {
        // readmatrix(filename, options, 'fieldname', fieldvalue)
        if (argIn[1].isClassType() && argIn[1].getClassType() == "DelimitedTextImportOptions") {
            populateImportOptions(argIn[1], options);
        } else {
            Error(_W("Import options object expected."));
        }
        std::wstring fieldname = argIn[2].getContentAsWideString();
        if (fieldname != L"OutputType") {
            Error(_W("OutputType name expected."));
        }
        OutputType = convertToNelsonType(argIn[3].getContentAsWideString());
    } break;
    default: {
    } break;
    }

    retval << ReadMatrix(filename, options, OutputType, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
NelsonType
convertToNelsonType(const std::wstring& typeStr)
{
    static const std::map<std::wstring, NelsonType> typeMap
        = { { L"double", NLS_DOUBLE }, { L"single", NLS_SINGLE }, { L"string", NLS_STRING_ARRAY },
              { L"char", NLS_CELL_ARRAY }, { L"int8", NLS_INT8 }, { L"int16", NLS_INT16 },
              { L"int32", NLS_INT32 }, { L"int64", NLS_INT64 }, { L"uint8", NLS_UINT8 },
              { L"uint16", NLS_UINT16 }, { L"uint32", NLS_UINT32 }, { L"uint64", NLS_UINT64 } };

    auto it = typeMap.find(typeStr);
    if (it != typeMap.end()) {
        return it->second;
    }

    Error(_W("Unsupported type."));
    return NLS_DOUBLE;
}
//=============================================================================
void
populateImportOptions(const ArrayOf& importOptionsObj, detectImportOptions& options)
{
    options.Delimiter = importOptionsObj.getField("Delimiter").getContentAsCStringRowVector();
    options.LineEnding = importOptionsObj.getField("LineEnding").getContentAsCStringRowVector();
    options.CommentStyle = importOptionsObj.getField("CommentStyle").getContentAsCStringRowVector();
    options.EmptyLineRule = importOptionsObj.getField("EmptyLineRule").getContentAsCString();
    options.VariableNamesLine
        = (int)importOptionsObj.getField("VariableNamesLine").getContentAsDoubleScalar();
    options.VariableNames
        = importOptionsObj.getField("VariableNames").getContentAsCStringRowVector();
    options.RowNamesColumn
        = (int)importOptionsObj.getField("RowNamesColumn").getContentAsDoubleScalar();
    options.DataLines = importOptionsObj.getField("DataLines").getContentAsDoubleVector();
}
//=============================================================================
