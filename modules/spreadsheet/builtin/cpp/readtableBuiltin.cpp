//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "readtableBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ReadTable.hpp"
#include "DetectImportOptions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::readtableBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].getContentAsWideString();
    std::string errorMessage;
    detectImportOptions options;

    initializeDetectImportOptions(options);

    if (argIn.size() > 1 && argIn[1].isClassType()
        && argIn[1].getClassType() == "DelimitedTextImportOptions") {

        options.Delimiter = argIn[1].getField("Delimiter").getContentAsCStringRowVector();
        options.LineEnding = argIn[1].getField("LineEnding").getContentAsCStringRowVector();
        options.CommentStyle = argIn[1].getField("CommentStyle").getContentAsCStringRowVector();
        options.EmptyLineRule = argIn[1].getField("EmptyLineRule").getContentAsCString();
        options.VariableNamesLine
            = argIn[1].getField("VariableNamesLine").getContentAsDoubleScalar();
        options.VariableNames = argIn[1].getField("VariableNames").getContentAsCStringRowVector();
        options.RowNamesColumn = argIn[1].getField("RowNamesColumn").getContentAsDoubleScalar();
        options.DataLines = argIn[1].getField("DataLines").getContentAsDoubleVector();

    } else {
        analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
    }

    retval << ReadTable(filename, options, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }
    return retval;
}
//=============================================================================
