//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "detectImportOptionsBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "DetectImportOptions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::detectImportOptionsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);

    std::wstring filename = argIn[0].getContentAsWideString();

    detectImportOptions options;
    initializeDetectImportOptions(options);

    std::string errorMessage;
    analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
    if (!errorMessage.empty()) {
        Error(errorMessage);
    }

    stringVector fieldnames = { "Delimiter", "LineEnding", "CommentStyle", "EmptyLineRule",
        "VariableNamesLine", "VariableNames", "RowNamesColumn", "DataLines" };
    ArrayOfVector fieldvalues;

    fieldvalues << ArrayOf::toCellArrayOfCharacterRowVectors(options.Delimiter);
    fieldvalues << ArrayOf::toCellArrayOfCharacterRowVectors(options.LineEnding);
    fieldvalues << ArrayOf::toCellArrayOfCharacterRowVectors(options.CommentStyle);
    fieldvalues << ArrayOf::characterArrayConstructor(options.EmptyLineRule);
    fieldvalues << ArrayOf::doubleConstructor(options.VariableNamesLine);
    fieldvalues << ArrayOf::toCellArrayOfCharacterRowVectors(options.VariableNames);
    fieldvalues << ArrayOf::doubleConstructor(options.RowNamesColumn);
    fieldvalues << ArrayOf::doubleVectorConstructor(options.DataLines);

    ArrayOf importOptions
        = ArrayOf::classConstructor("DelimitedTextImportOptions", fieldnames, fieldvalues);
    retval << importOptions;
    return retval;
}
//=============================================================================
