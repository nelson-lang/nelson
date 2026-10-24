//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "detectImportOptionsBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "DetectImportOptions.hpp"
#include "DelimitedTextImportOptionsHelpers.hpp"
#include "JsonTableIOHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::detectImportOptionsBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);

    std::wstring filename = argIn[0].asWideString();
    if (isJsonReadRequest(filename, argIn, 1)) {
        return callJsonTableFunction(eval, "__detectImportOptions_json__", argIn, 1);
    }
    nargincheck(argIn, 1, 1);

    detectImportOptions options;
    initializeDetectImportOptions(options);

    std::string errorMessage;
    analyzeFileFormatImportOptions(filename, 4096, options, errorMessage);
    if (!errorMessage.empty()) {
        Error("Nelson:detectImportOptions:failed", errorMessage);
    }

    retval << toDelimitedTextImportOptions(options);
    return retval;
}
//=============================================================================
