//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "writetableBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SpreadsheetTableIOHelpers.hpp"
#include "WriteTable.hpp"
#include "JsonTableIOHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::writetableBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    // writetable(T)
    // writetable(T, filename)
    // writetable(T, filename, Name1, Value1, ... , Name2, Value2)
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 17);

    writeTableOptions options;
    initializeWriteTableOptions(options);

    ArrayOf T = argIn[0];
    if (!T.isTable()) {
        Error(L"Nelson:types:wrongArgumentTypeExpectedDot",
            L"Wrong type for argument #%d: %s expected.", 1, L"table");
    }
    std::wstring filename;
    if (argIn.size() > 1) {
        filename = argIn[1].asWideString();
    }
    if (filename.empty()) {
        filename = argIn[1].wname();
        if (filename.empty()) {
            filename = L"table";
        }
        filename = filename + L".txt";
    }

    if (isJsonWriteRequest(filename, argIn, 2)) {
        ArrayOfVector args(argIn);
        args[1] = ArrayOf::characterArray(filename);
        callJsonTableFunction(eval, "__writetable_json__", args, 0);
        return retval;
    }
    parseWriteTableOptions(argIn, 2, options);
    options._FileType = determineWriteFileType(filename, options._FileType);
    validateWriteTableOptions(options);

    std::wstring errorMessage;

    WriteTable(T, filename, options, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:writetable:writeFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
