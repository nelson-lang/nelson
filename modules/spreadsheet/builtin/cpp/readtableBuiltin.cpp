//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "readtableBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SpreadsheetTableIOHelpers.hpp"
#include "JsonTableIOHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::readtableBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);
    std::wstring filename = argIn[0].asWideString();
    if (isJsonReadRequest(filename, argIn, 1)) {
        ArrayOfVector result = callJsonTableFunction(eval, "__readtable_json__", argIn, 1);
        retval << normalizeImportedTimeTextColumns(result[0]);
        return retval;
    }
    nargincheck(argIn, 1, 16);
    size_t firstNameValue = 1;
    retval << readTableWithImportOptions(eval, filename, argIn, 1, firstNameValue);
    if (firstNameValue != argIn.size()) {
        Error(L"Nelson:spreadsheet:importOptionsExpected", L"Import options object expected.");
    }
    return retval;
}
//=============================================================================
