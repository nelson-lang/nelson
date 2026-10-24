//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__write_json_table__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "JsonTableWriter.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __write_json_table__(filename, keys, columns, prettyPrint, preserveInfAndNaN)
// Internal writer used by writetable and writetimetable for JSON files.
ArrayOfVector
Nelson::SpreadsheetGateway::__write_json_table__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 5, 5);
    std::wstring filename = argIn[0].asWideString();
    stringVector keys = argIn[1].isEmpty() ? stringVector() : argIn[1].asUtf8StringVector();
    ArrayOf columnsCell = argIn[2];
    if (!columnsCell.isCell() || columnsCell.elementCount() != keys.size()) {
        Error(L"Nelson:spreadsheet:invalidJsonColumns", L"Invalid JSON table columns.");
    }
    ArrayOfVector columns;
    const ArrayOf* data = (const ArrayOf*)columnsCell.dataPointer();
    for (indexType k = 0; k < columnsCell.elementCount(); ++k) {
        columns.push_back(data[k]);
    }
    std::wstring errorMessage;
    WriteJsonTable(filename, keys, columns, argIn[3].asLogicalScalar(), argIn[4].asLogicalScalar(),
        errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:writetable:writeFailed", errorMessage);
    }
    return {};
}
//=============================================================================
