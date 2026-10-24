//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "writetimetableBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SpreadsheetTableIOHelpers.hpp"
#include "WriteTable.hpp"
#include "JsonTableIOHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
static std::string
rowTimeName(const ArrayOf& timetable);
static ArrayOf
tableForWriting(const ArrayOf& timetable);
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::writetimetableBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 17);
    if (!argIn[0].isTimetable()) {
        Error(L"Nelson:types:wrongArgumentTypeExpectedDot",
            L"Wrong type for argument #%d: %s expected.", 1, L"timetable");
    }

    std::wstring filename;
    if (argIn.size() > 1) {
        filename = argIn[1].asWideString();
    }
    if (filename.empty()) {
        filename = L"timetable.txt";
    }

    if (isJsonWriteRequest(filename, argIn, 2)) {
        ArrayOfVector args(argIn);
        args[1] = ArrayOf::characterArray(filename);
        callJsonTableFunction(eval, "__writetable_json__", args, 0);
        return retval;
    }
    writeTableOptions options;
    initializeWriteTableOptions(options);
    parseWriteTableOptions(argIn, 2, options);
    options._FileType = determineWriteFileType(filename, options._FileType);
    validateWriteTableOptions(options);

    std::wstring errorMessage;
    WriteTable(tableForWriting(argIn[0]), filename, options, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:writetimetable:writeFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
namespace {
//=============================================================================
std::string
rowTimeName(const ArrayOf& timetable)
{
    ArrayOf props = timetable.field("Properties");
    ArrayOf names = props.field("DimensionNames");
    stringVector values = names.asUtf8StringVector();
    return values.empty() ? "Time" : values[0];
}
//=============================================================================
ArrayOf
tableForWriting(const ArrayOf& timetable)
{
    ArrayOf props = timetable.field("Properties");
    ArrayOf rowTimes = timeObjectToText(props.field("RowTimes"));
    ArrayOf table = timetable.field("Table");

    stringVector names = { rowTimeName(timetable) };
    ArrayOfVector values;
    values.push_back(rowTimes);
    stringVector tableNames = table.tableVariableNames();
    for (const std::string& name : tableNames) {
        values.push_back(timeObjectToText(table.tableColumn(name)));
        names.push_back(name);
    }
    return ArrayOf::table(values, names, stringVector {});
}
//=============================================================================
} // namespace
//=============================================================================
