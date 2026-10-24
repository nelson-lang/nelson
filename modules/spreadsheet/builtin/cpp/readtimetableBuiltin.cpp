//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "readtimetableBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SpreadsheetTableIOHelpers.hpp"
#include "JsonTableIOHelpers.hpp"
#include "Context.hpp"
#include "FunctionDef.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
struct TimetableReadOptions
{
    std::string timeVariable;
    ArrayOf rowTimes;
    ArrayOf startTime;
    ArrayOf timeStep;
    double sampleRate = 0;
    std::wstring inputFormat;
    bool hasRowTimes = false;
    bool hasStartTime = false;
    bool hasTimeStep = false;
    bool hasSampleRate = false;
};
//=============================================================================
static void
parseTimetableOptions(const ArrayOfVector& argIn, size_t first, TimetableReadOptions& options);
static void
splitTimetableArguments(
    const ArrayOfVector& argIn, ArrayOfVector& tableArgs, ArrayOfVector& timetableArgs);
static ArrayOf
makeRegularTimes(indexType rows, const TimetableReadOptions& options);
static int
findVariableIndex(const stringVector& names, const std::string& name);
static std::string
uniqueDimensionName(const std::string& name, const stringVector& variableNames);
static ArrayOf
tableWithoutColumn(const ArrayOf& table, int removeIndex);
static ArrayOf
constructTimetable(
    Evaluator* eval, const ArrayOf& table, const ArrayOf& rowTimes, const std::string& rowTimeName);
static bool
columnToTimes(const ArrayOf& column, const TimetableReadOptions& options, ArrayOf& rowTimes);
static ArrayOf
resolveRowTimes(const ArrayOf& table, const TimetableReadOptions& options, int& timeColumn);
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::readtimetableBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);

    std::wstring filename = argIn[0].asWideString();
    TimetableReadOptions timetableOptions;
    ArrayOf table;
    if (SpreadsheetGateway::isJsonReadRequest(filename, argIn, 1)) {
        ArrayOfVector tableArgs;
        ArrayOfVector timetableArgs;
        splitTimetableArguments(argIn, tableArgs, timetableArgs);
        ArrayOfVector result
            = SpreadsheetGateway::callJsonTableFunction(eval, "__readtable_json__", tableArgs, 1);
        table = normalizeImportedTimeTextColumns(result[0]);
        parseTimetableOptions(timetableArgs, 0, timetableOptions);
    } else {
        nargincheck(argIn, 1, 18);
        size_t firstNameValue;
        table = readTableWithImportOptions(eval, filename, argIn, 1, firstNameValue);
        parseTimetableOptions(argIn, firstNameValue, timetableOptions);
    }

    int timeColumn = -1;
    ArrayOf rowTimes = resolveRowTimes(table, timetableOptions, timeColumn);
    if (timeLength(rowTimes) != table.tableHeight()) {
        Error(L"Nelson:readtimetable:rowTimesMismatch", L"%s must match the number of rows.",
            L"RowTimes");
    }
    stringVector tableNames = table.tableVariableNames();
    std::string rowTimeName = timeColumn >= 0 ? tableNames[timeColumn] : "";
    ArrayOf dataTable = timeColumn >= 0 ? tableWithoutColumn(table, timeColumn) : table;
    retval << constructTimetable(eval, dataTable, rowTimes, rowTimeName);
    return retval;
}
//=============================================================================
namespace {
//=============================================================================
void
parseTimetableOptions(const ArrayOfVector& argIn, size_t first, TimetableReadOptions& options)
{
    if (((argIn.size() - first) % 2) != 0) {
        Error(L"Nelson:spreadsheet:nameValueNotPaired", L"Name-value arguments must be paired.");
    }
    for (size_t k = first; k < argIn.size(); k += 2) {
        std::string name = normalizedNameValueName(argIn[k]);
        if (name == "rowtimes") {
            if (argIn[k + 1].isScalarStringArray() || argIn[k + 1].isRowVectorCharacterArray()) {
                options.timeVariable = argIn[k + 1].asUtf8String();
            } else {
                options.rowTimes = argIn[k + 1];
                options.hasRowTimes = true;
            }
        } else if (name == "starttime") {
            options.startTime = argIn[k + 1];
            options.hasStartTime = true;
        } else if (name == "timestep") {
            options.timeStep = argIn[k + 1];
            options.hasTimeStep = true;
        } else if (name == "samplerate") {
            options.sampleRate = argIn[k + 1].asDoubleScalar();
            options.hasSampleRate = true;
        } else if (name == "inputformat") {
            options.inputFormat = argIn[k + 1].asWideString();
        } else {
            Error(L"Nelson:spreadsheet:propertyNotSupported", L"Property name not supported.");
        }
    }
}
//=============================================================================
void
splitTimetableArguments(
    const ArrayOfVector& argIn, ArrayOfVector& tableArgs, ArrayOfVector& timetableArgs)
{
    tableArgs.push_back(argIn[0]);
    size_t first = 1;
    if (argIn.size() > 1 && argIn[1].isClassType()) {
        tableArgs.push_back(argIn[1]);
        first = 2;
    }
    for (size_t k = first; k < argIn.size(); k += 2) {
        std::string name = normalizedNameValueName(argIn[k]);
        bool timetableOption = name == "rowtimes" || name == "starttime" || name == "timestep"
            || name == "samplerate" || name == "inputformat";
        ArrayOfVector& target = timetableOption ? timetableArgs : tableArgs;
        target.push_back(argIn[k]);
        if (k + 1 < argIn.size()) {
            target.push_back(argIn[k + 1]);
        }
    }
}
//=============================================================================
ArrayOf
makeRegularTimes(indexType rows, const TimetableReadOptions& options)
{
    double step = 1;
    if (options.hasSampleRate) {
        if (options.sampleRate <= 0) {
            Error(L"Nelson:validators:mustBePositive", L"%s must be positive.", L"SampleRate");
        }
        step = 1.0 / options.sampleRate;
    } else if (options.hasTimeStep && isTimeObject(options.timeStep)
        && options.timeStep.classType() == "duration") {
        step = scalarTimeStorage(options.timeStep, "SecondsValue");
    } else if (options.hasTimeStep) {
        Error(
            L"Nelson:readtimetable:invalidTimeStep", L"%s must be a duration scalar.", L"TimeStep");
    }

    std::vector<double> values(rows);
    if (options.hasStartTime && isTimeObject(options.startTime)
        && options.startTime.classType() == "datetime") {
        double start = scalarTimeStorage(options.startTime, "SerialValue");
        for (indexType k = 0; k < rows; ++k) {
            values[k] = start + (double)k * step / 86400.0;
        }
        return makeDatetime(values);
    }
    double start = 0;
    if (options.hasStartTime && isTimeObject(options.startTime)
        && options.startTime.classType() == "duration") {
        start = scalarTimeStorage(options.startTime, "SecondsValue");
    } else if (options.hasStartTime) {
        Error(L"Nelson:readtimetable:invalidStartTime", L"%s must be datetime or duration.",
            L"StartTime");
    }
    for (indexType k = 0; k < rows; ++k) {
        values[k] = start + (double)k * step;
    }
    return makeDuration(values);
}
//=============================================================================
int
findVariableIndex(const stringVector& names, const std::string& name)
{
    for (size_t k = 0; k < names.size(); ++k) {
        if (names[k] == name) {
            return (int)k;
        }
    }
    return -1;
}
//=============================================================================
std::string
uniqueDimensionName(const std::string& name, const stringVector& variableNames)
{
    std::string candidate = name;
    for (int k = 1; findVariableIndex(variableNames, candidate) != -1; ++k) {
        candidate = name + "_" + std::to_string(k);
    }
    return candidate;
}
//=============================================================================
ArrayOf
tableWithoutColumn(const ArrayOf& table, int removeIndex)
{
    stringVector names = table.tableVariableNames();
    ArrayOfVector values;
    stringVector keptNames;
    for (size_t k = 0; k < names.size(); ++k) {
        if ((int)k != removeIndex) {
            keptNames.push_back(names[k]);
            values.push_back(table.tableColumn(names[k]));
        }
    }
    return ArrayOf::table(values, keptNames, stringVector {});
}
//=============================================================================
ArrayOf
constructTimetable(
    Evaluator* eval, const ArrayOf& table, const ArrayOf& rowTimes, const std::string& rowTimeName)
{
    Context* context = eval->getContext();
    FunctionDef* functionDef = nullptr;
    std::string functionName = "timetable";
    if (!context->lookupFunction(functionName, functionDef)) {
        Error(L"Nelson:readtimetable:timetableNotFound", L"timetable function not found.");
    }

    stringVector names = table.tableVariableNames();
    ArrayOfVector inputs;
    inputs.push_back(rowTimes);
    for (const std::string& name : names) {
        inputs.push_back(table.tableColumn(name));
    }
    inputs.push_back(ArrayOf::characterArray("VariableNames"));
    inputs.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(names));
    // A timetable rejects a dimension name that is also a variable name: the
    // default names are made unique against the variables read from the file.
    std::string timeName = rowTimeName.empty() ? "Time" : rowTimeName;
    inputs.push_back(ArrayOf::characterArray("DimensionNames"));
    inputs.push_back(ArrayOf::cellArrayFromArrayOfCharacterRowVectors(stringVector {
        uniqueDimensionName(timeName, names), uniqueDimensionName("Variables", names) }));

    ArrayOfVector result = functionDef->evaluateFunction(eval, inputs, 1);
    if (result.size() != 1) {
        Error(L"Nelson:readtimetable:invalidResult", L"timetable returned an invalid result.");
    }
    return result[0];
}
//=============================================================================
bool
columnToTimes(const ArrayOf& column, const TimetableReadOptions& options, ArrayOf& rowTimes)
{
    if (isTimeObject(column)) {
        rowTimes = column;
        return true;
    }
    wstringVector text;
    if (!cellText(column, text)) {
        return false;
    }
    if (parseTextAsDuration(text, rowTimes)) {
        return true;
    }
    if (parseTextAsDatetime(text, options.inputFormat, rowTimes)) {
        return true;
    }
    return false;
}
//=============================================================================
ArrayOf
resolveRowTimes(const ArrayOf& table, const TimetableReadOptions& options, int& timeColumn)
{
    if (options.hasRowTimes) {
        return options.rowTimes;
    }
    if (options.hasSampleRate || options.hasTimeStep || options.hasStartTime) {
        return makeRegularTimes(table.tableHeight(), options);
    }

    stringVector names = table.tableVariableNames();
    int first = options.timeVariable.empty() ? 0 : findVariableIndex(names, options.timeVariable);
    int last = options.timeVariable.empty() ? (int)names.size() - 1 : first;
    if (first < 0) {
        Error(L"Nelson:readtimetable:timeVariableNotFound", L"TimeVariable not found.");
    }
    for (int k = first; k <= last; ++k) {
        ArrayOf rowTimes;
        if (columnToTimes(table.tableColumn(names[k]), options, rowTimes)) {
            timeColumn = k;
            return rowTimes;
        }
    }
    Error(L"Nelson:readtimetable:rowTimesNotDetected", L"Unable to detect timetable row times.");
    return {};
}
//=============================================================================
} // namespace
//=============================================================================
