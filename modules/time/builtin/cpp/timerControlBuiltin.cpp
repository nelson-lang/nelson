//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timerBuiltin.hpp"
#include <cmath>
#include "ClassName.hpp"
#include "DateNumber.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Now.hpp"
#include "TimerHelpers.hpp"
#include "TimerObject.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static double
getStartDelay(TimerObject* object)
{
    ArrayOf value;
    object->get(L"StartDelay", value);
    return value.asDoubleScalar();
}
//=============================================================================
static void
startOne(TimerObject* object, double delaySeconds)
{
    std::wstring errorMessage;
    if (!object->start(delaySeconds, errorMessage)) {
        Error(L"Nelson:timer:startFailed", errorMessage);
    }
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_startBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    for (TimerObject* object : getTimerObjects(argIn[0])) {
        startOne(object, getStartDelay(object));
    }
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_stopBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    for (TimerObject* object : getTimerObjects(argIn[0])) {
        object->stop(true);
    }
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_waitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    for (TimerObject* object : getTimerObjects(argIn[0])) {
        object->wait(eval);
    }
    return {};
}
//=============================================================================
static std::vector<double>
numericDateValues(const ArrayOf& value)
{
    ArrayOf asDouble(value);
    asDouble.promoteType(NLS_DOUBLE);
    const auto* data = static_cast<const double*>(asDouble.dataPointer());
    return std::vector<double>(data, data + asDouble.elementCount());
}
//=============================================================================
static double
dateVectorRowToSerial(const std::vector<double>& values, indexType rows, indexType row)
{
    if (values.size() == static_cast<size_t>(rows * 3)) {
        return DateNumber(values[row], values[row + rows], values[row + rows * 2]);
    }
    return DateNumber(values[row], values[row + rows], values[row + rows * 2],
        values[row + rows * 3], values[row + rows * 4], values[row + rows * 5]);
}
//=============================================================================
static std::vector<double>
serialsFromNumericFiringTime(const ArrayOf& value, size_t timerCount)
{
    std::vector<double> values = numericDateValues(value);
    if (timerCount == 1 && value.isRowVector() && values.size() == 3) {
        return { DateNumber(values[0], values[1], values[2]) };
    }
    if (timerCount == 1 && value.isRowVector() && values.size() == 6) {
        return { DateNumber(values[0], values[1], values[2], values[3], values[4], values[5]) };
    }
    if (value.is2D() && (value.columns() == 3 || value.columns() == 6)) {
        std::vector<double> serials;
        serials.reserve(static_cast<size_t>(value.rows()));
        for (indexType row = 0; row < value.rows(); ++row) {
            serials.push_back(dateVectorRowToSerial(values, value.rows(), row));
        }
        return serials;
    }
    return values;
}
//=============================================================================
static std::vector<double>
serialsFromFiringTime(const ArrayOf& value, size_t timerCount)
{
    if (value.isClassType()) {
        std::string className;
        ClassName(value, className);
        if (className == "datetime") {
            return serialsFromFiringTime(value.field("SerialValue"), timerCount);
        }
    }
    if (value.isNumeric()) {
        return serialsFromNumericFiringTime(value, timerCount);
    }
    bool parsed = false;
    double serial = DateNumber(value.asWideString(), false, 0, parsed);
    if (!parsed) {
        Error(L"Nelson:time:invalidFiringTime", L"Invalid %s.", L"firing time");
    }
    return { serial };
}
//=============================================================================
static std::vector<double>
expandSerials(const std::vector<double>& serials, size_t timerCount)
{
    if (serials.size() == 1) {
        return std::vector<double>(timerCount, serials[0]);
    }
    if (serials.size() != timerCount) {
        Error(L"Nelson:time:firingTimeScalarMatchTimer",
            L"Firing time must be scalar or match the timer array size.");
    }
    return serials;
}
//=============================================================================
static double
delayFromSerial(double serial)
{
    double delaySeconds = (serial - Now()) * 86400.;
    if (!std::isfinite(delaySeconds) || delaySeconds < 0. || delaySeconds > 25. * 86400.) {
        Error(L"Nelson:time:firingTimeWithin25Days",
            L"Firing time must be within 25 days of the current time.");
    }
    return delaySeconds;
}
//=============================================================================
static double
serialFromDateParts(const ArrayOfVector& argIn)
{
    double y = argIn[1].asDoubleScalar();
    double m = argIn[2].asDoubleScalar();
    double d = argIn[3].asDoubleScalar();
    if (argIn.size() == 4) {
        return DateNumber(y, m, d);
    }
    double h = argIn[4].asDoubleScalar();
    double mi = argIn[5].asDoubleScalar();
    double s = argIn[6].asDoubleScalar();
    return DateNumber(y, m, d, h, mi, s);
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timer_startatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 0);
    if (!(argIn.size() == 2 || argIn.size() == 4 || argIn.size() == 7)) {
        Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    }
    std::vector<TimerObject*> objects = getTimerObjects(argIn[0]);
    std::vector<double> serials;
    if (argIn.size() == 2) {
        serials = serialsFromFiringTime(argIn[1], objects.size());
    } else {
        serials = { serialFromDateParts(argIn) };
    }
    serials = expandSerials(serials, objects.size());
    for (size_t k = 0; k < objects.size(); ++k) {
        startOne(objects[k], delayFromSerial(serials[k]));
    }
    return {};
}
//=============================================================================
