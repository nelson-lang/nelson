//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "timeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "Time.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
const double*
realDoubleData(const ArrayOf& value)
{
    if (value.dataClass() != NLS_DOUBLE || value.isSparse()) {
        Error(L"Nelson:time:fullRealDoubleArraysExpected", L"%s expected.",
            L"Full real double arrays");
    }
    return static_cast<const double*>(value.dataPointer());
}
//=============================================================================
Dimensions
expandedDimensions(const ArrayOf& left, const ArrayOf& right)
{
    if (left.isScalar()) {
        return right.dimensions();
    }
    if (right.isScalar()) {
        return left.dimensions();
    }
    if (!left.dimensions().equals(right.dimensions())) {
        Error(L"Nelson:time:inputSizesMatchScalar", L"Input sizes must match or be scalar.");
    }
    return left.dimensions();
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::datetime_serial_add_secondsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 1);
    const ArrayOf& serial = argIn[0];
    const ArrayOf& seconds = argIn[1];
    const double sign = argIn[2].asDoubleScalar();
    const double* serialData = realDoubleData(serial);
    const double* secondsData = realDoubleData(seconds);
    Dimensions outDims = expandedDimensions(serial, seconds);
    const indexType outCount = outDims.elementCount();
    auto* out = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, outCount, stringVector(), false));
    const bool serialScalar = serial.isScalar();
    const bool secondsScalar = seconds.isScalar();
    for (indexType k = 0; k < outCount; ++k) {
        const double serialValue = serialData[serialScalar ? 0 : k];
        const double secondsValue = secondsData[secondsScalar ? 0 : k];
        out[k] = serialValue + sign * secondsValue / 86400.;
    }
    return ArrayOfVector { ArrayOf(NLS_DOUBLE, outDims, out) };
}
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::timeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(1);
    switch (argIn.size()) {
    case 0: {
        retval << ArrayOf::doubleScalar(TimeAsSeconds());
    } break;
    case 1: {
        std::wstring param1 = argIn[0].asWideString();
        if (param1 == L"ns") {
            retval << ArrayOf::uint64Scalar(TimeAsNanoSeconds());
        } else if (param1 == L"s") {
            retval << ArrayOf::doubleScalar(TimeAsSeconds());
        } else {
            Error(
                L"Nelson:time:argument2NsSExpected", L"%s expected.", L"Argument #2: 'ns' or 's'");
        }
    } break;
    }
    return retval;
}
//=============================================================================
