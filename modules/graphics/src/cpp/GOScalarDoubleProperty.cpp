//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "GOScalarDoubleProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOScalarProperty::set(ArrayOf num)
{
    if (haveLimits) {
        ArrayOf copyNum(num);
        copyNum.promoteType(NLS_DOUBLE);
        if (!copyNum.isScalar()) {
            Error(_W("Scalar value expected."));
        }
        const double* dp = (const double*)copyNum.getDataPointer();
        if (dp[0] >= _minValue && dp[0] <= _maxValue) {
            GOFixedVectorProperty::set(num);
        } else {
            std::wstring message = fmt::sprintf(
                _W("Value is out of range %f <= value <= %f."), _minValue, _maxValue);
            Error(message);
        }
    } else {
        GOFixedVectorProperty::set(num);
    }
}
//=============================================================================
void
GOScalarProperty::data(double x)
{
    if (haveLimits) {
        if (x >= _minValue && x <= _maxValue) {
            at(0) = x;
        } else {
            std::wstring message = fmt::sprintf(
                _W("Value is out of range %f <= value <= %f."), _minValue, _maxValue);
            Error(message);
        }
    } else {
        at(0) = x;
    }
}
//=============================================================================
double
GOScalarProperty::data()
{
    return at(0);
}
//=============================================================================
std::wstring
GOScalarProperty::toWideString()
{
    double value = at(0);
    std::wstring msg;
    if (std::fabs((double)(int)(value)-value) < std::numeric_limits<double>::epsilon()) {
        msg = std::to_wstring((int)value);
    } else {
        msg = std::to_wstring(value);
    }
    return msg;
}
//=============================================================================
}
