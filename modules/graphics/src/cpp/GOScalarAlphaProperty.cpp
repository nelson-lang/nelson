//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "GOScalarAlphaProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOScalarAlphaProperty::set(ArrayOf num)
{
    if (num.isRowVectorCharacterArray() || num.isScalarStringArray()) {
        std::wstring str = num.getContentAsWideString();
        if (str == L"flat") {
            _isFlat = true;
        } else {
            Error(_W("'flat' expected."));
        }
        return;
    }
    ArrayOf copyNum(num);
    copyNum.promoteType(NLS_DOUBLE);
    if (!copyNum.isScalar()) {
        Error(_W("Scalar value expected."));
    }
    const double* dp = (const double*)copyNum.getDataPointer();
    if (dp[0] >= _minValue && dp[0] <= _maxValue) {
        GOFixedVectorProperty::set(num);
        _isFlat = false;
    } else {
        std::wstring message
            = fmt::sprintf(_W("Value is out of range %f <= value <= %f."), _minValue, _maxValue);
        Error(message);
    }
}
//=============================================================================
void
GOScalarAlphaProperty::data(double x)
{
    if (x >= _minValue && x <= _maxValue) {
        at(0) = x;
        _isFlat = false;
    } else {
        std::wstring message
            = fmt::sprintf(_W("Value is out of range %f <= value <= %f."), _minValue, _maxValue);
        Error(message);
    }
}
//=============================================================================
double
GOScalarAlphaProperty::data()
{
    return at(0);
}
//=============================================================================
bool
GOScalarAlphaProperty::isFlat()
{
    return _isFlat;
}
//=============================================================================
std::wstring
GOScalarAlphaProperty::toWideString()
{
    if (_isFlat) {
        return L"'flat'";
    }
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
//=============================================================================
