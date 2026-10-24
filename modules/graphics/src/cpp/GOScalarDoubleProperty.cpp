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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "GOScalarDoubleProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
            Error(L"Nelson:validators:mustBeScalar", L"%s must be a scalar.", L"Value");
        }
        const double* dp = (const double*)copyNum.dataPointer();
        if (dp[0] >= _minValue && dp[0] <= _maxValue) {
            GOFixedVectorProperty::set(num);
        } else {
            std::wstring message
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:graphics:valueOutRangeS",
                                        L"Value is out of range %s <= value <= %s."),
                    fmt::to_wstring(_minValue), fmt::to_wstring(_maxValue));
            Error(L"Nelson:graphics:valueOutOfRange", message);
        }
    } else {
        GOFixedVectorProperty::set(num);
    }
}
//=============================================================================
ArrayOf
GOScalarProperty::get()
{
    if (_data.empty()) {
        return ArrayOf::empty(0, 0);
    }
    return GOVectorProperty::get();
}
//=============================================================================
void
GOScalarProperty::data(double x)
{
    if (haveLimits) {
        if (x >= _minValue && x <= _maxValue) {
            at(0) = x;
        } else {
            std::wstring message
                = formatI18nMessage(resolveMessageTemplate(L"Nelson:graphics:valueOutRangeS",
                                        L"Value is out of range %s <= value <= %s."),
                    fmt::to_wstring(_minValue), fmt::to_wstring(_maxValue));
            Error(L"Nelson:graphics:valueOutOfRange", message);
        }
    } else {
        at(0) = x;
    }
}
//=============================================================================
double
GOScalarProperty::data()
{
    if (_data.empty()) {
        return 0;
    }
    return at(0);
}
//=============================================================================
std::wstring
GOScalarProperty::toWideString()
{
    if (_data.empty()) {
        return L"[]";
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
} // namespace Nelson
//=============================================================================
