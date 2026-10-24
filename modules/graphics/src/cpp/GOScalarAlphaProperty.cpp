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
#include "GOScalarAlphaProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOScalarAlphaProperty::set(ArrayOf num)
{
    if (num.isRowVectorCharacterArray() || num.isScalarStringArray()) {
        std::wstring str = num.asWideString();
        if (str == L"flat") {
            _isFlat = true;
        } else {
            Error(L"Nelson:graphics:expectedFlat", L"'%s' expected.", L"flat");
        }
        return;
    }
    ArrayOf copyNum(num);
    copyNum.promoteType(NLS_DOUBLE);
    if (!copyNum.isScalar()) {
        Error(L"Nelson:validators:mustBeScalar", L"%s must be a scalar.", L"Value");
    }
    const double* dp = (const double*)copyNum.dataPointer();
    if (dp[0] >= _minValue && dp[0] <= _maxValue) {
        GOFixedVectorProperty::set(num);
        _isFlat = false;
    } else {
        std::wstring message
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:graphics:valueOutRangeS",
                                    L"Value is out of range %s <= value <= %s."),
                fmt::to_wstring(_minValue), fmt::to_wstring(_maxValue));
        Error(L"Nelson:graphics:valueOutOfRange", message);
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
            = formatI18nMessage(resolveMessageTemplate(L"Nelson:graphics:valueOutRangeS",
                                    L"Value is out of range %s <= value <= %s."),
                fmt::to_wstring(_minValue), fmt::to_wstring(_maxValue));
        Error(L"Nelson:graphics:valueOutOfRange", message);
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
} // namespace Nelson
//=============================================================================
