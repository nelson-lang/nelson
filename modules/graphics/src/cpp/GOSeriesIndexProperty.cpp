//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cerrno>
#include <cmath>
#include <cwchar>
#include <cwctype>
#include "GOSeriesIndexProperty.hpp"
#include "GOPropertyValues.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
parseWideDouble(const std::wstring& str, double& value)
{
    const wchar_t* begin = str.c_str();
    wchar_t* end = nullptr;
    errno = 0;
    value = wcstod(begin, &end);
    if (end == begin || errno == ERANGE) {
        return false;
    }
    while (*end != L'\0' && iswspace(*end)) {
        ++end;
    }
    return *end == L'\0';
}
//=============================================================================
static void
checkSeriesIndex(double value)
{
    if (!std::isfinite(value) || value < 1 || value != std::floor(value)) {
        Error(L"Nelson:seriesindex:invalidValue",
            L"SeriesIndex must be a positive integer or 'none'.");
    }
}
//=============================================================================
bool
GOSeriesIndexProperty::isNone()
{
    return _isNone;
}
//=============================================================================
double
GOSeriesIndexProperty::value()
{
    return _value;
}
//=============================================================================
void
GOSeriesIndexProperty::setNone()
{
    _isNone = true;
}
//=============================================================================
void
GOSeriesIndexProperty::setValue(double value)
{
    checkSeriesIndex(value);
    _isNone = false;
    _value = value;
}
//=============================================================================
ArrayOf
GOSeriesIndexProperty::get()
{
    if (_isNone) {
        return ArrayOf::characterArray(GO_PROPERTY_VALUE_NONE_STR);
    }
    return ArrayOf::doubleScalar(_value);
}
//=============================================================================
void
GOSeriesIndexProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::wstring value = arg.asWideString();
        StringHelpers::to_lower(value);
        if (value == GO_PROPERTY_VALUE_NONE_STR) {
            setNone();
            return;
        }
        double numericValue = 0;
        if (!parseWideDouble(value, numericValue)) {
            Error(L"Nelson:seriesindex:invalidValue",
                L"SeriesIndex must be a positive integer or 'none'.");
        }
        setValue(numericValue);
        return;
    }
    if (!arg.isNumeric() || arg.isComplex() || !arg.isScalar()) {
        Error(L"Nelson:seriesindex:invalidValue",
            L"SeriesIndex must be a positive integer or 'none'.");
    }
    setValue(arg.asDoubleScalar());
}
//=============================================================================
std::wstring
GOSeriesIndexProperty::toWideString()
{
    if (_isNone) {
        return GO_PROPERTY_VALUE_NONE_STR;
    }
    return std::to_wstring((int64)_value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
