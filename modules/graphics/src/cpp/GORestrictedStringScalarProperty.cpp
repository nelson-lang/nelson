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
#include <cwchar>
#include <cwctype>
#include "GORestrictedStringScalarProperty.hpp"
#include "GOPropertyValues.hpp"
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
double
GORestrictedStringScalarProperty::scalar()
{
    return _scalar;
}
//=============================================================================
void
GORestrictedStringScalarProperty::scalar(double scal)
{
    _scalar = scal;
}
//=============================================================================
ArrayOf
GORestrictedStringScalarProperty::get()
{
    if (!isEqual(GO_PROPERTY_VALUE_SCALAR_STR)) {
        return GORestrictedStringProperty::get();
    }
    return ArrayOf::doubleConstructor(_scalar);
}
//=============================================================================
void
GORestrictedStringScalarProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    if (arg.isRowVectorCharacterArray() || (arg.isStringArray() && arg.isScalar())) {
        std::wstring str = arg.getContentAsWideString();
        double num = 0;
        if (parseWideDouble(str, num)) {
            data(GO_PROPERTY_VALUE_SCALAR_STR);
            _scalar = num;
        } else {
            GORestrictedStringProperty::set(arg);
        }
    } else {
        data(GO_PROPERTY_VALUE_SCALAR_STR);
        _scalar = arg.getContentAsDoubleScalar();
    }
}
//=============================================================================
}
//=============================================================================
