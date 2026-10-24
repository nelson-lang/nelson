//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOThemeProperty.hpp"
#include "GOPropertyValues.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOThemeProperty::get()
{
    const bool dark = _data == GO_PROPERTY_VALUE_DARK_STR;
    stringVector fieldNames { "Name", "BaseColorStyle" };
    ArrayOfVector values;
    values << ArrayOf::characterArray(dark ? L"Dark Theme" : L"Light Theme");
    values << ArrayOf::characterArray(dark ? L"dark" : L"light");
    return ArrayOf::classArray(GO_CLASS_NAME_GRAPHICS_THEME_STR, fieldNames, values);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
