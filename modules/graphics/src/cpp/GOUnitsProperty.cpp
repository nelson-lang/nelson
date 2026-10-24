//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUnitsProperty.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* UNITS_DICT[8] = { GO_PROPERTY_VALUE_INCHES_STR,
    GO_PROPERTY_VALUE_CENTIMETERS_STR, GO_PROPERTY_VALUE_NORMALIZED_STR,
    GO_PROPERTY_VALUE_POINTS_STR, GO_PROPERTY_VALUE_PIXELS_STR, GO_PROPERTY_VALUE_CHARACTERS_STR,
    GO_PROPERTY_VALUE_DATA_STR, 0 };
//=============================================================================
GOUnitsProperty::GOUnitsProperty() : GORestrictedStringProperty(UNITS_DICT) { }
//=============================================================================
void
GOUnitsProperty::set(ArrayOf arg)
{
    // The base restricted-string property validates the value and resolves prefixes
    // (e.g. 'n' -> 'normalized'). Record the unit in effect before the change for conversions.
    std::wstring before = data();
    GORestrictedStringProperty::set(arg);
    if (data() != before) {
        previousUnits = before;
    }
}
//=============================================================================
std::wstring
GOUnitsProperty::getPreviousUnits()
{
    return previousUnits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
