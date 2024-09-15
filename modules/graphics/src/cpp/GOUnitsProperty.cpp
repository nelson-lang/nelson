//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUnitsProperty.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
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
    if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
        Error(_W("Expecting a string for property."));
    }
    std::wstring tst(arg.getContentAsWideString());
    if (std::find(m_dictionary.begin(), m_dictionary.end(), tst) == m_dictionary.end()) {
        Error(_W("Illegal selection for property."));
    }
    if (data() != tst) {
        previousUnits = data();
        GOStringProperty::set(arg);
    }
}
//=============================================================================
std::wstring
GOUnitsProperty::getPreviousUnits()
{
    return previousUnits;
}
//=============================================================================
}
//=============================================================================
