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
}
//=============================================================================
