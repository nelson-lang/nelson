//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFontUnitsProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* FONT_UNITS_DICT[6] = { GO_PROPERTY_VALUE_POINTS_STR,
    GO_PROPERTY_VALUE_NORMALIZED_STR, GO_PROPERTY_VALUE_INCHES_STR,
    GO_PROPERTY_VALUE_CENTIMETERS_STR, GO_PROPERTY_VALUE_PIXELS_STR, 0 };
//=============================================================================
GOFontUnitsProperty::GOFontUnitsProperty() : GORestrictedStringProperty(FONT_UNITS_DICT) { }
//=============================================================================
}
//=============================================================================
