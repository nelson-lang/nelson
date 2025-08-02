//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFontAngleProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* FONT_ANGLE_DICT[4] = { GO_PROPERTY_VALUE_NORMAL_STR,
    GO_PROPERTY_VALUE_ITALIC_STR, GO_PROPERTY_VALUE_OBLIQUE_STR, 0 };
//=============================================================================
GOFontAngleProperty::GOFontAngleProperty() : GORestrictedStringProperty(FONT_ANGLE_DICT) { }
//=============================================================================
}
//=============================================================================
