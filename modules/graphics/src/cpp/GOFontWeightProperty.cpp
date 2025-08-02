//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFontWeightProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* FONT_WEIGHT_DICT[5] = { GO_PROPERTY_VALUE_NORMAL_STR,
    GO_PROPERTY_VALUE_BOLD_STR, GO_PROPERTY_VALUE_LIGHT_STR, GO_PROPERTY_VALUE_DEMI_STR, 0 };
//=============================================================================
GOFontWeightProperty::GOFontWeightProperty() : GORestrictedStringProperty(FONT_WEIGHT_DICT) { }
//=============================================================================
}
//=============================================================================
