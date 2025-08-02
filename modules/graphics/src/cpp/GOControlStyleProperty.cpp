//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOControlStyleProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* CONTROL_STYLES_DICT[10]
    = { GO_PROPERTY_VALUE_PUSHBUTTON_STR, GO_PROPERTY_VALUE_TOGGLEBUTTON_STR,
          GO_PROPERTY_VALUE_CHECKBOX_STR, GO_PROPERTY_VALUE_RADIOBUTTON_STR,
          GO_PROPERTY_VALUE_EDIT_STR, GO_PROPERTY_VALUE_TEXT_STR, GO_PROPERTY_VALUE_SLIDER_STR,
          GO_PROPERTY_VALUE_LISTBOX_STR, GO_PROPERTY_VALUE_POPUPMENU_STR, 0 };

//=============================================================================
GOControlStyleProperty::GOControlStyleProperty() : GORestrictedStringProperty(CONTROL_STYLES_DICT)
{
}
//=============================================================================
}
//=============================================================================
