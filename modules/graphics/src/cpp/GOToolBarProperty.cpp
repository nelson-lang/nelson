//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOToolBarProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* TOOL_BAR_DICT[4]
    = { GO_PROPERTY_VALUE_NONE_STR, GO_PROPERTY_VALUE_AUTO_STR, GO_PROPERTY_VALUE_FIGURE_STR, 0 };
//=============================================================================
GOToolBarProperty::GOToolBarProperty() : GORestrictedStringProperty(TOOL_BAR_DICT) { }
//=============================================================================
}
//=============================================================================
