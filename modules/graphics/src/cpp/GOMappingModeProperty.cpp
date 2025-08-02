//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOMappingModeProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* MAP_MODE_DICT[4]
    = { GO_PROPERTY_VALUE_NONE_STR, GO_PROPERTY_VALUE_DIRECT_STR, GO_PROPERTY_VALUE_SCALED_STR, 0 };
//=============================================================================
GOMappingModeProperty::GOMappingModeProperty() : GORestrictedStringProperty(MAP_MODE_DICT) { }
//=============================================================================
}
//=============================================================================
