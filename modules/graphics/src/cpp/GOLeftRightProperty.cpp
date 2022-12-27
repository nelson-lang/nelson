//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOLeftRightProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* LEFT_RIGHT_DICT[3]
    = { GO_PROPERTY_VALUE_LEFT_STR, GO_PROPERTY_VALUE_RIGHT_STR, 0 };
//=============================================================================
GOLeftRightProperty::GOLeftRightProperty() : GORestrictedStringProperty(LEFT_RIGHT_DICT) { }
//=============================================================================
}
//=============================================================================
