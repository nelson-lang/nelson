//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPositionProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* POSITION_DICT[3]
    = { GO_PROPERTY_VALUE_OUTERPOSITION_STR, GO_PROPERTY_VALUE_POSITION_STR, 0 };
//=============================================================================
GOPositionProperty::GOPositionProperty() : GORestrictedStringProperty(POSITION_DICT) { }
//=============================================================================
}
//=============================================================================
