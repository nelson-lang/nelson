//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOStringOnOffProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* ON_OFF_DICT[3] = { GO_PROPERTY_VALUE_ON_STR, GO_PROPERTY_VALUE_OFF_STR, 0 };
//=============================================================================
GOOnOffProperty::GOOnOffProperty() : GORestrictedStringProperty(ON_OFF_DICT) { }
//=============================================================================
bool
GOOnOffProperty::asBool()
{
    return isEqual(GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
}
//=============================================================================
