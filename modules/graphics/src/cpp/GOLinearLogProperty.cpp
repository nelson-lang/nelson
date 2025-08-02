//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOLinearLogProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* LINEAR_LOG_DICT[3]
    = { GO_PROPERTY_VALUE_LINEAR_STR, GO_PROPERTY_VALUE_LOG_STR, 0 };
//=============================================================================
GOLinearLogProperty::GOLinearLogProperty() : GORestrictedStringProperty(LINEAR_LOG_DICT) { }
//=============================================================================
}
//=============================================================================
