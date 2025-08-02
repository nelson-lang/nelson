//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOAlignVertProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* VERT_DICT[6] = { GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_CAP_STR,
    GO_PROPERTY_VALUE_MIDDLE_STR, GO_PROPERTY_VALUE_BASELINE_STR, GO_PROPERTY_VALUE_BOTTOM_STR, 0 };
//=============================================================================
GOAlignVertProperty::GOAlignVertProperty() : GORestrictedStringProperty(VERT_DICT) { }
//=============================================================================
}
//=============================================================================
