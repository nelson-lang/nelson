//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOTopBottomProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* TOP_BOTTOM_DICT[3]
    = { GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_BOTTOM_STR, 0 };
//=============================================================================
GOTopBottomProperty::GOTopBottomProperty() : GORestrictedStringProperty(TOP_BOTTOM_DICT) { }
//=============================================================================
}
//=============================================================================
