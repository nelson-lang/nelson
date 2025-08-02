//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOBackFaceLightingProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* BACKFACE_DICT[4] = { GO_PROPERTY_VALUE_UNLIT_STR, GO_PROPERTY_VALUE_LIT_STR,
    GO_PROPERTY_VALUE_REVERSELIT_STR, 0 };
//=============================================================================
GOBackFaceLightingProperty::GOBackFaceLightingProperty() : GORestrictedStringProperty(BACKFACE_DICT)
{
}
//=============================================================================
}
//=============================================================================
