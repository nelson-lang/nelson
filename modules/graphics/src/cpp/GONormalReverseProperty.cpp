//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GONormalReverseProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* NORMAL_REVERSE_DICT[3]
    = { GO_PROPERTY_VALUE_NORMAL_STR, GO_PROPERTY_VALUE_REVERSE_STR, 0 };
//=============================================================================
GONormalReverseProperty::GONormalReverseProperty() : GORestrictedStringProperty(NORMAL_REVERSE_DICT)
{
}
//=============================================================================
}
//=============================================================================
