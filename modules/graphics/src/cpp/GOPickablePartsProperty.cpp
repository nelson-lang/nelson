//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* PICKABLE_PARTS_DICT[4]
    = { GO_PROPERTY_VALUE_VISIBLE_STR, GO_PROPERTY_VALUE_ALL_STR, GO_PROPERTY_VALUE_NONE_STR, 0 };
//=============================================================================
GOPickablePartsProperty::GOPickablePartsProperty() : GORestrictedStringProperty(PICKABLE_PARTS_DICT)
{
}
//=============================================================================
} // namespace Nelson
//=============================================================================
