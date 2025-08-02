//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GODataMappingModeProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* DATA_MAP_MODE_DICT[3]
    = { GO_PROPERTY_VALUE_SCALED_STR, GO_PROPERTY_VALUE_DIRECT_STR, 0 };
//=============================================================================
GODataMappingModeProperty::GODataMappingModeProperty()
    : GORestrictedStringProperty(DATA_MAP_MODE_DICT)
{
}
//=============================================================================
}
//=============================================================================
