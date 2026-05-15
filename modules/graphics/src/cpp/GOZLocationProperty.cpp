//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOZLocationProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* Z_LOCATION_DICT[4]
    = { GO_PROPERTY_VALUE_ZMIN_STR, GO_PROPERTY_VALUE_ZMAX_STR, GO_PROPERTY_VALUE_SCALAR_STR, 0 };
//=============================================================================
GOZLocationProperty::GOZLocationProperty() : GORestrictedStringScalarProperty(Z_LOCATION_DICT) { }
//=============================================================================
}
//=============================================================================
