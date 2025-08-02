//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GORowColumnProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* ROWS_COLS_DICT[4]
    = { GO_PROPERTY_VALUE_BOTH_STR, GO_PROPERTY_VALUE_ROW_STR, GO_PROPERTY_VALUE_COLUMN_STR, 0 };
//=============================================================================
GORowColumnProperty::GORowColumnProperty() : GORestrictedStringProperty(ROWS_COLS_DICT) { }
//=============================================================================
}
//=============================================================================
