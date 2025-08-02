//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOWindowStateProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* WINDOW_STATE_DICT[5]
    = { GO_PROPERTY_VALUE_NORMAL_STR, GO_PROPERTY_VALUE_MINIMIZED_STR,
          GO_PROPERTY_VALUE_MAXIMIZED_STR, GO_PROPERTY_VALUE_FULLSCREEN_STR, 0 };
//=============================================================================
GOWindowStateProperty::GOWindowStateProperty() : GORestrictedStringProperty(WINDOW_STATE_DICT) { }
//=============================================================================
}
//=============================================================================
