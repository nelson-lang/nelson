//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOBusyActionProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* BUSY_ACTION_DICT[3]
    = { GO_PROPERTY_VALUE_QUEUE_STR, GO_PROPERTY_VALUE_CANCEL_STR, 0 };
//=============================================================================
GOBusyActionProperty::GOBusyActionProperty() : GORestrictedStringProperty(BUSY_ACTION_DICT) { }
//=============================================================================
}
//=============================================================================
