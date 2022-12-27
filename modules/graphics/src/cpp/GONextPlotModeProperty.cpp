//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GONextPlotModeProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* NEXT_PLOT_DICT[4] = { GO_PROPERTY_VALUE_ADD_STR,
    GO_PROPERTY_VALUE_REPLACE_STR, GO_PROPERTY_VALUE_REPLACECHILDREN_STR, 0 };
//=============================================================================
GONextPlotModeProperty::GONextPlotModeProperty() : GORestrictedStringProperty(NEXT_PLOT_DICT) { }
//=============================================================================
}
//=============================================================================
