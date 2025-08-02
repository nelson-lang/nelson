//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOColorInterpProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* INTERP_MODE_DICT[5] = { GO_PROPERTY_VALUE_NONE_STR,
    GO_PROPERTY_VALUE_FLAT_STR, GO_PROPERTY_VALUE_INTERP_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0 };
//=============================================================================
GOColorInterpProperty::GOColorInterpProperty() : GORestrictedStringColorProperty(INTERP_MODE_DICT)
{
}
//=============================================================================
}
//=============================================================================
