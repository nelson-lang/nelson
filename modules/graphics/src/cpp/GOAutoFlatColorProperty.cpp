//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOAutoFlatColorProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* AUTO_FLAT_DICT[5] = { GO_PROPERTY_VALUE_NONE_STR, GO_PROPERTY_VALUE_AUTO_STR,
    GO_PROPERTY_VALUE_FLAT_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0 };
//=============================================================================
GOAutoFlatColorProperty::GOAutoFlatColorProperty() : GORestrictedStringColorProperty(AUTO_FLAT_DICT)
{
}
//=============================================================================
}
//=============================================================================
