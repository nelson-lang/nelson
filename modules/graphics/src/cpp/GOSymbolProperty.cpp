//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOSymbolProperty.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* SYMBOLS_DICT[20] = { L"+", L"o", L"*", L".", L"x",
    GO_PROPERTY_VALUE_SQUARE_STR, L"s", GO_PROPERTY_VALUE_DIAMOND_STR, L"d", L"^", L"v", L">", L"<",
    GO_PROPERTY_VALUE_PENTAGRAM_STR, L"p", GO_PROPERTY_VALUE_HEXAGRAM_STR, L"h",
    GO_PROPERTY_VALUE_NONE_STR, L"", 0 };
//=============================================================================
GOSymbolProperty::GOSymbolProperty() : GORestrictedStringProperty(SYMBOLS_DICT) { }
//=============================================================================
}
//=============================================================================
