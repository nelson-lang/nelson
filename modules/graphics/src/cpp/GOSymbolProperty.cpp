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
void
GOSymbolProperty::set(ArrayOf arg)
{
    // Canonicalize the one-letter marker aliases: s -> square, d -> diamond,
    // p -> pentagram, h -> hexagram. The other markers (+, o, *, ., x, ^, v, >, <, none)
    // have a single name.
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::wstring value(arg.asWideString());
        if (value == L"s") {
            value = GO_PROPERTY_VALUE_SQUARE_STR;
        } else if (value == L"d") {
            value = GO_PROPERTY_VALUE_DIAMOND_STR;
        } else if (value == L"p") {
            value = GO_PROPERTY_VALUE_PENTAGRAM_STR;
        } else if (value == L"h") {
            value = GO_PROPERTY_VALUE_HEXAGRAM_STR;
        }
        GORestrictedStringProperty::set(ArrayOf::characterArray(value));
        return;
    }
    GORestrictedStringProperty::set(arg);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
