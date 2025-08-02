//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "GORestrictedStringProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GORestrictedStringProperty::GORestrictedStringProperty(const wchar_t** dict)
{
    while (*dict) {
        m_dictionary.emplace_back(*dict);
        dict++;
    }
}
//=============================================================================
void
GORestrictedStringProperty::set(ArrayOf arg)
{
    if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
        Error(_W("Expecting a string for property."));
    }
    std::wstring tst(arg.getContentAsWideString());
    if (std::find(m_dictionary.begin(), m_dictionary.end(), tst) == m_dictionary.end()) {
        Error(_W("Illegal selection for property."));
    }
    if (data() != tst) {
        GOStringProperty::set(arg);
    }
}
//=============================================================================
}
//=============================================================================
