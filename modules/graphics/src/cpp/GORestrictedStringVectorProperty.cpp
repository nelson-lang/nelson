//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "GORestrictedStringVectorProperty.hpp"
#include "GOHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GORestrictedStringVectorProperty::GORestrictedStringVectorProperty(const wchar_t** dict)
{
    while (*dict) {
        m_dictionary.emplace_back(*dict);
        dict++;
    }
}
//=============================================================================
void
GORestrictedStringVectorProperty::set(ArrayOf arg)
{
    ArrayOf _arg = uniformizeStringVector(arg, _data);
    GOGenericProperty::set(_arg);
    GOStringVectorProperty::set(_arg);
    for (int i = 0; i < _data.size(); i++) {
        if (find(m_dictionary.begin(), m_dictionary.end(), _data[i]) == m_dictionary.end()) {
            Error(_W("Illegal selection for property."));
        }
    }
}
//=============================================================================
}
//=============================================================================
