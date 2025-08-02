//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOStringOnOffProperty.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* ON_OFF_DICT[3] = { GO_PROPERTY_VALUE_ON_STR, GO_PROPERTY_VALUE_OFF_STR, 0 };
//=============================================================================
GOOnOffProperty::GOOnOffProperty() : GORestrictedStringProperty(ON_OFF_DICT) { }
//=============================================================================
bool
GOOnOffProperty::asBool()
{
    return isEqual(GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
void
GOOnOffProperty::set(ArrayOf arg)
{
    GOGenericProperty::set(arg);
    if (arg.isScalar() && arg.isLogical()) {
        bool logicalValue = arg.getContentAsLogicalScalar();
        ArrayOf strArrayOf = logicalValue
            ? ArrayOf::characterArrayConstructor(GO_PROPERTY_VALUE_ON_STR)
            : ArrayOf::characterArrayConstructor(GO_PROPERTY_VALUE_OFF_STR);
        GOStringProperty::set(strArrayOf);
        return;
    }
    if (!arg.isRowVectorCharacterArray() && !(arg.isStringArray() && arg.isScalar())) {
        Error(_W("Expecting a string for property."));
    }
    std::wstring tst(arg.getContentAsWideString());
    if (std::find(m_dictionary.begin(), m_dictionary.end(), tst) == m_dictionary.end()) {
        Error(_W("Illegal selection for property."));
    }
    GOStringProperty::set(arg);
}
//=============================================================================
}
//=============================================================================
