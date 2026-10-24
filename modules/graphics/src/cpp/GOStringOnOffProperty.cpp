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
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const wchar_t* ON_OFF_DICT[3] = { GO_PROPERTY_VALUE_ON_STR, GO_PROPERTY_VALUE_OFF_STR, 0 };
//=============================================================================
static const wchar_t* HANDLE_VISIBILITY_DICT[4]
    = { GO_PROPERTY_VALUE_ON_STR, L"callback", GO_PROPERTY_VALUE_OFF_STR, 0 };
//=============================================================================
GOOnOffProperty::GOOnOffProperty() : GORestrictedStringProperty(ON_OFF_DICT) { }
//=============================================================================
GOHandleVisibilityProperty::GOHandleVisibilityProperty()
    : GORestrictedStringProperty(HANDLE_VISIBILITY_DICT)
{
}
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
    // get() reports a pure on/off property as a nelson.lang.OnOffSwitchState
    // enumeration; accept it back so set(h, 'P', get(h, 'P')) round-trips. Its
    // 'on'/'off' member name is read from the classdef "Name" field (no Evaluator
    // needed on this path).
    if (arg.isClassType() && arg.classType() == "nelson.lang.OnOffSwitchState") {
        arg = arg.field("Name");
    }
    if (arg.isScalar() && arg.isLogical()) {
        GOGenericProperty::set(arg);
        bool logicalValue = arg.asLogicalScalar();
        ArrayOf strArrayOf = logicalValue ? ArrayOf::characterArray(GO_PROPERTY_VALUE_ON_STR)
                                          : ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR);
        GOStringProperty::set(strArrayOf);
        return;
    }
    // The base class handles the string path (case-insensitive and shortest
    // unambiguous prefix, e.g. Visible 'of' -> 'off').
    GORestrictedStringProperty::set(arg);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
