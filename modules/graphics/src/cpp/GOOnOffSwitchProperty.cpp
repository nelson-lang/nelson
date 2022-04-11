//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOOnOffSwitchProperty.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOOnOffSwitchProperty::get()
{
    return (m_data == 0) ? ArrayOf::characterArrayConstructor("off")
                         : ArrayOf::characterArrayConstructor("on");
}
//=============================================================================
void
GOOnOffSwitchProperty::set(ArrayOf _value)
{
    if (isWriteProtected()) {
        Error(_W("Read only property."));
    }
    if (_value.isLogical()) {
        m_data = _value.getContentAsLogicalScalar();
    } else {
        std::string onOff = _value.getContentAsCString();
        value(onOff);
    }
    GOProperty::set(_value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
