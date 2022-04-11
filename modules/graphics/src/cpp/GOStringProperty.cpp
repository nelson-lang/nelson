//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOStringProperty.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOStringProperty::get()
{
    return ArrayOf::characterArrayConstructor(m_data);
}
//=============================================================================
void
GOStringProperty::set(ArrayOf value)
{
    if (isWriteProtected()) {
        Error(_W("Read only property."));
    }
    m_data = value.getContentAsCString();
    GOProperty::set(value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
