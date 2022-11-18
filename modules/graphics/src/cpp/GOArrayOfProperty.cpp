//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOArrayOfProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOArrayOfProperty::get()
{
    return m_data;
}
//=============================================================================
void
GOArrayOfProperty::set(ArrayOf value)
{
    if (isWriteProtected()) {
        Error(_W("Read only property."));
    }
    m_data = value;
    GOProperty::set(value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
