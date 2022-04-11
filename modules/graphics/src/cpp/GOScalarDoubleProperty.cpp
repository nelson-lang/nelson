//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOScalarDoubleProperty.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOScalarDoubleProperty::get()
{
    return ArrayOf::doubleConstructor(m_data);
}
//=============================================================================
void
GOScalarDoubleProperty::set(ArrayOf value)
{
    if (isWriteProtected()) {
        Error(_W("Read only property."));
    }
    m_data = value.getContentAsDoubleScalar();
    GOProperty::set(value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
