//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOVector2DProperty.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOVector2DProperty::get()
{
    Dimensions dims(1, 2);
    double* ptrDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.getElementCount(), stringVector(), false));
    ptrDouble[0] = m_data[0];
    ptrDouble[1] = m_data[1];
    return ArrayOf(NLS_DOUBLE, dims, ptrDouble);
}
//=============================================================================
void
GOVector2DProperty::set(ArrayOf _value)
{
    if (isWriteProtected()) {
        Error(_W("Read only property."));
    }
    if (!_value.isNumeric()) {
        Error(_W("Numeric values expected."));
    }
    if (_value.isSparse()) {
        Error(_W("full values expected."));
    }
    Dimensions dims = _value.getDimensions();
    if (dims.getElementCount() != 2) {
        Error(_W("Value must be a 2 element vector."));
    }
    _value.promoteType(NLS_DOUBLE);
    auto* ptrValues = (double*)_value.getDataPointer();
    value(ptrValues[0], ptrValues[1]);
    GOProperty::set(_value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
