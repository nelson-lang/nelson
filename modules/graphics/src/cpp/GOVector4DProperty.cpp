//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOVector4DProperty.hpp"
#include "Error.hpp"
#include "PrintPropertyHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOVector4DProperty::get()
{
    Dimensions dims(1, 4);
    double* ptrDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.getElementCount(), stringVector(), false));
    ptrDouble[0] = m_data[0];
    ptrDouble[1] = m_data[1];
    ptrDouble[2] = m_data[2];
    ptrDouble[3] = m_data[3];
    return ArrayOf(NLS_DOUBLE, dims, ptrDouble);
}
//=============================================================================
void
GOVector4DProperty::set(double v1, double v2, double v3, double v4)
{
    Dimensions dims(1, 4);
    double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, 4);
    ptr[0] = v1;
    ptr[1] = v2;
    ptr[2] = v3;
    ptr[3] = v4;
    ArrayOf _value = ArrayOf(NLS_DOUBLE, dims, ptr);
    value(v1, v2, v3, v4);
    GOProperty::set(_value);
}
//=============================================================================
void
GOVector4DProperty::set(ArrayOf _value)
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
    if (dims.getElementCount() != 4) {
        Error(_W("Value must be a 4 element vector."));
    }
    _value.promoteType(NLS_DOUBLE);
    auto* ptrValues = (double*)_value.getDataPointer();
    value(ptrValues[0], ptrValues[1], ptrValues[2], ptrValues[3]);
    GOProperty::set(_value);
}
//=============================================================================
std::string
GOVector4DProperty::print(const std::string& propertyName)
{
    std::string res = "\t" + propertyName + ":\t";
    if (m_data.size() == 4) {
        res = res + " [ " + printNumber(m_data[0]) + " " + printNumber(m_data[1]) + " "
            + printNumber(m_data[2]) + " " + printNumber(m_data[3]) + " ]";
    }
    return res;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
