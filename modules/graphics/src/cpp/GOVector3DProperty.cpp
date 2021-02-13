//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "GOVector3DProperty.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOVector3DProperty::get()
{
    Dimensions dims(1, 3);
    double* ptrDouble = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.getElementCount(), stringVector(), false));
    ptrDouble[0] = m_data[0];
    ptrDouble[1] = m_data[1];
    ptrDouble[2] = m_data[2];
    return ArrayOf(NLS_DOUBLE, dims, ptrDouble);
}
//=============================================================================
void
GOVector3DProperty::set(ArrayOf _value)
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
    if (_value.getElementCount() != 3) {
        Error(_W("Value must be a 3 element vector."));
    }
    _value.promoteType(NLS_DOUBLE);
    auto* ptrValues = (double*)_value.getDataPointer();
    value(ptrValues[0], ptrValues[1], ptrValues[2]);
    GOProperty::set(_value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
