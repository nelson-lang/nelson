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
#include <QtGui/QColor>
#include "GOColorProperty.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GOColorProperty::get()
{
    std::vector<double> rgb = asVector();
    Dimensions dims(1, rgb.size());
    double* values = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, rgb.size());
    memcpy(values, rgb.data(), sizeof(double) * rgb.size());
    return ArrayOf(NLS_DOUBLE, dims, values);
}
//=============================================================================
void
GOColorProperty::set(ArrayOf _value)
{
    if (isWriteProtected()) {
        Error(_W("Read only property."));
    }
    Dimensions dims = _value.getDimensions();
    if (_value.isVector() && _value.isDoubleType(true) && dims.getLength() == 3) {
        double* ptr = (double*)_value.getDataPointer();
        R = ptr[0];
        G = ptr[1];
        B = ptr[2];
    } else if (_value.isRowVectorCharacterArray()) {
        std::string colorName = _value.getContentAsCString();
        if (QColor::isValidColor(colorName.c_str())) {
            QColor color;
            color.setNamedColor(colorName.c_str());
            qreal r;
            qreal g;
            qreal b;
            qreal a;
            color.getRgbF(&r, &g, &b, &a);
            R = r;
            G = g;
            B = b;
        } else {
            Error(_("an valid string color expected."));
        }
    } else {
        Error(_("[R, G, B] vector or string color expected."));
    }
    GOProperty::set(_value);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
