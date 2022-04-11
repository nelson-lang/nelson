//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QColor>
#include "GOColorProperty.hpp"
#include "Error.hpp"
#include "PrintPropertyHelpers.hpp"
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
    Dimensions dims = _value.getDimensions();
    if (_value.isVector() && _value.isDoubleType(true) && (dims.getElementCount() == 3)) {
        auto* ptr = (double*)_value.getDataPointer();
        QColor color;
        color.setRgbF(ptr[0], ptr[1], ptr[2]);
        if (!color.isValid()) {
            Error(_("an valid [R, G, B] color expected."));
        }
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
        float r;
        float g;
        float b;
#else
        qreal r;
        qreal g;
        qreal b;
#endif
        color.getRgbF(&r, &g, &b);
        R = r;
        G = g;
        B = b;
    } else if (_value.isRowVectorCharacterArray()) {
        std::string colorName = _value.getContentAsCString();
        if (QColor::isValidColor(colorName.c_str())) {
            QColor color;
            color.setNamedColor(colorName.c_str());
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
            float r;
            float g;
            float b;
#else
            qreal r;
            qreal g;
            qreal b;
#endif
            color.getRgbF(&r, &g, &b);
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
std::string
GOColorProperty::print(const std::string& propertyName)
{
    std::string v = "[" + printNumber(R) + " " + printNumber(G) + " " + printNumber(B) + "]";
    return "\t" + propertyName + "\t" + v;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
