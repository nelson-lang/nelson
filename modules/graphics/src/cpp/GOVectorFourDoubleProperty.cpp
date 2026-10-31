//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOVectorFourDoubleProperty.hpp"
#include "GraphicsObject.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUnitsProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOFourVectorProperty::value(double x, double y, double z, double w)
{
    at(0) = x;
    at(1) = y;
    at(2) = z;
    at(3) = w;
}
//=============================================================================
std::wstring
GOFourVectorProperty::toWideString()
{
    return L"[" + std::to_wstring((at(0))) + L" " + std::to_wstring((at(1))) + L" "
        + std::to_wstring((at(2))) + L" " + std::to_wstring((at(3))) + L"]";
}
//=============================================================================
bool
GOPixelOriginPositionProperty::ownerUsesPixels()
{
    if (m_owner == nullptr) {
        return false;
    }
    auto* units = dynamic_cast<GOUnitsProperty*>(
        m_owner->findProperty(GO_UNITS_PROPERTY_NAME_STR, false));
    return units != nullptr && units->data() == GO_PROPERTY_VALUE_PIXELS_STR;
}
//=============================================================================
ArrayOf
GOPixelOriginPositionProperty::get()
{
    ArrayOf value = GOFourVectorProperty::get();
    if (ownerUsesPixels() && value.elementCount() == 4) {
        auto* dp = static_cast<double*>(value.readWriteDataPointer());
        dp[0] += 1.;
        dp[1] += 1.;
    }
    return value;
}
//=============================================================================
void
GOPixelOriginPositionProperty::set(ArrayOf value)
{
    const bool shifted = ownerUsesPixels() && value.elementCount() == 4;
    GOFourVectorProperty::set(value);
    m_shiftedForPixels = shifted;
    if (shifted) {
        at(0) -= 1.;
        at(1) -= 1.;
    }
}
//=============================================================================
void
GOPixelOriginPositionProperty::reconcileUnits()
{
    const bool pixels = ownerUsesPixels();
    if (pixels == m_shiftedForPixels) {
        return;
    }
    const double shift = (m_shiftedForPixels ? 1. : 0.) - (pixels ? 1. : 0.);
    at(0) += shift;
    at(1) += shift;
    m_shiftedForPixels = pixels;
}
//=============================================================================
std::wstring
GOPixelOriginPositionProperty::toWideString()
{
    const double shift = ownerUsesPixels() ? 1. : 0.;
    return L"[" + std::to_wstring(at(0) + shift) + L" " + std::to_wstring(at(1) + shift) + L" "
        + std::to_wstring(at(2)) + L" " + std::to_wstring(at(3)) + L"]";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
