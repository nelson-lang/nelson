//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "GOVectorFourDoubleProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GraphicsObject.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUnitsProperty.hpp"
#include "GOUITab.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
checkRectangle(ArrayOf value);
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
void
GOFourVectorProperty::set(ArrayOf value)
{
    GOFixedVectorProperty::set(value);
    m_setOrder = nextUnitsPositionSetOrder();
}
//=============================================================================
uint64_t
GOFourVectorProperty::getSetOrder()
{
    return m_setOrder;
}
//=============================================================================
void
GORectanglePositionProperty::set(ArrayOf value)
{
    checkRectangle(value);
    GOFourVectorProperty::set(value);
}
//=============================================================================
bool
GOPixelOriginPositionProperty::ownerUsesPixels()
{
    if (m_owner == nullptr) {
        return false;
    }
    auto* units
        = dynamic_cast<GOUnitsProperty*>(m_owner->findProperty(GO_UNITS_PROPERTY_NAME_STR, false));
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
        // A tab stores the size of its group and reports its page.
        if (auto* tab = dynamic_cast<GOUITab*>(m_owner)) {
            tab->reportedPageSize(dp[2], dp[3]);
        }
    }
    return value;
}
//=============================================================================
void
GOPixelOriginPositionProperty::set(ArrayOf value)
{
    checkRectangle(value);
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
void
GOPixelOriginPositionProperty::adoptOwnerUnits()
{
    m_shiftedForPixels = ownerUsesPixels();
}
//=============================================================================
std::wstring
GOPixelOriginPositionProperty::toWideString()
{
    const bool pixels = ownerUsesPixels();
    const double shift = pixels ? 1. : 0.;
    double width = at(2);
    double height = at(3);
    if (auto* tab = pixels ? dynamic_cast<GOUITab*>(m_owner) : nullptr) {
        tab->reportedPageSize(width, height);
    }
    return L"[" + std::to_wstring(at(0) + shift) + L" " + std::to_wstring(at(1) + shift) + L" "
        + std::to_wstring(width) + L" " + std::to_wstring(height) + L"]";
}
//=============================================================================
// A [left bottom width height] given by the user: four finite numbers, no negative size.
void
checkRectangle(ArrayOf value)
{
    if (!value.isNumeric() || value.isSparse()) {
        Error(L"Nelson:graphics:positionMustBeNumeric", L"Position values must be numeric.");
    }
    if (value.elementCount() != 4) {
        Error(L"Nelson:graphics:positionFourElements",
            L"Position must be a four-element vector.");
    }
    value.promoteType(NLS_DOUBLE);
    const double* dp = static_cast<const double*>(value.dataPointer());
    for (int k = 0; k < 4; ++k) {
        if (!std::isfinite(dp[k])) {
            Error(L"Nelson:graphics:positionMustBeFinite", L"Position values must be finite.");
        }
    }
    if (dp[2] < 0 || dp[3] < 0) {
        Error(L"Nelson:graphics:positionNegativeSize",
            L"Width and height must be greater than or equal to 0.");
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
