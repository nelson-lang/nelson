//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOColorBar.hpp"
#include "GOFigure.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUnitsProperty.hpp"
#include "GOPositionHelpers.hpp"
#include "GOUIComponentUnits.hpp"
#include "GOSurfaceScale.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOColorBar::updateUnits()
{
    if (!hasChanged(GO_UNITS_PROPERTY_NAME_STR)) {
        return;
    }
    // properties apply in argument order: a Position given after Units is in the new units.
    if (positionSetAfterUnits(this, GO_POSITION_PROPERTY_NAME_STR)) {
        clearChanged(GO_UNITS_PROPERTY_NAME_STR);
        return;
    }
    bool positionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR);
    GOUnitsProperty* unitsProperty
        = static_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR));
    double width = 0;
    double height = 0;
    decorationHostSizeInPixels(this, width, height);
    std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    std::wstring previousUnits = unitsProperty->getPreviousUnits();
    std::wstring currentUnits = unitsProperty->data();
    if (previousUnits == GO_PROPERTY_VALUE_PIXELS_STR
        && currentUnits != GO_PROPERTY_VALUE_PIXELS_STR && position.size() >= 2) {
        position[0] -= 1.0;
        position[1] -= 1.0;
    }
    convertPosition(previousUnits, currentUnits, position, width, height);
    if (previousUnits != GO_PROPERTY_VALUE_PIXELS_STR
        && currentUnits == GO_PROPERTY_VALUE_PIXELS_STR && position.size() >= 2) {
        position[0] += 1.0;
        position[1] += 1.0;
    }
    setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
    if (!positionChanged) {
        clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    }
    clearChanged(GO_UNITS_PROPERTY_NAME_STR);
}
//=============================================================================
std::vector<double>
GOColorBar::positionAsPixels(RenderInterface& gc)
{
    return positionAsPixels(gc.width(), gc.height());
}
//=============================================================================
std::vector<double>
GOColorBar::positionAsPixels(double width, double height)
{
    return positionOnSurface(this, findStringProperty(GO_UNITS_PROPERTY_NAME_STR),
        findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR), 1.0, width, height);
}
//=============================================================================
bool
GOColorBar::hitTest(double x, double y, double width, double height)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
        || stringCheck(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
        || stringCheck(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        return false;
    }
    if (stringCheck(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR)
        && stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return false;
    }
    std::vector<double> pos = positionAsPixels(width, height);
    if (pos.size() < 4 || pos[2] <= 0.0 || pos[3] <= 0.0) {
        return false;
    }
    return x >= pos[0] && x <= pos[0] + pos[2] && y >= pos[1] && y <= pos[1] + pos[3];
}
//=============================================================================
} // namespace Nelson
//=============================================================================
