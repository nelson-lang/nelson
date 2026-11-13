//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFontUnitsProperty.hpp"
#include "GOPropertyValues.hpp"
#include "GOUnitsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOAxis.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static double
pointsPerUnit(const std::wstring& units, double heightInPixels);
//=============================================================================
static const wchar_t* FONT_UNITS_DICT[6] = { GO_PROPERTY_VALUE_POINTS_STR,
    GO_PROPERTY_VALUE_NORMALIZED_STR, GO_PROPERTY_VALUE_INCHES_STR,
    GO_PROPERTY_VALUE_CENTIMETERS_STR, GO_PROPERTY_VALUE_PIXELS_STR, 0 };
//=============================================================================
GOFontUnitsProperty::GOFontUnitsProperty() : GORestrictedStringProperty(FONT_UNITS_DICT) { }
//=============================================================================
void
GOFontUnitsProperty::set(ArrayOf arg)
{
    std::wstring before = data();
    GORestrictedStringProperty::set(arg);
    // Keep the oldest units when several sets come before the owner applies them.
    if (pendingFrom.empty()) {
        pendingFrom = before;
    }
    setOrder = nextUnitsPositionSetOrder();
}
//=============================================================================
std::wstring
GOFontUnitsProperty::takePendingFrom()
{
    std::wstring from = pendingFrom;
    pendingFrom.clear();
    return from == data() ? std::wstring() : from;
}
//=============================================================================
uint64_t
GOFontUnitsProperty::getSetOrder()
{
    return setOrder;
}
//=============================================================================
void
GOFontSizeProperty::set(ArrayOf arg)
{
    GOScalarProperty::set(arg);
    setOrder = nextUnitsPositionSetOrder();
}
//=============================================================================
uint64_t
GOFontSizeProperty::getSetOrder()
{
    return setOrder;
}
//=============================================================================
double
convertFontSizeUnits(
    double size, const std::wstring& from, const std::wstring& to, double heightInPixels)
{
    if (from == to) {
        return size;
    }
    double fromFactor = pointsPerUnit(from, heightInPixels);
    double toFactor = pointsPerUnit(to, heightInPixels);
    // No height to convert normalized units with: keep the value.
    if (fromFactor <= 0 || toFactor <= 0) {
        return size;
    }
    return size * fromFactor / toFactor;
}
//=============================================================================
double
fontUnitsReferenceHeight(GraphicsObject* object)
{
    GOAxis* axis = dynamic_cast<GOAxis*>(object);
    if (!axis && object) {
        axis = object->getParentAxis();
    }
    if (!axis) {
        return 0;
    }
    std::vector<double> position = axis->getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR);
    return position.size() >= 4 ? position[3] : 0;
}
//=============================================================================
double
fontSizeInPoints(GraphicsObject* object)
{
    double size = object->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    if (!object->haveProperty(GO_FONT_UNITS_PROPERTY_NAME_STR)) {
        return size;
    }
    std::wstring units = object->findStringProperty(GO_FONT_UNITS_PROPERTY_NAME_STR);
    if (units == GO_PROPERTY_VALUE_POINTS_STR) {
        return size;
    }
    double height
        = units == GO_PROPERTY_VALUE_NORMALIZED_STR ? fontUnitsReferenceHeight(object) : 0;
    return convertFontSizeUnits(size, units, GO_PROPERTY_VALUE_POINTS_STR, height);
}
//=============================================================================
static double
pointsPerUnit(const std::wstring& units, double heightInPixels)
{
    // Fixed 96 pixels per inch, as the reference reports font sizes.
    if (units == GO_PROPERTY_VALUE_PIXELS_STR) {
        return 72.0 / 96.0;
    }
    if (units == GO_PROPERTY_VALUE_INCHES_STR) {
        return 72.0;
    }
    if (units == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
        return 72.0 / 2.54;
    }
    if (units == GO_PROPERTY_VALUE_NORMALIZED_STR) {
        return heightInPixels > 0 ? heightInPixels * 72.0 / 96.0 : 0.0;
    }
    return 1.0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
