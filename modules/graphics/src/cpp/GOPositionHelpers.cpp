//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPositionHelpers.hpp"
#include "GraphicsObject.hpp"
#include "ArrayOf.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOSurfaceScale.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isFourVector(const std::vector<double>& value)
{
    return value.size() == 4;
}
//=============================================================================
static bool
setFourIfPresent(GraphicsObject* object, const std::wstring& name, const std::vector<double>& value)
{
    if (!object || !isFourVector(value) || !object->haveProperty(name)) {
        return false;
    }
    auto* property = static_cast<GOFourVectorProperty*>(object->findProperty(name, false));
    if (!property) {
        return false;
    }
    property->value(value[0], value[1], value[2], value[3]);
    object->clearChanged(name);
    return true;
}
//=============================================================================
static bool
setStringIfPresent(GraphicsObject* object, const std::wstring& name, const std::wstring& value)
{
    if (!object || !object->haveProperty(name)) {
        return false;
    }
    GOGenericProperty* property = object->findProperty(name, false);
    if (!property) {
        return false;
    }
    property->set(ArrayOf::stringArray(value));
    object->clearChanged(name);
    return true;
}
//=============================================================================
static std::vector<double>
selectedPosition(GraphicsObject* object)
{
    if (!object) {
        return {};
    }
    if (object->hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR)) {
        return object->findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    }
    if (object->hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)) {
        return object->findVectorDoubleProperty(GO_INNER_POSITION_PROPERTY_NAME_STR);
    }
    return object->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
bool
syncSimplePositionTriplet(GraphicsObject* object)
{
    if (!object) {
        return false;
    }
    bool changed = object->hasChanged(GO_POSITION_PROPERTY_NAME_STR)
        || object->hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)
        || object->hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (!changed) {
        return false;
    }
    std::vector<double> position = selectedPosition(object);
    if (!isFourVector(position)) {
        return false;
    }
    setFourIfPresent(object, GO_POSITION_PROPERTY_NAME_STR, position);
    setFourIfPresent(object, GO_INNER_POSITION_PROPERTY_NAME_STR, position);
    setFourIfPresent(object, GO_OUTER_POSITION_PROPERTY_NAME_STR, position);
    return true;
}
//=============================================================================
bool
syncInnerDrivenPositionTriplet(GraphicsObject* object)
{
    if (!object) {
        return false;
    }
    bool positionChanged = object->hasChanged(GO_POSITION_PROPERTY_NAME_STR);
    bool innerChanged = object->hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
    if (!positionChanged && !innerChanged) {
        return false;
    }
    std::vector<double> position = innerChanged
        ? object->findVectorDoubleProperty(GO_INNER_POSITION_PROPERTY_NAME_STR)
        : object->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (!isFourVector(position)) {
        return false;
    }
    setFourIfPresent(object, GO_POSITION_PROPERTY_NAME_STR, position);
    setFourIfPresent(object, GO_INNER_POSITION_PROPERTY_NAME_STR, position);
    if (object->haveProperty(GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR)) {
        setStringIfPresent(
            object, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INNERPOSITION_STR);
    }
    return true;
}
//=============================================================================
bool
syncOuterDrivenPositionTriplet(GraphicsObject* object)
{
    if (!object || !object->hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR)) {
        return false;
    }
    if (object->haveProperty(GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR)) {
        setStringIfPresent(
            object, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    }
    return true;
}
//=============================================================================
bool
setPositionConstraintIfPresent(GraphicsObject* object, const std::wstring& value)
{
    return setStringIfPresent(object, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, value);
}
//=============================================================================
void
reconcilePositionPixelOrigin(GraphicsObject* object)
{
    const std::wstring names[] = { GO_POSITION_PROPERTY_NAME_STR,
        GO_INNER_POSITION_PROPERTY_NAME_STR, GO_OUTER_POSITION_PROPERTY_NAME_STR };
    for (const std::wstring& name : names) {
        if (object == nullptr || !object->haveProperty(name) || !object->hasChanged(name)) {
            continue;
        }
        if (auto* shifted
            = dynamic_cast<GOPixelOriginPositionProperty*>(object->findProperty(name, false))) {
            shifted->reconcileUnits();
        }
    }
}
//=============================================================================
bool
syncPositionUnitsTriplet(GraphicsObject* object, const std::wstring& previousUnits,
    const std::wstring& currentUnits, double width, double height, bool pixelOrigin)
{
    if (!object) {
        return false;
    }
    bool converted = false;
    bool unitsDiffer = previousUnits != currentUnits;
    if (unitsDiffer) {
        reconcilePositionPixelOrigin(object);
    }
    const std::wstring names[] = { GO_POSITION_PROPERTY_NAME_STR,
        GO_INNER_POSITION_PROPERTY_NAME_STR, GO_OUTER_POSITION_PROPERTY_NAME_STR };
    for (const std::wstring& name : names) {
        if (!object->haveProperty(name)) {
            continue;
        }
        // A position set in the same call as new Units is already expressed
        // in those units, whatever the name/value order: keep it and its
        // change flag so the position triplet sync still propagates it.
        if (unitsDiffer && object->hasChanged(name)) {
            continue;
        }
        std::vector<double> position = object->findVectorDoubleProperty(name);
        if (!isFourVector(position)) {
            continue;
        }
        if (pixelOrigin) {
            convertPositionPixelOrigin(previousUnits, currentUnits, position, width, height);
        } else {
            convertPosition(previousUnits, currentUnits, position, width, height);
        }
        setFourIfPresent(object, name, position);
        converted = true;
    }
    return converted;
}
//=============================================================================
void
syncChartPositionUnits(GraphicsObject* object, bool clearPositionChanged)
{
    if (object == nullptr || !object->haveProperty(GO_UNITS_PROPERTY_NAME_STR)) {
        return;
    }
    if (object->hasChanged(GO_UNITS_PROPERTY_NAME_STR)) {
        auto* unitsProperty = dynamic_cast<GOUnitsProperty*>(
            object->findProperty(GO_UNITS_PROPERTY_NAME_STR, false));
        if (unitsProperty != nullptr) {
            std::wstring previousUnits = unitsProperty->getPreviousUnits();
            std::wstring currentUnits = unitsProperty->data();
            double width = 0;
            double height = 0;
            axesParentLogicalSizeInPixels(object, width, height);
            // pixel origin 1: the stored value is the shown value in pixel units.
            syncPositionUnitsTriplet(object, previousUnits, currentUnits, width, height, true);
        }
        object->clearChanged(GO_UNITS_PROPERTY_NAME_STR);
    }
    // drop stale Position flags so a later Units change still converts.
    if (!clearPositionChanged) {
        return;
    }
    const std::wstring names[] = { GO_POSITION_PROPERTY_NAME_STR,
        GO_INNER_POSITION_PROPERTY_NAME_STR, GO_OUTER_POSITION_PROPERTY_NAME_STR };
    for (const std::wstring& name : names) {
        if (object->haveProperty(name)) {
            object->clearChanged(name);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
