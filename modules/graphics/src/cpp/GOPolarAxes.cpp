//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPolarAxes.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GORuler.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GONormalReverseProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Default colour for the theta/r axis lines, grid and minor grid.
static const double POLAR_AXIS_COLOR[3]
    = { 0.129411764705882, 0.129411764705882, 0.129411764705882 };
//=============================================================================
// Restricted-string dictionaries (nullptr terminated), first entry is the default.
static const wchar_t* THETA_DIR_DICT[]
    = { GO_PROPERTY_VALUE_COUNTERCLOCKWISE_STR, GO_PROPERTY_VALUE_CLOCKWISE_STR, nullptr };
static const wchar_t* THETA_ZERO_LOCATION_DICT[] = { GO_PROPERTY_VALUE_RIGHT_STR,
    GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_LEFT_STR, GO_PROPERTY_VALUE_BOTTOM_STR, nullptr };
//=============================================================================
static const wchar_t* TITLE_HORIZONTAL_ALIGNMENT_DICT[] = { GO_PROPERTY_VALUE_CENTER_STR,
    GO_PROPERTY_VALUE_LEFT_STR, GO_PROPERTY_VALUE_RIGHT_STR, nullptr };
//=============================================================================
GOPolarAxes::GOPolarAxes()
{
    constructPolarProperties();
    setupPolarDefaults();
    sortProperties();
}
//=============================================================================
GOPolarAxes::~GOPolarAxes() = default;
//=============================================================================
std::string
GOPolarAxes::getClassName()
{
    return GO_CLASS_NAME_POLAR_AXES_STR;
}
//=============================================================================
void
GOPolarAxes::constructPolarProperties()
{
    registerProperty(new GOColorProperty, GO_R_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_THETA_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_THETA_COLOR_MODE_PROPERTY_NAME_STR);

    registerProperty(new GOAutoManualProperty, GO_GRID_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MINOR_GRID_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_GRID_ALPHA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_MINOR_GRID_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MINOR_GRID_ALPHA_MODE_PROPERTY_NAME_STR);

    registerProperty(
        new GORestrictedStringProperty(THETA_DIR_DICT), GO_THETA_DIR_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(THETA_ZERO_LOCATION_DICT),
        GO_THETA_ZERO_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_R_DIR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_R_AXIS_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_AXIS_LOCATION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_R_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);

    registerProperty(new GOOnOffProperty, GO_THETA_MINOR_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_R_MINOR_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_THETA_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_R_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_THETA_MINOR_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_R_MINOR_GRID_PROPERTY_NAME_STR);

    registerProperty(new GORestrictedStringProperty(TITLE_HORIZONTAL_ALIGNMENT_DICT),
        GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);

    // RAxis / ThetaAxis: the NumericRuler viewing each polar axis, mirroring the
    // XAxis/YAxis rulers on cartesian axes. Populated lazily by updateState.
    registerProperty(new GOGObjectsProperty, GO_R_AXIS_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_THETA_AXIS_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOPolarAxes::ensurePolarRuler(const wchar_t* propName)
{
    int64 axisHandle = findGraphicsObjectHandle(this);
    if (axisHandle == -1) {
        return;
    }
    GOGObjectsProperty* ap = static_cast<GOGObjectsProperty*>(findProperty(propName));
    std::vector<int64> handles = ap->data();
    if (handles.empty()) {
        GORuler* ruler = new GORuler;
        int64 rulerHandle = assignGraphicsObject(ruler);
        ruler->linkToAxis(axisHandle, 1);
        handles.push_back(rulerHandle);
        ap->data(handles);
    }
}
//=============================================================================
void
GOPolarAxes::updateState()
{
    GOAxis::updateState();
    ensurePolarRuler(GO_R_AXIS_PROPERTY_NAME_STR);
    ensurePolarRuler(GO_THETA_AXIS_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOPolarAxes::setupPolarDefaults()
{
    // Force polar mode: this drives painting, class name and the Type property.
    setRestrictedStringDefault(GO_POLAR_AXES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POLAR_AXES_STR);

    // Polar defaults that differ from the cartesian axes.
    setThreeVectorDefault(GO_GRID_COLOR_PROPERTY_NAME_STR, POLAR_AXIS_COLOR[0], POLAR_AXIS_COLOR[1],
        POLAR_AXIS_COLOR[2]);
    setTwoVectorDefault(GO_TICK_LENGTH_PROPERTY_NAME_STR, 0, 0);
    // Theta tick labels default to degree-suffixed values (U+00B0 DEGREE SIGN).
    static_cast<GOStringVectorProperty*>(findProperty(GO_THETA_TICK_LABEL_PROPERTY_NAME_STR))
        ->data({ L"0\u00B0", L"30\u00B0", L"60\u00B0", L"90\u00B0", L"120\u00B0", L"150\u00B0",
            L"180\u00B0", L"210\u00B0", L"240\u00B0", L"270\u00B0", L"300\u00B0", L"330\u00B0" });

    setThreeVectorDefault(GO_R_COLOR_PROPERTY_NAME_STR, POLAR_AXIS_COLOR[0], POLAR_AXIS_COLOR[1],
        POLAR_AXIS_COLOR[2]);
    setRestrictedStringDefault(GO_R_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_THETA_COLOR_PROPERTY_NAME_STR, POLAR_AXIS_COLOR[0],
        POLAR_AXIS_COLOR[1], POLAR_AXIS_COLOR[2]);
    setRestrictedStringDefault(GO_THETA_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);

    setRestrictedStringDefault(GO_GRID_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR, POLAR_AXIS_COLOR[0],
        POLAR_AXIS_COLOR[1], POLAR_AXIS_COLOR[2]);
    setRestrictedStringDefault(
        GO_MINOR_GRID_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_GRID_ALPHA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_MINOR_GRID_ALPHA_PROPERTY_NAME_STR, 0.15);
    setRestrictedStringDefault(
        GO_MINOR_GRID_ALPHA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);

    setRestrictedStringDefault(
        GO_THETA_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COUNTERCLOCKWISE_STR);
    setRestrictedStringDefault(
        GO_THETA_ZERO_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RIGHT_STR);
    setRestrictedStringDefault(GO_R_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setScalarDoubleDefault(GO_R_AXIS_LOCATION_PROPERTY_NAME_STR, 80);
    setRestrictedStringDefault(
        GO_R_AXIS_LOCATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_R_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, 0);
    setRestrictedStringDefault(
        GO_R_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);

    setRestrictedStringDefault(GO_THETA_MINOR_TICK_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_R_MINOR_TICK_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_THETA_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_R_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_THETA_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_R_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);

    setRestrictedStringDefault(
        GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
