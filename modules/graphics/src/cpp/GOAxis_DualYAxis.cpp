//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// GOAxis dual-Y-axis / secondary-ruler methods, split out of GOAxis.cpp to
// reduce that translation unit. Behaviour is unchanged: these are the same
// GOAxis member definitions, moved verbatim.
//=============================================================================
#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <math.h>
#include <sstream>
#include <unordered_set>
#include "nlsBuildConfig.h"
#include "GOFontSpecHelpers.hpp"
#include "GOChartDecorators.hpp"
#include "omp_for_loop.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAxesToolbar.hpp"
#include "GOAxis.hpp"
#include "GOFigureHost.hpp"
#include "RenderWeb.hpp"
#include "GOPositionHelpers.hpp"
#include "GOUIComponentUnits.hpp"
#include "GOAxisHelpers.hpp"
#include "GOSurfaceScale.hpp"
#include "GOPositionProperty.hpp"
#include "GOAlignVertProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOInOutProperty.hpp"
#include "GOProjectionModeProperty.hpp"
#include "GONextPlotModeProperty.hpp"
#include "GOLinearLogProperty.hpp"
#include "GOLineStyleOrderProperty.hpp"
#include "GONormalReverseProperty.hpp"
#include "GOLeftRightProperty.hpp"
#include "GOTopBottomProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOColorVectorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOCategoricalVectorProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOVectorSixDoubleProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorThreeDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "RenderHelpers.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GOList.hpp"
#include "GOFigure.hpp"
#include "GOText.hpp"
#include "GORuler.hpp"
#include "GOHelpers.hpp"
#include "GOUIAxes.hpp"
#include "GORoot.hpp"
#include "GOAxesLayoutManager.hpp"
#include "GOLayoutOptions.hpp"
#include "RenderHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "TexToUnicode.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
GOAxis::isDualYAxis()
{
    return stringCheck(GO_YY_AXIS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
void
GOAxis::swapSecondaryYState()
{
    // Exchange the active (canonical) Y ruler state with the stashed secondary
    // side. The exchange goes through the low-level accessors so it never looks
    // like a user edit (which would flip YLimMode/YTickMode to manual): every
    // touched property has its changed flag cleared afterwards.
    static const wchar_t* valuePairs[][2] = {
        { GO_Y_LIM_PROPERTY_NAME_STR, GO_Y_LIM_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_Y_LIM_MODE_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_COLOR_PROPERTY_NAME_STR, GO_Y_COLOR_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_SCALE_PROPERTY_NAME_STR, GO_Y_SCALE_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_DIR_PROPERTY_NAME_STR, GO_Y_DIR_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_TICK_PROPERTY_NAME_STR, GO_Y_TICK_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_TICK_MODE_PROPERTY_NAME_STR, GO_Y_TICK_MODE_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_TICK_LABEL_PROPERTY_NAME_STR, GO_Y_TICK_LABEL_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR,
            GO_Y_TICK_LABEL_MODE_SECONDARY_PROPERTY_NAME_STR },
        { GO_COLOR_ORDER_INDEX_PROPERTY_NAME_STR,
            GO_COLOR_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR },
        { GO_LINE_STYLE_ORDER_INDEX_PROPERTY_NAME_STR,
            GO_LINE_STYLE_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR },
    };
    for (auto& pr : valuePairs) {
        GOGenericProperty* pa = findProperty(pr[0], false);
        GOGenericProperty* pb = findProperty(pr[1], false);
        if (!pa || !pb) {
            continue;
        }
        ArrayOf va = pa->get();
        ArrayOf vb = pb->get();
        pa->set(vb);
        pb->set(va);
        clearChanged(pr[0]);
        clearChanged(pr[1]);
    }
    // Handle lists go through the typed accessor so no reparenting is triggered.
    static const wchar_t* handlePairs[][2] = {
        { GO_CHILDREN_PROPERTY_NAME_STR, GO_CHILDREN_SECONDARY_PROPERTY_NAME_STR },
        { GO_Y_LABEL_PROPERTY_NAME_STR, GO_Y_LABEL_SECONDARY_PROPERTY_NAME_STR },
    };
    for (auto& pr : handlePairs) {
        GOGObjectsProperty* pa = static_cast<GOGObjectsProperty*>(findProperty(pr[0], false));
        GOGObjectsProperty* pb = static_cast<GOGObjectsProperty*>(findProperty(pr[1], false));
        if (!pa || !pb) {
            continue;
        }
        std::vector<int64> tmp = pa->data();
        pa->data(pb->data());
        pb->data(tmp);
        clearChanged(pr[0]);
        clearChanged(pr[1]);
    }
}
//=============================================================================
void
GOAxis::enableDualYAxis()
{
    if (isDualYAxis()) {
        return;
    }
    // The left ruler takes the first ColorOrder entry, the right ruler the second.
    std::vector<double> co = findVectorDoubleProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR);
    if (co.size() >= 6) {
        setThreeVectorDefault(GO_Y_COLOR_PROPERTY_NAME_STR, co[0], co[1], co[2]);
        clearChanged(GO_Y_COLOR_PROPERTY_NAME_STR);
        setThreeVectorDefault(GO_Y_COLOR_SECONDARY_PROPERTY_NAME_STR, co[3], co[4], co[5]);
        clearChanged(GO_Y_COLOR_SECONDARY_PROPERTY_NAME_STR);
    }
    // The stashed side starts empty, auto-scaled, and cycling from the second color.
    GOGObjectsProperty* sc
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_SECONDARY_PROPERTY_NAME_STR));
    sc->data(std::vector<int64>());
    setRestrictedStringDefault(
        GO_Y_LIM_MODE_SECONDARY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarPositiveIntegerValueDefault(GO_COLOR_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR, 2.0);
    clearChanged(GO_COLOR_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR);
    setRestrictedStringDefault(GO_YY_AXIS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    clearChanged(GO_YY_AXIS_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOAxis::disableDualYAxis()
{
    if (!isDualYAxis()) {
        return;
    }
    // Return to a single left ruler; the stashed side is discarded (the reset /
    // clear that triggers this also blanks the surviving side).
    setRestrictedStringDefault(GO_YY_AXIS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    clearChanged(GO_YY_AXIS_PROPERTY_NAME_STR);
    setRestrictedStringDefault(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    clearChanged(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);
    static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_SECONDARY_PROPERTY_NAME_STR))
        ->data(std::vector<int64>());
    static_cast<GOGObjectsProperty*>(findProperty(GO_Y_LABEL_SECONDARY_PROPERTY_NAME_STR))
        ->data(std::vector<int64>());
    ensureYAxisRulers();
}
//=============================================================================
void
GOAxis::setActiveYSide(const std::wstring& side)
{
    std::wstring requested = (side == GO_PROPERTY_VALUE_RIGHT_STR) ? GO_PROPERTY_VALUE_RIGHT_STR
                                                                   : GO_PROPERTY_VALUE_LEFT_STR;
    std::wstring current = findStringProperty(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);
    if (current != requested) {
        swapSecondaryYState();
        setRestrictedStringDefault(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, requested);
        clearChanged(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOAxis::updateSecondaryYLimits()
{
    // Compute the stashed side's auto limits and ticks by briefly making it the
    // canonical ruler and reusing the standard single-sided machinery.
    swapSecondaryYState();
    std::vector<double> savedDataLimits
        = findVectorDoubleProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
    bool yAuto = isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    updateLimits(false, yAuto, false, false, false);
    // Only the Y ruler differs between the two sides; restore the shared extents.
    if (savedDataLimits.size() >= 6) {
        GOSixVectorProperty* dl
            = static_cast<GOSixVectorProperty*>(findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR));
        dl->value(savedDataLimits[0], savedDataLimits[1], savedDataLimits[2], savedDataLimits[3],
            savedDataLimits[4], savedDataLimits[5]);
    }
    recalculateTicks();
    swapSecondaryYState();
}
//=============================================================================
void
GOAxis::drawSecondaryYRuler(RenderInterface& gc)
{
    // Render the stashed ruler and its children by swapping it in as canonical,
    // flipping the drawn edge, redrawing, then restoring the active side.
    std::wstring activeSide = findStringProperty(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);
    std::wstring otherSide = (activeSide == GO_PROPERTY_VALUE_RIGHT_STR)
        ? GO_PROPERTY_VALUE_LEFT_STR
        : GO_PROPERTY_VALUE_RIGHT_STR;
    swapSecondaryYState();
    setRestrictedStringDefault(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, otherSide);
    clearChanged(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);

    setupAxis(gc);
    setupProjection(gc, false);
    drawChildren(gc);
    drawAxisLines(gc);
    drawTickMarks(gc);
    drawAxisLabels(gc);

    setRestrictedStringDefault(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, activeSide);
    clearChanged(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);
    swapSecondaryYState();
    setupAxis(gc);
    setupProjection(gc, false);
}
//=============================================================================
std::wstring
GOAxis::axesPropertyForRulerProperty(int axisKind, int side, const std::wstring& rulerProperty)
{
    if (axisKind == RULER_AXIS_X || axisKind == RULER_AXIS_Z) {
        // X and Z are single-sided: map the ruler property straight to the
        // matching axes X*/Z* property.
        struct SingleAxisPair
        {
            const wchar_t* ruler;
            const wchar_t* xName;
            const wchar_t* zName;
        };
        static const SingleAxisPair singlePairs[] = {
            { GO_LIMITS_PROPERTY_NAME_STR, GO_X_LIM_PROPERTY_NAME_STR, GO_Z_LIM_PROPERTY_NAME_STR },
            { GO_LIMITS_MODE_PROPERTY_NAME_STR, GO_X_LIM_MODE_PROPERTY_NAME_STR,
                GO_Z_LIM_MODE_PROPERTY_NAME_STR },
            { GO_COLOR_PROPERTY_NAME_STR, GO_X_COLOR_PROPERTY_NAME_STR,
                GO_Z_COLOR_PROPERTY_NAME_STR },
            { GO_SCALE_PROPERTY_NAME_STR, GO_X_SCALE_PROPERTY_NAME_STR,
                GO_Z_SCALE_PROPERTY_NAME_STR },
            { GO_DIRECTION_PROPERTY_NAME_STR, GO_X_DIR_PROPERTY_NAME_STR,
                GO_Z_DIR_PROPERTY_NAME_STR },
            { GO_TICK_VALUES_PROPERTY_NAME_STR, GO_X_TICK_PROPERTY_NAME_STR,
                GO_Z_TICK_PROPERTY_NAME_STR },
            { GO_TICK_VALUES_MODE_PROPERTY_NAME_STR, GO_X_TICK_MODE_PROPERTY_NAME_STR,
                GO_Z_TICK_MODE_PROPERTY_NAME_STR },
            { GO_TICK_LABELS_PROPERTY_NAME_STR, GO_X_TICK_LABEL_PROPERTY_NAME_STR,
                GO_Z_TICK_LABEL_PROPERTY_NAME_STR },
            { GO_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR,
                GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR },
            { GO_LABEL_PROPERTY_NAME_STR, GO_X_LABEL_PROPERTY_NAME_STR,
                GO_Z_LABEL_PROPERTY_NAME_STR },
            // Font is shared by the axes (not per-axis): X and Z rulers both
            // mirror the same axes Font* property.
            { GO_FONT_SIZE_PROPERTY_NAME_STR, GO_FONT_SIZE_PROPERTY_NAME_STR,
                GO_FONT_SIZE_PROPERTY_NAME_STR },
            { GO_FONT_NAME_PROPERTY_NAME_STR, GO_FONT_NAME_PROPERTY_NAME_STR,
                GO_FONT_NAME_PROPERTY_NAME_STR },
            { GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_FONT_WEIGHT_PROPERTY_NAME_STR,
                GO_FONT_WEIGHT_PROPERTY_NAME_STR },
            { GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_FONT_ANGLE_PROPERTY_NAME_STR,
                GO_FONT_ANGLE_PROPERTY_NAME_STR },
            // The ruler's TickDirection mirrors the shared axes TickDir.
            { GO_TICK_DIRECTION_PROPERTY_NAME_STR, GO_TICK_DIR_PROPERTY_NAME_STR,
                GO_TICK_DIR_PROPERTY_NAME_STR },
            // TickLength is shared by the axes (not per-axis): rulers mirror it directly.
            { GO_TICK_LENGTH_PROPERTY_NAME_STR, GO_TICK_LENGTH_PROPERTY_NAME_STR,
                GO_TICK_LENGTH_PROPERTY_NAME_STR },
            // These are shared axes state the plotly export reads on a ruler when it
            // stands in as axisData; they mirror the identically named axes property.
            { GO_FONT_UNITS_PROPERTY_NAME_STR, GO_FONT_UNITS_PROPERTY_NAME_STR,
                GO_FONT_UNITS_PROPERTY_NAME_STR },
            { GO_LINE_WIDTH_PROPERTY_NAME_STR, GO_LINE_WIDTH_PROPERTY_NAME_STR,
                GO_LINE_WIDTH_PROPERTY_NAME_STR },
            { GO_TICK_DIR_PROPERTY_NAME_STR, GO_TICK_DIR_PROPERTY_NAME_STR,
                GO_TICK_DIR_PROPERTY_NAME_STR },
            { GO_GRID_COLOR_PROPERTY_NAME_STR, GO_GRID_COLOR_PROPERTY_NAME_STR,
                GO_GRID_COLOR_PROPERTY_NAME_STR },
            { GO_GRID_ALPHA_PROPERTY_NAME_STR, GO_GRID_ALPHA_PROPERTY_NAME_STR,
                GO_GRID_ALPHA_PROPERTY_NAME_STR },
            { GO_BOX_PROPERTY_NAME_STR, GO_BOX_PROPERTY_NAME_STR, GO_BOX_PROPERTY_NAME_STR },
            { GO_UNITS_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR },
            // The ruler's date/datetime TickLabelFormat maps to the matching axes
            // X*/Z* TickLabelFormat.
            { GO_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, GO_X_TICK_LABEL_FORMAT_PROPERTY_NAME_STR,
                GO_Z_TICK_LABEL_FORMAT_PROPERTY_NAME_STR },
        };
        for (const SingleAxisPair& p : singlePairs) {
            if (rulerProperty == p.ruler) {
                return (axisKind == RULER_AXIS_X) ? p.xName : p.zName;
            }
        }
        return std::wstring();
    }
    // side 1 == left, side 2 == right. The active side lives in the canonical Y
    // properties; the other side in the *_Secondary stash.
    int canonicalSide
        = stringCheck(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RIGHT_STR) ? 2 : 1;
    bool canonical = (side == canonicalSide);
    struct RulerAxisPair
    {
        const wchar_t* ruler;
        const wchar_t* canon;
        const wchar_t* secondary;
    };
    static const RulerAxisPair pairs[] = {
        { GO_LIMITS_PROPERTY_NAME_STR, GO_Y_LIM_PROPERTY_NAME_STR,
            GO_Y_LIM_SECONDARY_PROPERTY_NAME_STR },
        { GO_LIMITS_MODE_PROPERTY_NAME_STR, GO_Y_LIM_MODE_PROPERTY_NAME_STR,
            GO_Y_LIM_MODE_SECONDARY_PROPERTY_NAME_STR },
        { GO_COLOR_PROPERTY_NAME_STR, GO_Y_COLOR_PROPERTY_NAME_STR,
            GO_Y_COLOR_SECONDARY_PROPERTY_NAME_STR },
        { GO_SCALE_PROPERTY_NAME_STR, GO_Y_SCALE_PROPERTY_NAME_STR,
            GO_Y_SCALE_SECONDARY_PROPERTY_NAME_STR },
        { GO_DIRECTION_PROPERTY_NAME_STR, GO_Y_DIR_PROPERTY_NAME_STR,
            GO_Y_DIR_SECONDARY_PROPERTY_NAME_STR },
        { GO_TICK_VALUES_PROPERTY_NAME_STR, GO_Y_TICK_PROPERTY_NAME_STR,
            GO_Y_TICK_SECONDARY_PROPERTY_NAME_STR },
        { GO_TICK_VALUES_MODE_PROPERTY_NAME_STR, GO_Y_TICK_MODE_PROPERTY_NAME_STR,
            GO_Y_TICK_MODE_SECONDARY_PROPERTY_NAME_STR },
        { GO_TICK_LABELS_PROPERTY_NAME_STR, GO_Y_TICK_LABEL_PROPERTY_NAME_STR,
            GO_Y_TICK_LABEL_SECONDARY_PROPERTY_NAME_STR },
        { GO_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR,
            GO_Y_TICK_LABEL_MODE_SECONDARY_PROPERTY_NAME_STR },
        { GO_LABEL_PROPERTY_NAME_STR, GO_Y_LABEL_PROPERTY_NAME_STR,
            GO_Y_LABEL_SECONDARY_PROPERTY_NAME_STR },
        // Font is not stashed per Y side: both sides mirror the shared axes font.
        { GO_FONT_SIZE_PROPERTY_NAME_STR, GO_FONT_SIZE_PROPERTY_NAME_STR,
            GO_FONT_SIZE_PROPERTY_NAME_STR },
        { GO_FONT_NAME_PROPERTY_NAME_STR, GO_FONT_NAME_PROPERTY_NAME_STR,
            GO_FONT_NAME_PROPERTY_NAME_STR },
        { GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_FONT_WEIGHT_PROPERTY_NAME_STR,
            GO_FONT_WEIGHT_PROPERTY_NAME_STR },
        { GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_FONT_ANGLE_PROPERTY_NAME_STR,
            GO_FONT_ANGLE_PROPERTY_NAME_STR },
        // TickDirection mirrors the shared axes TickDir (not stashed per side).
        { GO_TICK_DIRECTION_PROPERTY_NAME_STR, GO_TICK_DIR_PROPERTY_NAME_STR,
            GO_TICK_DIR_PROPERTY_NAME_STR },
        // TickLength is shared by the axes (not stashed per side).
        { GO_TICK_LENGTH_PROPERTY_NAME_STR, GO_TICK_LENGTH_PROPERTY_NAME_STR,
            GO_TICK_LENGTH_PROPERTY_NAME_STR },
        // Shared axes state (not stashed per side); the ruler mirrors the axes
        // property of the same name so the plotly yyaxis export can read it.
        { GO_FONT_UNITS_PROPERTY_NAME_STR, GO_FONT_UNITS_PROPERTY_NAME_STR,
            GO_FONT_UNITS_PROPERTY_NAME_STR },
        { GO_LINE_WIDTH_PROPERTY_NAME_STR, GO_LINE_WIDTH_PROPERTY_NAME_STR,
            GO_LINE_WIDTH_PROPERTY_NAME_STR },
        { GO_TICK_DIR_PROPERTY_NAME_STR, GO_TICK_DIR_PROPERTY_NAME_STR,
            GO_TICK_DIR_PROPERTY_NAME_STR },
        { GO_GRID_COLOR_PROPERTY_NAME_STR, GO_GRID_COLOR_PROPERTY_NAME_STR,
            GO_GRID_COLOR_PROPERTY_NAME_STR },
        { GO_GRID_ALPHA_PROPERTY_NAME_STR, GO_GRID_ALPHA_PROPERTY_NAME_STR,
            GO_GRID_ALPHA_PROPERTY_NAME_STR },
        { GO_BOX_PROPERTY_NAME_STR, GO_BOX_PROPERTY_NAME_STR, GO_BOX_PROPERTY_NAME_STR },
        { GO_UNITS_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR },
        // TickLabelFormat is shared axes Y state (not stashed per side); both sides
        // mirror the axes YTickLabelFormat.
        { GO_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, GO_Y_TICK_LABEL_FORMAT_PROPERTY_NAME_STR,
            GO_Y_TICK_LABEL_FORMAT_PROPERTY_NAME_STR },
    };
    for (const RulerAxisPair& p : pairs) {
        if (rulerProperty == p.ruler) {
            return canonical ? p.canon : p.secondary;
        }
    }
    return std::wstring();
}
//=============================================================================
void
GOAxis::applyRulerSideProperty(
    int axisKind, int side, const std::wstring& rulerProperty, const ArrayOf& value)
{
    std::wstring target = axesPropertyForRulerProperty(axisKind, side, rulerProperty);
    if (target.empty()) {
        return;
    }
    GOGenericProperty* prop = findProperty(target, false);
    if (!prop) {
        return;
    }
    prop->set(value);
    // Setting limits or ticks pins the matching mode to manual, as on the axes.
    auto pinMode = [&](const std::wstring& modeName) {
        if (modeName.empty()) {
            return;
        }
        setRestrictedStringDefault(modeName, GO_PROPERTY_VALUE_MANUAL_STR);
        clearChanged(modeName);
    };
    if (rulerProperty == GO_LIMITS_PROPERTY_NAME_STR) {
        pinMode(axesPropertyForRulerProperty(axisKind, side, GO_LIMITS_MODE_PROPERTY_NAME_STR));
    } else if (rulerProperty == GO_TICK_VALUES_PROPERTY_NAME_STR) {
        pinMode(
            axesPropertyForRulerProperty(axisKind, side, GO_TICK_VALUES_MODE_PROPERTY_NAME_STR));
    } else if (rulerProperty == GO_TICK_LABELS_PROPERTY_NAME_STR) {
        pinMode(
            axesPropertyForRulerProperty(axisKind, side, GO_TICK_LABELS_MODE_PROPERTY_NAME_STR));
    }
}
//=============================================================================
void
GOAxis::ensureYAxisRulers()
{
    int64 axisHandle = findGraphicsObjectHandle(this);
    if (axisHandle == -1) {
        return;
    }
    int wanted = isDualYAxis() ? 2 : 1;
    GOGObjectsProperty* yap
        = static_cast<GOGObjectsProperty*>(findProperty(GO_Y_AXIS_PROPERTY_NAME_STR));
    std::vector<int64> handles = yap->data();
    while (static_cast<int>(handles.size()) < wanted) {
        GORuler* ruler = new GORuler;
        int64 rulerHandle = assignGraphicsObject(ruler);
        ruler->linkToAxis(axisHandle, static_cast<int>(handles.size()) + 1, RULER_AXIS_Y);
        handles.push_back(rulerHandle);
    }
    if (static_cast<int>(handles.size()) > wanted) {
        handles.resize(wanted);
    }
    yap->data(handles);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
