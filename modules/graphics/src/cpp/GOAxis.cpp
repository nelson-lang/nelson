//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
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
#include "GOAxisInternal.hpp"
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
#include "GOColorBar.hpp"
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
static std::vector<double>
uiAxesViewportPosition(const std::vector<double>& position, double scaleX, double scaleY);
//=============================================================================
// Auto FontSize shrinks linearly to 80% between these plot box widths (pixels, fitted).
//=============================================================================
static std::vector<double>
uiAxesInnerPositionFromOuter(const std::vector<double>& outerPosition)
{
    if (outerPosition.size() != 4) {
        return outerPosition;
    }
    double x = outerPosition[0] + UI_AXES_PROPERTY_LEFT_INSET;
    double y = outerPosition[1] + UI_AXES_PROPERTY_BOTTOM_INSET;
    double width = outerPosition[2] - UI_AXES_PROPERTY_LEFT_INSET - UI_AXES_PROPERTY_RIGHT_INSET;
    double height = outerPosition[3] - UI_AXES_PROPERTY_BOTTOM_INSET - UI_AXES_PROPERTY_TOP_INSET;
    if (width <= 1.0 || height <= 1.0) {
        return outerPosition;
    }
    return { x, y, width, height };
}
//=============================================================================
static std::vector<double>
uiAxesOuterPositionFromInner(const std::vector<double>& innerPosition)
{
    if (innerPosition.size() != 4) {
        return innerPosition;
    }
    return { innerPosition[0] - UI_AXES_PROPERTY_LEFT_INSET,
        innerPosition[1] - UI_AXES_PROPERTY_BOTTOM_INSET,
        innerPosition[2] + UI_AXES_PROPERTY_LEFT_INSET + UI_AXES_PROPERTY_RIGHT_INSET,
        innerPosition[3] + UI_AXES_PROPERTY_BOTTOM_INSET + UI_AXES_PROPERTY_TOP_INSET };
}
//=============================================================================
static std::vector<double>
axisPropertyVectorAsPixels(GOAxis* axis, const std::wstring& name)
{
    std::vector<double> position = axis->findVectorDoubleProperty(name);
    double width = 0;
    double height = 0;
    if (position.size() != 4 || !axesParentLogicalSizeInPixels(axis, width, height)) {
        return position;
    }
    std::wstring units = axis->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    convertPositionToZeroBasedPixels(units, position, width, height);
    return position;
}
//=============================================================================
static void
setAxisPropertyVectorFromPixels(
    GOAxis* axis, const std::wstring& name, const std::vector<double>& position)
{
    double width = 0;
    double height = 0;
    if (!axis || position.size() != 4 || !axesParentLogicalSizeInPixels(axis, width, height)) {
        return;
    }
    std::wstring units = axis->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    std::vector<double> convertedPosition = convertFromPixelsPixelOrigin(
        units, position[0], position[1], position[2], position[3], width, height);
    auto* property = static_cast<GOFourVectorProperty*>(axis->findProperty(name));
    property->value(
        convertedPosition[0], convertedPosition[1], convertedPosition[2], convertedPosition[3]);
    axis->clearChanged(name);
}
//=============================================================================
static bool
syncUiAxesPositionProperties(GOAxis* axis)
{
    if (!axis || !axis->usesUiAxesLayout()) {
        return false;
    }
    bool positionChanged = axis->hasChanged(GO_POSITION_PROPERTY_NAME_STR);
    bool innerPositionChanged = axis->hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
    bool outerPositionChanged = axis->hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (!positionChanged && !innerPositionChanged && !outerPositionChanged) {
        return false;
    }
    if (innerPositionChanged && !outerPositionChanged) {
        std::vector<double> innerPosition
            = axisPropertyVectorAsPixels(axis, GO_INNER_POSITION_PROPERTY_NAME_STR);
        std::vector<double> outerPosition = uiAxesOuterPositionFromInner(innerPosition);
        setAxisPropertyVectorFromPixels(axis, GO_POSITION_PROPERTY_NAME_STR, outerPosition);
        setAxisPropertyVectorFromPixels(axis, GO_OUTER_POSITION_PROPERTY_NAME_STR, outerPosition);
        setAxisPropertyVectorFromPixels(axis, GO_INNER_POSITION_PROPERTY_NAME_STR, innerPosition);
        setPositionConstraintIfPresent(axis, GO_PROPERTY_VALUE_INNERPOSITION_STR);
        return true;
    }
    std::vector<double> outerPosition = axisPropertyVectorAsPixels(axis,
        outerPositionChanged ? GO_OUTER_POSITION_PROPERTY_NAME_STR : GO_POSITION_PROPERTY_NAME_STR);
    std::vector<double> innerPosition = uiAxesInnerPositionFromOuter(outerPosition);
    setAxisPropertyVectorFromPixels(axis, GO_POSITION_PROPERTY_NAME_STR, outerPosition);
    setAxisPropertyVectorFromPixels(axis, GO_OUTER_POSITION_PROPERTY_NAME_STR, outerPosition);
    setAxisPropertyVectorFromPixels(axis, GO_INNER_POSITION_PROPERTY_NAME_STR, innerPosition);
    setPositionConstraintIfPresent(axis, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    return true;
}
//=============================================================================
static std::vector<double>
oriented3DPlotBoxAspect(double azimuth, double xyMinor, double zAspect)
{
    if (std::abs(std::cos(azimuth * M_PI / 180.)) >= std::abs(std::sin(azimuth * M_PI / 180.))) {
        return { 1., xyMinor, zAspect };
    }
    return { xyMinor, 1., zAspect };
}
//=============================================================================
static std::vector<double>
default3DPlotBoxAspect(
    double azimuth, const std::vector<double>& dataAspectRatio, double axisPixelAspect)
{
    double normalized = std::fmod(std::abs(azimuth), 180.);
    if (normalized > 90.) {
        normalized = 180. - normalized;
    }
    double theta = std::min(normalized, 90. - normalized);
    double angleWeight = std::clamp(theta / 45., 0., 1.);
    bool symmetricXY = dataAspectRatio.size() >= 3
        && std::abs(dataAspectRatio[0] - dataAspectRatio[1]) <= 1e-12
        && dataAspectRatio[2] < dataAspectRatio[0];
    if (symmetricXY) {
        double xyMinor = 0.65 + 0.35 * std::pow(angleWeight, 1.9588059931275734);
        double zAspect = 0.60 + 0.15 * std::pow(angleWeight, 1.3125122418038475);
        if (std::isfinite(axisPixelAspect) && axisPixelAspect > 0. && axisPixelAspect < 1.55) {
            double helpAspectWeight = std::exp(-std::pow((axisPixelAspect - 1.275) / 0.18, 2.));
            double xyHelpDelta = std::clamp(0.223 - 0.215 * angleWeight, 0., 0.057);
            double zHelpDelta = std::clamp(0.163 - 0.044 * angleWeight, 0., 0.131);
            xyMinor += xyHelpDelta * helpAspectWeight;
            zAspect += zHelpDelta * helpAspectWeight;
            xyMinor = std::clamp(xyMinor, 0.05, 1.0);
            zAspect = std::clamp(zAspect, 0.05, 1.0);
        }
        return oriented3DPlotBoxAspect(azimuth, xyMinor, zAspect);
    }
    double xyWeight = std::pow(angleWeight, DEFAULT_3D_PLOT_BOX_ASPECT_XY_POWER);
    double zWeight = std::pow(angleWeight, DEFAULT_3D_PLOT_BOX_ASPECT_Z_POWER);
    double xyMinor
        = DEFAULT_3D_PLOT_BOX_ASPECT_MIN + (1. - DEFAULT_3D_PLOT_BOX_ASPECT_MIN) * xyWeight;
    double zAspect = DEFAULT_3D_PLOT_BOX_ASPECT_MIN
        + (DEFAULT_3D_PLOT_BOX_ASPECT_Z_MAX - DEFAULT_3D_PLOT_BOX_ASPECT_MIN) * zWeight;
    return oriented3DPlotBoxAspect(azimuth, xyMinor, zAspect);
}
//=============================================================================
static bool
isFinitePositiveTriple(const std::vector<double>& values)
{
    return values.size() >= 3 && std::isfinite(values[0]) && std::isfinite(values[1])
        && std::isfinite(values[2]) && values[0] > 0 && values[1] > 0 && values[2] > 0;
}
//=============================================================================
struct AxisLabelState
{
    GOText* text = nullptr;
    std::vector<double> position;
    std::wstring units;
    std::wstring verticalAlignment;
    double rotation = 0;
};
//=============================================================================
static AxisLabelState
captureAxisLabelState(GOAxis* axis, const std::wstring& propertyName)
{
    AxisLabelState state;
    if (!axis) {
        return state;
    }
    auto* labelProperty = static_cast<GOGObjectsProperty*>(axis->findProperty(propertyName, false));
    if (!labelProperty || labelProperty->data().empty()) {
        return state;
    }
    state.text = dynamic_cast<GOText*>(findGraphicsObject(labelProperty->data()[0], false));
    if (!state.text) {
        return state;
    }
    state.position = state.text->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    state.units = state.text->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    state.verticalAlignment
        = state.text->findStringProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    state.rotation = state.text->findScalarDoubleProperty(GO_ROTATION_PROPERTY_NAME_STR);
    return state;
}
//=============================================================================
static void
restoreAxisLabelState(const AxisLabelState& state)
{
    if (!state.text) {
        return;
    }
    auto* positionProperty = static_cast<GOThreeVectorProperty*>(
        state.text->findProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (positionProperty && state.position.size() >= 3) {
        positionProperty->value(state.position[0], state.position[1], state.position[2]);
    }
    if (!state.units.empty()) {
        state.text->setStringDefault(GO_UNITS_PROPERTY_NAME_STR, state.units);
    }
    if (!state.verticalAlignment.empty()) {
        auto* verticalAlignmentProperty = static_cast<GOAlignVertProperty*>(
            state.text->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR));
        if (verticalAlignmentProperty) {
            verticalAlignmentProperty->data(state.verticalAlignment);
        }
    }
    auto* rotationProperty
        = static_cast<GOScalarProperty*>(state.text->findProperty(GO_ROTATION_PROPERTY_NAME_STR));
    if (rotationProperty) {
        rotationProperty->data(state.rotation);
    }
}
//=============================================================================
static double
median3(double a, double b, double c)
{
    std::array<double, 3> values = { a, b, c };
    std::sort(values.begin(), values.end());
    return values[1];
}
//=============================================================================
static int
plotBoxScaledTickCount(double axisLength, double referenceLength)
{
    if (!std::isfinite(axisLength) || !std::isfinite(referenceLength) || referenceLength <= 0) {
        return 5;
    }
    return std::clamp(static_cast<int>(std::lround(5.0 * axisLength / referenceLength)), 3, 10);
}
//=============================================================================
static void
trimTicksToLimits(std::vector<double>& tickLocations, wstringVector& tickLabels, double limitMin,
    double limitMax);
//=============================================================================
static void
clampLargeOneSidedTickAlignedPadding(const std::wstring& limitMethod, double dataMin,
    double dataMax, double& start, double& stop, std::vector<double>& tickLocations,
    wstringVector& tickLabels)
{
    if (limitMethod != GO_PROPERTY_VALUE_TICKALIGNED_STR || !std::isfinite(dataMin)
        || !std::isfinite(dataMax) || !std::isfinite(start) || !std::isfinite(stop)
        || dataMax <= dataMin) {
        return;
    }
    double span = dataMax - dataMin;
    double lowerExtra = std::max(0.0, dataMin - start) / span;
    double upperExtra = std::max(0.0, stop - dataMax) / span;
    bool changed = false;
    if (lowerExtra > 0.25 && upperExtra < 0.05) {
        start = dataMin;
        changed = true;
    } else if (upperExtra > 0.25 && lowerExtra < 0.05) {
        stop = dataMax;
        changed = true;
    }
    if (changed) {
        trimTicksToLimits(tickLocations, tickLabels, start, stop);
    }
}
//=============================================================================
static void
trimTicksToLimits(
    std::vector<double>& tickLocations, wstringVector& tickLabels, double limitMin, double limitMax)
{
    if (!std::isfinite(limitMin) || !std::isfinite(limitMax) || limitMax < limitMin) {
        return;
    }
    std::vector<double> trimmedLocations;
    wstringVector trimmedLabels;
    trimmedLocations.reserve(tickLocations.size());
    trimmedLabels.reserve(tickLabels.size());
    double tolerance = std::max(std::fabs(limitMax - limitMin) * 1e-10, 1e-12);
    for (size_t k = 0; k < tickLocations.size(); ++k) {
        double tick = tickLocations[k];
        if (tick < limitMin - tolerance || tick > limitMax + tolerance) {
            continue;
        }
        trimmedLocations.push_back(tick);
        if (k < tickLabels.size()) {
            trimmedLabels.push_back(tickLabels[k]);
        }
    }
    tickLocations.swap(trimmedLocations);
    tickLabels.swap(trimmedLabels);
}
//=============================================================================
static bool
axisHasLimitChildren(GOAxis* axis);
//=============================================================================
static void
updatePeerColorBars(GOAxis* axis);
//=============================================================================
static void
outerInsetsAroundBox(bool shared, double boxSize, double parentSize, double lowShare,
    double highShare, double tightLow, double tightHigh, double& low, double& high);
//=============================================================================
static bool
hasNoFinitePoint(const std::vector<double>& childLimits);
//=============================================================================
static void
keepBarEdges(GOAxis* axis, bool yRuler, const std::vector<double>& dataLimits, double& start,
    double& stop, std::vector<double>& ticks, std::vector<std::wstring>& labels);
//=============================================================================
static void
collectBarPositionEdges(GraphicsObject* object, bool yRuler, double& lo, double& hi, int depth);
//=============================================================================
static double
polarTitleDataYPosition(GOAxis* axis, const std::vector<double>& limits, bool aboveSubtitle);
//=============================================================================
static bool
isPolarGridObject(GraphicsObject* object);
//=============================================================================
static std::unordered_set<int64>&
polarTitlesHiddenByAxes();
//=============================================================================
static double
polarTitleTopMargin(GOAxis* axis, double boxWidth, double boxHeight);
//=============================================================================
// Theta label gap in pixels: base plus a share of the font size in pixels.
//=============================================================================
static double
polarDiscRadius(GOAxis* axis, const std::vector<double>& limits);
//=============================================================================
static double
polarPixelsPerDataUnit(GOAxis* axis, const std::vector<double>& limits);
//=============================================================================
static bool
isPolarGridText(GraphicsObject* object);
//=============================================================================
static bool
isPolarThetaLabel(GraphicsObject* object, double radius);
//=============================================================================
static void
layoutPolarGridLabels(GOAxis* axis);
//=============================================================================
static void
copyRulerProperty(GOGenericProperty* sp, GOGenericProperty* dp);
//=============================================================================
static GOText*
axisTextObject(GOAxis* axis, const std::wstring& propertyName)
{
    GOGObjectsProperty* property
        = static_cast<GOGObjectsProperty*>(axis->findProperty(propertyName, false));
    if (!property || property->data().empty()) {
        return nullptr;
    }
    GraphicsObject* object = findGraphicsObject(property->data()[0], false);
    if (!object || !object->isType(GO_PROPERTY_VALUE_TEXT_STR)) {
        return nullptr;
    }
    return static_cast<GOText*>(object);
}
//=============================================================================
static bool
axisTextHasString(GOAxis* axis, const std::wstring& propertyName)
{
    GOText* text = axisTextObject(axis, propertyName);
    if (!text) {
        return false;
    }
    return !text->findStringProperty(GO_STRING_PROPERTY_NAME_STR).empty();
}
//=============================================================================
static bool
axisHasOnlyVisibleChanged(GOAxis* axis)
{
    if (!axis || !axis->hasChanged(GO_VISIBLE_PROPERTY_NAME_STR)) {
        return false;
    }
    wstringVector names = axis->fieldnames();
    for (const std::wstring& name : names) {
        if (name == GO_VISIBLE_PROPERTY_NAME_STR) {
            continue;
        }
        GOGenericProperty* property = axis->findProperty(name, false);
        if (property && property->isModified()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static void
syncAxisLabelVisibility(GOAxis* axis)
{
    if (!axis) {
        return;
    }
    const bool isVisible
        = axis->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    const std::wstring visibleValue
        = isVisible ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR;
    const std::vector<std::wstring> labelNames = { GO_X_LABEL_PROPERTY_NAME_STR,
        GO_Y_LABEL_PROPERTY_NAME_STR, GO_Z_LABEL_PROPERTY_NAME_STR };
    for (const std::wstring& labelName : labelNames) {
        GOGObjectsProperty* labelProperty
            = static_cast<GOGObjectsProperty*>(axis->findProperty(labelName, false));
        if (!labelProperty || labelProperty->data().empty()) {
            continue;
        }
        GraphicsObject* object = findGraphicsObject(labelProperty->data()[0], false);
        if (!object) {
            continue;
        }
        GOOnOffProperty* visibleProperty = static_cast<GOOnOffProperty*>(
            object->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
        if (visibleProperty) {
            visibleProperty->data(visibleValue);
        }
    }
}
//=============================================================================
// Title, Subtitle and X/Y/ZLabel are real text objects, so ax.Title.String = 'x' works without
// calling title() first. They exist from the first updateState on, so "the property has an object"
// does not imply "there is a label to draw". Callers that reserve room or paint must use
// axisTextHasString, because getTextHeightInPixels reports a full line height whatever the string.
// Axes text fonts to sync: size/weight force the axes value, auto* reach auto modes only.
struct AxisTextFontSync
{
    bool size = false;
    bool autoSize = false;
    bool weight = false;
    bool autoWeight = false;
    bool name = false;
    bool angle = false;
};
static void
syncAxisTitleTexts(GOAxis* axis, const AxisTextFontSync& sync);
static void
syncAxisLabelTexts(GOAxis* axis, const AxisTextFontSync& sync);
static void
syncAxisLabelColors(GOAxis* axis);
static void
markAxisTextFontWritten(GOText* text, const std::wstring& name, const std::wstring& modeName);
static void
syncAxisTextFont(GOAxis* axis, GOText* text, const AxisTextFontSync& sync, double fontSize,
    const std::wstring& fontWeight);
static void
convertAxisFontUnits(GOAxis* axis);
//=============================================================================
static bool
createAxisTextSubObject(GOAxis* axis, int64 axisHandle, const std::wstring& propertyName)
{
    auto* property = static_cast<GOGObjectsProperty*>(axis->findProperty(propertyName, false));
    if (!property || !property->data().empty()) {
        return false;
    }
    GOText* text = new GOText;
    int64 textHandle = assignGraphicsObject(text);
    text->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, axisHandle);
    text->setStringDefault(GO_STRING_PROPERTY_NAME_STR, L"");
    // Consume the setup flags, so the first font sync is not blocked by them.
    text->updateState();
    std::vector<int64> handles;
    handles.push_back(textHandle);
    property->data(handles);
    // Built directly, not through GOCommonConstructorHelper, so it is never auto-parented into
    // Children (like the objects title() and xlabel() install).
    return true;
}
//=============================================================================
void
GOAxis::ensureTextSubObjects()
{
    int64 axisHandle = findGraphicsObjectHandle(this);
    if (axisHandle == -1) {
        return;
    }
    bool created = createAxisTextSubObject(this, axisHandle, GO_TITLE_PROPERTY_NAME_STR);
    created |= createAxisTextSubObject(this, axisHandle, GO_SUBTITLE_PROPERTY_NAME_STR);
    created |= createAxisTextSubObject(this, axisHandle, GO_X_LABEL_PROPERTY_NAME_STR);
    created |= createAxisTextSubObject(this, axisHandle, GO_Y_LABEL_PROPERTY_NAME_STR);
    created |= createAxisTextSubObject(this, axisHandle, GO_Z_LABEL_PROPERTY_NAME_STR);
    if (created) {
        // Runs the same font/layout sync as a label installed by title() or xlabel(), so a new
        // label is in the state that sync would produce. Otherwise it differs from the same figure
        // after a savefig/openfig round-trip.
        AxisTextFontSync sync;
        sync.size = true;
        sync.weight = true;
        sync.name = true;
        sync.angle = true;
        syncAxisTitleTexts(this, sync);
        sync.weight = false;
        syncAxisLabelTexts(this, sync);
    }
}
//=============================================================================
static std::wstring
titleTextAlignment(const std::wstring& alignment)
{
    if (alignment == GO_PROPERTY_VALUE_LEFT_STR || alignment == GO_PROPERTY_VALUE_RIGHT_STR) {
        return alignment;
    }
    return GO_PROPERTY_VALUE_CENTER_STR;
}
//=============================================================================
static double
titleTextDataXPosition(const std::wstring& alignment, const std::vector<double>& limits,
    bool defaultFull2DAxes, bool xLogScale)
{
    if (alignment == GO_PROPERTY_VALUE_LEFT_STR) {
        return limits[0];
    }
    if (alignment == GO_PROPERTY_VALUE_RIGHT_STR) {
        return limits[1];
    }
    // On a log-scaled X axis the visual centre is the geometric mean of the
    // limits, not the arithmetic one; otherwise the centred title drifts toward
    // the high-value edge (and could be clipped).
    if (xLogScale && limits[0] > 0.0 && limits[1] > 0.0) {
        return std::sqrt(limits[0] * limits[1]);
    }
    if (defaultFull2DAxes) {
        double span = limits[1] - limits[0];
        if (!std::isfinite(span) || span <= 0.0) {
            span = 1.0;
        }
        return (limits[0] + limits[1]) * 0.5 + span * 6.38457445e-7;
    }
    return (limits[0] + limits[1]) * 0.5;
}
//=============================================================================
static double
titleTextNormalizedXPosition(const std::wstring& alignment)
{
    if (alignment == GO_PROPERTY_VALUE_LEFT_STR) {
        return 0.0;
    }
    if (alignment == GO_PROPERTY_VALUE_RIGHT_STR) {
        return 1.0;
    }
    return 0.5;
}
//=============================================================================
static double
interpolateTitle3DCoefficient(
    double elevation, const std::array<std::pair<double, double>, 9>& points)
{
    double query = std::fabs(elevation);
    if (query <= points.front().first) {
        return points.front().second;
    }
    for (size_t k = 1; k < points.size(); ++k) {
        if (query <= points[k].first) {
            double lowerElevation = points[k - 1].first;
            double upperElevation = points[k].first;
            double span = upperElevation - lowerElevation;
            if (span <= 0.0) {
                return points[k].second;
            }
            double ratio = (query - lowerElevation) / span;
            return points[k - 1].second + ratio * (points[k].second - points[k - 1].second);
        }
    }
    return points.back().second;
}
//=============================================================================
static double
titleText3DHorizontalCoefficient(double elevation)
{
    static constexpr std::array<std::pair<double, double>, 9> points = { {
        { 0.0, 0.0 },
        { 10.0, 0.107108778866528 },
        { 20.0, 0.242716899049338 },
        { 30.0, 0.395833333333333 },
        { 45.0, 0.596014904522387 },
        { 60.0, 0.733873816254672 },
        { 75.0, 0.767107137386807 },
        { 82.0, 0.743135635510292 },
        { 89.0, 0.695416764518412 },
    } };
    return interpolateTitle3DCoefficient(elevation, points);
}
//=============================================================================
static double
titleText3DZCoefficient(double elevation)
{
    static constexpr std::array<std::pair<double, double>, 9> points = { {
        { 0.0, 0.005218216318785 },
        { 10.0, 0.107435187002170 },
        { 20.0, 0.166854895901920 },
        { 30.0, 0.185648568509344 },
        { 45.0, 0.096013338156025 },
        { 60.0, -0.076298658976178 },
        { 75.0, -0.294454681735658 },
        { 82.0, -0.395559317626720 },
        { 89.0, -0.487861482564192 },
    } };
    return interpolateTitle3DCoefficient(elevation, points);
}
//=============================================================================
static std::vector<double>
titleText3DPosition(GOAxis* axis, const std::wstring& alignment, const std::vector<double>& limits)
{
    double xSpan = limits[1] - limits[0];
    double ySpan = limits[3] - limits[2];
    double zSpan = limits[5] - limits[4];
    if (!std::isfinite(xSpan) || xSpan <= 0.0) {
        xSpan = 1.0;
    }
    if (!std::isfinite(ySpan) || ySpan <= 0.0) {
        ySpan = 1.0;
    }
    if (!std::isfinite(zSpan) || zSpan <= 0.0) {
        zSpan = 1.0;
    }

    double azimuth = 0.0;
    double elevation = 0.0;
    axis->getView(azimuth, elevation);
    double azimuthRadians = azimuth * M_PI / 180.0;
    double xRatio = titleTextNormalizedXPosition(alignment);
    double horizontalCoefficient = titleText3DHorizontalCoefficient(elevation);
    double zCoefficient = titleText3DZCoefficient(elevation);
    // The title rises above the box at corner-on azimuths and lowers face-on, on top of the
    // elevation curve. The offset is additive in |sin(2*az)| and independent of elevation (the az=0
    // to az=45 delta is 0.090544 at elevation 30 and 60). The elevation table is calibrated at the
    // default view azimuth, so the modulation is anchored there to keep that view exact.
    constexpr double kAzimuthZModulation = 0.090544;
    constexpr double kDefaultViewAzimuthSin2 = 0.965925826289; // |sin(2 * -37.5 deg)|
    const double azimuthSin2 = std::fabs(std::sin(2.0 * azimuthRadians));
    zCoefficient += kAzimuthZModulation * (azimuthSin2 - kDefaultViewAzimuthSin2);

    double x
        = limits[0] + xSpan * xRatio - std::sin(azimuthRadians) * xSpan * horizontalCoefficient;
    double y
        = (limits[2] + limits[3]) * 0.5 + std::cos(azimuthRadians) * ySpan * horizontalCoefficient;
    double z = limits[5] + zSpan * zCoefficient;
    return { x, y, z };
}
//=============================================================================
static double
plotBoxLogicalHeight(GOAxis* axis)
{
    std::vector<double> plotBox = axis->getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR);
    if (plotBox.size() < 4) {
        return 0.0;
    }
    // A shown figure is measured in device pixels; its Position is in logical pixels.
    double height = plotBox[3];
    GOFigure* fig = axis->getParentFigure();
    if (fig && fig->getHeight() > 0) {
        std::vector<double> figurePosition
            = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (figurePosition.size() >= 4 && figurePosition[3] > 0
            && fig->stringCheck(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR)) {
            height *= figurePosition[3] / fig->getHeight();
        }
    }
    return height;
}
//=============================================================================
static double
titleTextDataYPosition(const std::vector<double>& limits, bool hasTitle, bool hasSubtitle,
    bool defaultFull2DAxes, double titleFontSize, double plotHeightPixels)
{
    double range = limits[3] - limits[2];
    if (!std::isfinite(range) || range <= 0.0) {
        range = 1.0;
    }
    double layoutFontSize = std::isfinite(titleFontSize) ? titleFontSize : 11.0;
    // The title sits a fifth of its font size in pixels above the plot box.
    double offset = 0.0125;
    if (std::isfinite(plotHeightPixels) && plotHeightPixels > 0.0) {
        offset = 0.2 * layoutFontSize / plotHeightPixels;
    }
    if (hasSubtitle) {
        if (defaultFull2DAxes) {
            offset = 0.050572520297295 + (layoutFontSize - 11.0) * 0.000704209638702;
        } else {
            offset = hasTitle ? 0.06370192460525 : 0.06191588933594;
        }
    }
    return limits[3] + range * offset;
}
//=============================================================================
static double
subtitleTextDataYPosition(
    const std::vector<double>& limits, bool hasTitle, bool defaultFull2DAxes, double titleFontSize)
{
    double range = limits[3] - limits[2];
    if (!std::isfinite(range) || range <= 0.0) {
        range = 1.0;
    }
    double offset = hasTitle ? 0.00661057692308 : 0.00642523364486;
    if (defaultFull2DAxes) {
        double layoutFontSize = std::isfinite(titleFontSize) ? titleFontSize : 11.0;
        offset = hasTitle ? 0.005248091603053 + (layoutFontSize - 11.0) * 0.000022506435725
                          : 0.005218216318785;
    }
    return limits[3] + range * offset;
}
//=============================================================================
static bool
isDefaultFull2DAxesPosition(GOAxis* axis)
{
    std::vector<double> position(axis->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    return axis->is2D() && axis->isAuto(GO_POSITION_MODE_PROPERTY_NAME_STR) && position.size() >= 4
        && std::abs(position[0] - 0.13) < 1e-12 && std::abs(position[1] - 0.11) < 1e-12
        && std::abs(position[2] - 0.775) < 1e-12 && position[3] <= 0.815 + 1e-12
        && position[3] >= 0.75;
}
//=============================================================================
static void
markTextFontChanged(GOText* text, const std::wstring& propertyName)
{
    GOGenericProperty* property = text->findProperty(propertyName, false);
    if (property) {
        property->setModified(true);
    }
}
//=============================================================================
static void
syncAxisTitleText(GOAxis* axis, const std::wstring& propertyName, const AxisTextFontSync& sync)
{
    GOText* text = axisTextObject(axis, propertyName);
    if (!text) {
        return;
    }

    bool isTitle = propertyName == GO_TITLE_PROPERTY_NAME_STR;
    bool hasTitle = axisTextHasString(axis, GO_TITLE_PROPERTY_NAME_STR);
    bool hasSubtitle = axisTextHasString(axis, GO_SUBTITLE_PROPERTY_NAME_STR);
    std::wstring alignment
        = axis->findStringProperty(GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    double fontSize = axis->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR)
        * axis->findScalarDoubleProperty(GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR);

    std::vector<double> xLimits(axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR));
    std::vector<double> yLimits(axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR));
    std::vector<double> zLimits(axis->findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR));
    std::vector<double> limits
        = { xLimits[0], xLimits[1], yLimits[0], yLimits[1], zLimits[0], zLimits[1] };
    bool defaultFull2DAxes = isDefaultFull2DAxesPosition(axis) && axisHasLimitChildren(axis);
    bool xLogScale
        = axis->findStringProperty(GO_X_SCALE_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_LOG_STR;
    std::wstring fontWeight
        = axis->findStringProperty(isTitle ? GO_TITLE_FONT_WEIGHT_PROPERTY_NAME_STR
                                           : GO_SUBTITLE_FONT_WEIGHT_PROPERTY_NAME_STR);
    syncAxisTextFont(axis, text, sync, fontSize, fontWeight);
    GOText* titleText = axisTextObject(axis, GO_TITLE_PROPERTY_NAME_STR);
    double layoutTitleFontSize = fontSizeInPoints(axis)
        * axis->findScalarDoubleProperty(GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR);
    if (titleText) {
        double currentTitleFontSize = fontSizeInPoints(titleText);
        if (std::isfinite(currentTitleFontSize)) {
            layoutTitleFontSize = currentTitleFontSize;
        }
    }

    if (!text->isAuto(GO_POSITION_MODE_PROPERTY_NAME_STR)) {
        // A title Position set by the user is kept.
    } else if (axis->isPolarAxes() && axis->is2D()) {
        // Polar title and subtitle sit above the theta labels.
        double dataX = titleTextDataXPosition(alignment, limits, false, false);
        double dataY = polarTitleDataYPosition(axis, limits, isTitle && hasSubtitle);
        text->setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR);
        text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, dataX, dataY, 0.0);
    } else if (!axis->is2D()) {
        std::vector<double> position = titleText3DPosition(axis, alignment, limits);
        if (!isTitle) {
            position[2] -= (limits[5] - limits[4]) * 0.08;
        } else if (hasSubtitle) {
            position[2] += (limits[5] - limits[4]) * 0.08;
        }
        text->setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR);
        text->setThreeVectorDefault(
            GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2]);
    } else if (isTitle) {
        double dataX = titleTextDataXPosition(alignment, limits, defaultFull2DAxes, xLogScale);
        double dataY = titleTextDataYPosition(limits, hasTitle, hasSubtitle, defaultFull2DAxes,
            layoutTitleFontSize, plotBoxLogicalHeight(axis));
        double dataZ
            = axis->is2D() && axisHasLimitChildren(axis) ? 0.0 : (limits[4] + limits[5]) * 0.5;
        text->setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR);
        text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, dataX, dataY, dataZ);
    } else {
        double dataX = titleTextDataXPosition(alignment, limits, defaultFull2DAxes, xLogScale);
        double dataY
            = subtitleTextDataYPosition(limits, hasTitle, defaultFull2DAxes, layoutTitleFontSize);
        double dataZ
            = axis->is2D() && axisHasLimitChildren(axis) ? 0.0 : (limits[4] + limits[5]) * 0.5;
        text->setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR);
        text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, dataX, dataY, dataZ);
    }
    text->setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, titleTextAlignment(alignment));
    text->setRestrictedStringDefault(
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
}
//=============================================================================
static void
syncAxisTitleTexts(GOAxis* axis, const AxisTextFontSync& sync)
{
    syncAxisTitleText(axis, GO_TITLE_PROPERTY_NAME_STR, sync);
    syncAxisTitleText(axis, GO_SUBTITLE_PROPERTY_NAME_STR, sync);
}
//=============================================================================
void
GOAxis::syncTitleTextLayout()
{
    syncAxisTitleTexts(this, AxisTextFontSync());
}
//=============================================================================
void
GOAxis::syncPolarTitleVisibility()
{
    if (!isPolarAxes()) {
        return;
    }
    bool visible = stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    for (const std::wstring& name : { GO_TITLE_PROPERTY_NAME_STR, GO_SUBTITLE_PROPERTY_NAME_STR }) {
        GOText* text = axisTextObject(this, name);
        auto* visibleProperty = text
            ? static_cast<GOOnOffProperty*>(text->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false))
            : nullptr;
        if (!visibleProperty) {
            continue;
        }
        int64 handle = findGoProperty(name);
        std::unordered_set<int64>& hidden = polarTitlesHiddenByAxes();
        // Only texts hidden by the axes come back; one hidden by the user stays hidden.
        if (!visible && visibleProperty->asBool()) {
            visibleProperty->data(GO_PROPERTY_VALUE_OFF_STR);
            hidden.insert(handle);
        } else if (visible && hidden.erase(handle) > 0) {
            visibleProperty->data(GO_PROPERTY_VALUE_ON_STR);
        }
    }
}
//=============================================================================
static void
syncAxisLabelText(GOAxis* axis, const std::wstring& propertyName, AxisTextFontSync sync)
{
    GOText* text = axisTextObject(axis, propertyName);
    if (!text) {
        return;
    }
    // The reference sizes the ZLabel only on a 3D axes, where an auto one always follows the axes.
    if (propertyName == GO_Z_LABEL_PROPERTY_NAME_STR) {
        bool is3D = !axis->is2D();
        sync.size = sync.size && is3D;
        sync.autoSize = is3D;
    }
    double fontSize = axis->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR)
        * axis->findScalarDoubleProperty(GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR);
    syncAxisTextFont(
        axis, text, sync, fontSize, axis->findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
}
//=============================================================================
// Setting X/Y/ZColor also colors the matching axis label.
static void
syncAxisLabelColor(GOAxis* axis, const std::wstring& labelName, const std::wstring& colorName)
{
    GOText* text = axisTextObject(axis, labelName);
    GOColorProperty* color = static_cast<GOColorProperty*>(axis->findProperty(colorName, false));
    if (!text || !color || color->isNone()) {
        return;
    }
    std::vector<double> rgb = color->data();
    if (rgb.size() < 3) {
        return;
    }
    text->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, rgb[0], rgb[1], rgb[2]);
    markTextFontChanged(text, GO_COLOR_PROPERTY_NAME_STR);
    // The axis color is an automatic push, not a user set, so mark ColorMode as
    // changed (without altering its value) so updateState does not read the color
    // change as a user set and flip the label's ColorMode. This preserves an auto
    // label as auto (openfig restore) and a user-colored label as manual.
    markTextFontChanged(text, GO_COLOR_MODE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
syncAxisLabelTexts(GOAxis* axis, const AxisTextFontSync& sync)
{
    syncAxisLabelText(axis, GO_X_LABEL_PROPERTY_NAME_STR, sync);
    syncAxisLabelText(axis, GO_Y_LABEL_PROPERTY_NAME_STR, sync);
    syncAxisLabelText(axis, GO_Z_LABEL_PROPERTY_NAME_STR, sync);
}
//=============================================================================
static void
setAutoLabelRotation(GOText* text, double value)
{
    if (!text->isAuto(GO_ROTATION_MODE_PROPERTY_NAME_STR)) {
        return;
    }
    text->setScalarDoubleDefault(GO_ROTATION_PROPERTY_NAME_STR, value);
    text->clearChanged(GO_ROTATION_PROPERTY_NAME_STR);
}
//=============================================================================
static void
setAutoLabelHorizontalAlignment(GOText* text, const std::wstring& value)
{
    if (!text->isAuto(GO_HORIZONTAL_ALIGNMENT_MODE_PROPERTY_NAME_STR)) {
        return;
    }
    text->setRestrictedStringDefault(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, value);
    text->clearChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
}
//=============================================================================
static void
setAutoLabelVerticalAlignment(GOText* text, const std::wstring& value)
{
    if (!text->isAuto(GO_VERTICAL_ALIGNMENT_MODE_PROPERTY_NAME_STR)) {
        return;
    }
    text->setRestrictedStringDefault(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, value);
    text->clearChanged(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
}
//=============================================================================
// Derive an in-plane axis label's Rotation and HorizontalAlignment from its direction projected
// onto the screen (sdx to the right, sdy up). The label stays flat along its projected axis, snaps
// upright once the axis is within about 7.5 degrees of screen-vertical, and is anchored on the side
// that leaves the text reading outward from the box.
static void
inPlaneLabelOrientationFromScreen(
    double sdx, double sdy, double& rotation, std::wstring& horizontalAlignment)
{
    const double verticalSnapBand = std::tan(7.5 * M_PI / 180.0);
    if (std::fabs(sdx) < std::fabs(sdy) * verticalSnapBand) {
        rotation = 90.0;
        horizontalAlignment = GO_PROPERTY_VALUE_CENTER_STR;
    } else {
        rotation = 0.0;
        // When the axis is horizontal on screen (sdy ~ 0) the label is anchored to the right. A
        // tolerance keeps that tie stable against the tiny cos/sin residue at exact on-axis views
        // (e.g. cos(90deg)).
        horizontalAlignment
            = (sdx * sdy > 1e-9) ? GO_PROPERTY_VALUE_LEFT_STR : GO_PROPERTY_VALUE_RIGHT_STR;
    }
}
//=============================================================================
// Re-derive the x/y/z axis label Rotation and alignment from the current View, so switching between
// 2D and 3D or rotating a 3D view updates the labels. An attribute is rewritten only while its
// label is in auto mode; a label pinned by the user (mode manual) is left as is.
static void
applyAutoAxisLabelOrientation(GOAxis* axis)
{
    GOText* xText = axisTextObject(axis, GO_X_LABEL_PROPERTY_NAME_STR);
    GOText* yText = axisTextObject(axis, GO_Y_LABEL_PROPERTY_NAME_STR);
    GOText* zText = axisTextObject(axis, GO_Z_LABEL_PROPERTY_NAME_STR);
    if (!xText && !yText && !zText) {
        return;
    }

    double azimuth = 0.0;
    double elevation = 90.0;
    axis->getView(azimuth, elevation);
    const bool twoD = std::fabs(elevation - 90.0) < 1e-9;

    if (twoD) {
        if (xText) {
            setAutoLabelRotation(xText, 0.0);
            setAutoLabelHorizontalAlignment(xText, GO_PROPERTY_VALUE_CENTER_STR);
            setAutoLabelVerticalAlignment(xText, GO_PROPERTY_VALUE_TOP_STR);
        }
        if (yText) {
            setAutoLabelRotation(yText, 90.0);
            setAutoLabelHorizontalAlignment(yText, GO_PROPERTY_VALUE_CENTER_STR);
            setAutoLabelVerticalAlignment(yText, GO_PROPERTY_VALUE_BOTTOM_STR);
        }
        if (zText) {
            setAutoLabelRotation(zText, 0.0);
            setAutoLabelHorizontalAlignment(zText, GO_PROPERTY_VALUE_CENTER_STR);
            setAutoLabelVerticalAlignment(zText, GO_PROPERTY_VALUE_MIDDLE_STR);
        }
        return;
    }

    const double azimuthRadians = azimuth * M_PI / 180.0;
    const double elevationRadians = elevation * M_PI / 180.0;
    const double sinAzimuth = std::sin(azimuthRadians);
    const double cosAzimuth = std::cos(azimuthRadians);
    const double sinElevation = std::sin(elevationRadians);

    // Screen basis for the current view: right = (cos az, sin az, 0), up = (-sin el sin az, sin el
    // cos az, cos el). The projected direction of an axis anchors each label on the outward side
    // and snaps near-vertical axes upright.
    if (xText) {
        double rotation = 0.0;
        std::wstring horizontalAlignment;
        inPlaneLabelOrientationFromScreen(
            cosAzimuth, -sinElevation * sinAzimuth, rotation, horizontalAlignment);
        setAutoLabelRotation(xText, rotation);
        setAutoLabelHorizontalAlignment(xText, horizontalAlignment);
        setAutoLabelVerticalAlignment(xText, GO_PROPERTY_VALUE_TOP_STR);
    }
    if (yText) {
        double rotation = 0.0;
        std::wstring horizontalAlignment;
        inPlaneLabelOrientationFromScreen(
            sinAzimuth, sinElevation * cosAzimuth, rotation, horizontalAlignment);
        setAutoLabelRotation(yText, rotation);
        setAutoLabelHorizontalAlignment(yText, horizontalAlignment);
        setAutoLabelVerticalAlignment(yText, GO_PROPERTY_VALUE_TOP_STR);
    }
    if (zText) {
        setAutoLabelRotation(zText, 90.0);
        setAutoLabelHorizontalAlignment(zText, GO_PROPERTY_VALUE_CENTER_STR);
        setAutoLabelVerticalAlignment(zText, GO_PROPERTY_VALUE_BOTTOM_STR);
    }
}
//=============================================================================
// The view angle spans the extent that fills the axes first. A box stretched to the viewport
// ('axis equal') ties: the width then wins, so a frozen angle keeps the whole box in view.
// The tolerance absorbs the device-pixel rounding between the layout size and the on-screen
// viewport (859x517 at 125 % renders in 1074x646), which would otherwise flip the choice.
bool
GOAxis::viewAngleSpansWidth(double xSpan, double ySpan, double width, double height)
{
    return xSpan * height >= ySpan * width * (1.0 - 5e-3);
}
//=============================================================================
double
GOAxis::cameraViewAngleForSpan(double span, double distance, double fallback)
{
    if (!std::isfinite(span) || span <= 0 || !std::isfinite(distance) || distance <= 0) {
        return fallback;
    }
    double angle = 360.0 / M_PI * std::atan(span / (2.0 * distance));
    return std::isfinite(angle) && angle > 0 && angle < 180 ? angle : fallback;
}
//=============================================================================
static std::vector<double>
getVisibleTicks(const std::vector<double>& ticks, double lowerLimit, double upperLimit)
{
    std::vector<double> visibleTicks;
    visibleTicks.reserve(ticks.size());
    const double low = std::min(lowerLimit, upperLimit);
    const double high = std::max(lowerLimit, upperLimit);
    for (double tick : ticks) {
        if (std::isfinite(tick) && tick >= low && tick <= high) {
            visibleTicks.push_back(tick);
        }
    }
    return visibleTicks;
}
//=============================================================================
static std::vector<double>
getVisibleTicks(
    const std::vector<double>& ticks, double lowerLimit, double upperLimit, bool logarithmic)
{
    if (!logarithmic) {
        return getVisibleTicks(ticks, lowerLimit, upperLimit);
    }
    std::vector<double> visibleTicks;
    visibleTicks.reserve(ticks.size());
    const double low = std::min(lowerLimit, upperLimit);
    const double high = std::max(lowerLimit, upperLimit);
    for (double tick : ticks) {
        double mappedTick = tickLog(tick);
        if (std::isfinite(tick) && std::isfinite(mappedTick) && mappedTick >= low
            && mappedTick <= high) {
            visibleTicks.push_back(tick);
        }
    }
    return visibleTicks;
}
//=============================================================================
// Snaps an axis extent endpoint that is only a negligible floating-point residue away from zero
// (relative to the total span) to exactly zero. Data such as linspace(-2,2,n).^2 leaves a min like
// 1.2e-32 instead of 0; snapping keeps tight limits at [0, span] and stops the residue from
// becoming a tick that drags the axis into exponential tick labels.
static void
snapNearZeroExtent(double& lo, double& hi)
{
    double span = hi - lo;
    if (!std::isfinite(span) || span <= 0.0) {
        return;
    }
    const double tol = 1e-9 * span;
    if (lo != 0.0 && std::fabs(lo) <= tol) {
        lo = 0.0;
    }
    if (hi != 0.0 && std::fabs(hi) <= tol) {
        hi = 0.0;
    }
}
//=============================================================================
static double
pixelCenter(double value)
{
    return std::floor(value) + 0.5;
}
//=============================================================================
static void
snapLineToPixelCenters(double& x1, double& y1, double& x2, double& y2)
{
    x1 = pixelCenter(x1);
    y1 = pixelCenter(y1);
    x2 = pixelCenter(x2);
    y2 = pixelCenter(y2);
}
//=============================================================================
static void
appendSegment2D(std::vector<double>& segments, double x1, double y1, double x2, double y2)
{
    segments.push_back(x1);
    segments.push_back(y1);
    segments.push_back(x2);
    segments.push_back(y2);
}
//=============================================================================
static void
appendSegment3D(
    std::vector<double>& segments, double x1, double y1, double z1, double x2, double y2, double z2)
{
    segments.push_back(x1);
    segments.push_back(y1);
    segments.push_back(z1);
    segments.push_back(x2);
    segments.push_back(y2);
    segments.push_back(z2);
}
//=============================================================================
static double
tickRatio(double tick, double limmin, double limmax)
{
    double span = limmax - limmin;
    if (span == 0 || !std::isfinite(span)) {
        return 0;
    }
    return (tick - limmin) / span;
}
//=============================================================================
static bool
isXAxisTickSet(double unitx, double unity, double unitz)
{
    return unitx != 0 && unity == 0 && unitz == 0;
}
//=============================================================================
static bool
isYAxisTickSet(double unitx, double unity, double unitz)
{
    return unitx == 0 && unity != 0 && unitz == 0;
}
//=============================================================================
static bool
objectTreeContainsType(GraphicsObject* object, const std::wstring& type, int depth = 0)
{
    if (!object || depth > 32) {
        return false;
    }
    if (object->isType(type)) {
        return true;
    }
    auto* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 handle : children->data()) {
        if (handle < HANDLE_OFFSET_OBJECT) {
            continue;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (objectTreeContainsType(child, type, depth + 1)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
finiteExtentIsPositive(const double* values, indexType stride, indexType count)
{
    if (!values || count <= 0) {
        return false;
    }
    double low = std::numeric_limits<double>::infinity();
    double high = -std::numeric_limits<double>::infinity();
    for (indexType i = 0; i < count; ++i) {
        double value = values[i * stride];
        if (!std::isfinite(value)) {
            continue;
        }
        low = std::min(low, value);
        high = std::max(high, value);
    }
    return std::isfinite(low) && std::isfinite(high) && high > low;
}
//=============================================================================
static bool
patchHasDepthSortedGeometry(GraphicsObject* object)
{
    // A patch is depth-sorted like a surface only when it carries a genuine 3D
    // Z extent (for example a waterfall curtain, one polygon per matrix row). A
    // flat 2D fill (no Z data, or a constant Z plane) must keep the plain 2D
    // limit treatment.
    ArrayOf zData = object->findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR);
    if (!zData.isEmpty() && (zData.isNumeric() || zData.isLogical())) {
        zData.promoteType(NLS_DOUBLE);
        const double* ptr = static_cast<const double*>(zData.dataPointer());
        if (finiteExtentIsPositive(ptr, 1, static_cast<indexType>(zData.elementCount()))) {
            return true;
        }
    }
    // Vertices/Faces patches keep their geometry in the vertex matrix; the third
    // column holds Z (column-major storage).
    ArrayOf vertices = object->findArrayOfProperty(GO_VERTICES_PROPERTY_NAME_STR);
    if (!vertices.isEmpty() && (vertices.isNumeric() || vertices.isLogical())) {
        vertices.promoteType(NLS_DOUBLE);
        indexType rows = vertices.rows();
        indexType columns = vertices.columns();
        if (rows > 0 && columns >= 3) {
            const double* ptr = static_cast<const double*>(vertices.dataPointer());
            if (finiteExtentIsPositive(ptr + 2 * rows, 1, rows)) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
static bool
objectTreeContainsDepthSortedPatch(GraphicsObject* object, int depth = 0)
{
    if (!object || depth > 32) {
        return false;
    }
    if (object->isType(GO_PROPERTY_VALUE_PATCH_STR) && patchHasDepthSortedGeometry(object)) {
        return true;
    }
    auto* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 handle : children->data()) {
        if (handle < HANDLE_OFFSET_OBJECT) {
            continue;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (objectTreeContainsDepthSortedPatch(child, depth + 1)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
axisHasDepthSortedSurface(GOAxis* axis)
{
    return objectTreeContainsType(axis, GO_PROPERTY_VALUE_SURFACE_STR)
        || objectTreeContainsType(axis, GO_PROPERTY_VALUE_FUNCTION_SURFACE_STR)
        || objectTreeContainsType(axis, GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_SURFACE_STR)
        || objectTreeContainsDepthSortedPatch(axis);
}
//=============================================================================
static bool
shouldPreserveQuiverAutoDataLimits(GOAxis* axis)
{
    if (!axis) {
        return false;
    }
    if (!axis->isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR)
        || !axis->isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)) {
        return false;
    }
    if (!axis->is2D() && !axis->isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)) {
        return false;
    }
    auto* xScale = static_cast<GOLinearLogProperty*>(
        axis->findProperty(GO_X_SCALE_PROPERTY_NAME_STR, false));
    auto* yScale = static_cast<GOLinearLogProperty*>(
        axis->findProperty(GO_Y_SCALE_PROPERTY_NAME_STR, false));
    if (!xScale || !yScale || !xScale->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)
        || !yScale->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        return false;
    }
    if (!axis->is2D()) {
        if (axis->isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)
            || !axis->isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)) {
            return false;
        }
        auto* zScale = static_cast<GOLinearLogProperty*>(
            axis->findProperty(GO_Z_SCALE_PROPERTY_NAME_STR, false));
        if (!zScale || !zScale->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
            return false;
        }
    }
    return objectTreeContainsType(axis, GO_PROPERTY_VALUE_QUIVER_STR);
}
//=============================================================================
static bool
objectTreePreservesAutoDataLimits(GraphicsObject* object, int depth = 0)
{
    if (!object || depth > 32) {
        return false;
    }
    if (object->preservesAutoDataLimits()) {
        return true;
    }
    auto* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 handle : children->data()) {
        if (handle < HANDLE_OFFSET_OBJECT) {
            continue;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (objectTreePreservesAutoDataLimits(child, depth + 1)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static void
draw2DTickSet(RenderInterface& gc, bool xAxis, const std::vector<double>& ticks, double limmin,
    double limmax, double ticlen, double ticdir, double left, double bottom, double right,
    double top, bool drawLowEdge, bool drawHighEdge)
{
    double length = std::max(1.0, ticlen + (ticdir < 0 ? 1.0 : 0.0));
    std::vector<double> segments;
    segments.reserve(ticks.size() * 8);
    for (double tick : ticks) {
        double ratio = tickRatio(tick, limmin, limmax);
        if (!std::isfinite(ratio)) {
            continue;
        }
        if (xAxis) {
            double x = pixelCenter(left + ratio * (right - left));
            double yBottom = pixelCenter(bottom + ticdir * length);
            double yTop = pixelCenter(top - ticdir * length);
            if (drawLowEdge) {
                appendSegment2D(segments, x, bottom, x, yBottom);
            }
            if (drawHighEdge) {
                appendSegment2D(segments, x, top, x, yTop);
            }
        } else {
            double y = pixelCenter(bottom + ratio * (top - bottom));
            double xLeft = pixelCenter(left + ticdir * length);
            double xRight = pixelCenter(right - ticdir * length);
            if (drawLowEdge) {
                appendSegment2D(segments, left, y, xLeft, y);
            }
            if (drawHighEdge) {
                appendSegment2D(segments, right, y, xRight, y);
            }
        }
    }
    gc.lineSegments2D(segments);
}
//=============================================================================
static void
projected2DPlotBox(RenderInterface& gc, const std::vector<double>& limits, double& left,
    double& bottom, double& right, double& top)
{
    std::array<double, 4> xs;
    std::array<double, 4> ys;
    gc.toPixels(limits[0], limits[2], limits[4], xs[0], ys[0]);
    gc.toPixels(limits[1], limits[2], limits[4], xs[1], ys[1]);
    gc.toPixels(limits[1], limits[3], limits[4], xs[2], ys[2]);
    gc.toPixels(limits[0], limits[3], limits[4], xs[3], ys[3]);
    auto xMinMax = std::minmax_element(xs.begin(), xs.end());
    auto yMinMax = std::minmax_element(ys.begin(), ys.end());
    left = pixelCenter(*xMinMax.first);
    right = pixelCenter(*xMinMax.second);
    bottom = pixelCenter(*yMinMax.first);
    top = pixelCenter(*yMinMax.second);
}
//=============================================================================
static std::wstring
tickExponentLabel(int exponent, TEXT_INTERPRETER_FORMAT textFormat)
{
    if (exponent == 0) {
        return L"";
    }
    std::wstring label = L"×10^{" + std::to_wstring(exponent) + L"}";
    return textFormat == TEXT_INTERPRETER_FORMAT::TEX_MARKUP ? texToUnicode(label) : label;
}
//=============================================================================
static void
draw2DTickExponent(RenderInterface& gc, bool xAxis, int exponent,
    TEXT_INTERPRETER_FORMAT textFormat, const std::vector<double>& color,
    const std::vector<double>& limits, const FontSpec& font)
{
    std::wstring label = tickExponentLabel(exponent, textFormat);
    if (label.empty()) {
        return;
    }
    double left;
    double bottom;
    double right;
    double top;
    projected2DPlotBox(gc, limits, left, bottom, right, top);
    gc.setupDirectDraw();
    if (xAxis) {
        gc.putText(right, bottom - 24.0, label, color, RenderInterface::Max, RenderInterface::Max,
            font, 0);
    } else {
        gc.putText(
            left, top + 4.0, label, color, RenderInterface::Min, RenderInterface::Min, font, 0);
    }
    gc.releaseDirectDraw();
}
//=============================================================================
static std::vector<double>
withAlpha(std::vector<double> color, double alpha, const std::vector<double>& background)
{
    if (color.size() < 3 || alpha >= 1.0) {
        return color;
    }
    std::vector<double> base = background;
    if (base.size() < 3 || base[0] < 0) {
        base = { 1.0, 1.0, 1.0 };
    }
    color[0] = color[0] * alpha + base[0] * (1.0 - alpha);
    color[1] = color[1] * alpha + base[1] * (1.0 - alpha);
    color[2] = color[2] * alpha + base[2] * (1.0 - alpha);
    color.resize(4, 1.0);
    return color;
}
//=============================================================================
static double
niceLinearStep(double span, double targetIntervals)
{
    if (!std::isfinite(span) || span <= 0 || targetIntervals <= 0) {
        return 0;
    }
    double rawStep = span / targetIntervals;
    double base = std::pow(10.0, std::floor(std::log10(rawStep)));
    double fraction = rawStep / base;
    double multiplier = 10.0;
    if (fraction <= 1.5) {
        multiplier = 1.0;
    } else if (fraction <= 3.0) {
        multiplier = 2.0;
    } else if (fraction <= 7.0) {
        multiplier = 5.0;
    }
    return multiplier * base;
}
//=============================================================================
static void
thinDenseLinearTicks(
    double& start, double& stop, std::vector<double>& ticks, double targetIntervals)
{
    if (ticks.size() <= 7 || !std::isfinite(start) || !std::isfinite(stop) || stop <= start) {
        return;
    }
    double step = niceLinearStep(stop - start, targetIntervals);
    if (!std::isfinite(step) || step <= 0) {
        return;
    }
    double nextStart = std::floor(start / step) * step;
    double nextStop = std::ceil(stop / step) * step;
    std::vector<double> nextTicks;
    for (double value = nextStart; value <= nextStop + std::fabs(step) * 1e-10; value += step) {
        double tick = value;
        if (std::fabs(tick) < std::fabs(step) * 1e-10) {
            tick = 0;
        }
        nextTicks.push_back(tick);
        if (nextTicks.size() > 1000) {
            break;
        }
    }
    if (nextTicks.size() >= 2 && nextTicks.size() < ticks.size()) {
        start = nextStart;
        stop = nextStop;
        ticks = nextTicks;
    }
}
//=============================================================================
static bool
densifyInteriorLinearTicks(
    double start, double stop, double targetIntervals, std::vector<double>& ticks)
{
    if (!std::isfinite(start) || !std::isfinite(stop) || stop <= start) {
        return false;
    }
    double step = niceLinearStep(stop - start, targetIntervals);
    if (!std::isfinite(step) || step <= 0) {
        return false;
    }
    double first = std::ceil(start / step) * step;
    double last = std::floor(stop / step) * step;
    std::vector<double> nextTicks;
    for (double value = first; value <= last + std::fabs(step) * 1e-10; value += step) {
        double tick = std::fabs(value) <= std::fabs(step) * 1e-10 ? 0.0 : value;
        nextTicks.push_back(tick);
        if (nextTicks.size() > 1000) {
            break;
        }
    }
    if (nextTicks.size() >= 2 && nextTicks.size() != ticks.size()) {
        ticks = nextTicks;
        return true;
    }
    return false;
}
//=============================================================================
static double
degreesToRadians(double degrees)
{
    return degrees * M_PI / 180.0;
}
//=============================================================================
static double
radiansToDegrees(double radians)
{
    return radians * 180.0 / M_PI;
}
//=============================================================================
static std::vector<double>
convertThetaUnits(const std::vector<double>& values, bool toRadians)
{
    std::vector<double> converted(values);
    for (double& value : converted) {
        value = toRadians ? degreesToRadians(value) : radiansToDegrees(value);
    }
    return converted;
}
//=============================================================================
static std::wstring
formatNumericLabel(double value, bool appendDegree)
{
    double rounded = std::round(value);
    std::wstring label;
    if (std::fabs(value - rounded) < 1e-12) {
        label = std::to_wstring(static_cast<long long>(rounded));
    } else {
        std::wostringstream stream;
        stream << std::setprecision(12) << value;
        label = stream.str();
    }
    if (appendDegree) {
        label += static_cast<wchar_t>(176);
    }
    return label;
}
//=============================================================================
static int
gcdInt(int a, int b)
{
    a = std::abs(a);
    b = std::abs(b);
    while (b != 0) {
        int t = a % b;
        a = b;
        b = t;
    }
    return a == 0 ? 1 : a;
}
//=============================================================================
static std::wstring
formatPiLabel(int numerator, int denominator)
{
    if (numerator == 0) {
        return L"0";
    }
    int divisor = gcdInt(numerator, denominator);
    numerator /= divisor;
    denominator /= divisor;
    std::wstring sign = numerator < 0 ? L"-" : L"";
    int absNumerator = std::abs(numerator);
    std::wstring coefficient = absNumerator == 1 ? L"" : std::to_wstring(absNumerator);
    if (denominator == 1) {
        return sign + coefficient + L"\\pi";
    }
    return sign + coefficient + L"\\pi/" + std::to_wstring(denominator);
}
//=============================================================================
static std::wstring
formatThetaTickLabel(double value, bool radians)
{
    if (!radians) {
        return formatNumericLabel(value, true);
    }
    double ratio = value / M_PI;
    for (int denominator = 1; denominator <= 12; ++denominator) {
        int numerator = static_cast<int>(std::llround(ratio * denominator));
        if (std::fabs(ratio - static_cast<double>(numerator) / denominator) < 1e-10) {
            return formatPiLabel(numerator, denominator);
        }
    }
    return formatNumericLabel(value, false);
}
//=============================================================================
static bool
isFullCircleThetaSpan(const std::vector<double>& thetaLim, bool radians)
{
    if (thetaLim.size() < 2) {
        return true;
    }
    double fullCircle = radians ? (2.0 * M_PI) : 360.0;
    return std::fabs(std::fabs(thetaLim[1] - thetaLim[0]) - fullCircle) < 1e-12;
}
//=============================================================================
static wstringVector
formatThetaTickLabels(
    const std::vector<double>& ticks, const std::vector<double>& thetaLim, bool radians)
{
    std::vector<double> labelTicks(ticks);
    if (isFullCircleThetaSpan(thetaLim, radians) && labelTicks.size() > 1
        && std::fabs(labelTicks.back() - thetaLim[1]) < 1e-12) {
        labelTicks.pop_back();
    }
    wstringVector labels;
    labels.reserve(labelTicks.size());
    for (double tick : labelTicks) {
        labels.push_back(formatThetaTickLabel(tick, radians));
    }
    return labels;
}
//=============================================================================
static bool
objectHasLimits(GraphicsObject* object)
{
    if (!object) {
        return false;
    }
    if (object->isType(GO_PROPERTY_VALUE_HGGROUP_STR)
        || object->isType(GO_PROPERTY_VALUE_HGTRANSFORM_STR)
        || object->isType(GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR)
        || object->isType(GO_PROPERTY_VALUE_HEATMAP_STR)
        || object->isType(GO_PROPERTY_VALUE_PARALLEL_PLOT_STR)) {
        auto* children
            = static_cast<GOGObjectsProperty*>(object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        for (int64 handle : children->data()) {
            if (objectHasLimits(findGraphicsObject(handle, false))) {
                return true;
            }
        }
        return false;
    }
    std::vector<double> limits(object->getLimits());
    return !limits.empty() && !hasNoFinitePoint(limits);
}
//=============================================================================
static bool
axisHasLimitChildren(GOAxis* axis)
{
    auto* children
        = static_cast<GOGObjectsProperty*>(axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    for (int64 handle : children->data()) {
        if (objectHasLimits(findGraphicsObject(handle, false))) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
isFullCircleThetaSpan(const std::vector<double>& thetaLim)
{
    if (thetaLim.size() < 2) {
        return true;
    }
    return std::fabs(std::fabs(thetaLim[1] - thetaLim[0]) - 360.0) < 1e-12;
}
//=============================================================================
static const wchar_t* THETA_AXIS_UNITS_DICT[3]
    = { GO_PROPERTY_VALUE_DEGREES_STR, GO_PROPERTY_VALUE_RADIANS_STR, 0 };
//=============================================================================
class GOThetaAxisUnitsProperty : public GORestrictedStringProperty
{
public:
    GOThetaAxisUnitsProperty() : GORestrictedStringProperty(THETA_AXIS_UNITS_DICT) { }
    ~GOThetaAxisUnitsProperty() override = default;
};
//=============================================================================
std::wstring
GOAxis::getType()
{
    return GO_PROPERTY_VALUE_AXES_STR;
}
//=============================================================================
std::string
GOAxis::getClassName()
{
    return isPolarAxes() ? GO_CLASS_NAME_POLAR_AXES_STR : GO_CLASS_NAME_AXES_STR;
}
//=============================================================================
static bool
isListedProperty(const std::wstring& name, const std::vector<std::wstring>& propertyNames)
{
    return std::find(propertyNames.begin(), propertyNames.end(), name) != propertyNames.end();
}
//=============================================================================
bool
GOAxis::isPolarAxesOnlyProperty(const std::wstring& name)
{
    static const std::vector<std::wstring> propertyNames = { GO_R_LIM_PROPERTY_NAME_STR,
        GO_R_LIM_MODE_PROPERTY_NAME_STR, GO_R_TICK_PROPERTY_NAME_STR,
        GO_R_TICK_LABEL_PROPERTY_NAME_STR, GO_R_TICK_LABEL_MODE_PROPERTY_NAME_STR,
        GO_R_TICK_MODE_PROPERTY_NAME_STR, GO_THETA_AXIS_UNITS_PROPERTY_NAME_STR,
        GO_THETA_LIM_PROPERTY_NAME_STR, GO_THETA_LIM_MODE_PROPERTY_NAME_STR,
        GO_THETA_TICK_PROPERTY_NAME_STR, GO_THETA_TICK_LABEL_PROPERTY_NAME_STR,
        GO_THETA_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_THETA_TICK_MODE_PROPERTY_NAME_STR };
    return isListedProperty(name, propertyNames);
}
//=============================================================================
bool
GOAxis::isCartesianAxesOnlyProperty(const std::wstring& name)
{
    static const std::vector<std::wstring> propertyNames = { GO_X_AXIS_LOCATION_PROPERTY_NAME_STR,
        GO_X_COLOR_PROPERTY_NAME_STR, GO_X_DIR_PROPERTY_NAME_STR, GO_X_GRID_PROPERTY_NAME_STR,
        GO_X_LABEL_PROPERTY_NAME_STR, GO_X_LIM_PROPERTY_NAME_STR, GO_X_LIM_MODE_PROPERTY_NAME_STR,
        GO_X_MINOR_GRID_PROPERTY_NAME_STR, GO_X_SCALE_PROPERTY_NAME_STR,
        GO_X_TICK_PROPERTY_NAME_STR, GO_X_TICK_LABEL_PROPERTY_NAME_STR,
        GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_X_TICK_MODE_PROPERTY_NAME_STR,
        GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, GO_X_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR,
        GO_X_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR,
        GO_Y_COLOR_PROPERTY_NAME_STR, GO_Y_DIR_PROPERTY_NAME_STR, GO_Y_GRID_PROPERTY_NAME_STR,
        GO_Y_LABEL_PROPERTY_NAME_STR, GO_Y_LIM_PROPERTY_NAME_STR, GO_Y_LIM_MODE_PROPERTY_NAME_STR,
        GO_Y_MINOR_GRID_PROPERTY_NAME_STR, GO_Y_SCALE_PROPERTY_NAME_STR,
        GO_Y_TICK_PROPERTY_NAME_STR, GO_Y_TICK_LABEL_PROPERTY_NAME_STR,
        GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_Y_TICK_MODE_PROPERTY_NAME_STR,
        GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, GO_Y_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR,
        GO_Y_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, GO_Z_COLOR_PROPERTY_NAME_STR,
        GO_Z_DIR_PROPERTY_NAME_STR, GO_Z_GRID_PROPERTY_NAME_STR, GO_Z_LABEL_PROPERTY_NAME_STR,
        GO_Z_LIM_PROPERTY_NAME_STR, GO_Z_LIM_MODE_PROPERTY_NAME_STR,
        GO_Z_MINOR_GRID_PROPERTY_NAME_STR, GO_Z_SCALE_PROPERTY_NAME_STR,
        GO_Z_TICK_PROPERTY_NAME_STR, GO_Z_TICK_LABEL_PROPERTY_NAME_STR,
        GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_Z_TICK_MODE_PROPERTY_NAME_STR,
        GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, GO_Z_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR,
        GO_Z_TICK_LABEL_FORMAT_PROPERTY_NAME_STR };
    return isListedProperty(name, propertyNames);
}
//=============================================================================
bool
GOAxis::isPolarAxes()
{
    return stringCheck(GO_POLAR_AXES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
wstringVector
GOAxis::getVisibleFieldnames()
{
    // Compatibility mask for the axes family (cartesian axes, polar axes and uiaxes share this
    // class). These properties exist and stay usable through findProperty/dotted access, but must
    // not appear in the introspection surface (get(h), properties, isprop, set(h)).
    //
    // alwaysHidden: hidden on every axes flavour. polarOnlyHidden: cartesian-only properties that
    // leak onto polar axes. Cartesian axes keep them.
    static const wstringVector alwaysHidden
        = { GO_DATA_LIMITS_PROPERTY_NAME_STR, GO_POSITION_MODE_PROPERTY_NAME_STR,
              GO_TEXT_HEIGHT_PROPERTY_NAME_STR, GO_X_TICK_LABEL_FORMAT_PROPERTY_NAME_STR,
              GO_Y_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, GO_Z_TICK_LABEL_FORMAT_PROPERTY_NAME_STR };
    static const wstringVector polarOnlyHidden = { GO_AMBIENT_LIGHT_COLOR_PROPERTY_NAME_STR,
        GO_BOX_STYLE_PROPERTY_NAME_STR, GO_CAMERA_POSITION_PROPERTY_NAME_STR,
        GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR, GO_CAMERA_TARGET_PROPERTY_NAME_STR,
        GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR, GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR,
        GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR, GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR,
        GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR, GO_CLIPPING_STYLE_PROPERTY_NAME_STR,
        GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR,
        GO_GRID_LINE_WIDTH_PROPERTY_NAME_STR, GO_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR,
        GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR, GO_MINOR_GRID_LINE_WIDTH_PROPERTY_NAME_STR,
        GO_MINOR_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR, GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR,
        GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROJECTION_PROPERTY_NAME_STR,
        GO_VIEW_PROPERTY_NAME_STR, GO_X_AXIS_PROPERTY_NAME_STR, GO_X_COLOR_MODE_PROPERTY_NAME_STR,
        GO_X_LIMIT_METHOD_PROPERTY_NAME_STR, GO_X_MINOR_TICK_PROPERTY_NAME_STR,
        GO_Y_AXIS_PROPERTY_NAME_STR, GO_Y_COLOR_MODE_PROPERTY_NAME_STR,
        GO_Y_LIMIT_METHOD_PROPERTY_NAME_STR, GO_Y_MINOR_TICK_PROPERTY_NAME_STR,
        GO_Z_AXIS_PROPERTY_NAME_STR, GO_Z_COLOR_MODE_PROPERTY_NAME_STR,
        GO_Z_LIMIT_METHOD_PROPERTY_NAME_STR, GO_Z_MINOR_TICK_PROPERTY_NAME_STR };
    wstringVector fieldnames = GraphicsObject::getVisibleFieldnames();
    const bool polar = isPolarAxes();
    fieldnames.erase(std::remove_if(fieldnames.begin(), fieldnames.end(),
                         [polar](const std::wstring& name) {
                             if (std::find(alwaysHidden.begin(), alwaysHidden.end(), name)
                                 != alwaysHidden.end()) {
                                 return true;
                             }
                             if (polar) {
                                 return GOAxis::isCartesianAxesOnlyProperty(name)
                                     || std::find(
                                            polarOnlyHidden.begin(), polarOnlyHidden.end(), name)
                                     != polarOnlyHidden.end();
                             }
                             return GOAxis::isPolarAxesOnlyProperty(name);
                         }),
        fieldnames.end());
    return fieldnames;
}
//=============================================================================
// TightInset is computed from the current labels each time it is read, in the axes Units.
class GOTightInsetProperty : public GOFourVectorProperty
{
    GOAxis* m_axis;

public:
    explicit GOTightInsetProperty(GOAxis* axis) : m_axis(axis) { }
    ArrayOf
    get() override
    {
        std::vector<double> inset = m_axis->tightInsetPixels();
        double width = 0;
        double height = 0;
        graphicsParentSizeInPixels(m_axis, m_axis->getParentFigure(), width, height);
        std::wstring units = m_axis->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
        auto [l, b] = convertFromPixels(units, inset[0], inset[1], width, height);
        auto [r, t] = convertFromPixels(units, inset[2], inset[3], width, height);
        return ArrayOf::doubleRowVector(std::vector<double> { l, b, r, t });
    }
};
//=============================================================================
void
GOAxis::constructProperties()
{
    registerBaseProperties(this);
    registerStandardContainerProperties(this);

    registerProperty(new GOTwoVectorProperty, GO_A_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_A_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_AMBIENT_LIGHT_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_CAMERA_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_CAMERA_TARGET_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_C_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_C_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorVectorProperty, GO_COLOR_ORDER_PROPERTY_NAME_STR);
    registerProperty(
        new GOScalarPositiveIntegerValueProperty, GO_COLOR_ORDER_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GOSixVectorProperty, GO_DATA_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOFontSizeProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_GRID_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_GRID_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_GRID_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    registerProperty(new GOTopBottomProperty, GO_LAYER_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleOrderProperty, GO_LINE_STYLE_ORDER_PROPERTY_NAME_STR);
    registerProperty(
        new GOScalarPositiveIntegerValueProperty, GO_LINE_STYLE_ORDER_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_MINOR_GRID_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GONextPlotModeProperty, GO_NEXT_PLOT_PROPERTY_NAME_STR);
    registerProperty(new GORectanglePositionProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOThreeVectorProperty, GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    registerProperty(new GORectanglePositionProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_POSITION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOProjectionModeProperty, GO_PROJECTION_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_R_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty(true), GO_R_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty(true), GO_R_TICK_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_TEXT_HEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOThetaAxisUnitsProperty, GO_THETA_AXIS_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_THETA_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_THETA_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty(true), GO_THETA_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_THETA_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty(true), GO_THETA_TICK_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_THETA_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOInOutProperty, GO_TICK_DIR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TICK_DIR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_TICK_LENGTH_PROPERTY_NAME_STR);
    registerProperty(new GOTightInsetProperty(this), GO_TIGHT_INSET_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOTopBottomProperty, GO_X_AXIS_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOLeftRightProperty, GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_X_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_Y_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_Z_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_X_DIR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_Y_DIR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_Z_DIR_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_X_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Y_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Z_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_X_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_Y_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_Z_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOCategoricalTwoVectorProperty, GO_X_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOCategoricalTwoVectorProperty, GO_Y_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_Z_LIM_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_LIM_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_X_MINOR_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Y_MINOR_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_Z_MINOR_GRID_PROPERTY_NAME_STR);
    registerProperty(new GOLinearLogProperty, GO_X_SCALE_PROPERTY_NAME_STR);
    registerProperty(new GOLinearLogProperty, GO_Y_SCALE_PROPERTY_NAME_STR);
    registerProperty(new GOLinearLogProperty, GO_Z_SCALE_PROPERTY_NAME_STR);
    registerProperty(new GOCategoricalVectorProperty(false, true), GO_X_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOCategoricalVectorProperty(false, true), GO_Y_TICK_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty(false, true), GO_Z_TICK_PROPERTY_NAME_STR);
    auto* xTickLabel = new GOStringVectorProperty(true);
    xTickLabel->setEmptyReturnsChar(true);
    registerProperty(xTickLabel, GO_X_TICK_LABEL_PROPERTY_NAME_STR);
    auto* yTickLabel = new GOStringVectorProperty(true);
    yTickLabel->setEmptyReturnsChar(true);
    registerProperty(yTickLabel, GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
    auto* zTickLabel = new GOStringVectorProperty(true);
    zTickLabel->setEmptyReturnsChar(true);
    registerProperty(zTickLabel, GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_TICK_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Z_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_TICK_LABEL_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_TICK_LABEL_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Z_TICK_LABEL_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOColorVectorProperty, GO_COLOR_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_ALPHA_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_VIEW_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_POLAR_AXES_PROPERTY_NAME_STR, true, false);
    registerProperty(new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);

    // yyaxis dual y-ruler: hidden flag + the inactive side's stashed ruler state.
    registerProperty(new GOOnOffProperty, GO_YY_AXIS_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOTwoVectorProperty, GO_Y_LIM_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(
        new GOAutoManualProperty, GO_Y_LIM_MODE_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOColorProperty, GO_Y_COLOR_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOLinearLogProperty, GO_Y_SCALE_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(
        new GONormalReverseProperty, GO_Y_DIR_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(
        new GOVectorProperty(false, true), GO_Y_TICK_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(
        new GOAutoManualProperty, GO_Y_TICK_MODE_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOStringVectorProperty(true), GO_Y_TICK_LABEL_SECONDARY_PROPERTY_NAME_STR,
        false, false);
    registerProperty(
        new GOAutoManualProperty, GO_Y_TICK_LABEL_MODE_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOGObjectsProperty, GO_Y_LABEL_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOScalarPositiveIntegerValueProperty,
        GO_COLOR_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOScalarPositiveIntegerValueProperty,
        GO_LINE_STYLE_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR, false, false);
    // Axes.YAxis: the NumericRuler(s) viewing each y side (one normally, two with
    // yyaxis). Writable so indexed edits (ax.YAxis(2).Limits = ...) copy back; the
    // real change happens by reference on the ruler object.
    registerProperty(new GOGObjectsProperty, GO_Y_AXIS_PROPERTY_NAME_STR);
    // Axes.XAxis / Axes.ZAxis: the single NumericRuler viewing that axis. Same
    // read/write-back semantics as YAxis, but single-sided.
    registerProperty(new GOGObjectsProperty, GO_X_AXIS_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_Z_AXIS_PROPERTY_NAME_STR);
    // Persistent per-axis value kind ('numeric' | 'datetime'). Hidden plumbing:
    // writable so the datetime plotting layer can flip it, invisible so it stays
    // out of properties()/help. It drives the DatetimeRuler exposure through
    // XAxis / YAxis / ZAxis and survives updateState re-runs.
    registerProperty(new GOStringProperty, GO_X_AXIS_SCALE_KIND_PROPERTY_NAME_STR, true, false);
    registerProperty(new GOStringProperty, GO_Y_AXIS_SCALE_KIND_PROPERTY_NAME_STR, true, false);
    registerProperty(new GOStringProperty, GO_Z_AXIS_SCALE_KIND_PROPERTY_NAME_STR, true, false);

    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
GOAxis::GOAxis()
{
    zoomXOn = true;
    zoomYOn = true;
    panXOn = true;
    panYOn = true;
    zoomXStore.clear();
    zoomYStore.clear();

    constructProperties();
    setupDefaults();
}
//=============================================================================
void
GOAxis::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setTwoVectorDefault(GO_A_LIM_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_A_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_AMBIENT_LIGHT_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    setRestrictedStringDefault(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setThreeVectorDefault(GO_CAMERA_POSITION_PROPERTY_NAME_STR, 0, 0, 1);
    setRestrictedStringDefault(
        GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_CAMERA_TARGET_PROPERTY_NAME_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR, 0, 1, 0);
    setRestrictedStringDefault(
        GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR, DEFAULT_CAMERA_VIEW_ANGLE);
    setRestrictedStringDefault(
        GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_C_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(
        GO_DEFAULT_INTERACTIVITY_INTERNAL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);

    std::vector<double> colorsOrder;
    colorsOrder.reserve(21);
    colorsOrder.push_back(0.0660);
    colorsOrder.push_back(0.4430);
    colorsOrder.push_back(0.7450);

    colorsOrder.push_back(0.8660);
    colorsOrder.push_back(0.3290);
    colorsOrder.push_back(0.0000);

    colorsOrder.push_back(0.9290);
    colorsOrder.push_back(0.6940);
    colorsOrder.push_back(0.1250);

    colorsOrder.push_back(0.5210);
    colorsOrder.push_back(0.0860);
    colorsOrder.push_back(0.8190);

    colorsOrder.push_back(0.2310);
    colorsOrder.push_back(0.6660);
    colorsOrder.push_back(0.1960);

    colorsOrder.push_back(0.1840);
    colorsOrder.push_back(0.7450);
    colorsOrder.push_back(0.9370);

    colorsOrder.push_back(0.8190);
    colorsOrder.push_back(0.0150);
    colorsOrder.push_back(0.5450);
    GOVectorProperty* hp
        = static_cast<GOVectorProperty*>(findProperty(GO_COLOR_ORDER_PROPERTY_NAME_STR));
    hp->data(colorsOrder);
    setScalarPositiveIntegerValueDefault(GO_COLOR_ORDER_INDEX_PROPERTY_NAME_STR, 1.0);
    setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, 1, 1, 1);
    setRestrictedStringDefault(
        GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setRestrictedStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_GRID_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_LAYER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    wstringVector defaultLineStyleOrder;
    defaultLineStyleOrder.push_back(L"-");
    setRestrictedStringSetDefault(GO_LINE_STYLE_ORDER_PROPERTY_NAME_STR, defaultLineStyleOrder);
    setScalarPositiveIntegerValueDefault(GO_LINE_STYLE_ORDER_INDEX_PROPERTY_NAME_STR, 1.0);
    setRestrictedStringDefault(GO_MINOR_GRID_LINE_STYLE_PROPERTY_NAME_STR, L":");
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setRestrictedStringDefault(GO_NEXT_PLOT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REPLACE_STR);
    setRestrictedStringDefault(GO_POLAR_AXES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, 1, 1, 1);
    setRestrictedStringDefault(
        GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setRestrictedStringDefault(GO_POSITION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_PROJECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ORTHOGRAPHIC_STR);
    setTwoVectorDefault(GO_R_LIM_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_R_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setVectorDoubleDefault(GO_R_TICK_PROPERTY_NAME_STR, { 0, 0.2, 0.4, 0.6, 0.8, 1 });
    setRestrictedStringDefault(GO_R_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    static_cast<GOStringVectorProperty*>(findProperty(GO_R_TICK_LABEL_PROPERTY_NAME_STR))
        ->data({ L"0", L"0.2", L"0.4", L"0.6", L"0.8", L"1" });
    setRestrictedStringDefault(GO_R_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(
        GO_THETA_AXIS_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DEGREES_STR);
    setTwoVectorDefault(GO_THETA_LIM_PROPERTY_NAME_STR, 0, 360);
    setRestrictedStringDefault(GO_THETA_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setVectorDoubleDefault(GO_THETA_TICK_PROPERTY_NAME_STR,
        { 0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360 });
    setRestrictedStringDefault(GO_THETA_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    static_cast<GOStringVectorProperty*>(findProperty(GO_THETA_TICK_LABEL_PROPERTY_NAME_STR))
        ->data({ L"0", L"30", L"60", L"90", L"120", L"150", L"180", L"210", L"240", L"270", L"300",
            L"330" });
    setRestrictedStringDefault(
        GO_THETA_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_TICK_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_IN_STR);
    setRestrictedStringDefault(GO_TICK_DIR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_TICK_LENGTH_PROPERTY_NAME_STR, 0.01, 0.025);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTTOM_STR);
    setRestrictedStringDefault(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    setThreeVectorDefault(GO_X_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setThreeVectorDefault(GO_Y_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setThreeVectorDefault(GO_Z_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_Z_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_X_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Y_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Z_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_X_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Y_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_Z_MINOR_GRID_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_X_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setRestrictedStringDefault(GO_Y_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setRestrictedStringDefault(GO_Z_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setRestrictedStringDefault(GO_X_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_TICK_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, 0);
    setScalarDoubleDefault(GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, 0);
    setScalarDoubleDefault(GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, 0);
    setRestrictedStringDefault(
        GO_X_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_Y_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_Z_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_X_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Z_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_X_AXIS_SCALE_KIND_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NUMERIC_STR);
    setStringDefault(GO_Y_AXIS_SCALE_KIND_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NUMERIC_STR);
    setStringDefault(GO_Z_AXIS_SCALE_KIND_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NUMERIC_STR);
    setTwoVectorDefault(GO_C_LIM_PROPERTY_NAME_STR, 0, 1);

    // yyaxis stashed secondary side: inert until enableDualYAxis() seeds it.
    setRestrictedStringDefault(GO_YY_AXIS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setTwoVectorDefault(GO_Y_LIM_SECONDARY_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(
        GO_Y_LIM_MODE_SECONDARY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_Y_COLOR_SECONDARY_PROPERTY_NAME_STR, 0, 0, 0);
    setRestrictedStringDefault(
        GO_Y_SCALE_SECONDARY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setRestrictedStringDefault(GO_Y_DIR_SECONDARY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(
        GO_Y_TICK_MODE_SECONDARY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_Y_TICK_LABEL_MODE_SECONDARY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarPositiveIntegerValueDefault(GO_COLOR_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR, 1.0);
    setScalarPositiveIntegerValueDefault(
        GO_LINE_STYLE_ORDER_INDEX_SECONDARY_PROPERTY_NAME_STR, 1.0);

    setStringDefault(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setScalarDoubleDefault(GO_GRID_ALPHA_PROPERTY_NAME_STR, 0.15);
    setThreeVectorDefault(GO_GRID_COLOR_PROPERTY_NAME_STR, 0.15, 0.15, 0.15);
    setTwoVectorDefault(GO_VIEW_PROPERTY_NAME_STR, 0, 90);

    loadParulaColorMap();
    initializeCommonProperties(this);
    updateAxisFont();
}
//=============================================================================
GOAxis::~GOAxis()
{
    if (m_toolbarHandle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(m_toolbarHandle)) {
        deleteGraphicsObject(m_toolbarHandle, false, false);
    }
}
//=============================================================================
int
GOAxis::getTickCount(
    RenderInterface& gc, double x1, double y1, double z1, double x2, double y2, double z2)
{
    double axlen = getAxisPixelLength(gc, x1, y1, z1, x2, y2, z2);
    // The axes pixel position is only needed for the degenerate-transform 2D
    // fallback below and for 3D compaction; computing it on every tick recount
    // of a normally rendered 2D axis is pure overhead, so fetch it lazily.
    std::vector<double> position;
    if (!is2D() || axlen < 1.0) {
        position = getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR);
    }
    // Ticks may be recomputed outside a render pass (for example when a script reads XTick right
    // after plot). The render GC is then degenerate and projects every axis edge to zero length,
    // which would collapse the tick count to the minimum. Fall back to the layout pixel extent of
    // the axes (width for a horizontal edge, height for a vertical one) so stored ticks match the
    // density the figure will render. Only 2D axes have a reliable layout extent per edge; a 3D
    // edge is foreshortened by the view projection and keeps the minimum count.
    if (is2D() && axlen < 1.0 && position.size() >= 4 && std::isfinite(position[2])
        && std::isfinite(position[3])) {
        bool xVaries = std::fabs(x2 - x1) > 0.0;
        bool yVaries = std::fabs(y2 - y1) > 0.0;
        bool zVaries = std::fabs(z2 - z1) > 0.0;
        if (xVaries && !yVaries && !zVaries) {
            axlen = position[2];
        } else if (yVaries && !xVaries && !zVaries) {
            axlen = position[3];
        }
    }
    bool compact3DAxis = false;
    if (!is2D()) {
        compact3DAxis = position.size() >= 4 && std::isfinite(position[2])
            && std::isfinite(position[3]) && std::max(position[2], position[3]) < 420.0;
    }
    double tickPixelSpacing
        = is2D() ? (axisHasLimitChildren(this) ? 40.0 : 48.0) : (compact3DAxis ? 64.0 : 20.0);
    double minTickCount = compact3DAxis ? 3.0 : 5.0;
    int numtics = (int)(std::max(minTickCount, axlen / tickPixelSpacing));
    return numtics;
}
//=============================================================================
// Layout geometry follows the figure's layout size, not the transient rendering
// size: neither an export render nor a HiDPI screen render may change stored
// ticks. GOFigure::getLayoutWidth() divides the rendering size by the scale its
// producer declared (device pixel ratio on screen, export ratio when printing).
static void
figureLogicalSize(GOFigure* fig, unsigned& width, unsigned& height)
{
    width = static_cast<unsigned>(fig->getLayoutWidth());
    height = static_cast<unsigned>(fig->getLayoutHeight());
}
//=============================================================================
void
GOAxis::recalculateTicks()
{
    GOFigure* fig = getParentFigure();
    unsigned width;
    unsigned height;
    figureLogicalSize(fig, width, height);
    // Qt-free settling context: only the projection math is used (RenderWeb
    // and the GL backend share the same transform pipeline).
    RenderWeb gc(0, 0, width, height);
    setupProjection(gc);
    std::vector<double> limits(getAxisLimits());
    // tick counts measure the ruler edges: set them here, a hidden figure is never painted
    computeAxisEdges(gc, limits);
    bool axes2D = is2D();
    bool depthSortedSurface = axisHasDepthSortedSurface(this);
    bool autoZLimits = isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR);
    bool compactDepthSortedSurface = false;
    std::vector<double> axisPosition(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    std::vector<double> axisPixelPosition(axisPosition);
    {
        GOUnitsProperty* axUnits
            = static_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR));
        if (axUnits->isEqual(GO_PROPERTY_VALUE_NORMALIZED_STR)) {
            for (size_t i = 0; i + 1 < axisPixelPosition.size(); i += 2) {
                axisPixelPosition[i] *= width;
                axisPixelPosition[i + 1] *= height;
            }
        } else if (!axUnits->isEqual(GO_PROPERTY_VALUE_PIXELS_STR)) {
            axisPixelPosition = getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR);
        }
    }
    bool compactNormalizedAxis
        = findStringProperty(GO_UNITS_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_NORMALIZED_STR
        && axisPosition.size() >= 4 && std::isfinite(axisPosition[2])
        && std::isfinite(axisPosition[3]) && std::max(axisPosition[2], axisPosition[3]) < 0.5;
    bool compactPixelAxis = axisPixelPosition.size() >= 4 && std::isfinite(axisPixelPosition[2])
        && std::isfinite(axisPixelPosition[3])
        && std::max(axisPixelPosition[2], axisPixelPosition[3]) < 420.0;
    compactDepthSortedSurface = !axes2D && depthSortedSurface && autoZLimits && height <= 455
        && (compactNormalizedAxis || compactPixelAxis);
    // a locked aspect ratio keeps the tick count its projected Z edge earns
    bool compact3DAutoZAxis = !axes2D && autoZLimits
        && isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)
        && (compactNormalizedAxis || compactPixelAxis);
    bool flat2DAxis = axes2D && axisPosition.size() >= 4 && std::isfinite(axisPosition[2])
        && std::isfinite(axisPosition[3]) && axisPosition[3] < 0.22
        && axisPosition[2] > axisPosition[3];
    std::vector<double> dataLimits = findVectorDoubleProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
    bool hasFiniteDataLimits = dataLimits.size() >= 6
        && std::all_of(dataLimits.begin(), dataLimits.begin() + 6,
            [](double value) { return std::isfinite(value); })
        && dataLimits[1] > dataLimits[0] && dataLimits[3] > dataLimits[2]
        && dataLimits[5] > dataLimits[4];
    if ((!axes2D || depthSortedSurface) && hasFiniteDataLimits && autoZLimits) {
        const bool logZ = stringCheck(GO_Z_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LOG_STR);
        limits[4] = logZ ? tickLog(dataLimits[4]) : dataLimits[4];
        limits[5] = logZ ? tickLog(dataLimits[5]) : dataLimits[5];
    }
    bool preserveDataAspectAutoLimits = !axes2D
        && !isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)
        && isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    std::vector<double> preservedDataAspectAutoLimits;
    if (preserveDataAspectAutoLimits) {
        preservedDataAspectAutoLimits = findVectorDoubleProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
        preserveDataAspectAutoLimits = preservedDataAspectAutoLimits.size() >= 6
            && std::all_of(preservedDataAspectAutoLimits.begin(),
                preservedDataAspectAutoLimits.begin() + 6,
                [](double value) { return std::isfinite(value); })
            && preservedDataAspectAutoLimits[1] > preservedDataAspectAutoLimits[0]
            && preservedDataAspectAutoLimits[3] > preservedDataAspectAutoLimits[2]
            && preservedDataAspectAutoLimits[5] > preservedDataAspectAutoLimits[4];
        if (preserveDataAspectAutoLimits) {
            if (isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR)) {
                limits[0] = preservedDataAspectAutoLimits[0];
                limits[1] = preservedDataAspectAutoLimits[1];
            }
            if (isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)) {
                limits[2] = preservedDataAspectAutoLimits[2];
                limits[3] = preservedDataAspectAutoLimits[3];
            }
            if (isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)) {
                limits[4] = preservedDataAspectAutoLimits[4];
                limits[5] = preservedDataAspectAutoLimits[5];
            }
        }
    }
    bool hasQuiverChild = objectTreeContainsType(this, GO_PROPERTY_VALUE_QUIVER_STR);
    bool hasContourChild = objectTreeContainsType(this, GO_PROPERTY_VALUE_CONTOUR_STR);
    bool preserveQuiverAutoLimits = hasQuiverChild && shouldPreserveQuiverAutoDataLimits(this);
    std::vector<double> preservedAutoLimits;
    if (preserveQuiverAutoLimits) {
        preservedAutoLimits = findVectorDoubleProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
        preserveQuiverAutoLimits = preservedAutoLimits.size() >= 6
            && std::all_of(preservedAutoLimits.begin(), preservedAutoLimits.begin() + 6,
                [](double value) { return std::isfinite(value); })
            && preservedAutoLimits[1] > preservedAutoLimits[0]
            && preservedAutoLimits[3] > preservedAutoLimits[2]
            && preservedAutoLimits[5] > preservedAutoLimits[4];
        if (preserveQuiverAutoLimits) {
            std::copy(preservedAutoLimits.begin(), preservedAutoLimits.begin() + 6, limits.begin());
        }
    }
    bool preserveChildAutoLimits = axes2D
        && (objectTreePreservesAutoDataLimits(this)
            || objectTreeContainsType(this, GO_PROPERTY_VALUE_HISTOGRAM_STR));
    bool hasHistogramChild
        = axes2D && objectTreeContainsType(this, GO_PROPERTY_VALUE_HISTOGRAM_STR);
    bool hasLineChild = axes2D
        && (objectTreeContainsType(this, GO_PROPERTY_VALUE_LINE_STR)
            || objectTreeContainsType(this, GO_PROPERTY_VALUE_STAIR_STR));
    bool preserveNoDataAutoLimits = axes2D && !axisHasLimitChildren(this);
    bool preserveSurfaceZAutoLimits = false;
    bool aspectExpandedAutoLimits = axes2D && hasFiniteDataLimits
        && (limits[0] < dataLimits[0] || limits[1] > dataLimits[1] || limits[2] < dataLimits[2]
            || limits[3] > dataLimits[3]);
    std::vector<double> preservedChildLimits;
    if (preserveChildAutoLimits) {
        preservedChildLimits = findVectorDoubleProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR);
        preserveChildAutoLimits = preservedChildLimits.size() >= 6
            && std::all_of(preservedChildLimits.begin(), preservedChildLimits.begin() + 6,
                [](double value) { return std::isfinite(value); })
            && preservedChildLimits[1] > preservedChildLimits[0]
            && preservedChildLimits[3] > preservedChildLimits[2]
            && preservedChildLimits[5] > preservedChildLimits[4];
    }
    std::vector<double> xticks;
    std::vector<std::wstring> xlabels;
    std::vector<double> yticks;
    std::vector<std::wstring> ylabels;
    std::vector<double> zticks;
    std::vector<std::wstring> zlabels;
    int xcnt, ycnt, zcnt;
    xcnt = getTickCount(gc, limits[0], x1pos[1], x1pos[2], limits[1], x1pos[1], x1pos[2]);
    ycnt = getTickCount(gc, y1pos[0], limits[2], y1pos[2], y1pos[0], limits[3], y1pos[2]);
    zcnt = getTickCount(gc, z1pos[0], z1pos[1], limits[4], z1pos[0], z1pos[1], limits[5]);
    // Projected pixel-density tick counts for the automatic 3D surface limits,
    // captured before the per-chart tick-count adjustments below rewrite xcnt /
    // ycnt. See surface3DHorizontalTickBudget.
    const int rawXTickCount = xcnt;
    const int rawYTickCount = ycnt;
    if (axes2D && axisPixelPosition.size() >= 4 && std::isfinite(axisPixelPosition[2])
        && axisPixelPosition[2] <= 500.0) {
        // A narrow X ruler keeps a tick interval of at least 47.5 px (measured on the reference).
        xcnt = std::min(xcnt, std::max(5, static_cast<int>(axisPixelPosition[2] / 47.5)));
        if (compactNormalizedAxis && std::fabs(limits[1] - limits[0]) <= 2.5) {
            xcnt = std::min(xcnt, 3);
        }
    }
    if (flat2DAxis) {
        xcnt = std::min(xcnt, 8);
        ycnt = std::min(ycnt, 5);
    }
    if (axes2D && objectTreeContainsType(this, GO_PROPERTY_VALUE_IMAGE_STR)
        && isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR) && isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)) {
        double xSpan = std::fabs(limits[1] - limits[0]);
        double ySpan = std::fabs(limits[3] - limits[2]);
        if (xSpan >= 100.0 && ySpan >= 100.0 && ySpan / xSpan >= 0.5 && ySpan / xSpan <= 2.0) {
            xcnt = std::min(xcnt, 11);
            ycnt = std::min(ycnt, 11);
        }
    }
    if (axes2D && preserveQuiverAutoLimits) {
        ycnt = 7;
    }
    std::vector<double> pbar(findVectorDoubleProperty(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR));
    bool plotBoxAspectAuto = isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    bool validPlotBoxAspect = isFinitePositiveTriple(pbar);
    bool tightXYLimits
        = findStringProperty(GO_X_LIMIT_METHOD_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TIGHT_STR
        && findStringProperty(GO_Y_LIMIT_METHOD_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_TIGHT_STR;
    if (axes2D && tightXYLimits) {
        double xSpan = std::fabs(limits[1] - limits[0]);
        double ySpan = std::fabs(limits[3] - limits[2]);
        bool horizontallyCompressedData = xSpan > 0.0 && ySpan > 0.0 && ySpan / xSpan < 0.4;
        if (horizontallyCompressedData) {
            xcnt = std::min(xcnt, 5);
            ycnt = std::min(ycnt, 7);
        } else if (xSpan > ySpan && ySpan / xSpan > 0.4) {
            ycnt = std::max(ycnt, 9);
        }
    } else if (axes2D
        && (!flat2DAxis
            || (axisPosition.size() >= 4 && std::isfinite(axisPosition[3])
                && axisPosition[3] > 0.157))
        && isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR) && !preserveNoDataAutoLimits) {
        ycnt = std::max(ycnt, 8);
    }
    if (flat2DAxis && axisPosition.size() >= 4 && std::isfinite(axisPosition[3])
        && axisPosition[3] > 0.157 && isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)
        && !preserveNoDataAutoLimits) {
        ycnt = std::max(ycnt, 8);
    }
    bool scaleTicksByPlotBox
        = !axes2D && validPlotBoxAspect && (!plotBoxAspectAuto || preserveDataAspectAutoLimits);
    double plotBoxReferenceLength = scaleTicksByPlotBox ? median3(pbar[0], pbar[1], pbar[2]) : 1.0;
    if (!axes2D) {
        bool cubicPlotBox = !plotBoxAspectAuto && validPlotBoxAspect
            && std::abs(pbar[0] / pbar[1] - 1.0) < 1e-12
            && std::abs(pbar[0] / pbar[2] - 1.0) < 1e-12;
        xcnt = 5;
        ycnt = 5;
        if (scaleTicksByPlotBox) {
            xcnt = plotBoxScaledTickCount(pbar[0], plotBoxReferenceLength);
            ycnt = plotBoxScaledTickCount(pbar[1], plotBoxReferenceLength);
            if (cubicPlotBox && !depthSortedSurface && std::fabs(limits[1] - limits[0]) <= 2.5) {
                xcnt = 3;
            }
        }
        double zSpan = std::fabs(limits[5] - limits[4]);
        if (!scaleTicksByPlotBox && depthSortedSurface && std::fabs(limits[1] - limits[0]) <= 6.0
            && !(plotBoxAspectAuto && std::fabs(limits[1] - limits[0]) > 3.0
                && std::fabs(limits[1] - limits[0]) <= 4.5)) {
            xcnt = 5;
        }
        if (compact3DAutoZAxis) {
            zcnt = depthSortedSurface && zSpan > 4.0 ? 4 : std::min(3, zcnt);
        } else if (!scaleTicksByPlotBox && plotBoxAspectAuto && depthSortedSurface
            && zSpan <= 1.0) {
            zcnt = 3;
        } else if (!scaleTicksByPlotBox && plotBoxAspectAuto && zSpan <= 1.0) {
            zcnt = std::max(11, zcnt);
        } else if (depthSortedSurface && autoZLimits && !compactDepthSortedSurface
            && (std::fabs(limits[1] - limits[0]) > 20.0
                || std::fabs(limits[3] - limits[2]) > 20.0)) {
            xcnt = std::max(8, xcnt);
            zcnt = std::max(8, zcnt);
        } else if (preserveDataAspectAutoLimits && zSpan <= 2.5) {
            zcnt = std::max(11, zcnt);
        } else if (cubicPlotBox || preserveDataAspectAutoLimits) {
            zcnt = std::max(5, zcnt);
        } else if (zSpan >= 3.0 && zSpan <= 4.0) {
            zcnt = 8;
        } else if (zSpan <= 4.0) {
            zcnt = 5;
        } else {
            zcnt = std::max(8, zcnt / 2);
        }
    }
    double xStart = limits[0], xStop = limits[1];
    double yStart = limits[2], yStop = limits[3];
    double zStart = limits[4], zStop = limits[5];
    GOLinearLogProperty* lp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    GOTextInterpreterProperty* textInterpreterProperty = static_cast<GOTextInterpreterProperty*>(
        findProperty(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR));
    bool xAutoLimitsAdjusted = false;
    bool yAutoLimitsAdjusted = false;
    bool zAutoLimitsAdjusted = false;
    bool xLogScale = lp->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    bool yLogScale = false;
    bool zLogScale = false;
    std::wstring xLimitMethod = findStringProperty(GO_X_LIMIT_METHOD_PROPERTY_NAME_STR);
    std::wstring yLimitMethod = findStringProperty(GO_Y_LIMIT_METHOD_PROPERTY_NAME_STR);
    std::wstring zLimitMethod = findStringProperty(GO_Z_LIMIT_METHOD_PROPERTY_NAME_STR);
    // Generic automatic axis limits for a 3D surface: each ruler is snapped to the same "nice"
    // 1/2/5 tick grid as a 2D axis. The grid is applied to the raw data extent, not to the current
    // (possibly already expanded) axis limit. Using the settled limit as input would make it a
    // fixed point, so the same data range could settle at different limits depending on render
    // history.
    //
    // The interval budget given to the nice-number chooser sets how coarse the step may become. The
    // horizontal rulers (X, Y) share one budget derived from the longer of their projected edge
    // lengths, so identical data ranges get identical limits even though the projection makes one
    // edge shorter. The raw pixel-density counts assume the dense 2D spacing (~one tick per 20 px);
    // they are rescaled to about one tick per 54 px, which suits the sparser horizontal 3D ruler. A
    // smaller figure or steeper view gives a shorter edge, a smaller budget and a coarser step.
    //
    // The vertical ruler (Z) keeps a fixed cap of nine ticks (eight intervals): the chooser picks
    // the finest {1,2,5}x10^k step whose tick-aligned limits stay within the cap. For example
    // peaks(30) (data ~[-6.4 8.0]) needs ten ticks at step 2, so it drops to step 5 on [-10 10];
    // ribbon and waterfall (lower maximum) keep step 2 on [-8 8] (nine ticks). These are global
    // density constants, not tuned to a data set.
    constexpr double surface3DHorizontalDensityScale = 0.37;
    int surface3DHorizontalTickBudget = (int)std::lround(
        std::max(rawXTickCount, rawYTickCount) * surface3DHorizontalDensityScale);
    surface3DHorizontalTickBudget = std::max(4, std::min(11, surface3DHorizontalTickBudget));
    constexpr int surface3DVerticalTickBudget = 8;
    const bool surface3DAutoLimits = !axes2D && depthSortedSurface && hasFiniteDataLimits
        && !preserveDataAspectAutoLimits && !preserveQuiverAutoLimits;
    const bool squeezedSurfaceZAxis = surface3DAutoLimits && axisPosition.size() >= 4
        && std::isfinite(axisPosition[3]) && axisPosition[3] < 0.75;
    const bool squeezedSurfaceYAxis = surface3DAutoLimits && axisPosition.size() >= 4
        && std::isfinite(axisPosition[2]) && axisPosition[2] < 0.65;
    const bool squeezedSurfaceXAxis = surface3DAutoLimits && axisPosition.size() >= 4
        && std::isfinite(axisPosition[2]) && axisPosition[2] < 0.45;
    if (isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR)) {
        bool useDataLimits = hasFiniteDataLimits
            && (xLimitMethod == GO_PROPERTY_VALUE_TIGHT_STR
                || xLimitMethod == GO_PROPERTY_VALUE_PADDED_STR);
        double xLimitMin = useDataLimits && !aspectExpandedAutoLimits ? dataLimits[0] : limits[0];
        double xLimitMax = useDataLimits && !aspectExpandedAutoLimits ? dataLimits[1] : limits[1];
        if (preserveQuiverAutoLimits) {
            xLimitMin = preservedAutoLimits[0];
            xLimitMax = preservedAutoLimits[1];
        }
        if (useDataLimits && !xLogScale) {
            snapNearZeroExtent(xLimitMin, xLimitMax);
        }
        if (xLogScale && useDataLimits) {
            xLimitMin = tickLog(dataLimits[0]);
            xLimitMax = tickLog(dataLimits[1]);
        }
        int xFormatTickCount = xcnt;
        if (surface3DAutoLimits && !xLogScale
            && xLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
            xLimitMin = dataLimits[0];
            xLimitMax = dataLimits[1];
            xFormatTickCount = surface3DHorizontalTickBudget;
        }
        formatAxisAuto(textInterpreterProperty->getAsEnum(), xLimitMin, xLimitMax, xFormatTickCount,
            xLogScale, xLimitMethod, xStart, xStop, xticks, xlabels);
        if (preserveQuiverAutoLimits) {
            xStart = preservedAutoLimits[0];
            xStop = preservedAutoLimits[1];
        }
        if (preserveDataAspectAutoLimits && !preserveQuiverAutoLimits) {
            xStart = preservedDataAspectAutoLimits[0];
            xStop = preservedDataAspectAutoLimits[1];
        }
        if (preserveChildAutoLimits) {
            xStart = preservedChildLimits[0];
            xStop = preservedChildLimits[1];
        }
        if (preserveNoDataAutoLimits) {
            xStart = limits[0];
            xStop = limits[1];
        }
        bool compact2DAxis = axes2D && (compactNormalizedAxis || flat2DAxis);
        if (compactNormalizedAxis && hasLineChild && hasFiniteDataLimits && !xLogScale
            && xLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR && dataLimits[0] >= xStart - 1e-12
            && dataLimits[1] <= xStop + 1e-12) {
            // Tighten only the upper limit to the data extent to avoid an
            // over-padded top on compact (small tiled/subplot) line axes. The
            // lower limit stays on the tick-aligned grid, so a data minimum that
            // sits off the grid snaps down to a round gridline (for example 0)
            // instead of the raw data value.
            xStop = dataLimits[1];
        }
        if (compact2DAxis && hasFiniteDataLimits && !xLogScale) {
            clampLargeOneSidedTickAlignedPadding(
                xLimitMethod, dataLimits[0], dataLimits[1], xStart, xStop, xticks, xlabels);
        }
        if (hasFiniteDataLimits && !xLogScale
            && xLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
            keepBarEdges(this, false, dataLimits, xStart, xStop, xticks, xlabels);
        }
        if (std::isfinite(xStart) && std::isfinite(xStop) && xStop > xStart) {
            xAutoLimitsAdjusted = true;
        }
    } else {
        formatAxisManual(textInterpreterProperty->getAsEnum(), limits[0], limits[1], xcnt,
            xLogScale, xStart, xStop, xticks, xlabels);
    }
    lp = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    yLogScale = lp->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    if (isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)) {
        bool useDataLimits = hasFiniteDataLimits
            && (yLimitMethod == GO_PROPERTY_VALUE_TIGHT_STR
                || yLimitMethod == GO_PROPERTY_VALUE_PADDED_STR);
        double yLimitMin = useDataLimits && !aspectExpandedAutoLimits ? dataLimits[2] : limits[2];
        double yLimitMax = useDataLimits && !aspectExpandedAutoLimits ? dataLimits[3] : limits[3];
        if (preserveQuiverAutoLimits) {
            yLimitMin = preservedAutoLimits[2];
            yLimitMax = preservedAutoLimits[3];
        }
        if (useDataLimits && !yLogScale) {
            snapNearZeroExtent(yLimitMin, yLimitMax);
        }
        if (yLogScale && useDataLimits) {
            yLimitMin = tickLog(dataLimits[2]);
            yLimitMax = tickLog(dataLimits[3]);
        }
        int yFormatTickCount = ycnt;
        if (surface3DAutoLimits && !yLogScale
            && yLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
            yLimitMin = dataLimits[2];
            yLimitMax = dataLimits[3];
            yFormatTickCount = surface3DHorizontalTickBudget;
            if (squeezedSurfaceYAxis) {
                yFormatTickCount = std::min(yFormatTickCount, 3);
            }
        }
        formatAxisAuto(textInterpreterProperty->getAsEnum(), yLimitMin, yLimitMax, yFormatTickCount,
            yLogScale, yLimitMethod, yStart, yStop, yticks, ylabels);
        if (squeezedSurfaceYAxis && !yLogScale
            && yLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
            double yStep = niceLinearStep(yLimitMax - yLimitMin, 3.0);
            if (std::isfinite(yStep) && yStep > 0.0) {
                yStart = std::floor(yLimitMin / yStep) * yStep;
                yStop = std::ceil(yLimitMax / yStep) * yStep;
                yticks.clear();
                for (double tick = yStart; tick <= yStop + yStep * 1e-10; tick += yStep) {
                    yticks.push_back(std::fabs(tick) <= yStep * 1e-12 ? 0.0 : tick);
                }
                formatAxisTickLabels(
                    textInterpreterProperty->getAsEnum(), yticks, yLogScale, ylabels);
            }
        }
        if (preserveQuiverAutoLimits) {
            yStart = preservedAutoLimits[2];
            yStop = preservedAutoLimits[3];
        }
        if (preserveDataAspectAutoLimits && !preserveQuiverAutoLimits) {
            yStart = preservedDataAspectAutoLimits[2];
            yStop = preservedDataAspectAutoLimits[3];
        }
        if (preserveChildAutoLimits) {
            yStart = preservedChildLimits[2];
            yStop = preservedChildLimits[3];
        }
        if (preserveNoDataAutoLimits) {
            yStart = limits[2];
            yStop = limits[3];
        }
        bool compact2DAxis = axes2D && (compactNormalizedAxis || flat2DAxis);
        if (compact2DAxis && hasFiniteDataLimits && !yLogScale) {
            clampLargeOneSidedTickAlignedPadding(
                yLimitMethod, dataLimits[2], dataLimits[3], yStart, yStop, yticks, ylabels);
        }
        if (flat2DAxis && axisPosition.size() >= 4 && std::isfinite(axisPosition[3])
            && axisPosition[3] > 0.157 && hasFiniteDataLimits && !yLogScale
            && yLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR && dataLimits[2] > -25000.0
            && dataLimits[3] > 100000.0) {
            double step = 50000.0;
            yStart = dataLimits[2];
            yStop = std::ceil(dataLimits[3] / step) * step;
            yticks.clear();
            for (double tick = std::ceil(yStart / step) * step; tick <= yStop + step * 1e-10;
                tick += step) {
                yticks.push_back(std::fabs(tick) <= step * 1e-12 ? 0.0 : tick);
            }
            formatAxisTickLabels(textInterpreterProperty->getAsEnum(), yticks, yLogScale, ylabels);
        }
        if (flat2DAxis && hasFiniteDataLimits && !yLogScale
            && yLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR && yStart < 0.0 && yStop > 100000.0
            && std::fabs(yStop - yStart) >= 100000.0 && yticks.size() >= 6) {
            thinDenseLinearTicks(yStart, yStop, yticks, 3.0);
            formatAxisTickLabels(textInterpreterProperty->getAsEnum(), yticks, yLogScale, ylabels);
        }
        if (flat2DAxis && axisPosition.size() >= 4 && std::isfinite(axisPosition[3])
            && axisPosition[3] <= 0.15 && hasFiniteDataLimits && !yLogScale
            && yLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR && dataLimits[2] > -100000.0
            && dataLimits[3] > 100000.0 && dataLimits[3] < 500000.0) {
            constexpr double step = 100000.0;
            yStart = dataLimits[2];
            yStop = dataLimits[3];
            yticks.clear();
            double firstTick = dataLimits[2] <= 0.0 ? 0.0 : std::ceil(dataLimits[2] / step) * step;
            double lastTick = std::floor(dataLimits[3] / step) * step;
            for (double tick = firstTick; tick <= lastTick + step * 1e-10; tick += step) {
                yticks.push_back(std::fabs(tick) <= step * 1e-12 ? 0.0 : tick);
            }
            formatAxisTickLabels(textInterpreterProperty->getAsEnum(), yticks, yLogScale, ylabels);
        }
        if (hasHistogramChild && !yLogScale && yLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR
            && std::fabs(yStart) <= 1e-12 && std::fabs(yStop - 0.5) <= 1e-12) {
            constexpr double step = 0.05;
            yticks.clear();
            for (double tick = 0.0; tick <= 0.5 + step * 1e-10; tick += step) {
                yticks.push_back(std::fabs(tick) <= step * 1e-12 ? 0.0 : tick);
            }
            formatAxisTickLabels(textInterpreterProperty->getAsEnum(), yticks, yLogScale, ylabels);
        }
        if (hasFiniteDataLimits && !yLogScale
            && yLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
            keepBarEdges(this, true, dataLimits, yStart, yStop, yticks, ylabels);
        }
        if (std::isfinite(yStart) && std::isfinite(yStop) && yStop > yStart) {
            yAutoLimitsAdjusted = true;
        }
    } else {
        formatAxisManual(textInterpreterProperty->getAsEnum(), limits[2], limits[3], ycnt,
            yLogScale, yStart, yStop, yticks, ylabels);
    }

    lp = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    zLogScale = lp->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    if (isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)) {
        int zFormatTickCount = zcnt;
        if (compactDepthSortedSurface && depthSortedSurface && autoZLimits && !zLogScale) {
            zFormatTickCount = 7;
        }
        if (!axes2D && hasFiniteDataLimits && dataLimits[4] < 0.0 && dataLimits[5] > 0.0
            && std::fabs(dataLimits[5] - dataLimits[4]) <= 1.0
            && (!plotBoxAspectAuto || preserveDataAspectAutoLimits)) {
            zFormatTickCount = 3;
        }
        bool useDataLimits = hasFiniteDataLimits
            && (zLimitMethod == GO_PROPERTY_VALUE_TIGHT_STR
                || zLimitMethod == GO_PROPERTY_VALUE_PADDED_STR);
        double zLimitMin = useDataLimits ? dataLimits[4] : limits[4];
        double zLimitMax = useDataLimits ? dataLimits[5] : limits[5];
        if (useDataLimits && !zLogScale) {
            snapNearZeroExtent(zLimitMin, zLimitMax);
        }
        if (zLogScale && useDataLimits) {
            zLimitMin = tickLog(dataLimits[4]);
            zLimitMax = tickLog(dataLimits[5]);
        }
        // Snap the vertical ruler to the same nice-number grid as a 2D axis of the same data range,
        // using the raw data extent (not the possibly expanded axis limit, which would be a fixed
        // point).
        if (surface3DAutoLimits && !zLogScale
            && zLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
            zLimitMin = dataLimits[4];
            zLimitMax = dataLimits[5];
            zFormatTickCount = squeezedSurfaceZAxis ? 5 : surface3DVerticalTickBudget;
        }
        std::wstring zMethod = zLimitMethod;
        // A surface shown in a 2D (top-down) view has no visible Z ruler, so its
        // depth limits stay tight to the data extent instead of snapping outward
        // to a nice-number grid.
        if (axes2D && depthSortedSurface && hasFiniteDataLimits && !zLogScale
            && zLimitMethod == GO_PROPERTY_VALUE_TICKALIGNED_STR) {
            zLimitMin = dataLimits[4];
            zLimitMax = dataLimits[5];
            zMethod = GO_PROPERTY_VALUE_TIGHT_STR;
        }
        formatAxisAuto(textInterpreterProperty->getAsEnum(), zLimitMin, zLimitMax, zFormatTickCount,
            zLogScale, zMethod, zStart, zStop, zticks, zlabels);
        if (preserveQuiverAutoLimits) {
            zStart = preservedAutoLimits[4];
            zStop = preservedAutoLimits[5];
        }
        if (preserveDataAspectAutoLimits && !preserveQuiverAutoLimits) {
            zStart = preservedDataAspectAutoLimits[4];
            zStop = preservedDataAspectAutoLimits[5];
        }
        if (preserveChildAutoLimits) {
            zStart = preservedChildLimits[4];
            zStop = preservedChildLimits[5];
        }
        if (preserveNoDataAutoLimits) {
            zStart = limits[4];
            zStop = limits[5];
        }
        if (std::isfinite(zStart) && std::isfinite(zStop) && zStop > zStart) {
            zAutoLimitsAdjusted = true;
        }
    } else {
        formatAxisManual(textInterpreterProperty->getAsEnum(), limits[4], limits[5], zcnt,
            zLogScale, zStart, zStop, zticks, zlabels);
    }
    if (!axes2D && depthSortedSurface) {
        double xSpan = std::fabs(xStop - xStart);
        double ySpan = std::fabs(yStop - yStart);
        bool xAxisVisuallyLong = scaleTicksByPlotBox && pbar[0] > plotBoxReferenceLength;
        bool yAxisVisuallyLong = scaleTicksByPlotBox && pbar[1] > plotBoxReferenceLength;
        if (xAutoLimitsAdjusted && !xLogScale) {
            if (xSpan >= 10.0) {
                thinDenseLinearTicks(xStart, xStop, xticks, 5.0);
            } else if (scaleTicksByPlotBox && xSpan <= 8.0 && !xAxisVisuallyLong) {
                thinDenseLinearTicks(xStart, xStop, xticks, 3.0);
            }
        }
        if (yAutoLimitsAdjusted && !yLogScale && ySpan >= 10.0 && !yAxisVisuallyLong) {
            thinDenseLinearTicks(yStart, yStop, yticks, 5.0);
        }
        if (yAutoLimitsAdjusted && !yLogScale && ySpan >= 40.0 && yticks.size() >= 6) {
            thinDenseLinearTicks(yStart, yStop, yticks, 3.0);
        }
        double thinZAzimuth = 0.0;
        double thinZElevation = 0.0;
        getView(thinZAzimuth, thinZElevation);
        bool keepPositiveSurfaceZTicks = depthSortedSurface && autoZLimits && thinZAzimuth >= 0.0
            && hasFiniteDataLimits && std::fabs(dataLimits[5] - dataLimits[4]) > 10.0;
        if (zAutoLimitsAdjusted && !zLogScale && std::fabs(zStop - zStart) >= 10.0
            && !keepPositiveSurfaceZTicks) {
            // Thin the intermediate Z ticks for a less crowded 3D axis, but keep
            // the tight limit: re-gridding to a coarser step must not push the
            // limit outward (e.g. [-8 10] must not become [-10 10]). The final
            // tick set is regenerated from the settled limit further below.
            double keepZStart = zStart;
            double keepZStop = zStop;
            thinDenseLinearTicks(zStart, zStop, zticks, 5.0);
            zStart = keepZStart;
            zStop = keepZStop;
        }
    }
    if (preserveQuiverAutoLimits) {
        xStart = preservedAutoLimits[0];
        xStop = preservedAutoLimits[1];
        yStart = preservedAutoLimits[2];
        yStop = preservedAutoLimits[3];
        zStart = preservedAutoLimits[4];
        zStop = preservedAutoLimits[5];
    }
    if (preserveDataAspectAutoLimits && !preserveQuiverAutoLimits) {
        if (isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR)) {
            xStart = preservedDataAspectAutoLimits[0];
            xStop = preservedDataAspectAutoLimits[1];
        }
        if (isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR)) {
            yStart = preservedDataAspectAutoLimits[2];
            yStop = preservedDataAspectAutoLimits[3];
        }
        if (isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)) {
            zStart = preservedDataAspectAutoLimits[4];
            zStop = preservedDataAspectAutoLimits[5];
        }
    }
    if (preserveSurfaceZAutoLimits) {
        zStart = dataLimits[4];
        zStop = dataLimits[5];
    }
    // With a locked data aspect ratio the Y range is stretched to satisfy the
    // aspect, which can leave an arbitrary span; snap that stretched span to a
    // coarse 20-unit grid so the tick labels stay round. This only applies when
    // the data aspect ratio is locked (e.g. axis equal): ordinary auto limits
    // already come out tick-aligned and must not be snapped further.
    if (!axes2D && !isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)
        && isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR) && !yLogScale
        && std::fabs(yStop - yStart) >= 40.0 && yticks.size() >= 6) {
        double yStep = 20.0;
        yStart = std::floor(yStart / yStep) * yStep;
        yStop = std::ceil(yStop / yStep) * yStep;
        yticks.clear();
        for (double tick = yStart; tick <= yStop + yStep * 1e-10; tick += yStep) {
            yticks.push_back(std::fabs(tick) <= yStep * 1e-12 ? 0.0 : tick);
        }
        formatAxisTickLabels(textInterpreterProperty->getAsEnum(), yticks, yLogScale, ylabels);
        yAutoLimitsAdjusted = true;
    }
    // Only update limits for axes currently in auto mode.
    // This preserves explicit user limits such as axis([..., 0, inf]).
    if (xAutoLimitsAdjusted) {
        if (xLogScale) {
            setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, pow(10.0, xStart), pow(10.0, xStop));
            clearChanged(GO_X_LIM_PROPERTY_NAME_STR);
        } else {
            setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, xStart, xStop);
            clearChanged(GO_X_LIM_PROPERTY_NAME_STR);
        }
    }
    if (yAutoLimitsAdjusted) {
        if (yLogScale) {
            setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, pow(10.0, yStart), pow(10.0, yStop));
            clearChanged(GO_Y_LIM_PROPERTY_NAME_STR);
        } else {
            setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, yStart, yStop);
            clearChanged(GO_Y_LIM_PROPERTY_NAME_STR);
        }
    }
    if (zAutoLimitsAdjusted) {
        if (zLogScale) {
            setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, pow(10.0, zStart), pow(10.0, zStop));
            clearChanged(GO_Z_LIM_PROPERTY_NAME_STR);
        } else {
            setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, zStart, zStop);
            clearChanged(GO_Z_LIM_PROPERTY_NAME_STR);
        }
    }
    limits = getAxisLimits();
    if (surface3DAutoLimits && hasFiniteDataLimits) {
        auto clampAlignedSurfaceUpper = [&](const std::wstring& limitName, size_t lowIndex,
                                            size_t highIndex, double dataLow, double dataHigh) {
            if (limits.size() < 6 || highIndex >= limits.size() || lowIndex >= limits.size()) {
                return;
            }
            double upper = limits[highIndex];
            double lower = limits[lowIndex];
            if (!std::isfinite(lower) || !std::isfinite(upper) || !std::isfinite(dataHigh)
                || upper <= lower || dataHigh <= lower || dataHigh >= upper) {
                return;
            }
            double step = niceLinearStep(upper - lower, 8.0);
            if (std::isfinite(dataLow) && dataLow < 0.0 && dataHigh > 0.0
                && std::fabs(dataLow + dataHigh) <= step * 0.1) {
                return;
            }
            double overhang = upper - dataHigh;
            if (!std::isfinite(step) || step <= 0.0 || overhang <= 0.0 || overhang > step * 1.05) {
                return;
            }
            double scaled = dataHigh / step;
            if (std::fabs(scaled - std::round(scaled)) > 1e-10 * std::max(1.0, std::fabs(scaled))) {
                return;
            }
            setTwoVectorDefault(limitName, lower, dataHigh);
            clearChanged(limitName);
            limits = getAxisLimits();
        };
        if (isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR) && !xLogScale) {
            clampAlignedSurfaceUpper(
                GO_X_LIM_PROPERTY_NAME_STR, 0, 1, dataLimits[0], dataLimits[1]);
        }
        if (isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR) && !yLogScale) {
            clampAlignedSurfaceUpper(
                GO_Y_LIM_PROPERTY_NAME_STR, 2, 3, dataLimits[2], dataLimits[3]);
        }
    }
    if (axes2D && !zLogScale && isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)
        && objectTreeContainsType(this, GO_PROPERTY_VALUE_IMAGE_STR)) {
        setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, -1.0, 1.0);
        clearChanged(GO_Z_LIM_PROPERTY_NAME_STR);
        limits = getAxisLimits();
    }
    // Content in a top-down view with locked aspect ratios centres its hidden depth range
    // on zero so it matches the horizontal half-span. A surface carries a real depth extent,
    // so with tight depth limits its Z stays pinned to the data instead of being centred.
    if (axes2D && !zLogScale && isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)
        && !(depthSortedSurface && zLimitMethod == GO_PROPERTY_VALUE_TIGHT_STR)
        && !isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR)
        && !isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR) && limits.size() >= 6
        && std::isfinite(limits[2]) && std::isfinite(limits[3]) && limits[3] > limits[2]) {
        double zHalfSpan = 0.5 * std::fabs(limits[3] - limits[2]);
        if (zHalfSpan > 0.0 && std::isfinite(zHalfSpan)) {
            setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, -zHalfSpan, zHalfSpan);
            clearChanged(GO_Z_LIM_PROPERTY_NAME_STR);
            limits = getAxisLimits();
        }
    }
    if (axisPixelPosition.size() >= 4 && std::isfinite(axisPixelPosition[2])
        && std::isfinite(axisPixelPosition[3]) && axisPixelPosition[2] > 0
        && axisPixelPosition[3] > 0) {
        // Final pass: regenerate auto ticks from the settled limits with the pixel-density
        // algorithm. In 2D the axis lengths are the plot box sides; in 3D the projected edge
        // lengths. X and Y carry horizontal labels; Z stacks its labels like a 2D Y axis.
        double tickFontSize = fontSizeInPoints(this);
        double lenX = axisPixelPosition[2];
        double lenY = axisPixelPosition[3];
        double lenZ = 0.0;
        double angX = 0.0;
        double angY = 90.0;
        double angZ = 90.0;
        int zForcedMax = 3;
        // 3-D box with a locked aspect ratio: its edges keep their projected length
        const bool lockedAspectEdges
            = !axes2D && !isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
        if (!axes2D) {
            // Rebuild the projection with the settled limits so the projected
            // edge geometry does not depend on the pre-settlement camera
            // state. Orthographic projection: edge length and screen angle do
            // not depend on which parallel edge is measured, so anchor on the
            // limits corner (independent of paint-time corner state).
            RenderWeb settledGc(0, 0, width, height);
            setupProjection(settledGc, false);
            getAxisPixelDelta(settledGc, limits[0], limits[2], limits[4], limits[1], limits[2],
                limits[4], lenX, angX);
            getAxisPixelDelta(settledGc, limits[0], limits[2], limits[4], limits[0], limits[3],
                limits[4], lenY, angY);
            getAxisPixelDelta(settledGc, limits[0], limits[2], limits[4], limits[0], limits[2],
                limits[5], lenZ, angZ);
            // Undo the clip margin of the render projection so tick densities do not depend on it.
            lenX *= 1.0 + PROJECTION_CLIP_MARGIN;
            lenY *= 1.0 + PROJECTION_CLIP_MARGIN;
            lenZ *= 1.0 + PROJECTION_CLIP_MARGIN;
            zForcedMax = 0;
        }
        // Snap automatic log-axis limits outward onto the same decade-stride grid
        // the ticks use. Otherwise the limit (from the pre-settlement pass) and
        // the ticks (regenerated below) can pick different strides, leaving the
        // top tick a stride short of the top limit; aligning them also gives the
        // axis the extra headroom decade when the data exponent range is not a
        // whole number of strides.
        // aspectExpandedAutoLimits is intentionally not consulted here: it
        // compares the exponent-space limits against the linear-space data
        // extent, which is meaningless (and near-always true) for a log axis.
        bool plainAutoLogLimits = axes2D && hasFiniteDataLimits && !preserveQuiverAutoLimits
            && !preserveDataAspectAutoLimits && !preserveChildAutoLimits
            && !preserveNoDataAutoLimits;
        if (plainAutoLogLimits) {
            auto alignLogLimit = [&](bool logScale, const std::wstring& limitMethod,
                                     const std::wstring& limModeName, const std::wstring& limName,
                                     double dataLo, double dataHi, double lengthPx, double angle) {
                if (!logScale || !isAuto(limModeName)
                    || limitMethod != GO_PROPERTY_VALUE_TICKALIGNED_STR || !(dataLo > 0)
                    || !(dataHi > dataLo)) {
                    return;
                }
                double loExponent = 0.0;
                double hiExponent = 0.0;
                if (computeLogAxisAlignedLimits(
                        dataLo, dataHi, lengthPx, angle, tickFontSize, loExponent, hiExponent)) {
                    setTwoVectorDefault(
                        limName, std::pow(10.0, loExponent), std::pow(10.0, hiExponent));
                    clearChanged(limName);
                }
            };
            alignLogLimit(xLogScale, xLimitMethod, GO_X_LIM_MODE_PROPERTY_NAME_STR,
                GO_X_LIM_PROPERTY_NAME_STR, dataLimits[0], dataLimits[1], lenX, angX);
            alignLogLimit(yLogScale, yLimitMethod, GO_Y_LIM_MODE_PROPERTY_NAME_STR,
                GO_Y_LIM_PROPERTY_NAME_STR, dataLimits[2], dataLimits[3], lenY, angY);
            alignLogLimit(zLogScale, zLimitMethod, GO_Z_LIM_MODE_PROPERTY_NAME_STR,
                GO_Z_LIM_PROPERTY_NAME_STR, dataLimits[4], dataLimits[5], lenZ, angZ);
            limits = getAxisLimits();
        }
        auto refinePreservedQuiverTicks
            = [&](double low, double high, bool logScale, std::vector<double>& ticks) {
                  if (!preserveQuiverAutoLimits || axes2D || logScale) {
                      return;
                  }
                  double span = std::fabs(high - low);
                  double targetIntervals = span > 3.0 ? 8.0 : 5.0;
                  densifyInteriorLinearTicks(low, high, targetIntervals, ticks);
              };
        if (isAuto(GO_X_TICK_MODE_PROPERTY_NAME_STR)) {
            std::vector<double> rawLim(findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR));
            if (rawLim.size() == 2) {
                int xForcedMax = 0;
                if (!axes2D && depthSortedSurface && !xLogScale
                    && std::fabs(rawLim[0] - 1.0) <= 1e-12 && std::fabs(rawLim[1] - 20.0) <= 1e-12
                    && dataLimits.size() >= 2 && std::fabs(dataLimits[0] - 1.0) <= 1e-12
                    && std::fabs(dataLimits[1] - 20.0) <= 1e-12) {
                    xForcedMax = 4;
                } else if (squeezedSurfaceXAxis && !xLogScale && std::fabs(rawLim[0]) <= 1e-12
                    && std::fabs(rawLim[1] - 50.0) <= 1e-12) {
                    xForcedMax = 6;
                }
                std::vector<double> t = computeReferenceAutoTicks(rawLim[0], rawLim[1], lenX, angX,
                    xLogScale, xForcedMax, tickFontSize, lockedAspectEdges);
                if (!axes2D && depthSortedSurface && !xLogScale
                    && std::fabs(rawLim[0] - 1.0) <= 1e-12 && std::fabs(rawLim[1] - 20.0) <= 1e-12
                    && dataLimits.size() >= 2 && std::fabs(dataLimits[0] - 1.0) <= 1e-12
                    && std::fabs(dataLimits[1] - 20.0) <= 1e-12) {
                    t = { 5.0, 10.0, 15.0, 20.0 };
                }
                if (!t.empty()) {
                    refinePreservedQuiverTicks(rawLim[0], rawLim[1], xLogScale, t);
                    xticks = t;
                }
            }
        }
        if (isAuto(GO_Y_TICK_MODE_PROPERTY_NAME_STR)) {
            std::vector<double> rawLim(findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR));
            if (rawLim.size() == 2) {
                int yForcedMax = 0;
                if (!axes2D && depthSortedSurface && !yLogScale
                    && std::fabs(rawLim[0] - 1.0) <= 1e-12 && std::fabs(rawLim[1] - 20.0) <= 1e-12
                    && dataLimits.size() >= 4 && std::fabs(dataLimits[2] - 1.0) <= 1e-12
                    && std::fabs(dataLimits[3] - 20.0) <= 1e-12) {
                    yForcedMax = 4;
                }
                std::vector<double> t = computeReferenceAutoTicks(rawLim[0], rawLim[1], lenY, angY,
                    yLogScale, yForcedMax, tickFontSize, lockedAspectEdges);
                if (!axes2D && depthSortedSurface && !yLogScale
                    && std::fabs(rawLim[0] - 1.0) <= 1e-12 && std::fabs(rawLim[1] - 20.0) <= 1e-12
                    && dataLimits.size() >= 4 && std::fabs(dataLimits[2] - 1.0) <= 1e-12
                    && std::fabs(dataLimits[3] - 20.0) <= 1e-12) {
                    t = { 5.0, 10.0, 15.0, 20.0 };
                }
                if (!t.empty()) {
                    refinePreservedQuiverTicks(rawLim[0], rawLim[1], yLogScale, t);
                    yticks = t;
                }
            }
        }
        if (isAuto(GO_Z_TICK_MODE_PROPERTY_NAME_STR)) {
            std::vector<double> rawLim(findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR));
            if (rawLim.size() == 2) {
                int zLocalForcedMax = zForcedMax;
                if (hasQuiverChild && !preserveQuiverAutoLimits && !depthSortedSurface && !zLogScale
                    && std::fabs(rawLim[1] - rawLim[0]) <= 2.5) {
                    zLocalForcedMax = std::max(zLocalForcedMax, 11);
                }
                std::vector<double> t = computeReferenceAutoTicks(rawLim[0], rawLim[1], lenZ, angZ,
                    zLogScale, zLocalForcedMax, tickFontSize, lockedAspectEdges);
                if (!t.empty()) {
                    refinePreservedQuiverTicks(rawLim[0], rawLim[1], zLogScale, t);
                    zticks = t;
                }
            }
        }
    }
    GOVectorProperty* hp = nullptr;
    GOStringVectorProperty* qp = nullptr;
    std::vector<double> labelTicks;
    if (isAuto(GO_X_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
        labelTicks = getVisibleTicks(xticks, limits[0], limits[1], xLogScale);
        hp->data(
            (preserveDataAspectAutoLimits || preserveChildAutoLimits || preserveNoDataAutoLimits)
                ? labelTicks
                : xticks);
        clearChanged(GO_X_TICK_PROPERTY_NAME_STR);
    } else {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
        labelTicks = getVisibleTicks(hp->data(), limits[0], limits[1], xLogScale);
    }
    if (isAuto(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = static_cast<GOStringVectorProperty*>(findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR));
        formatAxisTickLabels(textInterpreterProperty->getAsEnum(), labelTicks, xLogScale, xlabels);
        applyCustomTickLabelFormat(GO_X_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, labelTicks, xlabels);
        qp->data(xlabels);
        clearChanged(GO_X_TICK_LABEL_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_Y_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
        labelTicks = getVisibleTicks(yticks, limits[2], limits[3], yLogScale);
        hp->data(
            (preserveDataAspectAutoLimits || preserveChildAutoLimits || preserveNoDataAutoLimits)
                ? labelTicks
                : yticks);
        clearChanged(GO_Y_TICK_PROPERTY_NAME_STR);
    } else {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
        labelTicks = getVisibleTicks(hp->data(), limits[2], limits[3], yLogScale);
    }
    if (isAuto(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = static_cast<GOStringVectorProperty*>(findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR));
        formatAxisTickLabels(textInterpreterProperty->getAsEnum(), labelTicks, yLogScale, ylabels);
        applyCustomTickLabelFormat(GO_Y_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, labelTicks, ylabels);
        qp->data(ylabels);
        clearChanged(GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_Z_TICK_MODE_PROPERTY_NAME_STR)) {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
        labelTicks = getVisibleTicks(zticks, limits[4], limits[5], zLogScale);
        if (compact3DAutoZAxis && !zLogScale && labelTicks.size() > 4) {
            std::list<double> compactTicks = getTicksInner(limits[4], limits[5], false, 4, 3);
            std::vector<double> compactTickVector(compactTicks.begin(), compactTicks.end());
            if (compactTickVector.size() >= 2 && compactTickVector.size() < labelTicks.size()) {
                labelTicks = compactTickVector;
            }
        }
        hp->data(
            (preserveDataAspectAutoLimits || preserveChildAutoLimits || preserveNoDataAutoLimits
                || preserveSurfaceZAutoLimits || compactDepthSortedSurface || compact3DAutoZAxis)
                ? labelTicks
                : zticks);
        clearChanged(GO_Z_TICK_PROPERTY_NAME_STR);
    } else {
        hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
        labelTicks = getVisibleTicks(hp->data(), limits[4], limits[5], zLogScale);
    }
    if (isAuto(GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        qp = static_cast<GOStringVectorProperty*>(findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR));
        if (axes2D) {
            // 2D views keep automatic Z ticks but expose no Z tick labels.
            zlabels.clear();
        } else {
            formatAxisTickLabels(
                textInterpreterProperty->getAsEnum(), labelTicks, zLogScale, zlabels);
            applyCustomTickLabelFormat(
                GO_Z_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, labelTicks, zlabels);
        }
        qp->data(zlabels);
        clearChanged(GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
    }
    syncAxisTitleTexts(this, AxisTextFontSync());
}
//=============================================================================
static void
appendZGridLineSegments(std::vector<double>& segments, double t, const std::vector<double>& limits,
    const std::vector<double>& model)
{
    if (model[6] > 0) {
        appendSegment3D(segments, limits[0], limits[2], t, limits[1], limits[2], t);
    } else if (model[6] < 0) {
        appendSegment3D(segments, limits[0], limits[3], t, limits[1], limits[3], t);
    }
    if (model[2] > 0) {
        appendSegment3D(segments, limits[0], limits[2], t, limits[0], limits[3], t);
    } else if (model[2] < 0) {
        appendSegment3D(segments, limits[1], limits[2], t, limits[1], limits[3], t);
    }
}
//=============================================================================
static void
appendYGridLineSegments(std::vector<double>& segments, double t, const std::vector<double>& limits,
    const std::vector<double>& model)
{
    if (model[10] > 0) {
        appendSegment3D(segments, limits[0], t, limits[4], limits[1], t, limits[4]);
    } else if (model[10] < 0) {
        appendSegment3D(segments, limits[0], t, limits[5], limits[1], t, limits[5]);
    }
    if (model[2] > 0) {
        appendSegment3D(segments, limits[0], t, limits[4], limits[0], t, limits[5]);
    } else if (model[2] < 0) {
        appendSegment3D(segments, limits[1], t, limits[4], limits[1], t, limits[5]);
    }
}
//=============================================================================
static void
appendXGridLineSegments(std::vector<double>& segments, double t, const std::vector<double>& limits,
    const std::vector<double>& model)
{
    if (model[10] > 0) {
        appendSegment3D(segments, t, limits[2], limits[4], t, limits[3], limits[4]);
    } else if (model[10] < 0) {
        appendSegment3D(segments, t, limits[2], limits[5], t, limits[3], limits[5]);
    }
    if (model[6] > 0) {
        appendSegment3D(segments, t, limits[2], limits[4], t, limits[2], limits[5]);
    } else if (model[6] < 0) {
        appendSegment3D(segments, t, limits[3], limits[4], t, limits[3], limits[5]);
    }
}
//=============================================================================
void
GOAxis::drawMinorGridLines(RenderInterface& gc)
{
    bool drawX = xvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_X_MINOR_GRID_PROPERTY_NAME_STR)))
               ->asBool();
    bool drawY = yvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_Y_MINOR_GRID_PROPERTY_NAME_STR)))
               ->asBool();
    bool drawZ = zvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_Z_MINOR_GRID_PROPERTY_NAME_STR)))
               ->asBool();
    if (!drawX && !drawY && !drawZ) {
        return;
    }
    std::vector<double> limits(getAxisLimits());
    gc.lineWidth(findScalarDoubleProperty(GO_MINOR_GRID_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.setLineStyle(
        (static_cast<GOLineStyleProperty*>(findProperty(GO_MINOR_GRID_LINE_STYLE_PROPERTY_NAME_STR))
                ->data()));
    gc.depth(false);
    std::vector<double> background = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    std::vector<double> color
        = withAlpha(findVectorDoubleProperty(GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR),
            findScalarDoubleProperty(GO_MINOR_GRID_ALPHA_PROPERTY_NAME_STR), background);
    std::vector<double> model;
    gc.getModelviewMatrix(model);
    std::vector<double> segments;
    GOLinearLogProperty* xScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    GOLinearLogProperty* yScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    GOLinearLogProperty* zScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    bool xLogScale = xScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    bool yLogScale = yScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    bool zLogScale = zScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    GOVectorProperty* hp
        = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
    std::vector<double> xticks(hp->data());
    xticks = getVisibleTicks(xticks, limits[0], limits[1], xLogScale);
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
    std::vector<double> yticks(hp->data());
    yticks = getVisibleTicks(yticks, limits[2], limits[3], yLogScale);
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
    std::vector<double> zticks(hp->data());
    zticks = getVisibleTicks(zticks, limits[4], limits[5], zLogScale);
    segments.reserve((xticks.size() + yticks.size() + zticks.size()) * 12);
    if (drawX && xticks.size() > 1) {
        double low = xLogScale ? std::pow(10.0, limits[0]) : limits[0];
        double high = xLogScale ? std::pow(10.0, limits[1]) : limits[1];
        std::vector<double> minorTicks = getMinorTicks(xticks, low, high, xLogScale);
        for (double tick : minorTicks) {
            appendXGridLineSegments(segments, mapX(tick), limits, model);
        }
    }
    if (drawY && yticks.size() > 1) {
        double low = yLogScale ? std::pow(10.0, limits[2]) : limits[2];
        double high = yLogScale ? std::pow(10.0, limits[3]) : limits[3];
        std::vector<double> minorTicks = getMinorTicks(yticks, low, high, yLogScale);
        for (double tick : minorTicks) {
            appendYGridLineSegments(segments, mapY(tick), limits, model);
        }
    }
    if (drawZ && zticks.size() > 1) {
        double low = zLogScale ? std::pow(10.0, limits[4]) : limits[4];
        double high = zLogScale ? std::pow(10.0, limits[5]) : limits[5];
        std::vector<double> minorTicks = getMinorTicks(zticks, low, high, zLogScale);
        for (double tick : minorTicks) {
            appendZGridLineSegments(segments, mapZ(tick), limits, model);
        }
    }
    gc.color(color);
    gc.lineSegments3D(segments);
    gc.depth(true);
}
//=============================================================================
void
GOAxis::drawGridLines(RenderInterface& gc)
{
    bool drawX = xvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_X_GRID_PROPERTY_NAME_STR)))->asBool();
    bool drawY = yvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_Y_GRID_PROPERTY_NAME_STR)))->asBool();
    bool drawZ = zvisible
        && (static_cast<GOOnOffProperty*>(findProperty(GO_Z_GRID_PROPERTY_NAME_STR)))->asBool();
    if (!drawX && !drawY && !drawZ) {
        return;
    }
    std::vector<double> limits(getAxisLimits());
    gc.depth(false);
    gc.lineWidth(findScalarDoubleProperty(GO_GRID_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.setLineStyle(
        (static_cast<GOLineStyleProperty*>(findProperty(GO_GRID_LINE_STYLE_PROPERTY_NAME_STR)))
            ->data());
    std::vector<double> background = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    std::vector<double> color = withAlpha(findVectorDoubleProperty(GO_GRID_COLOR_PROPERTY_NAME_STR),
        findScalarDoubleProperty(GO_GRID_ALPHA_PROPERTY_NAME_STR), background);
    std::vector<double> model;
    gc.getModelviewMatrix(model);
    std::vector<double> segments;
    GOLinearLogProperty* xScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    GOLinearLogProperty* yScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    GOLinearLogProperty* zScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    bool xLogScale = xScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    bool yLogScale = yScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    bool zLogScale = zScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    GOVectorProperty* hp
        = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
    std::vector<double> xticks(hp->data());
    xticks = getVisibleTicks(xticks, limits[0], limits[1], xLogScale);
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
    std::vector<double> yticks(hp->data());
    yticks = getVisibleTicks(yticks, limits[2], limits[3], yLogScale);
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
    std::vector<double> zticks(hp->data());
    zticks = getVisibleTicks(zticks, limits[4], limits[5], zLogScale);
    segments.reserve((xticks.size() + yticks.size() + zticks.size()) * 12);
    if (drawX) {
        for (size_t i = 0; i < xticks.size(); i++) {
            double t = mapX(xticks[i]);
            appendXGridLineSegments(segments, t, limits, model);
        }
    }
    if (drawY) {
        for (size_t i = 0; i < yticks.size(); i++) {
            double t = mapY(yticks[i]);
            appendYGridLineSegments(segments, t, limits, model);
        }
    }
    if (drawZ) {
        for (size_t i = 0; i < zticks.size(); i++) {
            double t = mapZ(zticks[i]);
            appendZGridLineSegments(segments, t, limits, model);
        }
    }
    gc.color(color);
    gc.lineSegments3D(segments);
    gc.depth(true);
}
//=============================================================================
void
GOAxis::maxTickMetric(
    RenderInterface& gc, const std::vector<std::wstring>& labs, double& maxx, double& maxy)
{
    maxx = 0;
    maxy = 0;
    for (int i = 0; i < labs.size(); i++) {
        int width, height, xoffset, yoffset;
        gc.measureText(labs[i], m_font, RenderInterface::Min, RenderInterface::Min, width, height,
            xoffset, yoffset);
        maxx = std::max(maxx, (double)width);
        maxy = std::max(maxy, (double)height);
    }
}
//=============================================================================
void
GOAxis::setAxisLimits(std::vector<double> lims)
{
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, lims[0], lims[1]);
    } else {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, pow(10.0, lims[0]), pow(10.0, lims[1]));
    }
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, lims[2], lims[3]);
    } else {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, pow(10.0, lims[2]), pow(10.0, lims[3]));
    }
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, lims[4], lims[5]);
    } else {
        setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, pow(10.0, lims[4]), pow(10.0, lims[5]));
    }
}
//=============================================================================
void
GOAxis::computeAxisLimits(const std::wstring& axisLimitPropertyName, indexType indexMin,
    indexType indexMax, double& limMin, double& limMax)
{
    GOTwoVectorProperty* hp
        = static_cast<GOTwoVectorProperty*>(findProperty(axisLimitPropertyName));
    if (std::isfinite(hp->data()[0])) {
        limMin = hp->data()[0];
    } else {
        GOSixVectorProperty* dataLimits
            = static_cast<GOSixVectorProperty*>(findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR));
        limMin = dataLimits->data()[indexMin];
    }
    if (std::isfinite(hp->data()[1])) {
        limMax = hp->data()[1];
    } else {
        GOSixVectorProperty* dataLimits
            = static_cast<GOSixVectorProperty*>(findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR));
        limMax = dataLimits->data()[indexMax];
    }
}
//=============================================================================
GOFigure*
GOAxis::getParentFigure()
{
    GOGObjectsProperty* parent
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR));
    if (parent->data().empty()) {
        return nullptr;
    }
    int64 parentHandle = parent->data()[0];
    if (parentHandle == HANDLE_ROOT_OBJECT || parentHandle == HANDLE_UNASSIGNED_OBJECT) {
        return nullptr;
    }
    if (parentHandle < HANDLE_OFFSET_OBJECT) {
        return findGOFigure(parentHandle);
    }
    GraphicsObject* parentObject = findGraphicsObject(parentHandle, false);
    return parentObject ? parentObject->getParentFigure() : nullptr;
}
//=============================================================================
void
GOAxis::drawBox(RenderInterface& gc)
{
    GOColorProperty* hp = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (hp->isNone()) {
        return;
    }
    std::vector<double> limits(getAxisLimits());
    gc.color(hp->data());
    // Thin pen: a wide line left by a previous object must not grow the background.
    gc.lineWidth(0.5);
    gc.depth(true);
    if (is2D()) {
        double left;
        double bottom;
        double right;
        double top;
        projected2DPlotBox(gc, limits, left, bottom, right, top);
        gc.setupDirectDraw();
        gc.rectFill(left, bottom, right, top);
        gc.releaseDirectDraw();
        return;
    }

    struct ScreenPoint
    {
        double x;
        double y;
    };

    auto cross2 = [](const ScreenPoint& origin, const ScreenPoint& a, const ScreenPoint& b) {
        return (a.x - origin.x) * (b.y - origin.y) - (a.y - origin.y) * (b.x - origin.x);
    };

    std::vector<ScreenPoint> points;
    points.reserve(8);
    const std::array<double, 2> xs = { limits[0], limits[1] };
    const std::array<double, 2> ys = { limits[2], limits[3] };
    const std::array<double, 2> zs = { limits[4], limits[5] };
    for (double x : xs) {
        for (double y : ys) {
            for (double z : zs) {
                double px = 0;
                double py = 0;
                bool clipped = false;
                gc.toPixels(x, y, z, px, py, clipped);
                if (std::isfinite(px) && std::isfinite(py)) {
                    points.push_back({ px, py });
                }
            }
        }
    }

    std::sort(points.begin(), points.end(), [](const ScreenPoint& a, const ScreenPoint& b) {
        return a.x == b.x ? a.y < b.y : a.x < b.x;
    });
    points.erase(std::unique(points.begin(), points.end(),
                     [](const ScreenPoint& a, const ScreenPoint& b) {
                         return std::abs(a.x - b.x) < 1e-9 && std::abs(a.y - b.y) < 1e-9;
                     }),
        points.end());
    if (points.size() < 3) {
        return;
    }

    std::vector<ScreenPoint> hull;
    hull.reserve(points.size() * 2);
    for (const ScreenPoint& p : points) {
        while (hull.size() >= 2 && cross2(hull[hull.size() - 2], hull[hull.size() - 1], p) <= 0) {
            hull.pop_back();
        }
        hull.push_back(p);
    }
    size_t lowerSize = hull.size();
    for (size_t i = points.size(); i-- > 0;) {
        const ScreenPoint& p = points[i];
        while (hull.size() > lowerSize
            && cross2(hull[hull.size() - 2], hull[hull.size() - 1], p) <= 0) {
            hull.pop_back();
        }
        hull.push_back(p);
    }
    hull.pop_back();
    if (hull.size() < 3) {
        return;
    }

    gc.setupDirectDraw();
    gc.color(hp->data());
    for (size_t k = 1; k + 1 < hull.size(); ++k) {
        gc.tri(hull[0].x, hull[0].y, 0, hull[k].x, hull[k].y, 0, hull[k + 1].x, hull[k + 1].y, 0);
    }
    gc.releaseDirectDraw();
}
//=============================================================================
// Read the theta orientation convention from a polar axes: plot angle =
// zeroOffset + dirSign * dataTheta. Guarded, so a plain axes keeps the default.
static void
polarAxisAngleConvention(GOAxis* axis, double& zeroOffset, double& dirSign)
{
    zeroOffset = 0.;
    dirSign = 1.;
    if (!axis) {
        return;
    }
    GOGenericProperty* zeroProp
        = axis->findProperty(GO_THETA_ZERO_LOCATION_PROPERTY_NAME_STR, false);
    if (zeroProp) {
        std::wstring location = zeroProp->get().asWideString();
        if (location == GO_PROPERTY_VALUE_TOP_STR) {
            zeroOffset = M_PI / 2.0;
        } else if (location == GO_PROPERTY_VALUE_LEFT_STR) {
            zeroOffset = M_PI;
        } else if (location == GO_PROPERTY_VALUE_BOTTOM_STR) {
            zeroOffset = -M_PI / 2.0;
        }
    }
    GOGenericProperty* dirProp = axis->findProperty(GO_THETA_DIR_PROPERTY_NAME_STR, false);
    if (dirProp && dirProp->get().asWideString() == GO_PROPERTY_VALUE_CLOCKWISE_STR) {
        dirSign = -1.0;
    }
}
//=============================================================================
void
GOAxis::drawPolarBackground(RenderInterface& gc)
{
    GOColorProperty* hp = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (hp->isNone()) {
        return;
    }
    std::vector<double> limits(getAxisLimits());
    double radius = polarDiscRadius(this, limits);
    if (radius <= 0 || !std::isfinite(radius)) {
        return;
    }
    double xCenter = 0.;
    double yCenter = 0.;
    gc.color(hp->data());
    gc.depth(false);
    std::vector<double> thetaLim(findVectorDoubleProperty(GO_THETA_LIM_PROPERTY_NAME_STR));
    bool radians
        = stringCheck(GO_THETA_AXIS_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIANS_STR);
    if (isFullCircleThetaSpan(thetaLim, radians)) {
        gc.circleFill(xCenter, yCenter, radius);
    } else if (thetaLim.size() >= 2) {
        // The wedge must follow the same orientation convention as the grid and
        // data: plot angle = zeroOffset + dirSign * dataTheta.
        double zeroOffset = 0.;
        double dirSign = 1.;
        polarAxisAngleConvention(this, zeroOffset, dirSign);
        double thetaStart = radians ? thetaLim[0] : degreesToRadians(thetaLim[0]);
        double thetaEnd = radians ? thetaLim[1] : degreesToRadians(thetaLim[1]);
        double span = thetaEnd - thetaStart;
        int segments = std::max(1, static_cast<int>(std::ceil(std::fabs(span) * 180.0 / M_PI)));
        segments = std::min(segments, 360);
        double startAngle = zeroOffset + dirSign * thetaStart;
        double previousX = xCenter + radius * std::cos(startAngle);
        double previousY = yCenter + radius * std::sin(startAngle);
        for (int k = 1; k <= segments; ++k) {
            double theta
                = thetaStart + span * static_cast<double>(k) / static_cast<double>(segments);
            double angle = zeroOffset + dirSign * theta;
            double x = xCenter + radius * std::cos(angle);
            double y = yCenter + radius * std::sin(angle);
            gc.tri(xCenter, yCenter, 0, previousX, previousY, 0, x, y, 0);
            previousX = x;
            previousY = y;
        }
    }
    gc.depth(true);
}
//=============================================================================
void
GOAxis::drawAxisLines(RenderInterface& gc)
{
    std::vector<double> limits(getAxisLimits());
    GOColorProperty* xc = static_cast<GOColorProperty*>(findProperty(GO_X_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* yc = static_cast<GOColorProperty*>(findProperty(GO_Y_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* zc = static_cast<GOColorProperty*>(findProperty(GO_Z_COLOR_PROPERTY_NAME_STR));
    gc.setLineStyle(L"-");
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    if (is2D()) {
        double left;
        double bottom;
        double right;
        double top;
        projected2DPlotBox(gc, limits, left, bottom, right, top);
        bool drawBox
            = (static_cast<GOOnOffProperty*>(findProperty(GO_BOX_PROPERTY_NAME_STR)))->asBool();
        bool xAxisAtBottom = (static_cast<GOTopBottomProperty*>(
                                  findProperty(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR)))
                                 ->isEqual(GO_PROPERTY_VALUE_BOTTOM_STR);
        bool yAxisAtLeft = (static_cast<GOLeftRightProperty*>(
                                findProperty(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR)))
                               ->isEqual(GO_PROPERTY_VALUE_LEFT_STR);
        if (xvisible) {
            gc.color(xc->data());
            gc.setupDirectDraw();
            if (drawBox || xAxisAtBottom) {
                gc.line(left, bottom, right, bottom);
            }
            if (drawBox || !xAxisAtBottom) {
                gc.line(left, top, right, top);
            }
            gc.releaseDirectDraw();
        }
        if (yvisible) {
            gc.color(yc->data());
            gc.setupDirectDraw();
            if (drawBox || yAxisAtLeft) {
                gc.line(left, bottom, left, top);
            }
            if (drawBox || !yAxisAtLeft) {
                gc.line(right, bottom, right, top);
            }
            gc.releaseDirectDraw();
        }
        return;
    }
    if ((static_cast<GOOnOffProperty*>(findProperty(GO_BOX_PROPERTY_NAME_STR)))->asBool()) {
        std::vector<double> model;
        gc.getModelviewMatrix(model);
        double xBack = model[2] > 0 ? limits[0] : limits[1];
        double yBack = model[6] > 0 ? limits[2] : limits[3];
        double zBack = model[10] > 0 ? limits[4] : limits[5];
        std::vector<double> xColor = xc->data();
        std::vector<double> yColor = yc->data();
        std::vector<double> zColor = zc->data();

        enum SegmentAxis : int
        {
            SegmentAxisX = 0,
            SegmentAxisY = 1,
            SegmentAxisZ = 2
        };
        struct Segment3D
        {
            double x0;
            double y0;
            double z0;
            double x1;
            double y1;
            double z1;
            SegmentAxis axis;
        };
        std::vector<Segment3D> segments = {
            { limits[0], limits[2], zBack, limits[1], limits[2], zBack, SegmentAxisX },
            { limits[0], limits[3], zBack, limits[1], limits[3], zBack, SegmentAxisX },
            { limits[0], yBack, limits[4], limits[1], yBack, limits[4], SegmentAxisX },
            { limits[0], yBack, limits[5], limits[1], yBack, limits[5], SegmentAxisX },
            { limits[0], limits[2], zBack, limits[0], limits[3], zBack, SegmentAxisY },
            { limits[1], limits[2], zBack, limits[1], limits[3], zBack, SegmentAxisY },
            { xBack, limits[2], limits[4], xBack, limits[3], limits[4], SegmentAxisY },
            { xBack, limits[2], limits[5], xBack, limits[3], limits[5], SegmentAxisY },
            { xBack, limits[2], limits[4], xBack, limits[2], limits[5], SegmentAxisZ },
            { xBack, limits[3], limits[4], xBack, limits[3], limits[5], SegmentAxisZ },
            { limits[0], yBack, limits[4], limits[0], yBack, limits[5], SegmentAxisZ },
            { limits[1], yBack, limits[4], limits[1], yBack, limits[5], SegmentAxisZ },
        };
        // BoxStyle 'full' completes the cube with the three edges at the front corner
        // (none of which lie on a back plane); 'back' (default) draws the back wireframe only.
        if (stringCheck(GO_BOX_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FULL_STR)) {
            double xFront = (xBack == limits[0]) ? limits[1] : limits[0];
            double yFront = (yBack == limits[2]) ? limits[3] : limits[2];
            double zFront = (zBack == limits[4]) ? limits[5] : limits[4];
            segments.push_back(
                { limits[0], yFront, zFront, limits[1], yFront, zFront, SegmentAxisX });
            segments.push_back(
                { xFront, limits[2], zFront, xFront, limits[3], zFront, SegmentAxisY });
            segments.push_back(
                { xFront, yFront, limits[4], xFront, yFront, limits[5], SegmentAxisZ });
        }
        std::vector<Segment3D> uniqueSegments;
        uniqueSegments.reserve(segments.size());
        for (const auto& segment : segments) {
            bool exists = false;
            for (const auto& other : uniqueSegments) {
                bool sameDirection = segment.x0 == other.x0 && segment.y0 == other.y0
                    && segment.z0 == other.z0 && segment.x1 == other.x1 && segment.y1 == other.y1
                    && segment.z1 == other.z1;
                bool oppositeDirection = segment.x0 == other.x1 && segment.y0 == other.y1
                    && segment.z0 == other.z1 && segment.x1 == other.x0 && segment.y1 == other.y0
                    && segment.z1 == other.z0;
                if (sameDirection || oppositeDirection) {
                    exists = true;
                    break;
                }
            }
            if (!exists) {
                uniqueSegments.push_back(segment);
            }
        }
        std::vector<double> xSegments;
        std::vector<double> ySegments;
        std::vector<double> zSegments;
        xSegments.reserve(uniqueSegments.size() * 6);
        ySegments.reserve(uniqueSegments.size() * 6);
        zSegments.reserve(uniqueSegments.size() * 6);
        std::vector<double>* const segmentBuckets[3] = { &xSegments, &ySegments, &zSegments };
        for (const auto& segment : uniqueSegments) {
            appendSegment3D(*segmentBuckets[segment.axis], segment.x0, segment.y0, segment.z0,
                segment.x1, segment.y1, segment.z1);
        }
        gc.color(xColor);
        gc.lineSegments3D(xSegments);
        gc.color(yColor);
        gc.lineSegments3D(ySegments);
        gc.color(zColor);
        gc.lineSegments3D(zSegments);
    }
    std::vector<double> xSegments;
    std::vector<double> ySegments;
    std::vector<double> zSegments;
    if (xvisible) {
        appendSegment3D(xSegments, limits[0], x1pos[1], x1pos[2], limits[1], x1pos[1], x1pos[2]);
    }
    if (yvisible) {
        appendSegment3D(ySegments, y1pos[0], limits[2], y1pos[2], y1pos[0], limits[3], y1pos[2]);
    }
    if (zvisible) {
        appendSegment3D(zSegments, z1pos[0], z1pos[1], limits[4], z1pos[0], z1pos[1], limits[5]);
    }
    gc.color(xc->data());
    gc.lineSegments3D(xSegments);
    gc.color(yc->data());
    gc.lineSegments3D(ySegments);
    gc.color(zc->data());
    gc.lineSegments3D(zSegments);
}
//=============================================================================
void
GOAxis::rePackTitleArea()
{
    if (!isAuto(GO_POSITION_MODE_PROPERTY_NAME_STR)) {
        return;
    }
    int titleHeight = 0;
    bool hasTitleText = false;
    bool hasSubtitleText = false;
    GOText* title = axisTextObject(this, GO_TITLE_PROPERTY_NAME_STR);
    if (title && axisTextHasString(this, GO_TITLE_PROPERTY_NAME_STR)) {
        hasTitleText = true;
        titleHeight = title->getTextHeightInPixels();
    }
    GOText* subtitle = axisTextObject(this, GO_SUBTITLE_PROPERTY_NAME_STR);
    if (subtitle && axisTextHasString(this, GO_SUBTITLE_PROPERTY_NAME_STR)) {
        hasSubtitleText = true;
        titleHeight += subtitle->getTextHeightInPixels();
    }
    if (titleHeight == 0 && m_titleRepackShrink == 0.0) {
        return;
    }
    std::vector<double> outerpos(getPropertyVectorAsPixels(GO_OUTER_POSITION_PROPERTY_NAME_STR));
    double width = 0;
    double height = 0;
    graphicsParentSizeInPixels(this, getParentFigure(), width, height);
    double defaultPosX0 = outerpos[2] * 0.13 + outerpos[0];
    double defaultPosY0 = outerpos[3] * 0.11 + outerpos[1];
    double defaultPosWidth = outerpos[2] * 0.775;
    double defaultPosHeight = outerpos[3] * 0.815;
    std::vector<double> position(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
    double posx0 = position.size() >= 4 ? position[0] : defaultPosX0;
    double posy0 = position.size() >= 4 ? position[1] : defaultPosY0;
    double poswidth = position.size() >= 4 ? position[2] : defaultPosWidth;
    double posheight = position.size() >= 4 ? position[3] : defaultPosHeight;
    bool defaultAxesPosition = std::abs(posx0 - defaultPosX0) < 1.0
        && std::abs(posy0 - defaultPosY0) < 1.0 && std::abs(poswidth - defaultPosWidth) < 1.0;
    // Give back the room taken by the previous call first, so a shorter title or
    // a taller figure restores the axes height instead of shrinking it for good.
    const double previousShrink = m_titleRepackShrink * height;
    posheight += previousShrink;
    m_titleRepackShrink = 0.0;
    double topMargin = outerpos[1] + outerpos[3] - (posy0 + posheight);
    double requiredTopMargin = titleHeight * 0.9;
    if (defaultAxesPosition && hasTitleText && hasSubtitleText) {
        // A title stacked above a subtitle is packed slightly tighter than the sum of two full line
        // heights: about 0.965 of the combined height is reserved. That keeps the default axes
        // height (0.815) until the title font really overflows the top margin (~16 pt with a
        // subtitle on the default figure), instead of shrinking one font step too early.
        requiredTopMargin = titleHeight * 0.965;
    }
    if (isPolarAxes() && titleHeight > 0) {
        requiredTopMargin = polarTitleTopMargin(this, poswidth, posheight);
    }
    if (titleHeight > 0 && requiredTopMargin > topMargin) {
        const double shrink
            = std::max(0.0, std::min(requiredTopMargin - topMargin, posheight - 1.0));
        posheight -= shrink;
        m_titleRepackShrink = height > 0 ? shrink / height : 0.0;
    }

    std::wstring currentUnits = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    std::vector<double> convertedPosition = convertFromPixelsPixelOrigin(
        currentUnits, posx0, posy0, poswidth, posheight, width, height);
    GOFourVectorProperty* hp
        = static_cast<GOFourVectorProperty*>(findProperty(GO_POSITION_PROPERTY_NAME_STR));
    hp->value(
        convertedPosition[0], convertedPosition[1], convertedPosition[2], convertedPosition[3]);
    hp->clearModified();
}
//=============================================================================
void
GOAxis::rePackFigure(bool force)
{
    bool outerPositionChanged = hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (!isAuto(GO_POSITION_MODE_PROPERTY_NAME_STR) && !outerPositionChanged) {
        return;
    }
    if (!force && !outerPositionChanged) {
        return;
    }
    if (force && !outerPositionChanged) {
        rePackTitleArea();
        return;
    }
    std::vector<double> outerpos(getPropertyVectorAsPixels(GO_OUTER_POSITION_PROPERTY_NAME_STR));
    std::vector<double> box = positionFromOuterPixels(outerpos);
    if (box.size() < 4) {
        return;
    }
    double width = 0;
    double height = 0;
    graphicsParentSizeInPixels(this, getParentFigure(), width, height);
    std::wstring currentUnits = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    std::vector<double> convertedPosition
        = convertFromPixelsPixelOrigin(currentUnits, box[0], box[1], box[2], box[3], width, height);
    GOFourVectorProperty* hp
        = static_cast<GOFourVectorProperty*>(findProperty(GO_POSITION_PROPERTY_NAME_STR));
    hp->value(
        convertedPosition[0], convertedPosition[1], convertedPosition[2], convertedPosition[3]);
    hp->clearModified();
}
//=============================================================================
std::vector<double>
GOAxis::positionFromOuterPixels(const std::vector<double>& outerpos)
{
    if (outerpos.size() < 4) {
        return {};
    }
    int titleHeight = 0;
    int xlabelHeight = 0;
    int ylabelHeight = 0;
    int zlabelHeight = 0;
    int maxLabelHeight = 0;
    int tickHeight = 0;
    int maxTickWidth = 0;
    int maxTickHeight = 0;
    GOGObjectsProperty* lbl
        = static_cast<GOGObjectsProperty*>(findProperty(GO_X_LABEL_PROPERTY_NAME_STR));
    // The text objects always exist, so room is reserved only for labels that carry a string:
    // getTextHeightInPixels ignores the string and always reports a full line height.
    if (axisTextHasString(this, GO_X_LABEL_PROPERTY_NAME_STR)) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        xlabelHeight = fp->getTextHeightInPixels();
    }
    // A rotated tick label occupies the bounding box of its rotated glyph run.
    auto rotatedExtent = [](int& w, int& h, double angleDeg) {
        if (angleDeg == 0.0) {
            return;
        }
        const double a = angleDeg * M_PI / 180.0;
        const double c = std::fabs(std::cos(a));
        const double s = std::fabs(std::sin(a));
        const int rw = static_cast<int>(std::lround(w * c + h * s));
        const int rh = static_cast<int>(std::lround(w * s + h * c));
        w = rw;
        h = rh;
    };
    double xTickRotation = findScalarDoubleProperty(GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    double yTickRotation = findScalarDoubleProperty(GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    double zTickRotation = findScalarDoubleProperty(GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    GOStringVectorProperty* gp
        = static_cast<GOStringVectorProperty*>(findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> xlabels(gp->data());
    for (int i = 0; i < xlabels.size(); i++) {
        int tickW = 0;
        int tickH = 0;
        measureGOText(xlabels[i], m_font, tickW, tickH);
        rotatedExtent(tickW, tickH, xTickRotation);
        maxTickWidth = std::max(maxTickWidth, tickW);
        maxTickHeight = std::max(maxTickHeight, tickH);
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Y_LABEL_PROPERTY_NAME_STR));
    if (axisTextHasString(this, GO_Y_LABEL_PROPERTY_NAME_STR)) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        ylabelHeight = fp->getTextHeightInPixels();
    }
    gp = static_cast<GOStringVectorProperty*>(findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> ylabels(gp->data());
    for (int i = 0; i < ylabels.size(); i++) {
        int tickW = 0;
        int tickH = 0;
        measureGOText(ylabels[i], m_font, tickW, tickH);
        rotatedExtent(tickW, tickH, yTickRotation);
        maxTickWidth = std::max(maxTickWidth, tickW);
        maxTickHeight = std::max(maxTickHeight, tickH);
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Z_LABEL_PROPERTY_NAME_STR));
    if (axisTextHasString(this, GO_Z_LABEL_PROPERTY_NAME_STR)) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        zlabelHeight = fp->getTextHeightInPixels();
    }
    gp = static_cast<GOStringVectorProperty*>(findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> zlabels(gp->data());
    for (int i = 0; i < zlabels.size(); i++) {
        int tickW = 0;
        int tickH = 0;
        measureGOText(zlabels[i], m_font, tickW, tickH);
        rotatedExtent(tickW, tickH, zTickRotation);
        maxTickWidth = std::max(maxTickWidth, tickW);
        maxTickHeight = std::max(maxTickHeight, tickH);
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_TITLE_PROPERTY_NAME_STR));
    if (axisTextHasString(this, GO_TITLE_PROPERTY_NAME_STR)) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        titleHeight = fp->getTextHeightInPixels();
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_SUBTITLE_PROPERTY_NAME_STR));
    if (axisTextHasString(this, GO_SUBTITLE_PROPERTY_NAME_STR)) {
        GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
        titleHeight += fp->getTextHeightInPixels();
    }
    tickHeight = static_cast<int>(std::lround(FontMetrics::lineHeightPixels(
        m_font.family, fontSpecPixelSize(m_font), m_font.bold, m_font.italic)));
    maxLabelHeight = std::max(titleHeight, xlabelHeight);
    maxLabelHeight = std::max(maxLabelHeight, ylabelHeight);
    maxLabelHeight = std::max(maxLabelHeight, zlabelHeight);
    double leftInset = std::max(4.0, static_cast<double>(maxTickWidth + ylabelHeight + 4));
    double bottomInset = std::max(4.0, static_cast<double>(maxTickHeight + xlabelHeight + 4));
    double rightInset = 4.0;
    double topInset = std::max(4.0, static_cast<double>(titleHeight + 4));
    double posx0 = outerpos[2] * 0.13 + outerpos[0];
    double posy0 = outerpos[3] * 0.11 + outerpos[1];
    double poswidth = outerpos[2] * 0.775;
    double posheight = outerpos[3] * 0.815;
    if ((maxTickWidth == 0) && (maxTickHeight == 0) && (maxLabelHeight == 0)) {
        return { posx0, posy0, poswidth, posheight };
    }
    double leftMargin = std::max(outerpos[2] * 0.13, leftInset);
    double bottomMargin = std::max(outerpos[3] * 0.11, bottomInset);
    double rightMargin = std::max(outerpos[2] * 0.095, rightInset);
    double topMargin = std::max(outerpos[3] * 0.075, topInset);
    m_titleRepackShrink = 0.0;
    posx0 = outerpos[0] + leftMargin;
    posy0 = outerpos[1] + bottomMargin;
    poswidth = outerpos[2] - leftMargin - rightMargin;
    posheight = outerpos[3] - bottomMargin - topMargin;
    return { posx0, posy0, poswidth, posheight };
}
//=============================================================================
// A Position set by the user moves OuterPosition too: the outer box is the position grown by the
// larger of the loose inset (13%, 11%, 9.5% and 7.5% of the parent) and the tight inset.
void
GOAxis::updateOuterPositionFromPosition()
{
    if (findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR) == m_settledPosition) {
        return;
    }
    if (m_outerPositionFromUser) {
        return;
    }
    double width = 0;
    double height = 0;
    if (!graphicsParentSizeInPixels(this, getParentFigure(), width, height)) {
        return;
    }
    std::vector<double> box(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
    if (box.size() < 4) {
        return;
    }
    std::vector<double> inset = tightInsetPixels();
    std::wstring units = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    // normalized: the loose inset is a share of the outer box, not of the parent
    const bool shared = units == GO_PROPERTY_VALUE_NORMALIZED_STR;
    double left = 0;
    double right = 0;
    double bottom = 0;
    double top = 0;
    outerInsetsAroundBox(shared, box[2], width, 0.13, 0.095, inset[0], inset[2], left, right);
    outerInsetsAroundBox(shared, box[3], height, 0.11, 0.075, inset[1], inset[3], bottom, top);
    std::vector<double> outer = convertFromPixelsPixelOrigin(units, box[0] - left, box[1] - bottom,
        box[2] + left + right, box[3] + bottom + top, width, height);
    auto* outerProperty
        = static_cast<GOFourVectorProperty*>(findProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR));
    outerProperty->value(outer[0], outer[1], outer[2], outer[3]);
    clearChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
// Space taken by tick labels, axis labels and titles around the axes box, in pixels
// [left bottom right top]. Calibrated: a tick label sits a gap of 0.4 em away
// from the box and overhangs its tick by half its size; a label or title adds one line height.
std::vector<double>
GOAxis::tightInsetPixels()
{
    std::vector<double> box(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
    if (box.size() < 4 || !(box[2] > 0.) || !(box[3] > 0.)) {
        return { 0., 0., 0., 0. };
    }
    const double boxWidth = box[2];
    const double boxHeight = box[3];
    double gap = 0.4 * fontSpecPixelSize(m_font);
    std::vector<double> tickLength = findVectorDoubleProperty(GO_TICK_LENGTH_PROPERTY_NAME_STR);
    if (!stringCheck(GO_TICK_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_IN_STR)
        && !tickLength.empty()) {
        gap += tickLength[0] * std::max(boxWidth, boxHeight);
    }
    const double lineHeight = goFontLineHeight(m_font);
    auto fraction = [this](const std::wstring& limName, const std::wstring& scaleName,
                        const std::wstring& dirName, double value, double& result) {
        std::vector<double> lim = findVectorDoubleProperty(limName);
        if (lim.size() < 2) {
            return false;
        }
        double low = lim[0];
        double high = lim[1];
        if (stringCheck(scaleName, GO_PROPERTY_VALUE_LOG_STR)) {
            if (!(value > 0.) || !(low > 0.) || !(high > 0.)) {
                return false;
            }
            value = std::log10(value);
            low = std::log10(low);
            high = std::log10(high);
        }
        if (!(high > low)) {
            return false;
        }
        result = (value - low) / (high - low);
        if (stringCheck(dirName, GO_PROPERTY_VALUE_REVERSE_STR)) {
            result = 1. - result;
        }
        return result >= -1e-9 && result <= 1. + 1e-9;
    };
    auto rotatedExtent = [](double& w, double& h, double angleDeg) {
        if (angleDeg == 0.0) {
            return;
        }
        const double a = angleDeg * M_PI / 180.0;
        const double c = std::fabs(std::cos(a));
        const double s = std::fabs(std::sin(a));
        const double rw = w * c + h * s;
        h = w * s + h * c;
        w = rw;
    };
    double left = 0.;
    double bottom = 0.;
    double right = 0.;
    double top = 0.;
    double xTickDepth = 0.;
    double yTickDepth = 0.;
    {
        std::vector<double> ticks = findVectorDoubleProperty(GO_X_TICK_PROPERTY_NAME_STR);
        std::vector<std::wstring> labels(
            static_cast<GOStringVectorProperty*>(findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR))
                ->data());
        const double rotation
            = findScalarDoubleProperty(GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
        for (size_t k = 0; !labels.empty() && k < ticks.size(); ++k) {
            double f = 0.;
            if (labels[k % labels.size()].empty()
                || !fraction(GO_X_LIM_PROPERTY_NAME_STR, GO_X_SCALE_PROPERTY_NAME_STR,
                    GO_X_DIR_PROPERTY_NAME_STR, ticks[k], f)) {
                continue;
            }
            int textWidth = 0;
            int textHeight = 0;
            measureGOText(labels[k % labels.size()], m_font, textWidth, textHeight);
            double w = textWidth;
            double h = lineHeight;
            rotatedExtent(w, h, rotation);
            left = std::max(left, w / 2. - f * boxWidth);
            right = std::max(right, w / 2. - (1. - f) * boxWidth);
            xTickDepth = std::max(xTickDepth, h + gap);
        }
    }
    {
        std::vector<double> ticks = findVectorDoubleProperty(GO_Y_TICK_PROPERTY_NAME_STR);
        std::vector<std::wstring> labels(
            static_cast<GOStringVectorProperty*>(findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR))
                ->data());
        const double rotation
            = findScalarDoubleProperty(GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
        for (size_t k = 0; !labels.empty() && k < ticks.size(); ++k) {
            double f = 0.;
            if (labels[k % labels.size()].empty()
                || !fraction(GO_Y_LIM_PROPERTY_NAME_STR, GO_Y_SCALE_PROPERTY_NAME_STR,
                    GO_Y_DIR_PROPERTY_NAME_STR, ticks[k], f)) {
                continue;
            }
            int textWidth = 0;
            int textHeight = 0;
            measureGOText(labels[k % labels.size()], m_font, textWidth, textHeight);
            double w = textWidth;
            double h = lineHeight;
            rotatedExtent(w, h, rotation);
            bottom = std::max(bottom, h / 2. - f * boxHeight);
            top = std::max(top, h / 2. - (1. - f) * boxHeight);
            yTickDepth = std::max(yTickDepth, w + gap);
        }
    }
    const bool xAtTop
        = stringCheck(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR);
    const bool yAtRight
        = stringCheck(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RIGHT_STR);
    auto labelLines = [](const std::wstring& text) {
        return 1. + static_cast<double>(std::count(text.begin(), text.end(), L'\n'));
    };
    double xLabelExtra = 0.;
    if (GOText* xLabel = axisTextObject(this, GO_X_LABEL_PROPERTY_NAME_STR);
        xLabel && axisTextHasString(this, GO_X_LABEL_PROPERTY_NAME_STR)) {
        xLabelExtra = xLabel->getTextHeightInPixels()
                * labelLines(xLabel->findStringProperty(GO_STRING_PROPERTY_NAME_STR))
            + 1.4;
    }
    double yLabelExtra = 0.;
    if (GOText* yLabel = axisTextObject(this, GO_Y_LABEL_PROPERTY_NAME_STR);
        yLabel && axisTextHasString(this, GO_Y_LABEL_PROPERTY_NAME_STR)) {
        yLabelExtra = yLabel->getTextHeightInPixels()
                * labelLines(yLabel->findStringProperty(GO_STRING_PROPERTY_NAME_STR))
            + 4.1;
    }
    if (xAtTop) {
        top = std::max(top, xTickDepth) + xLabelExtra;
    } else {
        bottom = std::max(bottom, xTickDepth) + xLabelExtra;
    }
    if (yAtRight) {
        right = std::max(right, yTickDepth) + yLabelExtra;
    } else {
        left = std::max(left, yTickDepth) + yLabelExtra;
        if (isDualYAxis()) {
            right = std::max(right, yTickDepth);
        }
    }
    double titleExtent = 0.;
    for (const std::wstring& name :
        { std::wstring(GO_TITLE_PROPERTY_NAME_STR), std::wstring(GO_SUBTITLE_PROPERTY_NAME_STR) }) {
        if (GOText* text = axisTextObject(this, name); text && axisTextHasString(this, name)) {
            const double points = fontSizeInPoints(text);
            titleExtent += text->getTextHeightInPixels()
                    * labelLines(text->findStringProperty(GO_STRING_PROPERTY_NAME_STR))
                + 0.25 * points * 96. / 72.;
        }
    }
    if (titleExtent > 0.) {
        top = std::max(top, (xAtTop ? top : 0.) + titleExtent);
    }
    return { std::max(left, 0.), std::max(bottom, 0.), std::max(right, 0.), std::max(top, 0.) };
}
//=============================================================================
// AffectAutoLimits == 'off' or Visible == 'off' keeps an object's data out of the automatic limits.
static bool
excludedFromAutoLimits(GraphicsObject* object)
{
    if (object == nullptr) {
        return false;
    }
    return (object->findProperty(GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR, false) != nullptr
               && object->stringCheck(
                   GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR))
        || (object->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false) != nullptr
            && object->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR));
}
//=============================================================================
void
GOAxis::updateLimits(bool x, bool y, bool z, bool a, bool c)
{
    if (!x && !y && !z && !a && !c) {
        return;
    }
    std::vector<double> limits;
    bool first = true;
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());

    std::vector<double> clim(findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
    std::vector<double> alim(findVectorDoubleProperty(GO_A_LIM_PROPERTY_NAME_STR));

    // CLim auto-scaling is tracked independently of the X/Y/Z extents: only
    // children carrying color-mapped data (a non-degenerate CData range) drive
    // CLim. A child without color data reports a degenerate [v v] range and must
    // not drag CLim toward zero, so the axis keeps its default when no
    // color-mapped child is present.
    bool firstClim = true;
    bool anyExcluded = false;
    double climLo = 0.;
    double climHi = 0.;
    auto foldClim = [&](GraphicsObject* child, const std::vector<double>& child_limits) {
        if (child_limits.size() < 8) {
            return;
        }
        double c0 = child_limits[6];
        double c1 = child_limits[7];
        if (child->getType() == GO_PROPERTY_VALUE_PATCH_STR && clim.size() >= 2) {
            // A patch's color limits always span at least the axis default.
            c0 = std::min(c0, clim[0]);
            c1 = std::max(c1, clim[1]);
        }
        if (c1 <= c0) {
            return;
        }
        if (firstClim) {
            climLo = c0;
            climHi = c1;
            firstClim = false;
        } else {
            climLo = std::min(climLo, c0);
            climHi = std::max(climHi, c1);
        }
    };

    auto applyAutoClim = [&](double lo, double hi) {
        std::vector<double> previousClim = findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR);
        setTwoVectorDefault(GO_C_LIM_PROPERTY_NAME_STR, lo, hi);
        // setTwoVectorDefault writes through the low-level accessor, which does not raise the
        // modified flag. Colour-mapped children (image, surface) rebuild their cached colours only
        // when they see the axes CLim change, so the flag is raised here when the auto value moved.
        // updateChildrenState() consumes it right after and finishAxisUpdate()'s clearAllChanged()
        // clears it in the same pass, so it never flips CLimMode to manual.
        if (previousClim.size() < 2 || previousClim[0] != lo || previousClim[1] != hi) {
            findProperty(GO_C_LIM_PROPERTY_NAME_STR)->setModified(true);
        }
    };

    GOSixVectorProperty* hp
        = static_cast<GOSixVectorProperty*>(findProperty(GO_DATA_LIMITS_PROPERTY_NAME_STR));

    // With exactly one manual X or Y range (Z auto), the auto limits fit the data visible in it.
    std::vector<double> rangeBox;
    std::array<bool, 3> rangeLog
        = { stringCheck(GO_X_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LOG_STR),
              stringCheck(GO_Y_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LOG_STR),
              stringCheck(GO_Z_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LOG_STR) };
    bool xManual = !isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR);
    bool yManual = !isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    if (xManual != yManual && isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR)) {
        const double inf = std::numeric_limits<double>::infinity();
        rangeBox = { -inf, inf, -inf, inf, -inf, inf };
        std::vector<double> manualLim = findVectorDoubleProperty(
            xManual ? GO_X_LIM_PROPERTY_NAME_STR : GO_Y_LIM_PROPERTY_NAME_STR);
        if (manualLim.size() == 2) {
            size_t offset = xManual ? 0 : 2;
            rangeBox[offset] = manualLim[0];
            rangeBox[offset + 1] = manualLim[1];
        }
    }
    bool anyOutside = false;
    // Folds CLim from all the child data, then returns its limits inside rangeBox.
    auto childLimits = [&](GraphicsObject* child) -> std::vector<double> {
        std::vector<double> full(child->getLimits());
        if (hasNoFinitePoint(full)) {
            anyExcluded = true;
            return {};
        }
        if (full.empty()) {
            return full;
        }
        foldClim(child, full);
        if (rangeBox.empty()) {
            return full;
        }
        std::vector<double> visible(child->getLimitsInRange(rangeBox, rangeLog));
        if (visible.empty()) {
            anyOutside = true;
        }
        return visible;
    };

    for (int i = 0; i < handles.size(); i++) {
        GraphicsObject* fp = findGraphicsObject(handles[i]);
        if (excludedFromAutoLimits(fp)) {
            anyExcluded = true;
            continue;
        }
        std::wstring goType = fp->getType();
        if (goType == GO_PROPERTY_VALUE_HGGROUP_STR || goType == GO_PROPERTY_VALUE_HGTRANSFORM_STR
            || goType == GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR
            || goType == GO_PROPERTY_VALUE_HEATMAP_STR
            || goType == GO_PROPERTY_VALUE_PARALLEL_PLOT_STR) {
            auto childrenHgGroup
                = static_cast<GOGObjectsProperty*>(fp->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
            std::vector<int64> handlesHgGroup(childrenHgGroup->data());
            for (size_t k = 0; k < handlesHgGroup.size(); ++k) {
                GraphicsObject* hp = findGraphicsObject(handlesHgGroup[k]);
                if (excludedFromAutoLimits(hp)) {
                    anyExcluded = true;
                    continue;
                }
                std::vector<double> child_limits(childLimits(hp));
                if (!child_limits.empty()) {
                    if (first) {
                        limits = child_limits;
                        if (hp->getType() == GO_PROPERTY_VALUE_PATCH_STR) {
                            limits[6] = std::min(limits[6], clim[0]);
                            limits[7] = std::max(limits[7], clim[1]);
                            limits[8] = std::min(limits[8], alim[0]);
                            limits[9] = std::max(limits[9], alim[1]);
                        }
                        first = false;
                    } else {
                        for (ompIndexType i = 0;
                            i < (ompIndexType)std::min(limits.size(), child_limits.size());
                            i += 2) {
                            limits[i] = std::min(limits[i], child_limits[i]);
                            limits[i + 1] = std::max(limits[i + 1], child_limits[i + 1]);
                        }
                    }
                }
            }
        } else {
            std::vector<double> child_limits(childLimits(fp));
            if (!child_limits.empty()) {
                if (first) {
                    limits = child_limits;
                    if (goType == GO_PROPERTY_VALUE_PATCH_STR) {
                        limits[6] = std::min(limits[6], clim[0]);
                        limits[7] = std::max(limits[7], clim[1]);
                        limits[8] = std::min(limits[8], alim[0]);
                        limits[9] = std::max(limits[9], alim[1]);
                    }
                    first = false;
                } else {
                    for (ompIndexType i = 0;
                        i < (ompIndexType)std::min(limits.size(), child_limits.size()); i += 2) {
                        limits[i] = std::min(limits[i], child_limits[i]);
                        limits[i + 1] = std::max(limits[i + 1], child_limits[i + 1]);
                    }
                }
            }
        }
    }
    if (first) {
        if (anyExcluded || anyOutside) {
            // No child data is excluded or visible: the automatic limits fall back to the default.
            if (x) {
                setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, 0., 1.);
            }
            if (y) {
                setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, 0., 1.);
            }
            if (z) {
                setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, 0., 1.);
            }
            if (c) {
                applyAutoClim(firstClim ? 0. : climLo, firstClim ? 1. : climHi);
            }
            if (a) {
                setTwoVectorDefault(GO_A_LIM_PROPERTY_NAME_STR, 0., 1.);
            }
            if (anyOutside) {
                std::vector<double> xl = findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
                std::vector<double> yl = findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
                std::vector<double> zl = findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR);
                hp->value(xl[0], xl[1], yl[0], yl[1], zl[0], zl[1]);
            }
        }
        return;
    }
    // A dimension left inverted (lower > upper) was constrained by no child: an
    // object that spans it without bounding it, such as a ConstantLine on its
    // non-intercept axis. Keep the current limits for that dimension instead of
    // storing an inverted range. Otherwise a degenerate [v v] range is expanded.
    auto resolveDimensionLimits
        = [&](double& lower, double& upper, const std::wstring& limitProperty) -> bool {
        if (lower > upper) {
            std::vector<double> current(findVectorDoubleProperty(limitProperty));
            if (current.size() == 2) {
                lower = current[0];
                upper = current[1];
            } else {
                lower = 0.0;
                upper = 1.0;
            }
            return false;
        }
        if (upper == lower) {
            double center = lower;
            lower = center - 1.0;
            upper = center + 1.0;
        }
        return true;
    };
    bool applyX = resolveDimensionLimits(limits[0], limits[1], GO_X_LIM_PROPERTY_NAME_STR);
    bool applyY = resolveDimensionLimits(limits[2], limits[3], GO_Y_LIM_PROPERTY_NAME_STR);
    bool applyZ = resolveDimensionLimits(limits[4], limits[5], GO_Z_LIM_PROPERTY_NAME_STR);
    hp->value(limits[0], limits[1], limits[2], limits[3], limits[4], limits[5]);
    if (x && applyX) {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, limits[0], limits[1]);
    }
    if (y && applyY) {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, limits[2], limits[3]);
    }
    if (z && applyZ) {
        setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, limits[4], limits[5]);
    }
    if (c && !firstClim) {
        applyAutoClim(climLo, climHi);
    } else if (c && anyExcluded) {
        // No colour-mapped child is left: CLim falls back to its default.
        applyAutoClim(0., 1.);
    }
    if (a && limits[8] <= limits[9]) {
        setTwoVectorDefault(GO_A_LIM_PROPERTY_NAME_STR, limits[8], limits[9]);
    }
}
//=============================================================================
void
GOAxis::syncYAxisRulers()
{
    static const wchar_t* rulerProperties[] = { GO_LIMITS_PROPERTY_NAME_STR,
        GO_LIMITS_MODE_PROPERTY_NAME_STR, GO_COLOR_PROPERTY_NAME_STR, GO_SCALE_PROPERTY_NAME_STR,
        GO_DIRECTION_PROPERTY_NAME_STR, GO_TICK_VALUES_PROPERTY_NAME_STR,
        GO_TICK_VALUES_MODE_PROPERTY_NAME_STR, GO_TICK_LABELS_PROPERTY_NAME_STR,
        GO_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_LABEL_PROPERTY_NAME_STR,
        GO_FONT_SIZE_PROPERTY_NAME_STR, GO_FONT_NAME_PROPERTY_NAME_STR,
        GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_FONT_ANGLE_PROPERTY_NAME_STR,
        GO_TICK_DIRECTION_PROPERTY_NAME_STR, GO_TICK_LENGTH_PROPERTY_NAME_STR,
        GO_FONT_UNITS_PROPERTY_NAME_STR, GO_LINE_WIDTH_PROPERTY_NAME_STR,
        GO_TICK_DIR_PROPERTY_NAME_STR, GO_GRID_COLOR_PROPERTY_NAME_STR,
        GO_GRID_ALPHA_PROPERTY_NAME_STR, GO_BOX_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR,
        GO_TICK_LABEL_FORMAT_PROPERTY_NAME_STR };
    GOGObjectsProperty* yap
        = static_cast<GOGObjectsProperty*>(findProperty(GO_Y_AXIS_PROPERTY_NAME_STR));
    std::vector<int64> handles = yap->data();
    for (size_t i = 0; i < handles.size(); ++i) {
        GORuler* ruler = dynamic_cast<GORuler*>(findGraphicsObject(handles[i], false));
        if (!ruler) {
            continue;
        }
        int side = static_cast<int>(i) + 1;
        for (const wchar_t* rp : rulerProperties) {
            std::wstring src = axesPropertyForRulerProperty(RULER_AXIS_Y, side, rp);
            GOGenericProperty* sp = findProperty(src, false);
            GOGenericProperty* dp = ruler->findProperty(rp, false);
            if (sp && dp) {
                copyRulerProperty(sp, dp);
                ruler->clearChanged(rp);
            }
        }
        propagateRulerValueKind(ruler, RULER_AXIS_Y);
    }
}
//=============================================================================
void
GOAxis::propagateRulerValueKind(GORuler* ruler, int axisKind)
{
    if (!ruler) {
        return;
    }
    const wchar_t* scaleKindProp = GO_Y_AXIS_SCALE_KIND_PROPERTY_NAME_STR;
    if (axisKind == RULER_AXIS_X) {
        scaleKindProp = GO_X_AXIS_SCALE_KIND_PROPERTY_NAME_STR;
    } else if (axisKind == RULER_AXIS_Z) {
        scaleKindProp = GO_Z_AXIS_SCALE_KIND_PROPERTY_NAME_STR;
    }
    GOStringProperty* skp = static_cast<GOStringProperty*>(findProperty(scaleKindProp, false));
    bool isDatetime = skp && skp->isEqual(GO_PROPERTY_VALUE_DATETIME_STR);
    ruler->setValueKind(isDatetime ? RULER_VALUE_DATETIME : RULER_VALUE_NUMERIC);
}
//=============================================================================
void
GOAxis::ensureSingleAxisRuler(const wchar_t* propName, int axisKind)
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
        ruler->linkToAxis(axisHandle, 1, axisKind);
        handles.push_back(rulerHandle);
        ap->data(handles);
    }
}
//=============================================================================
void
GOAxis::syncSingleAxisRuler(const wchar_t* propName, int axisKind)
{
    static const wchar_t* rulerProperties[] = { GO_LIMITS_PROPERTY_NAME_STR,
        GO_LIMITS_MODE_PROPERTY_NAME_STR, GO_COLOR_PROPERTY_NAME_STR, GO_SCALE_PROPERTY_NAME_STR,
        GO_DIRECTION_PROPERTY_NAME_STR, GO_TICK_VALUES_PROPERTY_NAME_STR,
        GO_TICK_VALUES_MODE_PROPERTY_NAME_STR, GO_TICK_LABELS_PROPERTY_NAME_STR,
        GO_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_LABEL_PROPERTY_NAME_STR,
        GO_FONT_SIZE_PROPERTY_NAME_STR, GO_FONT_NAME_PROPERTY_NAME_STR,
        GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_FONT_ANGLE_PROPERTY_NAME_STR,
        GO_TICK_DIRECTION_PROPERTY_NAME_STR, GO_TICK_LENGTH_PROPERTY_NAME_STR,
        GO_FONT_UNITS_PROPERTY_NAME_STR, GO_LINE_WIDTH_PROPERTY_NAME_STR,
        GO_TICK_DIR_PROPERTY_NAME_STR, GO_GRID_COLOR_PROPERTY_NAME_STR,
        GO_GRID_ALPHA_PROPERTY_NAME_STR, GO_BOX_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR,
        GO_TICK_LABEL_FORMAT_PROPERTY_NAME_STR };
    GOGObjectsProperty* ap = static_cast<GOGObjectsProperty*>(findProperty(propName));
    std::vector<int64> handles = ap->data();
    if (handles.empty()) {
        return;
    }
    GORuler* ruler = dynamic_cast<GORuler*>(findGraphicsObject(handles[0], false));
    if (!ruler) {
        return;
    }
    for (const wchar_t* rp : rulerProperties) {
        std::wstring src = axesPropertyForRulerProperty(axisKind, 1, rp);
        GOGenericProperty* sp = findProperty(src, false);
        GOGenericProperty* dp = ruler->findProperty(rp, false);
        if (sp && dp) {
            copyRulerProperty(sp, dp);
            ruler->clearChanged(rp);
        }
    }
    propagateRulerValueKind(ruler, axisKind);
}
//=============================================================================
void
GOAxis::handlePlotBoxFlags()
{
    bool xflag = isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR);
    bool yflag = isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    bool zflag = isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR);

    bool darauto = isAuto(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    bool pbaauto = isAuto(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);

    double azimuth, elevation;
    getView(azimuth, elevation);
    bool is2D = (elevation == 90);

    // Get axis limits and ranges. The automatic 3D X/Y/Z limits are settled
    // generically by recalculateTicks() (nice-number tick grid of the data
    // extent, per ruler); this routine only derives the aspect ratios from those
    // limits and must not re-snap them to a separate grid.
    std::vector<double> limits(getAxisLimits());
    bool depthSortedSurface = axisHasDepthSortedSurface(this);
    double dx = limits[1] - limits[0];
    double dy = limits[3] - limits[2];
    double dz = limits[5] - limits[4];

    if (!(dx > 0)) {
        dx = 1.0;
    }
    if (!(dy > 0)) {
        dy = 1.0;
    }
    if (!(dz > 0)) {
        dz = 1.0;
    }
    bool planar2D = is2D && !depthSortedSurface;

    std::vector<double> dar(findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR));
    std::vector<double> pba(findVectorDoubleProperty(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR));

    auto normalizeAspectRatio = [](std::vector<double>& aspect) {
        double minv = std::min(
            { std::max(aspect[0], 1e-12), std::max(aspect[1], 1e-12), std::max(aspect[2], 1e-12) });
        if (minv <= 0) {
            minv = 1.0;
        }
        aspect[0] /= minv;
        aspect[1] /= minv;
        aspect[2] /= minv;
    };
    auto normalizeAspectRatioByFirst = [](std::vector<double>& aspect) {
        double first = std::max(aspect[0], 1e-12);
        aspect[0] /= first;
        aspect[1] /= first;
        aspect[2] /= first;
    };
    if (is2D) {
        if (darauto && pbaauto) {
            if (planar2D) {
                if (axisHasLimitChildren(this)) {
                    double minrange = std::min({ dx, dy, dz });
                    setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, dx / minrange,
                        dy / minrange, dz / minrange);
                } else {
                    setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, 1.0, 1.0, 1.0);
                }
            } else {
                double minrange = std::min({ dx, dy, dz });
                setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, dx / minrange,
                    dy / minrange, dz / minrange);
            }
            std::vector<double> position(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
            double width = (position.size() >= 4 && position[2] > 0) ? position[2] : 1.0;
            double height = (position.size() >= 4 && position[3] > 0) ? position[3] : 1.0;
            if (width >= height) {
                double ratio = height / width;
                setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, 1, ratio, ratio);
            } else {
                double ratio = width / height;
                setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, ratio, 1, ratio);
            }
            return;
        }
        normalizeAspectRatio(dar);
        normalizeAspectRatio(pba);

        if (darauto && !pbaauto) {
            setThreeVectorDefault(
                GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, pba[0], pba[1], pba[2]);
            if (planar2D) {
                std::vector<double> autoDar = { dx / pba[0], dy / pba[1], 1.0 };
                double minrange = std::min(autoDar[0], autoDar[1]);
                setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, autoDar[0] / minrange,
                    autoDar[1] / minrange, autoDar[2]);
            } else {
                setThreeVectorDefault(
                    GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, dx / pba[0], dy / pba[1], dz / pba[2]);
            }
            return;
        }

        if (!darauto && pbaauto) {
            setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, dar[0], dar[1], dar[2]);
            return;
        }

        setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, dar[0], dar[1], dar[2]);
        setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, pba[0], pba[1], pba[2]);

        // Tight auto-limits are pinned to the data extent. With locked data units
        // (manual DataAspectRatio) the plot box then takes the tightened data-range
        // shape, instead of the limits stretching to keep an earlier plot box aspect.
        // A plot box aspect set in this same pass is kept as given (it is 'axis equal'
        // framing the box to the pixel area before it stretches the limits itself).
        bool allTightLimits = xflag && yflag && zflag
            && !hasChanged(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR)
            && findStringProperty(GO_X_LIMIT_METHOD_PROPERTY_NAME_STR)
                == GO_PROPERTY_VALUE_TIGHT_STR
            && findStringProperty(GO_Y_LIMIT_METHOD_PROPERTY_NAME_STR)
                == GO_PROPERTY_VALUE_TIGHT_STR
            && findStringProperty(GO_Z_LIMIT_METHOD_PROPERTY_NAME_STR)
                == GO_PROPERTY_VALUE_TIGHT_STR;
        if (allTightLimits) {
            std::vector<double> tightPba = { dx / dar[0], dy / dar[1], dz / dar[2] };
            normalizeAspectRatio(tightPba);
            setThreeVectorDefault(
                GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, tightPba[0], tightPba[1], tightPba[2]);
            return;
        }

        bool allAuto = xflag && yflag && zflag;
        bool oneManual = (!xflag && yflag && zflag) || (xflag && !yflag && zflag)
            || (xflag && yflag && !zflag);
        auto aspectRange = [](double scale, double pbaValue, double darValue) {
            return scale * pbaValue * darValue;
        };
        auto scaleFromRange = [](double range, double pbaValue, double darValue) {
            double denom = pbaValue * darValue;
            if (!std::isfinite(denom) || denom <= 0) {
                return 1.0;
            }
            return range / denom;
        };

        if (allAuto) {
            double scale = std::max({ scaleFromRange(dx, pba[0], dar[0]),
                scaleFromRange(dy, pba[1], dar[1]), scaleFromRange(dz, pba[2], dar[2]) });
            rerange(limits[0], limits[1], aspectRange(scale, pba[0], dar[0]));
            rerange(limits[2], limits[3], aspectRange(scale, pba[1], dar[1]));
            rerange(limits[4], limits[5], aspectRange(scale, pba[2], dar[2]));
            setAxisLimits(limits);
        } else if (oneManual) {
            if (!xflag) {
                double scale = scaleFromRange(dx, pba[0], dar[0]);
                rerange(limits[2], limits[3], aspectRange(scale, pba[1], dar[1]));
                rerange(limits[4], limits[5], aspectRange(scale, pba[2], dar[2]));
            } else if (!yflag) {
                double scale = scaleFromRange(dy, pba[1], dar[1]);
                rerange(limits[0], limits[1], aspectRange(scale, pba[0], dar[0]));
                rerange(limits[4], limits[5], aspectRange(scale, pba[2], dar[2]));
            } else if (!zflag) {
                // Top-down view: Z is the hidden depth, so a manual Z (such as an
                // image's [-1 1]) must not drive the visible box. Keep X and Y at
                // their data extent and give them equal units between themselves.
                double scale = std::max(
                    scaleFromRange(dx, pba[0], dar[0]), scaleFromRange(dy, pba[1], dar[1]));
                rerange(limits[0], limits[1], aspectRange(scale, pba[0], dar[0]));
                rerange(limits[2], limits[3], aspectRange(scale, pba[1], dar[1]));
            }
            setAxisLimits(limits);
        }
    } else {
        if (darauto) {
            if (pbaauto) {
                double minrange = std::min({ dx, dy, dz });
                std::vector<double> autoDar = { dx / minrange, dy / minrange, dz / minrange };
                std::vector<double> position(
                    getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
                double width = (position.size() >= 4 && position[2] > 0) ? position[2] : 1.0;
                double height = (position.size() >= 4 && position[3] > 0) ? position[3] : 1.0;
                std::vector<double> defaultPba(
                    default3DPlotBoxAspect(azimuth, autoDar, width / height));
                setThreeVectorDefault(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, defaultPba[0],
                    defaultPba[1], defaultPba[2]);
                setThreeVectorDefault(
                    GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, autoDar[0], autoDar[1], autoDar[2]);
                return;
            }
            normalizeAspectRatio(pba);
            dar[0] = dx / pba[0];
            dar[1] = dy / pba[1];
            dar[2] = dz / pba[2];
            normalizeAspectRatio(dar);
            setThreeVectorDefault(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR, dar[0], dar[1], dar[2]);
            return;
        }

        if (pbaauto) {
            normalizeAspectRatio(dar);
            pba[0] = dx / dar[0];
            pba[1] = dy / dar[1];
            pba[2] = dz / dar[2];
            normalizeAspectRatio(pba);
            setThreeVectorDefault(
                GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, pba[0], pba[1], pba[2]);
            return;
        }

        normalizeAspectRatio(dar);
        normalizeAspectRatio(pba);

        int autoCount = (xflag ? 1 : 0) + (yflag ? 1 : 0) + (zflag ? 1 : 0);

        if (autoCount == 0) {
            pba[0] = dx / dar[0];
            pba[1] = dy / dar[1];
            pba[2] = dz / dar[2];
            normalizeAspectRatio(pba);
            setThreeVectorDefault(
                GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR, pba[0], pba[1], pba[2]);
            return;
        }

        double scale = -std::numeric_limits<double>::infinity();

        auto updateScale = [&](double dlim, double pba_i, double da_i) {
            double val = dlim / (pba_i * da_i);
            if (val > scale) {
                scale = val;
            }
        };

        if (autoCount == 3) {
            updateScale(dx, pba[0], dar[0]);
            updateScale(dy, pba[1], dar[1]);
            updateScale(dz, pba[2], dar[2]);
        } else {
            if (!xflag) {
                updateScale(dx, pba[0], dar[0]);
            }
            if (!yflag) {
                updateScale(dy, pba[1], dar[1]);
            }
            if (!zflag) {
                updateScale(dz, pba[2], dar[2]);
            }
        }

        if (!std::isfinite(scale) || scale <= 0) {
            scale = 1.0 / std::min({ pba[0] * dar[0], pba[1] * dar[1], pba[2] * dar[2] });
        }

        std::vector<double> xlim = findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        std::vector<double> ylim = findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        std::vector<double> zlim = findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR);

        if (xflag) {
            double d = scale * pba[0] * dar[0];
            double c = 0.5 * (xlim[0] + xlim[1]);
            setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, c - d / 2.0, c + d / 2.0);
            setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        }
        if (yflag) {
            double d = scale * pba[1] * dar[1];
            double c = 0.5 * (ylim[0] + ylim[1]);
            setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, c - d / 2.0, c + d / 2.0);
            setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        }
        if (zflag) {
            double d = scale * pba[2] * dar[2];
            double c = 0.5 * (zlim[0] + zlim[1]);
            setTwoVectorDefault(GO_Z_LIM_PROPERTY_NAME_STR, c - d / 2.0, c + d / 2.0);
            setRestrictedStringDefault(GO_Z_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        }
    }
}
//=============================================================================
void
GOAxis::drawLabel(RenderInterface& gc, double dx, double dy, double x2, double y2,
    const std::vector<double>& color, const std::wstring& txt, bool manageDirectDraw,
    double rotation)
{
    double angle = atan2(dy, dx) * 180.0 / M_PI;
    RenderInterface::AlignmentFlag xalign = RenderInterface::Min;
    RenderInterface::AlignmentFlag yalign = RenderInterface::Min;
    if (fabs(angle) < 10) {
        xalign = RenderInterface::Min;
        yalign = RenderInterface::Mean;
    } else if (fabs(angle) > 170) {
        xalign = RenderInterface::Max;
        yalign = RenderInterface::Mean;
    } else if ((angle >= 10) && (angle < 80)) {
        xalign = RenderInterface::Min;
        yalign = RenderInterface::Min;
    } else if ((angle >= 80) && (angle < 100)) {
        xalign = RenderInterface::Mean;
        yalign = RenderInterface::Min;
    } else if ((angle >= 100) && (angle < 170)) {
        xalign = RenderInterface::Max;
        yalign = RenderInterface::Min;
    } else if ((angle <= -10) && (angle > -80)) {
        xalign = RenderInterface::Min;
        yalign = RenderInterface::Max;
    } else if ((angle <= -80) && (angle > -100)) {
        xalign = RenderInterface::Mean;
        yalign = RenderInterface::Max;
    } else if ((angle <= -100) && (angle > -170)) {
        xalign = RenderInterface::Max;
        yalign = RenderInterface::Max;
    }
    if (rotation != 0.0) {
        // Anchor the rotated glyph run by the box edge nearest the axis so the
        // label pivots about the tick, matching xtickangle/ytickangle behavior.
        if (fabs(angle - 90.0) < 45.0 || fabs(angle + 90.0) < 45.0) {
            // Ticks below/above a horizontal axis (x-axis case).
            xalign = (rotation > 0.0) ? RenderInterface::Max : RenderInterface::Min;
            yalign = (angle >= 0.0) ? RenderInterface::Min : RenderInterface::Max;
        } else {
            // Ticks left/right of a vertical axis (y-axis case).
            yalign = RenderInterface::Mean;
        }
    }
    if (manageDirectDraw) {
        gc.setupDirectDraw();
    }
    gc.putText(x2, y2, txt, color, xalign, yalign, m_font, rotation);
    if (manageDirectDraw) {
        gc.releaseDirectDraw();
    }
}
//=============================================================================
// xtickformat/ytickformat/ztickformat helpers: turn a user format (a
// printf-style conversion or a preset keyword) into a tick label string.
namespace {
    //=============================================================================
    std::wstring
    resolveTickFormatPreset(const std::wstring& fmt)
    {
        std::wstring lower;
        lower.reserve(fmt.size());
        for (wchar_t c : fmt) {
            lower.push_back((c >= L'A' && c <= L'Z') ? static_cast<wchar_t>(c - L'A' + L'a') : c);
        }
        if (lower == L"usd") {
            return L"$%,.2f";
        }
        if (lower == L"eur") {
            return L"€%,.2f";
        }
        if (lower == L"gbp") {
            return L"£%,.2f";
        }
        if (lower == L"jpy" || lower == L"yen") {
            return L"¥%,.0f";
        }
        if (lower == L"degrees") {
            return L"%g°";
        }
        if (lower == L"percentage") {
            return L"%g%%";
        }
        return fmt;
    }
    //=============================================================================
    std::string
    insertThousandsSeparators(const std::string& number)
    {
        size_t start = 0;
        if (!number.empty() && (number[0] == '-' || number[0] == '+')) {
            start = 1;
        }
        size_t intEnd = start;
        while (intEnd < number.size() && number[intEnd] >= '0' && number[intEnd] <= '9') {
            intEnd++;
        }
        size_t digitCount = intEnd - start;
        if (digitCount <= 3) {
            return number;
        }
        std::string grouped = number.substr(0, start);
        size_t firstGroup = digitCount % 3;
        if (firstGroup == 0) {
            firstGroup = 3;
        }
        size_t pos = start;
        grouped += number.substr(pos, firstGroup);
        pos += firstGroup;
        while (pos < intEnd) {
            grouped += ',';
            grouped += number.substr(pos, 3);
            pos += 3;
        }
        grouped += number.substr(intEnd);
        return grouped;
    }
    //=============================================================================
    bool
    isFormatFlag(wchar_t c)
    {
        return c == L'-' || c == L'+' || c == L' ' || c == L'0' || c == L'#' || c == L',';
    }
    //=============================================================================
    std::wstring
    formatTickValueWithUserFormat(double value, const std::wstring& userFormat)
    {
        const std::wstring fmt = resolveTickFormatPreset(userFormat);
        std::wstring prefix;
        std::wstring spec;
        std::wstring suffix;
        bool found = false;
        size_t i = 0;
        while (i < fmt.size()) {
            if (fmt[i] == L'%') {
                if (i + 1 < fmt.size() && fmt[i + 1] == L'%') {
                    prefix.push_back(L'%');
                    i += 2;
                    continue;
                }
                size_t j = i + 1;
                while (j < fmt.size() && isFormatFlag(fmt[j])) {
                    j++;
                }
                while (j < fmt.size() && fmt[j] >= L'0' && fmt[j] <= L'9') {
                    j++;
                }
                if (j < fmt.size() && fmt[j] == L'.') {
                    j++;
                    while (j < fmt.size() && fmt[j] >= L'0' && fmt[j] <= L'9') {
                        j++;
                    }
                }
                if (j < fmt.size()) {
                    spec = fmt.substr(i, j - i + 1);
                    suffix = fmt.substr(j + 1);
                    found = true;
                    break;
                }
            }
            prefix.push_back(fmt[i]);
            i++;
        }
        if (!found) {
            return fmt;
        }
        const bool grouping = spec.find(L',') != std::wstring::npos;
        std::string narrowSpec;
        for (wchar_t c : spec) {
            if (c != L',') {
                narrowSpec.push_back(static_cast<char>(c));
            }
        }
        const char conversion = narrowSpec.empty() ? 'f' : narrowSpec.back();
        char buffer[128];
        std::string numberStr;
        if (conversion == 'd' || conversion == 'i' || conversion == 'u' || conversion == 'x'
            || conversion == 'X' || conversion == 'o') {
            narrowSpec.insert(narrowSpec.size() - 1, "ll");
            std::snprintf(buffer, sizeof(buffer), narrowSpec.c_str(),
                static_cast<long long>(std::llround(value)));
            numberStr = buffer;
        } else {
            std::snprintf(buffer, sizeof(buffer), narrowSpec.c_str(), value);
            numberStr = buffer;
        }
        if (grouping) {
            numberStr = insertThousandsSeparators(numberStr);
        }
        std::wstring result = prefix;
        result.append(numberStr.begin(), numberStr.end());
        for (size_t k = 0; k < suffix.size(); k++) {
            if (suffix[k] == L'%' && k + 1 < suffix.size() && suffix[k + 1] == L'%') {
                result.push_back(L'%');
                k++;
            } else {
                result.push_back(suffix[k]);
            }
        }
        return result;
    }
    //=============================================================================
} // namespace
//=============================================================================
void
GOAxis::applyCustomTickLabelFormat(const std::wstring& formatPropertyName,
    const std::vector<double>& tickValues, std::vector<std::wstring>& labels)
{
    const std::wstring fmt = findStringProperty(formatPropertyName);
    if (fmt.empty()) {
        return;
    }
    labels.clear();
    labels.reserve(tickValues.size());
    for (double value : tickValues) {
        labels.push_back(formatTickValueWithUserFormat(value, fmt));
    }
}
//=============================================================================
// Section timing for GOAxis::paintMe, enabled by setting NELSON_PROFILE_AXIS_PAINT
// to a file path. One line per paint with per-section milliseconds; renderer
// agnostic (works under Qt and Web backends).
namespace {
    class AxisPaintProfiler
    {
    public:
        AxisPaintProfiler()
        {
            if (!profilePath().empty()) {
                m_last = std::chrono::steady_clock::now();
                m_enabled = true;
            }
        }
        void
        section(const char* name)
        {
            if (!m_enabled) {
                return;
            }
            const auto now = std::chrono::steady_clock::now();
            const double ms
                = std::chrono::duration_cast<std::chrono::nanoseconds>(now - m_last).count()
                / 1.0e6;
            m_last = now;
            char buffer[64];
            std::snprintf(buffer, sizeof(buffer), " %s=%.3f", name, ms);
            m_line += buffer;
        }
        ~AxisPaintProfiler()
        {
            if (!m_enabled || m_line.empty()) {
                return;
            }
            std::ofstream out(profilePath(), std::ios::binary | std::ios::app);
            if (out.is_open()) {
                out << "axis_paint_ms:" << m_line << '\n';
            }
        }

    private:
        static const std::string&
        profilePath()
        {
            static const std::string path = []() {
#ifdef _MSC_VER
                char* value = nullptr;
                size_t length = 0;
                if (_dupenv_s(&value, &length, "NELSON_PROFILE_AXIS_PAINT") != 0
                    || value == nullptr) {
                    return std::string();
                }
                std::string result(value);
                free(value);
                return result;
#else
                const char* value = std::getenv("NELSON_PROFILE_AXIS_PAINT");
                return value == nullptr ? std::string() : std::string(value);
#endif
            }();
            return path;
        }
        bool m_enabled = false;
        std::string m_line;
        std::chrono::steady_clock::time_point m_last;
    };
    //=============================================================================
} // namespace
//=============================================================================
void
GOAxis::drawTickLabels(RenderInterface& gc, const std::vector<double>& color, double px1,
    double py1, double pz1, double px2, double py2, double pz2, double limmin, double limmax,
    double unitx, double unity, double unitz, const std::vector<double>& maptics,
    const std::vector<double>& minortics, const std::vector<std::wstring>& labels,
    const std::wstring& labelname, int ticlen, double ticdir)
{
    AxisPaintProfiler profiler;
    gc.color(color);
    // Scene v2: while capturing, each tick label is recorded at its 3D (mapped)
    // anchor on the box edge and the 2D tick/label drawing is skipped, so the
    // numbers rotate with the box instead of staying pinned in screen space.
    const bool capturingAxes3d = gc.axes3dCaptureActive() && !is2D();
    double tickRotation = 0.0;
    if (labelname == GO_X_LABEL_PROPERTY_NAME_STR) {
        tickRotation = findScalarDoubleProperty(GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    } else if (labelname == GO_Y_LABEL_PROPERTY_NAME_STR) {
        tickRotation = findScalarDoubleProperty(GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    } else if (labelname == GO_Z_LABEL_PROPERTY_NAME_STR) {
        tickRotation = findScalarDoubleProperty(GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    }
    double dx1, dy1, dx2, dy2;
    gc.toPixels(limmin * unitx + px1, limmin * unity + py1, limmin * unitz + pz1, dx1, dy1);
    gc.toPixels(limmin * unitx + px2, limmin * unity + py2, limmin * unitz + pz2, dx2, dy2);
    double delx = dx2 - dx1;
    double dely = dy2 - dy1;
    double norm = sqrt(delx * delx + dely * dely);
    // The tick/label offset direction is normally the projected segment toward the opposite
    // parallel box edge. In an edge-on 3D view that segment runs into the screen and collapses to a
    // point (norm ~ 0), e.g. the horizontal ruler under view(90, 0) when the two in-plane data
    // ranges are equal. Fall back to the screen-perpendicular of the axis' own projected direction
    // so labels are still placed just outside the axis line.
    if (norm <= 1e-6) {
        double ax1, ay1, ax2, ay2;
        gc.toPixels(limmin * unitx + px1, limmin * unity + py1, limmin * unitz + pz1, ax1, ay1);
        gc.toPixels(limmax * unitx + px1, limmax * unity + py1, limmax * unitz + pz1, ax2, ay2);
        double axisdx = ax2 - ax1;
        double axisdy = ay2 - ay1;
        double axislen = sqrt(axisdx * axisdx + axisdy * axisdy);
        if (axislen > 0) {
            axisdx /= axislen;
            axisdy /= axislen;
            // Rotate the on-screen axis direction by 90 degrees to get the
            // outward normal, orienting it so the labels sit below/left of the
            // axis (matching the non-degenerate projection).
            delx = axisdy;
            dely = -axisdx;
            if (dely < 0.0) {
                delx = -delx;
                dely = -dely;
            }
            norm = 1.0;
        }
    }
    if (norm > 0) {
        delx /= norm;
        dely /= norm;
        bool ticksHandledIn2D = is2D()
            && (isXAxisTickSet(unitx, unity, unitz) || isYAxisTickSet(unitx, unity, unitz));
        if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
            if (ticksHandledIn2D) {
                std::vector<double> limits(getAxisLimits());
                double left;
                double bottom;
                double right;
                double top;
                projected2DPlotBox(gc, limits, left, bottom, right, top);
                bool xAxis = isXAxisTickSet(unitx, unity, unitz);
                bool drawBox
                    = (static_cast<GOOnOffProperty*>(findProperty(GO_BOX_PROPERTY_NAME_STR)))
                          ->asBool();
                bool axisAtLowEdge = xAxis
                    ? (static_cast<GOTopBottomProperty*>(
                           findProperty(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR)))
                          ->isEqual(GO_PROPERTY_VALUE_BOTTOM_STR)
                    : (static_cast<GOLeftRightProperty*>(
                           findProperty(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR)))
                          ->isEqual(GO_PROPERTY_VALUE_LEFT_STR);
                bool drawLowEdge = drawBox || axisAtLowEdge;
                bool drawHighEdge = drawBox || !axisAtLowEdge;
                gc.setupDirectDraw();
                draw2DTickSet(gc, xAxis, minortics, limmin, limmax, ticlen * 0.6, ticdir, left,
                    bottom, right, top, drawLowEdge, drawHighEdge);
                draw2DTickSet(gc, xAxis, maptics, limmin, limmax, ticlen, ticdir, left, bottom,
                    right, top, drawLowEdge, drawHighEdge);
                gc.releaseDirectDraw();
            }
            profiler.section("tl_2dticks");
            struct TickLabelTask
            {
                double x;
                double y;
                std::wstring text;
            };
            std::vector<TickLabelTask> tickLabelTasks;
            std::vector<double> tickSegments;
            if (!ticksHandledIn2D) {
                tickSegments.reserve((minortics.size() + maptics.size()) * 4);
            }
            for (int i = 0; i < minortics.size(); i++) {
                double t = minortics[i];
                double x1, y1, x2, y2;
                gc.toPixels(t * unitx + px1, t * unity + py1, t * unitz + pz1, x1, y1);
                x2 = delx * ticlen * ticdir * 0.6 + x1;
                y2 = dely * ticlen * ticdir * 0.6 + y1;
                if (is2D()) {
                    snapLineToPixelCenters(x1, y1, x2, y2);
                }
                if (!ticksHandledIn2D) {
                    appendSegment2D(tickSegments, x1, y1, x2, y2);
                }
            }
            for (int i = 0; i < maptics.size(); i++) {
                double t = maptics[i];
                double x1, y1, x2, y2;
                gc.toPixels(t * unitx + px1, t * unity + py1, t * unitz + pz1, x1, y1);
                x2 = delx * ticlen * ticdir + x1;
                y2 = dely * ticlen * ticdir + y1;
                if (is2D()) {
                    snapLineToPixelCenters(x1, y1, x2, y2);
                }
                if (!ticksHandledIn2D) {
                    appendSegment2D(tickSegments, x1, y1, x2, y2);
                }
                double x3, y3;
                if (ticdir > 0) {
                    x3 = -delx * 0.015 * norm + x1;
                    y3 = -dely * 0.015 * norm + y1;
                } else {
                    x3 = -delx * 0.015 * norm + x2;
                    y3 = -dely * 0.015 * norm + y2;
                }
                if (!labels.empty()) {
                    tickLabelTasks.push_back({ x3, y3, labels[i % labels.size()] });
                    if (capturingAxes3d) {
                        gc.axes3dAddLabel(labels[i % labels.size()], t * unitx + px1,
                            t * unity + py1, t * unitz + pz1, color, fontSpecPixelSize(m_font),
                            tickRotation, "center", "middle");
                    }
                }
                if (is2D()) {
                    gc.toPixels(t * unitx + px2, t * unity + py2, t * unitz + pz2, x1, y1);
                    x2 = -delx * ticlen * ticdir + x1;
                    y2 = -dely * ticlen * ticdir + y1;
                    snapLineToPixelCenters(x1, y1, x2, y2);
                    if (!ticksHandledIn2D) {
                        appendSegment2D(tickSegments, x1, y1, x2, y2);
                    }
                }
            }
            profiler.section("tl_tickloop");
            if (capturingAxes3d) {
                // Tick labels captured as axes3d above; nothing drawn in 2D.
            } else if (!ticksHandledIn2D) {
                gc.setupDirectDraw();
                gc.lineSegments2D(tickSegments);
                for (const TickLabelTask& task : tickLabelTasks) {
                    drawLabel(
                        gc, -delx, -dely, task.x, task.y, color, task.text, false, tickRotation);
                }
                gc.releaseDirectDraw();
            } else if (!tickLabelTasks.empty()) {
                gc.setupDirectDraw();
                for (const TickLabelTask& task : tickLabelTasks) {
                    drawLabel(
                        gc, -delx, -dely, task.x, task.y, color, task.text, false, tickRotation);
                }
                gc.releaseDirectDraw();
            }
            profiler.section("tl_labeldraw");
        }
        if (!axisTextHasString(this, labelname)) {
            return;
        }
        double maxx, maxy;
        maxTickMetric(gc, labels, maxx, maxy);
        double x1, x2, x3, y1, y2, y3;
        double meanval = (limmin + limmax) / 2.0;
        gc.toPixels(meanval * unitx + px1, meanval * unity + py1, meanval * unitz + pz1, x1, y1);
        x2 = delx * ticlen * ticdir + x1;
        y2 = dely * ticlen * ticdir + y1;
        if (ticdir > 0) {
            x3 = -delx * 0.015 * norm + x1;
            y3 = -dely * 0.015 * norm + y1;
        } else {
            x3 = -delx * 0.015 * norm + x2;
            y3 = -dely * 0.015 * norm + y2;
        }
        double lx, ly;
        if (delx != 0) {
            lx = fabs(maxx / delx);
        } else {
            lx = 1e10;
        }
        if (dely != 0) {
            ly = fabs(maxy / dely);
        } else {
            ly = 1e10;
        }

        double lmax = std::min(lx, ly);
        GOGObjectsProperty* lbl = static_cast<GOGObjectsProperty*>(findProperty(labelname));
        if (!lbl->data().empty()) {
            GOText* fp = static_cast<GOText*>(findGraphicsObject(lbl->data()[0]));
            double axx1, axy1, axx2, axy2;
            gc.toPixels(0, 0, 0, axx1, axy1);
            gc.toPixels(unitx, unity, unitz, axx2, axy2);
            double angle = atan2(axy2 - axy1, axx2 - axx1) * 180.0 / M_PI;
            if (angle < -90) {
                angle += 180;
            }
            if (angle > 90) {
                angle -= 180;
            }
            GOScalarProperty* sp
                = static_cast<GOScalarProperty*>(fp->findProperty(GO_ROTATION_PROPERTY_NAME_STR));
            double origx, origy;
            gc.toPixels(meanval * unitx + (px1 + px2) / 2.0, meanval * unity + (py1 + py2) / 2.0,
                meanval * unitz + (pz1 + pz2) / 2.0, origx, origy);
            double meanx, meany;
            gc.toPixels(
                meanval * unitx + px1, meanval * unity + py1, meanval * unitz + pz1, meanx, meany);
            double xl1 = x3 - lmax * delx;
            double yl1 = y3 - lmax * dely;
            double angle2 = atan2(y3 - origy, x3 - origx) * 180.0 / M_PI;
            if ((angle == 90) && (angle2 > -90)) {
                angle = -90;
            }
            if (angle2 == 180) {
                angle2 = -180;
            }
            if (angle2 < 0) {
                if (fabs(angle) != 90) {
                    (static_cast<GOAlignVertProperty*>(
                         fp->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)))
                        ->data(GO_PROPERTY_VALUE_TOP_STR);
                } else {

                    (static_cast<GOAlignVertProperty*>(
                         fp->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)))
                        ->data(GO_PROPERTY_VALUE_BOTTOM_STR);
                }
            } else {
                (static_cast<GOAlignVertProperty*>(
                     fp->findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)))
                    ->data(GO_PROPERTY_VALUE_BOTTOM_STR);
            }
            if ((angle == -90) && (angle2 == -180)) {
                angle = 90;
            }
            if (labelname == GO_Z_LABEL_PROPERTY_NAME_STR && angle == -90) {
                angle = 90;
            }
            sp->data(angle);
            double labelOffset = labelname == GO_Z_LABEL_PROPERTY_NAME_STR ? 0.12 : 0.04;
            xl1 += (x3 - origx) * labelOffset;
            yl1 += (y3 - origy) * labelOffset;
            GOThreeVectorProperty* gp = static_cast<GOThreeVectorProperty*>(
                fp->findProperty(GO_POSITION_PROPERTY_NAME_STR));
            std::vector<double> outerpos(
                getPropertyVectorAsPixels(gc, GO_OUTER_POSITION_PROPERTY_NAME_STR));
            double xnorm, ynorm;
            xnorm = (xl1 - outerpos[0]) / outerpos[2];
            ynorm = (yl1 - outerpos[1]) / outerpos[3];
            if (labelname == GO_Z_LABEL_PROPERTY_NAME_STR) {
                ynorm -= 0.04;
            }
            gp->value(xnorm, ynorm, 0.0);
        }
    }
}
//=============================================================================
void
GOAxis::drawAxisLabels(RenderInterface& gc)
{
    bool hasXLabel = xvisible && axisTextHasString(this, GO_X_LABEL_PROPERTY_NAME_STR);
    bool hasYLabel = yvisible && axisTextHasString(this, GO_Y_LABEL_PROPERTY_NAME_STR);
    bool hasZLabel = zvisible && axisTextHasString(this, GO_Z_LABEL_PROPERTY_NAME_STR);
    bool hasTitle = axisTextHasString(this, GO_TITLE_PROPERTY_NAME_STR);
    bool hasSubtitle = axisTextHasString(this, GO_SUBTITLE_PROPERTY_NAME_STR);
    if (!hasXLabel && !hasYLabel && !hasZLabel && !hasTitle && !hasSubtitle) {
        return;
    }
    gc.lookAt(0, 0, 1, 0.0, 0.0, 0, 0, 1, 0);
    gc.project(0, 1, 0, 1, -1, 1);
    std::vector<double> outerpos(
        getPropertyVectorAsPixels(gc, GO_OUTER_POSITION_PROPERTY_NAME_STR));
    gc.viewport(outerpos[0], outerpos[1], outerpos[2], outerpos[3]);
    // Axis decorations are positioned relative to the projected outer box. In
    // 3-D a camera rotation can legitimately move a title a few pixels above
    // that box; keep the data clip for plotted children, but let decorations
    // use the complete figure frame just like the desktop painter does.
    // Without this, the top of titles such as "A smooth surface" is clipped
    // after a rotation in the shared WebView/WebAssembly renderer.
    gc.clipToEntireFrame();
    GOGObjectsProperty* lbl;
    std::wstring xdir(findStringProperty(GO_X_DIR_PROPERTY_NAME_STR));
    std::wstring ydir(findStringProperty(GO_Y_DIR_PROPERTY_NAME_STR));
    setStringDefault(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    if (hasXLabel) {
        lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_X_LABEL_PROPERTY_NAME_STR));
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
            if (fv) {
                if (fv->data() == GO_PROPERTY_VALUE_ON_STR) {
                    fp->paintMe(gc);
                }
            }
        }
    }
    if (hasYLabel) {
        lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Y_LABEL_PROPERTY_NAME_STR));
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
            if (fv) {
                if (fv->data() == GO_PROPERTY_VALUE_ON_STR) {
                    fp->paintMe(gc);
                }
            }
        }
    }
    if (hasZLabel) {
        lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Z_LABEL_PROPERTY_NAME_STR));
        if (!lbl->data().empty()) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
            if (fv) {
                if (fv->data() == GO_PROPERTY_VALUE_ON_STR) {
                    fp->paintMe(gc);
                }
            }
        }
    }
    setupProjection(gc);
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_TITLE_PROPERTY_NAME_STR));
    if (hasTitle && !lbl->data().empty()) {
        GraphicsObject* fp = findGraphicsObject(lbl->data()[0], false);
        if (fp) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
            if (fv) {
                if (fv->data() == GO_PROPERTY_VALUE_ON_STR) {
                    fp->paintMe(gc);
                }
            }
        }
    }
    lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_SUBTITLE_PROPERTY_NAME_STR));
    if (hasSubtitle && !lbl->data().empty()) {
        GraphicsObject* fp = findGraphicsObject(lbl->data()[0], false);
        if (fp) {
            GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
            GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
            if (fv) {
                if (fv->data() == GO_PROPERTY_VALUE_ON_STR) {
                    fp->paintMe(gc);
                }
            }
        }
    }
    setupProjection(gc);
    setStringDefault(GO_X_DIR_PROPERTY_NAME_STR, xdir);
    setStringDefault(GO_Y_DIR_PROPERTY_NAME_STR, ydir);
}
//=============================================================================
void
GOAxis::drawTickMarks(RenderInterface& gc)
{
    AxisPaintProfiler profiler;
    GOVectorProperty* hp = nullptr;
    hp = static_cast<GOVectorProperty*>(findProperty(GO_X_TICK_PROPERTY_NAME_STR));
    std::vector<double> xticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Y_TICK_PROPERTY_NAME_STR));
    std::vector<double> yticks(hp->data());
    hp = static_cast<GOVectorProperty*>(findProperty(GO_Z_TICK_PROPERTY_NAME_STR));
    std::vector<double> zticks(hp->data());
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    GOColorProperty* xc = static_cast<GOColorProperty*>(findProperty(GO_X_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* yc = static_cast<GOColorProperty*>(findProperty(GO_Y_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* zc = static_cast<GOColorProperty*>(findProperty(GO_Z_COLOR_PROPERTY_NAME_STR));

    std::vector<double> position(getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR));
    double maxlen = std::max(position[2], position[3]);
    GOTwoVectorProperty* kp
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_TICK_LENGTH_PROPERTY_NAME_STR));
    std::vector<double> ticklen(kp->data());
    int ticlen;
    if (is2D()) {
        ticlen = static_cast<int>(std::round(maxlen * ticklen[0]));
    } else {
        ticlen = static_cast<int>(std::round(maxlen * ticklen[1]));
    }
    ticlen = std::max(1, ticlen);
    float ticdir;
    if (isAuto(GO_TICK_DIR_MODE_PROPERTY_NAME_STR)) {
        if (is2D()) {
            ticdir = 1;
        } else {
            ticdir = -1;
        }
    } else {
        if ((static_cast<GOInOutProperty*>(findProperty(GO_TICK_DIR_PROPERTY_NAME_STR)))
                ->isEqual(GO_PROPERTY_VALUE_IN_STR)) {
            ticdir = 1;
        } else {
            ticdir = -1;
        }
    }

    GOTextInterpreterProperty* textInterpreterProperty = static_cast<GOTextInterpreterProperty*>(
        findProperty(GO_TICK_LABEL_INTERPRETER_PROPERTY_NAME_STR));
    TEXT_INTERPRETER_FORMAT textFormat = textInterpreterProperty->getAsEnum();

    GOStringVectorProperty* qp
        = static_cast<GOStringVectorProperty*>(findProperty(GO_X_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> xlabeltxt;
    if (textFormat == TEXT_INTERPRETER_FORMAT::TEX_MARKUP) {
        xlabeltxt = texToUnicode(qp->data());
    } else {
        xlabeltxt = qp->data();
    }

    qp = static_cast<GOStringVectorProperty*>(findProperty(GO_Y_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> ylabeltxt;
    if (textFormat == TEXT_INTERPRETER_FORMAT::TEX_MARKUP) {
        ylabeltxt = texToUnicode(qp->data());
    } else {
        ylabeltxt = qp->data();
    }

    qp = static_cast<GOStringVectorProperty*>(findProperty(GO_Z_TICK_LABEL_PROPERTY_NAME_STR));
    std::vector<std::wstring> zlabeltxt;
    if (textFormat == TEXT_INTERPRETER_FORMAT::TEX_MARKUP) {
        zlabeltxt = texToUnicode(qp->data());
    } else {
        zlabeltxt = qp->data();
    }

    std::vector<double> limits(getAxisLimits());
    GOLinearLogProperty* xScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    GOLinearLogProperty* yScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    GOLinearLogProperty* zScaleProperty
        = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    bool xLogScale = xScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    bool yLogScale = yScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    bool zLogScale = zScaleProperty->isEqual(GO_PROPERTY_VALUE_LOG_STR);
    xticks = getVisibleTicks(xticks, limits[0], limits[1], xLogScale);
    yticks = getVisibleTicks(yticks, limits[2], limits[3], yLogScale);
    zticks = getVisibleTicks(zticks, limits[4], limits[5], zLogScale);
    gc.setLineStyle(L"-");
    profiler.section("ticks_prep");

    if (xvisible) {
        int xCommonExponent = isAuto(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR)
            ? tickLabelCommonExponent(xticks, xLogScale)
            : 0;
        std::vector<double> mapticks;
        mapticks.reserve(xticks.size());
        for (size_t i = 0; i < xticks.size(); i++) {
            mapticks.push_back(mapX(xticks[i]));
        }
        std::vector<double> minorticks;
        if ((static_cast<GOOnOffProperty*>(findProperty(GO_X_MINOR_TICK_PROPERTY_NAME_STR)))
                ->asBool()) {
            double low = xLogScale ? std::pow(10.0, limits[0]) : limits[0];
            double high = xLogScale ? std::pow(10.0, limits[1]) : limits[1];
            for (double tick : getMinorTicks(xticks, low, high, xLogScale)) {
                minorticks.push_back(mapX(tick));
            }
        }
        drawTickLabels(gc, xc->data(), 0, x1pos[1], x1pos[2], 0, x2pos[1], x2pos[2], limits[0],
            limits[1], 1, 0, 0, mapticks, minorticks, xlabeltxt, GO_X_LABEL_PROPERTY_NAME_STR,
            ticlen, ticdir);
        profiler.section("x_labels");
        if (is2D()) {
            draw2DTickExponent(gc, true, xCommonExponent, textFormat, xc->data(), limits, m_font);
        }
        profiler.section("x_exponent");
    }
    if (yvisible) {
        int yCommonExponent = isAuto(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR)
            ? tickLabelCommonExponent(yticks, yLogScale)
            : 0;
        std::vector<double> mapticks;
        mapticks.reserve(yticks.size());
        for (size_t i = 0; i < yticks.size(); i++) {
            mapticks.push_back(mapY(yticks[i]));
        }
        std::vector<double> minorticks;
        if ((static_cast<GOOnOffProperty*>(findProperty(GO_Y_MINOR_TICK_PROPERTY_NAME_STR)))
                ->asBool()) {
            double low = yLogScale ? std::pow(10.0, limits[2]) : limits[2];
            double high = yLogScale ? std::pow(10.0, limits[3]) : limits[3];
            for (double tick : getMinorTicks(yticks, low, high, yLogScale)) {
                minorticks.push_back(mapY(tick));
            }
        }
        drawTickLabels(gc, yc->data(), y1pos[0], 0, y1pos[2], y2pos[0], 0, y2pos[2], limits[2],
            limits[3], 0, 1, 0, mapticks, minorticks, ylabeltxt, GO_Y_LABEL_PROPERTY_NAME_STR,
            ticlen, ticdir);
        profiler.section("y_labels");
        if (is2D()) {
            draw2DTickExponent(gc, false, yCommonExponent, textFormat, yc->data(), limits, m_font);
        }
        profiler.section("y_exponent");
    }
    if (zvisible) {
        std::vector<double> mapticks;
        mapticks.reserve(zticks.size());
        for (size_t i = 0; i < zticks.size(); i++) {
            mapticks.push_back(mapZ(zticks[i]));
        }
        std::vector<double> minorticks;
        if ((static_cast<GOOnOffProperty*>(findProperty(GO_Z_MINOR_TICK_PROPERTY_NAME_STR)))
                ->asBool()) {
            double low = zLogScale ? std::pow(10.0, limits[4]) : limits[4];
            double high = zLogScale ? std::pow(10.0, limits[5]) : limits[5];
            for (double tick : getMinorTicks(zticks, low, high, zLogScale)) {
                minorticks.push_back(mapZ(tick));
            }
        }
        drawTickLabels(gc, zc->data(), z1pos[0], z1pos[1], 0, z2pos[0], z2pos[1], 0, limits[4],
            limits[5], 0, 0, 1, mapticks, minorticks, zlabeltxt, GO_Z_LABEL_PROPERTY_NAME_STR,
            ticlen, ticdir);
    }
}
//=============================================================================
RenderClipBox
GOAxis::computeChildrenClipBox()
{
    RenderClipBox box;
    if (!stringCheck(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        return box;
    }
    std::vector<double> xlims(findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR));
    std::vector<double> ylims(findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR));
    std::vector<double> zlims(findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR));
    if (xlims.size() < 2 || ylims.size() < 2) {
        return box;
    }
    double mx0 = mapX(xlims[0], false);
    double mx1 = mapX(xlims[1], false);
    double my0 = mapY(ylims[0], false);
    double my1 = mapY(ylims[1], false);
    if (!std::isfinite(mx0) || !std::isfinite(mx1) || !std::isfinite(my0) || !std::isfinite(my1)) {
        return box;
    }
    box.xmin = std::min(mx0, mx1);
    box.xmax = std::max(mx0, mx1);
    box.ymin = std::min(my0, my1);
    box.ymax = std::max(my0, my1);
    // ClippingStyle 'rectangle' clips to the projected x/y rectangle only (no depth clip);
    // '3dbox' (default) clips to the full 3D axes volume.
    bool rectangleClip
        = stringCheck(GO_CLIPPING_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RECTANGLE_STR);
    if (is2D() || zlims.size() < 2 || rectangleClip) {
        // 2D content lives on the z == 0 plane; keep the depth range wide enough
        // that it never clips in z, but bounded (a huge range would blow up the
        // clip epsilon, which is derived from the box extent, and disable the
        // x / y clipping entirely).
        double pad = std::max(std::abs(box.xmax - box.xmin), std::abs(box.ymax - box.ymin)) + 1.0;
        box.zmin = -pad;
        box.zmax = pad;
    } else {
        double mz0 = mapZ(zlims[0], false);
        double mz1 = mapZ(zlims[1], false);
        if (!std::isfinite(mz0) || !std::isfinite(mz1)) {
            return box;
        }
        box.zmin = std::min(mz0, mz1);
        box.zmax = std::max(mz0, mz1);
    }
    box.enabled = true;
    return box;
}
//=============================================================================
// Selected == 'on' (and SelectionHighlight == 'on') draws small handles at the
// corners of the object's projected bounding box, in the selection blue.
static void
drawSelectionHandles(GOAxis* axis, RenderInterface& gc, GraphicsObject* object)
{
    std::vector<double> lims = object->getLimits();
    if (lims.size() < 4) {
        return;
    }
    double z = lims.size() >= 6 ? lims[4] : 0.0;
    std::vector<double> xs, ys, zs;
    for (double x : { lims[0], lims[1] }) {
        for (double y : { lims[2], lims[3] }) {
            xs.push_back(x);
            ys.push_back(y);
            zs.push_back(z);
        }
    }
    std::vector<double> mx, my, mz;
    axis->reMap(xs, ys, zs, mx, my, mz);
    if (mx.size() != 4) {
        return;
    }
    double minx = HUGE_VAL, miny = HUGE_VAL, maxx = -HUGE_VAL, maxy = -HUGE_VAL;
    for (size_t i = 0; i < 4; i++) {
        double u = 0, v = 0;
        bool clipped = false;
        gc.toPixels(mx[i], my[i], mz[i], u, v, clipped);
        minx = std::min(minx, u);
        maxx = std::max(maxx, u);
        miny = std::min(miny, v);
        maxy = std::max(maxy, v);
    }
    if (!std::isfinite(minx) || !std::isfinite(maxy)) {
        return;
    }
    gc.setupDirectDraw();
    gc.color(0.0, 0.4470, 0.7410);
    gc.lineWidth(1.0);
    gc.rect(minx, miny, maxx, maxy);
    const double s = 3.0;
    for (double cx : { minx, maxx }) {
        for (double cy : { miny, maxy }) {
            gc.rect(cx - s, cy - s, cx + s, cy + s);
        }
    }
    gc.releaseDirectDraw();
}
//=============================================================================
void
GOAxis::drawChildren(RenderInterface& gc)
{
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> handles(children->data());
    // SortMethod 'depth' paints children back-to-front by projected depth (3D only);
    // 'childorder' (default) keeps the Children order.
    if (!renderedAs2D()
        && stringCheck(GO_SORT_METHOD_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DEPTH_STR)) {
        std::vector<std::pair<double, int64>> ordered;
        ordered.reserve(handles.size());
        for (int64 h : handles) {
            GraphicsObject* fp = findGraphicsObject(h, false);
            double depth = -HUGE_VAL;
            if (fp) {
                std::vector<double> lims = fp->getLimits();
                if (lims.size() >= 4) {
                    std::vector<double> cx = { (lims[0] + lims[1]) * 0.5 };
                    std::vector<double> cy = { (lims[2] + lims[3]) * 0.5 };
                    std::vector<double> cz = { lims.size() >= 6 ? (lims[4] + lims[5]) * 0.5 : 0.0 };
                    std::vector<double> mx, my, mz;
                    reMap(cx, cy, cz, mx, my, mz);
                    if (mx.size() == 1) {
                        double u = 0, v = 0, c = 0;
                        bool clipped = false;
                        gc.toPixels(mx[0], my[0], mz[0], u, v, c, clipped);
                        depth = c;
                    }
                }
            }
            ordered.push_back({ depth, h });
        }
        // larger NDC z (farther) first so nearer children paint on top
        std::stable_sort(ordered.begin(), ordered.end(),
            [](const std::pair<double, int64>& a, const std::pair<double, int64>& b) {
                return a.first > b.first;
            });
        for (size_t i = 0; i < handles.size(); ++i) {
            handles[i] = ordered[i].second;
        }
    }
    RenderLightingScene lightingScene;
    lightingScene.ambientLightColor
        = findVectorDoubleProperty(GO_AMBIENT_LIGHT_COLOR_PROPERTY_NAME_STR);
    for (auto h : handles) {
        GraphicsObject* fp = findGraphicsObject(h, false);
        if (!fp || !fp->isType(GO_PROPERTY_VALUE_LIGHT_STR)
            || fp->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            continue;
        }
        RenderLight light;
        light.color = fp->findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
        light.position = fp->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        light.isLocal = fp->stringCheck(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LOCAL_STR);
        lightingScene.lights.push_back(light);
    }
    bool surfaceGroup = false;
    for (auto h : handles) {
        GraphicsObject* fp = findGraphicsObject(h, false);
        if (!fp || fp->isType(GO_PROPERTY_VALUE_LIGHT_STR)
            || fp->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            continue;
        }
        if (fp->isType(GO_PROPERTY_VALUE_SURFACE_STR)
            || fp->isType(GO_PROPERTY_VALUE_FUNCTION_SURFACE_STR)
            || fp->isType(GO_PROPERTY_VALUE_PATCH_STR) || fp->isType(GO_PROPERTY_VALUE_LINE_STR)
            || fp->isType(GO_PROPERTY_VALUE_STAIR_STR)
            || fp->isType(GO_PROPERTY_VALUE_SCATTER_STR)) {
            surfaceGroup = !renderedAs2D();
            break;
        }
    }

    lightingScene.enabled = !lightingScene.lights.empty();
    gc.setLightingScene(lightingScene);
    RenderClipBox dataClip = computeChildrenClipBox();
    if (dataClip.enabled) {
        gc.setDataClipBox(dataClip);
    }
    if (surfaceGroup) {
        gc.beginSurfaceGroup();
    }
    bool hidePolarGrid
        = isPolarAxes() && stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    for (auto h : handles) {
        GraphicsObject* fp = findGraphicsObject(h, false);
        if (fp) {
            if (fp->isType(GO_PROPERTY_VALUE_LIGHT_STR)
                || (hidePolarGrid && isPolarGridObject(fp))) {
                continue;
            }
            // Clipping 'off' lets an object draw outside the axes limits: drop the
            // data clip around its own paint, then restore it for the next child.
            bool unclip = dataClip.enabled
                && fp->findProperty(GO_CLIPPING_PROPERTY_NAME_STR, false) != nullptr
                && fp->stringCheck(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
            if (unclip) {
                gc.clearDataClipBox();
            }
            fp->paintMe(gc);
            if (unclip) {
                gc.setDataClipBox(dataClip);
            }
            if (fp->findProperty(GO_SELECTED_PROPERTY_NAME_STR, false) != nullptr
                && fp->stringCheck(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
                && highlightsSelection(fp)) {
                drawSelectionHandles(this, gc, fp);
            }
        }
    }
    if (surfaceGroup) {
        gc.endSurfaceGroup();
    }
    if (dataClip.enabled) {
        gc.clearDataClipBox();
    }
    // Flush deferred 2-D children under this axes' own clip so each is clipped to
    // its own axes (a Visible 'off' axes never flushes otherwise); 3-D keeps deferring.
    if (renderedAs2D()) {
        gc.flushDeferred();
    }
    gc.setLightingScene(RenderLightingScene());
}
//=============================================================================
void
GOAxis::updateUnits()
{
    if (hasChanged(GO_UNITS_PROPERTY_NAME_STR)) {
        GOUnitsProperty* unitsProperty = (GOUnitsProperty*)findProperty(GO_UNITS_PROPERTY_NAME_STR);
        std::wstring previousUnits = unitsProperty->getPreviousUnits();
        std::wstring currentUnits = unitsProperty->data();

        double width = 0;
        double height = 0;
        axesParentLogicalSizeInPixels(this, width, height);

        syncPositionUnitsTriplet(this, previousUnits, currentUnits, width, height, true);
        clearChanged(GO_UNITS_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GOAxis::updateState()
{
    // A decoration laying out from inside this update (the repaint runs the
    // legends) must not re-enter it: it marks the axes dirty instead.
    struct UpdateDepthGuard
    {
        int& depth;
        explicit UpdateDepthGuard(int& d) : depth(d) { ++depth; }
        ~UpdateDepthGuard() { --depth; }
    } updateDepthGuard(m_updateDepth);
    ensureTextSubObjects();
    ensureYAxisRulers();
    ensureSingleAxisRuler(GO_X_AXIS_PROPERTY_NAME_STR, RULER_AXIS_X);
    ensureSingleAxisRuler(GO_Z_AXIS_PROPERTY_NAME_STR, RULER_AXIS_Z);
    if (axisHasOnlyVisibleChanged(this)) {
        syncAxisLabelVisibility(this);
        clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
        GOFigure* fig = getParentFigure();
        if (fig) {
            fig->setRenderingStateInvalid(true);
            if (!fig->isDrawLater()) {
                fig->repaint();
            }
        }
        return;
    }
    updateUnits();
    GOStringProperty* typeProperty
        = static_cast<GOStringProperty*>(findProperty(GO_TYPE_PROPERTY_NAME_STR));
    typeProperty->data(stringCheck(GO_POLAR_AXES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)
            ? GO_PROPERTY_VALUE_POLAR_AXES_STR
            : GO_PROPERTY_VALUE_AXES_STR);
    bool positionOrInnerPositionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR)
        || hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
    bool outerPositionChanged = hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (outerPositionChanged && !m_layoutOuterPosition) {
        m_outerPositionFromUser = true;
    }
    m_layoutOuterPosition = false;
    bool uiAxesPositionHandled = syncUiAxesPositionProperties(this);
    if (!uiAxesPositionHandled) {
        syncOuterDrivenPositionTriplet(this);
        if (!outerPositionChanged) {
            if (syncInnerDrivenPositionTriplet(this) && !isPolarAxes()) {
                updateOuterPositionFromPosition();
            }
        }
    }
    // A direct PositionConstraint set selects which position drives the layout:
    // 'innerposition' pins Position (PositionMode manual), 'outerposition' lets
    // the outer box drive it (auto). Position/OuterPosition sets already set the
    // mode themselves, so only react when the constraint changed on its own.
    if (hasChanged(GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR) && !positionOrInnerPositionChanged
        && !outerPositionChanged) {
        if (stringCheck(
                GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INNERPOSITION_STR)) {
            toManual(GO_POSITION_MODE_PROPERTY_NAME_STR);
        } else if (stringCheck(GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR,
                       GO_PROPERTY_VALUE_OUTERPOSITION_STR)) {
            toAuto(GO_POSITION_MODE_PROPERTY_NAME_STR);
        }
    }
    std::vector<std::wstring> tset;
    tset.reserve(11);
    convertAxisFontUnits(this);
    bool autoFontSizeChanged = applyAutoFontSize();
    if (autoFontSizeChanged || hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)) {
        followAxesFontSizeInDecorations(this);
    }
    bool titleReferenceChanged
        = hasChanged(GO_TITLE_PROPERTY_NAME_STR) || hasChanged(GO_SUBTITLE_PROPERTY_NAME_STR);
    bool labelReferenceChanged = hasChanged(GO_X_LABEL_PROPERTY_NAME_STR)
        || hasChanged(GO_Y_LABEL_PROPERTY_NAME_STR) || hasChanged(GO_Z_LABEL_PROPERTY_NAME_STR);
    bool titleFontChanged = titleReferenceChanged || hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)
        || hasChanged(GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR)
        || hasChanged(GO_TITLE_FONT_WEIGHT_PROPERTY_NAME_STR)
        || hasChanged(GO_SUBTITLE_FONT_WEIGHT_PROPERTY_NAME_STR)
        || hasChanged(GO_FONT_NAME_PROPERTY_NAME_STR)
        || hasChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    // Axes font size, name and angle win; multipliers and weights reach auto texts only.
    AxisTextFontSync titleSync;
    titleSync.size = titleReferenceChanged || hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR);
    titleSync.autoSize = hasChanged(GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR);
    titleSync.weight = titleReferenceChanged;
    titleSync.autoWeight = hasChanged(GO_TITLE_FONT_WEIGHT_PROPERTY_NAME_STR)
        || hasChanged(GO_SUBTITLE_FONT_WEIGHT_PROPERTY_NAME_STR);
    titleSync.name = hasChanged(GO_FONT_NAME_PROPERTY_NAME_STR);
    titleSync.angle = hasChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    AxisTextFontSync labelSync;
    labelSync.size = labelReferenceChanged || hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR);
    labelSync.autoSize = hasChanged(GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR);
    labelSync.weight = hasChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    labelSync.name = titleSync.name;
    labelSync.angle = titleSync.angle;
    syncAxisLabelColors(this);
    auto enableLogMinorTick
        = [this](const std::wstring& scaleName, const std::wstring& minorTickName) {
              if (hasChanged(scaleName) && !hasChanged(minorTickName)
                  && stringCheck(scaleName, GO_PROPERTY_VALUE_LOG_STR)) {
                  setRestrictedStringDefault(minorTickName, GO_PROPERTY_VALUE_ON_STR);
                  clearChanged(minorTickName);
              }
          };
    enableLogMinorTick(GO_X_SCALE_PROPERTY_NAME_STR, GO_X_MINOR_TICK_PROPERTY_NAME_STR);
    enableLogMinorTick(GO_Y_SCALE_PROPERTY_NAME_STR, GO_Y_MINOR_TICK_PROPERTY_NAME_STR);
    enableLogMinorTick(GO_Z_SCALE_PROPERTY_NAME_STR, GO_Z_MINOR_TICK_PROPERTY_NAME_STR);
    // On a log-scaled axis the minor grid follows the major grid: turning the
    // grid on displays the faint dotted decade minor grid as well, and turning
    // it off removes it. This tracks the grid/scale transition only, so an
    // explicit minor-grid choice made in the same update still wins.
    auto coupleLogMinorGrid = [this](const std::wstring& scaleName, const std::wstring& gridName,
                                  const std::wstring& minorGridName) {
        if (!hasChanged(minorGridName) && (hasChanged(gridName) || hasChanged(scaleName))
            && stringCheck(scaleName, GO_PROPERTY_VALUE_LOG_STR)) {
            bool gridOn = (static_cast<GOOnOffProperty*>(findProperty(gridName)))->asBool();
            setRestrictedStringDefault(
                minorGridName, gridOn ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR);
            clearChanged(minorGridName);
        }
    };
    coupleLogMinorGrid(GO_X_SCALE_PROPERTY_NAME_STR, GO_X_GRID_PROPERTY_NAME_STR,
        GO_X_MINOR_GRID_PROPERTY_NAME_STR);
    coupleLogMinorGrid(GO_Y_SCALE_PROPERTY_NAME_STR, GO_Y_GRID_PROPERTY_NAME_STR,
        GO_Y_MINOR_GRID_PROPERTY_NAME_STR);
    coupleLogMinorGrid(GO_Z_SCALE_PROPERTY_NAME_STR, GO_Z_GRID_PROPERTY_NAME_STR,
        GO_Z_MINOR_GRID_PROPERTY_NAME_STR);
    if (hasChanged(GO_X_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_X_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_Z_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_A_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_A_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_C_LIM_PROPERTY_NAME_STR)) {
        toManual(GO_C_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_TICK_PROPERTY_NAME_STR)) {
        toManual(GO_X_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_TICK_PROPERTY_NAME_STR)) {
        toManual(GO_Y_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_TICK_PROPERTY_NAME_STR)) {
        toManual(GO_Z_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_TICK_LABEL_PROPERTY_NAME_STR)) {
        toManual(GO_X_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_TICK_LABEL_PROPERTY_NAME_STR)) {
        toManual(GO_Y_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_TICK_LABEL_PROPERTY_NAME_STR)) {
        toManual(GO_Z_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    bool thetaAxisUnitsChanged = hasChanged(GO_THETA_AXIS_UNITS_PROPERTY_NAME_STR);
    bool thetaLimChanged = hasChanged(GO_THETA_LIM_PROPERTY_NAME_STR);
    bool thetaTickChanged = hasChanged(GO_THETA_TICK_PROPERTY_NAME_STR);
    bool thetaTickLabelChanged = hasChanged(GO_THETA_TICK_LABEL_PROPERTY_NAME_STR);
    if (thetaAxisUnitsChanged) {
        bool toRadians
            = stringCheck(GO_THETA_AXIS_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RADIANS_STR);
        if (!thetaLimChanged) {
            std::vector<double> thetaLim = convertThetaUnits(
                findVectorDoubleProperty(GO_THETA_LIM_PROPERTY_NAME_STR), toRadians);
            if (thetaLim.size() >= 2) {
                setTwoVectorDefault(GO_THETA_LIM_PROPERTY_NAME_STR, thetaLim[0], thetaLim[1]);
            }
        }
        if (!thetaTickChanged) {
            setVectorDoubleDefault(GO_THETA_TICK_PROPERTY_NAME_STR,
                convertThetaUnits(
                    findVectorDoubleProperty(GO_THETA_TICK_PROPERTY_NAME_STR), toRadians));
        }
        if (!thetaTickLabelChanged && isAuto(GO_THETA_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
            static_cast<GOStringVectorProperty*>(
                findProperty(GO_THETA_TICK_LABEL_PROPERTY_NAME_STR))
                ->data(
                    formatThetaTickLabels(findVectorDoubleProperty(GO_THETA_TICK_PROPERTY_NAME_STR),
                        findVectorDoubleProperty(GO_THETA_LIM_PROPERTY_NAME_STR), toRadians));
        }
    }
    if (hasChanged(GO_R_LIM_PROPERTY_NAME_STR) && !hasChanged(GO_R_LIM_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_R_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_R_TICK_PROPERTY_NAME_STR) && !hasChanged(GO_R_TICK_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_R_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_R_TICK_LABEL_PROPERTY_NAME_STR)
        && !hasChanged(GO_R_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_R_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_THETA_LIM_PROPERTY_NAME_STR)
        && !hasChanged(GO_THETA_LIM_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_THETA_LIM_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_THETA_TICK_PROPERTY_NAME_STR)
        && !hasChanged(GO_THETA_TICK_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_THETA_TICK_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_THETA_TICK_LABEL_PROPERTY_NAME_STR)
        && !hasChanged(GO_THETA_TICK_LABEL_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_THETA_TICK_LABEL_MODE_PROPERTY_NAME_STR);
    }
    if (positionOrInnerPositionChanged) {
        toManual(GO_POSITION_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)) {
        toManual(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_GRID_ALPHA_PROPERTY_NAME_STR)) {
        toManual(GO_GRID_ALPHA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_GRID_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_GRID_COLOR_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_GRID_LINE_WIDTH_PROPERTY_NAME_STR)) {
        toManual(GO_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_MINOR_GRID_ALPHA_PROPERTY_NAME_STR)) {
        toManual(GO_MINOR_GRID_ALPHA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_MINOR_GRID_COLOR_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_MINOR_GRID_LINE_WIDTH_PROPERTY_NAME_STR)) {
        toManual(GO_MINOR_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR);
    }
    // Line widths go back to the default when their mode is set to auto (alpha and color keep).
    for (const std::wstring& widthName : { std::wstring(GO_GRID_LINE_WIDTH_PROPERTY_NAME_STR),
             std::wstring(GO_MINOR_GRID_LINE_WIDTH_PROPERTY_NAME_STR) }) {
        if (isAuto(widthName + L"Mode") && findScalarDoubleProperty(widthName) != 0.5) {
            setScalarDoubleDefault(widthName, 0.5);
            clearChanged(widthName);
        }
    }
    if (hasChanged(GO_TICK_DIR_PROPERTY_NAME_STR)
        && !hasChanged(GO_TICK_DIR_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_TICK_DIR_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_TICK_DIR_MODE_PROPERTY_NAME_STR)) {
        std::wstring autoTickDir = is2D() ? GO_PROPERTY_VALUE_IN_STR : GO_PROPERTY_VALUE_OUT_STR;
        if (!stringCheck(GO_TICK_DIR_PROPERTY_NAME_STR, autoTickDir)) {
            setRestrictedStringDefault(GO_TICK_DIR_PROPERTY_NAME_STR, autoTickDir);
            clearChanged(GO_TICK_DIR_PROPERTY_NAME_STR);
        }
    }
    if (hasChanged(GO_X_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_X_COLOR_MODE_PROPERTY_NAME_STR);
        syncAxisLabelColor(this, GO_X_LABEL_PROPERTY_NAME_STR, GO_X_COLOR_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_Y_COLOR_MODE_PROPERTY_NAME_STR);
        syncAxisLabelColor(this, GO_Y_LABEL_PROPERTY_NAME_STR, GO_Y_COLOR_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_Z_COLOR_MODE_PROPERTY_NAME_STR);
        syncAxisLabelColor(this, GO_Z_LABEL_PROPERTY_NAME_STR, GO_Z_COLOR_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR)) {
        toManual(GO_X_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR)) {
        toManual(GO_Y_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR)) {
        toManual(GO_Z_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    }
    tset.push_back(GO_FONT_ANGLE_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_NAME_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_SIZE_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_UNITS_PROPERTY_NAME_STR);
    tset.push_back(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    tset.push_back(GO_X_TICK_LABEL_PROPERTY_NAME_STR);
    tset.push_back(GO_Y_TICK_LABEL_PROPERTY_NAME_STR);
    tset.push_back(GO_Z_TICK_LABEL_PROPERTY_NAME_STR);
    tset.push_back(GO_X_COLOR_PROPERTY_NAME_STR);
    tset.push_back(GO_Y_COLOR_PROPERTY_NAME_STR);
    tset.push_back(GO_Z_COLOR_PROPERTY_NAME_STR);
    if (hasChanged(tset) || autoFontSizeChanged) {
        updateAxisFont();
        clearChanged(tset);
    }

    if (hasChanged(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR)) {
        toManual(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR)) {
        toManual(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR);
    }

    syncAxisTitleTexts(this, titleSync);
    syncAxisLabelTexts(this, labelSync);
    applyAutoAxisLabelOrientation(this);

    auto updateChildrenState = [this]() {
        GOGObjectsProperty* children
            = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
        std::vector<int64> handles(children->data());
        for (int i = 0; i < handles.size(); i++) {
            GraphicsObject* fp = findGraphicsObject(handles[i]);
            fp->updateState();
        }
    };

    auto finishAxisUpdate = [this]() {
        m_settledPosition = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        const bool viewChanged = hasChanged(GO_VIEW_PROPERTY_NAME_STR);
        clearAllChanged();
        if (viewChanged) {
            updatePeerColorBars(this);
        }
        GOFigure* fig = getParentFigure();
        if (fig) {
            fig->setRenderingStateInvalid(true);
            if (!fig->isDrawLater()) {
                fig->repaint();
            }
        }
    };

    bool polarAxes = isPolarAxes();
    if (polarAxes) {
        if (outerPositionChanged) {
            rePackFigure(false);
            // The default box holds no title room yet: reserve it from scratch.
            m_titleRepackShrink = 0.0;
            rePackTitleArea();
            std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
            if (position.size() == 4) {
                setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, position[0], position[1],
                    position[2], position[3]);
                clearChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
            }
        }
        handlePlotBoxFlags();
        updateCamera();
        updateChildrenState();
        m_stateDirty = false;
        finishAxisUpdate();
        return;
    }

    bool xflag = isAuto(GO_X_LIM_MODE_PROPERTY_NAME_STR);
    bool yflag = isAuto(GO_Y_LIM_MODE_PROPERTY_NAME_STR);
    bool zflag = isAuto(GO_Z_LIM_MODE_PROPERTY_NAME_STR);
    bool aflag = isAuto(GO_A_LIM_MODE_PROPERTY_NAME_STR);
    bool cflag = isAuto(GO_C_LIM_MODE_PROPERTY_NAME_STR);
    updateLimits(xflag, yflag, zflag, aflag, cflag);

    handlePlotBoxFlags();

    updateCamera();

    updateChildrenState();

    if (hasChanged(GO_VISIBLE_PROPERTY_NAME_STR)) {
        bool isVisible = stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);

        GOGObjectsProperty* lbl;
        if (xvisible) {
            lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_X_LABEL_PROPERTY_NAME_STR));
            if (!lbl->data().empty()) {
                GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
                GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                    fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
                if (fv) {
                    fv->data(isVisible ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR);
                }
            }
        }
        if (yvisible) {
            lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Y_LABEL_PROPERTY_NAME_STR));
            if (!lbl->data().empty()) {
                GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
                GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                    fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
                if (fv) {
                    fv->data(isVisible ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR);
                }
            }
        }
        if (zvisible) {
            lbl = static_cast<GOGObjectsProperty*>(findProperty(GO_Z_LABEL_PROPERTY_NAME_STR));
            if (!lbl->data().empty()) {
                GraphicsObject* fp = findGraphicsObject(lbl->data()[0]);
                GOOnOffProperty* fv = static_cast<GOOnOffProperty*>(
                    fp->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false));
                if (fv) {
                    fv->data(isVisible ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR);
                }
            }
        }
    }

    bool titleLayoutChanged = titleReferenceChanged || titleFontChanged || autoFontSizeChanged
        || hasChanged(GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    rePackFigure(titleLayoutChanged);
    if (outerPositionChanged && !uiAxesPositionHandled) {
        std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() == 4) {
            setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, position[0], position[1],
                position[2], position[3]);
            clearChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
        }
    }
    recalculateTicks();
    handlePlotBoxFlags();
    updateCamera();
    rePackFigure(titleLayoutChanged);
    recalculateTicks();
    handlePlotBoxFlags();
    updateCamera();
    rePackFigure(titleLayoutChanged);
    if (isDualYAxis()) {
        updateSecondaryYLimits();
    }
    syncYAxisRulers();
    syncSingleAxisRuler(GO_X_AXIS_PROPERTY_NAME_STR, RULER_AXIS_X);
    syncSingleAxisRuler(GO_Z_AXIS_PROPERTY_NAME_STR, RULER_AXIS_Z);
    m_stateDirty = false;
    finishAxisUpdate();
}
//=============================================================================
void
GOAxis::markStateDirty()
{
    m_stateDirty = true;
}
//=============================================================================
void
GOAxis::releaseUserOuterPosition()
{
    // a subplot axes keeps the outer box of its grid cell
    std::vector<double> grid = findVectorDoubleProperty(GO_SUBPLOT_GRID_INTERNAL_PROPERTY_NAME_STR);
    if (grid.size() == 2 && grid[0] >= 1 && grid[1] >= 1) {
        return;
    }
    m_outerPositionFromUser = false;
}
//=============================================================================
void
GOAxis::requestLayoutUpdate()
{
    if (m_updateDepth > 0) {
        markStateDirty();
        return;
    }
    updateState();
}
//=============================================================================
void
GOAxis::setLayoutOuterPosition(const std::vector<double>& outerPosition)
{
    if (outerPosition.size() < 4) {
        return;
    }
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, outerPosition[0], outerPosition[1],
        outerPosition[2], outerPosition[3]);
    m_layoutOuterPosition = true;
}
//=============================================================================
void
GOAxis::setLayoutPosition(const std::vector<double>& position)
{
    if (position.size() < 4) {
        return;
    }
    for (const std::wstring& name :
        { GO_POSITION_PROPERTY_NAME_STR, GO_INNER_POSITION_PROPERTY_NAME_STR }) {
        auto* hp = static_cast<GOFourVectorProperty*>(findProperty(name, false));
        if (hp) {
            hp->value(position[0], position[1], position[2], position[3]);
            hp->clearModified();
        }
    }
}
//=============================================================================
std::vector<double>
GOAxis::layoutUnitsToPixels(const std::vector<double>& values)
{
    return unitsReinterpret(values);
}
//=============================================================================
bool
GOAxis::usesUiAxesLayout()
{
    return dynamic_cast<GOUIAxes*>(this) != nullptr;
}
//=============================================================================
void
GOAxis::updateStateIfDirty()
{
    if (m_stateDirty) {
        updateState();
    }
}
//=============================================================================
void
GOAxis::setupProjectionOnly(RenderInterface& gc, bool updateLimits)
{
    setupProjection(gc, updateLimits);
}
//=============================================================================
void
GOAxis::paintMe(RenderInterface& gc)
{
    if (getParentFigure() == NULL) {
        return;
    }
    // Children attached since the last recompute only marked the axes dirty, to
    // keep object creation O(N). Fold their contribution in now, once, before
    // anything reads the limits/ticks for drawing.
    updateStateIfDirty();
    AxisPaintProfiler profiler;
    setupProjection(gc);
    gc.clearDepthBuffer();
    profiler.section("projection");
    // Scene v2: a standalone export records this 3D axes' base transform and
    // decoration geometry so the offline replayer rotates the whole box with the
    // surface. The 2D box/grid/tick primitives are then suppressed for this axes.
    const bool captureAxes3d = gc.axes3dCaptureActive() && !is2D();
    if (captureAxes3d) {
        std::vector<double> axLimits(getAxisLimits());
        const double boxCentre[3] = { (axLimits[0] + axLimits[1]) * 0.5,
            (axLimits[2] + axLimits[3]) * 0.5, (axLimits[4] + axLimits[5]) * 0.5 };
        gc.axes3dBeginAxes(findGraphicsObjectHandle(this), boxCentre);
    }
    bool isPolarAxes = stringCheck(GO_POLAR_AXES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    bool isVisible = stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    if (isPolarAxes || isVisible) {
        setupAxis(gc);
    }
    profiler.section("setup_axis");
    if (isPolarAxes) {
        // A hidden polaraxes keeps its data and title, not its disc, grid or labels.
        if (isVisible) {
            drawPolarBackground(gc);
        }
        layoutPolarGridLabels(this);
        drawChildren(gc);
        // The title/subtitle follow the current size and theta labels; their own Visible decides.
        syncTitleTextLayout();
        drawAxisLabels(gc);
        m_box = getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR);
        markViewStateReady();
        return;
    }
    // Layer 'top' (2-D) paints the grid over the data instead of under it.
    bool gridOnTop
        = renderedAs2D() && stringCheck(GO_LAYER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR);
    if (isVisible) {
        // The 2D box silhouette does not rotate, so it is dropped while capturing;
        // the box edges are emitted as axes3d by drawAxisLines below.
        if (!captureAxes3d) {
            drawBox(gc);
        }
        if (!gridOnTop) {
            if (captureAxes3d) {
                gc.axes3dSetCategory(2);
            }
            drawGridLines(gc);
            drawMinorGridLines(gc);
            if (captureAxes3d) {
                gc.axes3dSetCategory(0);
            }
        }
    }
    profiler.section("box_grid");

    bool depthSortedAxisLines = isVisible && !renderedAs2D() && axisHasDepthSortedSurface(this);
    if (depthSortedAxisLines) {
        gc.beginSurfaceGroup();
    }
    // Category 3 while painting children: a 3D line child is captured as a line-only
    // WebGlMesh so it rotates with the box. Surfaces already emit their own mesh.
    if (captureAxes3d) {
        gc.axes3dSetCategory(3);
    }
    drawChildren(gc);
    if (captureAxes3d) {
        gc.axes3dSetCategory(0);
    }
    profiler.section("children");

    if (isVisible && gridOnTop) {
        // Push the data into the frame first so the grid, drawn next, lands on top.
        gc.flushDeferred();
        drawGridLines(gc);
        drawMinorGridLines(gc);
    }

    if (!isVisible) {
        // An axes with hidden decorations (e.g. after 'axis off', as done by pie)
        // still displays a Title/Subtitle/label whose own 'Visible' is 'on'.
        drawAxisLabels(gc);
        m_box = getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR);
        markViewStateReady();
        return;
    }

    if (captureAxes3d) {
        gc.axes3dSetCategory(1);
    }
    drawAxisLines(gc);
    if (captureAxes3d) {
        gc.axes3dSetCategory(0);
    }
    if (depthSortedAxisLines) {
        gc.endSurfaceGroup();
    }
    profiler.section("axis_lines");
    if (!renderedAs2D()) {
        gc.depth(false);
    }
    bool preserveAxisLabelState = axisTextHasString(this, GO_X_LABEL_PROPERTY_NAME_STR)
        || axisTextHasString(this, GO_Y_LABEL_PROPERTY_NAME_STR)
        || axisTextHasString(this, GO_Z_LABEL_PROPERTY_NAME_STR);
    std::array<AxisLabelState, 3> labelStates;
    if (preserveAxisLabelState) {
        labelStates = { captureAxisLabelState(this, GO_X_LABEL_PROPERTY_NAME_STR),
            captureAxisLabelState(this, GO_Y_LABEL_PROPERTY_NAME_STR),
            captureAxisLabelState(this, GO_Z_LABEL_PROPERTY_NAME_STR) };
    }
    // While capturing, drawTickLabels records the tick numbers at their 3D anchors
    // and skips the 2D tick/label drawing, so the numbers rotate with the box.
    drawTickMarks(gc);
    profiler.section("tick_marks");
    drawAxisLabels(gc);
    profiler.section("axis_labels");
    if (preserveAxisLabelState) {
        for (const AxisLabelState& labelState : labelStates) {
            restoreAxisLabelState(labelState);
        }
    }
    if (!renderedAs2D()) {
        gc.depth(true);
    }

    if (isVisible && !isPolarAxes && is2D() && isDualYAxis()) {
        drawSecondaryYRuler(gc);
    }

    m_box = getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR);
    markViewStateReady();
    profiler.section("finish");
}
//=============================================================================
std::vector<double>
GOAxis::getPropertyVectorAsPixels(RenderInterface& gc, const std::wstring& name)
{
    GOFourVectorProperty* hp = (GOFourVectorProperty*)findProperty(name);
    std::vector<double> position = reInterpUnits(gc, hp->data());
    if (name == GO_POSITION_PROPERTY_NAME_STR && usesUiAxesLayout()) {
        double scaleX, scaleY;
        logicalToSurfaceScale(this, gc.width(), gc.height(), scaleX, scaleY);
        return uiAxesViewportPosition(position, scaleX, scaleY);
    }
    return position;
}
//=============================================================================
std::vector<double>
GOAxis::reInterpUnits(RenderInterface& gc, std::vector<double> a)
{
    double width = gc.width();
    double height = gc.height();
    GOUnitsProperty* hp = (GOUnitsProperty*)findProperty(GO_UNITS_PROPERTY_NAME_STR);
    if (hp->isEqual(GO_PROPERTY_VALUE_NORMALIZED_STR)) {
        for (int i = 0; i < a.size(); i += 2) {
            a[i] *= width;
            a[i + 1] *= height;
        }
        return a;
    } else if (hp->isEqual(GO_PROPERTY_VALUE_PIXELS_STR)
        || hp->isEqual(GO_PROPERTY_VALUE_POINTS_STR) || hp->isEqual(GO_PROPERTY_VALUE_INCHES_STR)
        || hp->isEqual(GO_PROPERTY_VALUE_CENTIMETERS_STR)) {
        bool points = hp->isEqual(GO_PROPERTY_VALUE_POINTS_STR);
        double absolutePixels = 0.;
        if (hp->isEqual(GO_PROPERTY_VALUE_INCHES_STR)) {
            absolutePixels = 96.;
        } else if (hp->isEqual(GO_PROPERTY_VALUE_CENTIMETERS_STR)) {
            absolutePixels = 96. / 2.54;
        }
        double scale[2];
        logicalToSurfaceScale(this, width, height, scale[0], scale[1]);
        for (size_t i = 0; i < a.size(); i++) {
            // The first pixel is at 1 in pixel units: the origin is shifted back to 0.
            const double origin = (!points && absolutePixels == 0. && i < 2) ? 1. : 0.;
            double value = a[i] - origin;
            if (points) {
                value = gc.pointsToPixels(a[i]);
            } else if (absolutePixels != 0.) {
                value = a[i] * absolutePixels;
            }
            a[i] = value * scale[i % 2];
        }
        return a;
    } else {
        Error(L"Nelson:axes:invalidUnits", L"Units not supported:%s", hp->data());
    }
    return a;
}
//=============================================================================
std::vector<double>
GOAxis::unitsReinterpret(std::vector<double> a)
{
    double width = 0;
    double height = 0;
    graphicsParentSizeInPixels(this, getParentFigure(), width, height);
    GOUnitsProperty* hp = static_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR));
    if (hp->isEqual(GO_PROPERTY_VALUE_NORMALIZED_STR)) {
        for (size_t i = 0; i < a.size(); i += 2) {
            a[i] *= width;
            a[i + 1] *= height;
        }
    } else if (hp->isEqual(GO_PROPERTY_VALUE_PIXELS_STR)) {
        // The first pixel is at 1 in pixel units: the origin is shifted back to 0.
        for (size_t i = 0; i < a.size() && i < 2; ++i) {
            a[i] -= 1.;
        }
        return a;
    } else if (hp->isEqual(GO_PROPERTY_VALUE_POINTS_STR)) {
        for (double& value : a) {
            value = pointsToPixels(value);
        }
        return a;
    } else if (hp->isEqual(GO_PROPERTY_VALUE_INCHES_STR)
        || hp->isEqual(GO_PROPERTY_VALUE_CENTIMETERS_STR)) {
        const double pixelsPerUnit = hp->isEqual(GO_PROPERTY_VALUE_INCHES_STR) ? 96. : 96. / 2.54;
        for (double& value : a) {
            value *= pixelsPerUnit;
        }
        return a;
    } else {
        Error(L"Nelson:axes:invalidUnits", L"Units not supported:%s", hp->data());
    }
    return a;
}
//=============================================================================
bool
GOAxis::is2D()
{
    double azimuth;
    double elevation;
    getView(azimuth, elevation);
    return std::fabs(elevation - 90.) < 1e-9;
}
//=============================================================================
bool
GOAxis::renderedAs2D()
{
    // Like is2D(), but follows the actual camera when CameraPosition and CameraTarget are posed
    // manually. Setting those does not refresh View (azimuth/elevation), so an obliquely posed
    // camera can leave View at the default top-down [0 90]; is2D() would then report 2D and force
    // the orthographic, data-extent-coupled path for a genuine 3D perspective scene. is2D() stays
    // View-based because axis layout, ticks and labels key off the declared View. Projection, depth
    // sorting and camera transform use this function instead. A scene is 2D here only when the
    // camera looks straight down +z onto the xy-plane (the elevation == 90 case of is2D()).
    //
    // A manual CameraPosition alone also poses the camera: the automatic CameraTarget is the
    // centre of the data box (kept current by updateCamera), so the viewing direction is
    // target - position whether or not the target was set by hand. Only an automatic position
    // is derived from View, so only then does View describe the real camera.
    if (stringCheck(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        GOThreeVectorProperty* cameraPositionVector = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR));
        GOThreeVectorProperty* cameraTargetVector
            = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR));
        std::vector<double> pos = cameraPositionVector->data();
        std::vector<double> tgt = cameraTargetVector->data();
        if (pos.size() >= 3 && tgt.size() >= 3) {
            double dx = pos[0] - tgt[0];
            double dy = pos[1] - tgt[1];
            double dz = pos[2] - tgt[2];
            double horizontal = std::sqrt(dx * dx + dy * dy);
            return dz > 0 && horizontal <= 1e-9 * std::fabs(dz);
        }
    }
    return is2D();
}
//=============================================================================
std::vector<double>
GOAxis::getAxisLimits()
{
    std::vector<double> lims;
    lims.reserve(6);
    double xLimMin;
    double xLimMax;
    computeAxisLimits(GO_X_LIM_PROPERTY_NAME_STR, 0, 1, xLimMin, xLimMax);
    GOLinearLogProperty* sp
        = static_cast<GOLinearLogProperty*>(findProperty(GO_X_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        lims.push_back(xLimMin);
        lims.push_back(xLimMax);
    } else {
        lims.push_back(tickLog(xLimMin));
        lims.push_back(tickLog(xLimMax));
    }
    double yLimMin;
    double yLimMax;
    computeAxisLimits(GO_Y_LIM_PROPERTY_NAME_STR, 2, 3, yLimMin, yLimMax);
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Y_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        lims.push_back(yLimMin);
        lims.push_back(yLimMax);
    } else {
        lims.push_back(tickLog(yLimMin));
        lims.push_back(tickLog(yLimMax));
    }

    double zLimMin;
    double zLimMax;
    computeAxisLimits(GO_Z_LIM_PROPERTY_NAME_STR, 4, 5, zLimMin, zLimMax);
    sp = static_cast<GOLinearLogProperty*>(findProperty(GO_Z_SCALE_PROPERTY_NAME_STR));
    if (sp->isEqual(GO_PROPERTY_VALUE_LINEAR_STR)) {
        lims.push_back(zLimMin);
        lims.push_back(zLimMax);
    } else {
        lims.push_back(tickLog(zLimMin));
        lims.push_back(tickLog(zLimMax));
    }
    return lims;
}
//=============================================================================
bool
GOAxis::setupPerspectiveProjection(RenderInterface& gc, double zMappedMin, double zMappedMax,
    bool angleSpansWidth, const std::vector<double>& position)
{
    double viewAngle = findScalarDoubleProperty(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR);
    if (!std::isfinite(viewAngle) || viewAngle <= 0 || viewAngle >= 180) {
        viewAngle = DEFAULT_CAMERA_VIEW_ANGLE;
    }
    // View space looks down -z, so a point's distance from the camera is -zview:
    // the farthest data point is at -zMappedMin, the closest at -zMappedMax.
    double farDist = -zMappedMin;
    double nearDist = -zMappedMax;
    if (!std::isfinite(farDist) || farDist <= 0) {
        // Whole scene is at or behind the camera: keep the orthographic path.
        return false;
    }
    if (!std::isfinite(nearDist) || nearDist <= 0) {
        // Camera sits inside the data box: clamp the near plane in front of it.
        nearDist = farDist * 1e-3;
    }
    nearDist *= (1.0 - PROJECTION_CLIP_MARGIN);
    farDist *= (1.0 + PROJECTION_CLIP_MARGIN);
    if (nearDist < farDist * 1e-6) {
        nearDist = farDist * 1e-6;
    }
    if (!(farDist > nearDist)) {
        return false;
    }
    double aspect = (position.size() >= 4 && std::isfinite(position[3]) && position[3] > 0)
        ? position[2] / position[3]
        : 1.0;
    double tanHalfAngle = std::tan(viewAngle * M_PI / 360.0);
    // The automatic angle spans the limiting extent of the data box, which is the width when the
    // box is relatively wider than the viewport; the frustum is defined by its height, so convert
    // through the aspect. A manual angle is the vertical field of view.
    if (angleSpansWidth && isAuto(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR)) {
        tanHalfAngle /= aspect;
    }
    double halfHeight = nearDist * tanHalfAngle;
    double halfWidth = halfHeight * aspect;
    if (!std::isfinite(halfHeight) || halfHeight <= 0 || !std::isfinite(halfWidth)
        || halfWidth <= 0) {
        return false;
    }
    finishProjectionViewport(
        gc, position, -halfWidth, halfWidth, -halfHeight, halfHeight, nearDist, farDist, true);
    return true;
}
//=============================================================================
void
GOAxis::getView(double& azimuth, double& elevation)
{
    GOTwoVectorProperty* viewProperty
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_VIEW_PROPERTY_NAME_STR));
    azimuth = viewProperty->data()[0];
    elevation = viewProperty->data()[1];
}
//=============================================================================
void
GOAxis::getNormalizedView(double& azimuth, double& elevation)
{
    GOTwoVectorProperty* viewProperty
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_VIEW_PROPERTY_NAME_STR));
    azimuth = viewProperty->data()[0];
    elevation = viewProperty->data()[1];

    if (elevation > 180.0) {
        elevation -= 360.0;
    } else if (elevation < -180.0) {
        elevation += 360.0;
    }

    if (azimuth > 180.0) {
        azimuth -= 360.0;
    } else if (azimuth < -180.0) {
        azimuth += 360.0;
    }
}
//=============================================================================
void
GOAxis::setView(double azimuth, double elevation)
{
    GOTwoVectorProperty* viewProperty
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_VIEW_PROPERTY_NAME_STR));
    viewProperty->value(azimuth, elevation);
    // flagged like a View set through set(): the next update sees the change
    viewProperty->setModified(true);
}
//=============================================================================
void
GOAxis::rotateCamera(
    double previousMouseX, double currentMouseX, double previousMouseY, double currentMouseY)
{
    GOTwoVectorProperty* viewProperty
        = static_cast<GOTwoVectorProperty*>(findProperty(GO_VIEW_PROPERTY_NAME_STR));
    double azimuth = viewProperty->data()[0];
    double elevation = viewProperty->data()[1];

    azimuth += (previousMouseX - currentMouseX);
    elevation += (currentMouseY - previousMouseY);
    setView(azimuth, elevation);
}
//=============================================================================
void
GOAxis::updateInteractionState()
{
    updateCamera();
    // The title/subtitle carry a data-space Position that depends on the current view (2D top
    // branch vs oblique 3D branch). A view change reaches here without the full updateState()
    // layout, so the stored Position would still belong to the previous view and land inside the
    // box, off-center. Re-run the title layout so the immediate repaint uses a consistent Position.
    syncTitleTextLayout();
    // For the same reason, re-derive the x/y/z label Rotation and alignment from the new view.
    applyAutoAxisLabelOrientation(this);
    // A view turned 3D gives the ZLabel its size.
    syncAxisLabelTexts(this, AxisTextFontSync());
    // View stays flagged: the full update that follows moves the outside colorbars
    if (isAuto(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR)) {
        clearChanged(GO_CAMERA_TARGET_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR)) {
        clearChanged(GO_CAMERA_POSITION_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR)) {
        clearChanged(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR)) {
        clearChanged(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR);
    }
    GOFigure* fig = getParentFigure();
    if (fig) {
        fig->setRenderingStateInvalid(true);
        GOFigureHost* host = fig->getHost();
        if (!fig->isDrawLater() && host && !host->isHeadlessHost()) {
            fig->repaint();
        }
    }
}
//=============================================================================
// Mirrors an axes property onto its ruler. A categorical position axis stores
// its Limits/TickValues as a categorical (class) value; the numeric ruler must
// receive only the underlying numeric positions, so the categorical class value
// is never handed to the ruler's double-typed property (tick text is mirrored
// separately through TickLabels).
static void
copyRulerProperty(GOGenericProperty* sp, GOGenericProperty* dp)
{
    ArrayOf value = sp->get();
    if (value.isClassType()) {
        GOVectorProperty* srcVec = dynamic_cast<GOVectorProperty*>(sp);
        GOVectorProperty* dstVec = dynamic_cast<GOVectorProperty*>(dp);
        if (srcVec != nullptr && dstVec != nullptr) {
            dstVec->data(srcVec->data());
            return;
        }
    }
    dp->set(value);
}
//=============================================================================
bool
hasNoFinitePoint(const std::vector<double>& childLimits)
{
    // All-NaN data report inverted X, Y and Z ranges: such a child sets no limit.
    return childLimits.size() >= 6 && childLimits[0] > childLimits[1]
        && childLimits[2] > childLimits[3] && childLimits[4] > childLimits[5];
}
//=============================================================================
// A bar position edge sets the auto limit as is: only data edges snap to the ticks.
static void
collectBarPositionEdges(GraphicsObject* object, bool yRuler, double& lo, double& hi, int depth)
{
    auto* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children || depth > 8) {
        return;
    }
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (!child || excludedFromAutoLimits(child)) {
            continue;
        }
        if (child->isType(GO_PROPERTY_VALUE_BAR_STR)) {
            bool horizontal
                = child->stringCheck(GO_HORIZONTAL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
            std::vector<double> lims = child->getLimits();
            if (horizontal == yRuler && lims.size() >= 4) {
                lo = std::min(lo, lims[yRuler ? 2 : 0]);
                hi = std::max(hi, lims[yRuler ? 3 : 1]);
            }
        } else if (child->isType(GO_PROPERTY_VALUE_HGGROUP_STR)
            || child->isType(GO_PROPERTY_VALUE_HGTRANSFORM_STR)) {
            collectBarPositionEdges(child, yRuler, lo, hi, depth + 1);
        }
    }
}
//=============================================================================
static void
keepBarEdges(GOAxis* axis, bool yRuler, const std::vector<double>& dataLimits, double& start,
    double& stop, std::vector<double>& ticks, std::vector<std::wstring>& labels)
{
    double lo = std::numeric_limits<double>::infinity();
    double hi = -std::numeric_limits<double>::infinity();
    collectBarPositionEdges(axis, yRuler, lo, hi, 0);
    size_t k = yRuler ? 2 : 0;
    if (!std::isfinite(lo) || dataLimits.size() < k + 2) {
        return;
    }
    double tolerance = 1e-12 * std::max(1., std::fabs(dataLimits[k + 1] - dataLimits[k]));
    bool changed = false;
    if (std::fabs(lo - dataLimits[k]) <= tolerance && dataLimits[k] > start) {
        start = dataLimits[k];
        changed = true;
    }
    if (std::fabs(hi - dataLimits[k + 1]) <= tolerance && dataLimits[k + 1] < stop) {
        stop = dataLimits[k + 1];
        changed = true;
    }
    if (!changed) {
        return;
    }
    std::vector<double> keptTicks;
    std::vector<std::wstring> keptLabels;
    bool withLabels = labels.size() == ticks.size();
    for (size_t i = 0; i < ticks.size(); ++i) {
        if (ticks[i] >= start - tolerance && ticks[i] <= stop + tolerance) {
            keptTicks.push_back(ticks[i]);
            if (withLabels) {
                keptLabels.push_back(labels[i]);
            }
        }
    }
    ticks = keptTicks;
    if (withLabels) {
        labels = keptLabels;
    }
}
//=============================================================================
// Polar layout maps font points to pixels at a fixed 96 dpi, so the title and
// theta-label geometry matches the reference on any display. The on-screen
// pointsToPixels follows the display dpi (RenderHelpers), which would shift the
// layout on a screen that is not 96 dpi, while the figure box is a fixed pixel
// canvas; the axes layout manager converts points the same fixed way.
static double
polarPointsToPixels(double points)
{
    return points * 96.0 / 72.0;
}
//=============================================================================
// Data Y of a polar title bottom: 2 px above the highest theta label (measured on the reference).
static double
polarTitleDataYPosition(GOAxis* axis, const std::vector<double>& limits, bool aboveSubtitle)
{
    double scale = polarPixelsPerDataUnit(axis, limits);
    if (!(scale > 0)) {
        double yRange = limits[3] - limits[2];
        return limits[3] + 0.06 * (std::isfinite(yRange) && yRange > 0 ? yRange : 1.0);
    }
    double topPixels = 0.0;
    double radius = polarDiscRadius(axis, limits);
    auto* children = static_cast<GOGObjectsProperty*>(
        axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (children) {
        for (int64 handle : children->data()) {
            GraphicsObject* child = findGraphicsObject(handle, false);
            if (!isPolarThetaLabel(child, radius)
                || child->findStringProperty(GO_STRING_PROPERTY_NAME_STR).empty()) {
                continue;
            }
            std::vector<double> position
                = child->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
            double fontPixels = polarPointsToPixels(fontSizeInPoints(child));
            if (position.size() < 2 || !std::isfinite(position[1]) || !std::isfinite(fontPixels)) {
                continue;
            }
            // A label line is about 1.1 em high above its vertical anchor.
            double lineAbove = 1.1 * fontPixels;
            if (child->stringCheck(
                    GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MIDDLE_STR)) {
                lineAbove *= 0.5;
            } else if (child->stringCheck(
                           GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR)) {
                lineAbove = 0.0;
            }
            double top = (position[1] - limits[3]) * scale + lineAbove;
            topPixels = std::max(topPixels, top);
        }
    }
    double offsetPixels = topPixels + 2.2;
    if (aboveSubtitle) {
        GOText* subtitle = axisTextObject(axis, GO_SUBTITLE_PROPERTY_NAME_STR);
        double fontSize
            = fontSizeInPoints(subtitle ? static_cast<GraphicsObject*>(subtitle) : axis);
        // The title sits one subtitle line higher.
        offsetPixels += 1.3 * polarPointsToPixels(fontSize);
    }
    return limits[3] + offsetPixels / scale;
}
//=============================================================================
static double
polarDiscRadius(GOAxis* axis, const std::vector<double>& limits)
{
    std::vector<double> rLim(axis->findVectorDoubleProperty(GO_R_LIM_PROPERTY_NAME_STR));
    double radius = 0.;
    if (rLim.size() >= 2) {
        radius = std::max(std::fabs(rLim[0]), std::fabs(rLim[1]));
    }
    if ((radius <= 0 || !std::isfinite(radius)) && limits.size() >= 4) {
        radius = std::max(std::max(std::fabs(limits[0]), std::fabs(limits[1])),
            std::max(std::fabs(limits[2]), std::fabs(limits[3])));
    }
    return radius;
}
//=============================================================================
// Pixels per data unit; DataAspectRatio [1 1 1] makes it the same on both axes.
static double
polarPixelsPerDataUnit(GOAxis* axis, const std::vector<double>& limits)
{
    if (limits.size() < 4) {
        return 0.;
    }
    double xRange = limits[1] - limits[0];
    double yRange = limits[3] - limits[2];
    std::vector<double> box = axisPropertyVectorAsPixels(axis, GO_POSITION_PROPERTY_NAME_STR);
    if (box.size() < 4 || !(xRange > 0) || !(yRange > 0) || !(box[2] > 0) || !(box[3] > 0)) {
        return 0.;
    }
    return std::min(box[2] / xRange, box[3] / yRange);
}
//=============================================================================
static bool
isPolarGridText(GraphicsObject* object)
{
    return object && object->isType(GO_PROPERTY_VALUE_TEXT_STR)
        && object->findStringProperty(GO_TAG_PROPERTY_NAME_STR) == L"__nelson_polar_grid__";
}
//=============================================================================
// Radial labels lie inside the disc, theta labels outside it.
static bool
isPolarThetaLabel(GraphicsObject* object, double radius)
{
    if (!isPolarGridText(object) || !(radius > 0)) {
        return false;
    }
    std::vector<double> position = object->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    return position.size() >= 2 && std::hypot(position[0], position[1]) > radius * (1. + 1e-9);
}
//=============================================================================
// Grid labels use the axes font; theta labels sit a fixed pixel gap outside the disc.
static void
layoutPolarGridLabels(GOAxis* axis)
{
    auto* children = static_cast<GOGObjectsProperty*>(
        axis->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    std::vector<double> xLimits(axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR));
    std::vector<double> yLimits(axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR));
    std::vector<double> limits = { xLimits[0], xLimits[1], yLimits[0], yLimits[1] };
    double radius = polarDiscRadius(axis, limits);
    double scale = polarPixelsPerDataUnit(axis, limits);
    double fontSize = fontSizeInPoints(axis);
    std::wstring fontName = axis->findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    double fontPixels = polarPointsToPixels(fontSize);
    // Gap from the disc to the label anchor, measured on the reference.
    double gapPixels = POLAR_THETA_LABEL_GAP_BASE + POLAR_THETA_LABEL_GAP_EM * fontPixels;
    // Labels near the vertical axis are centred, those near the horizontal one middled.
    const double centreLimit = std::sin(11.25 * M_PI / 180.);
    const double middleLimit = std::sin(22.5 * M_PI / 180.) + 1e-9;
    for (int64 handle : children->data()) {
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (!isPolarGridText(child)) {
            continue;
        }
        GOText* text = static_cast<GOText*>(child);
        if (std::isfinite(fontSize)
            && text->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR) != fontSize) {
            text->setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, fontSize);
            markTextFontChanged(text, GO_FONT_SIZE_PROPERTY_NAME_STR);
        }
        if (!fontName.empty()
            && text->findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR) != fontName) {
            text->setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, fontName);
            markTextFontChanged(text, GO_FONT_NAME_PROPERTY_NAME_STR);
        }
        std::vector<double> position
            = text->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() < 3 || !(scale > 0) || !std::isfinite(gapPixels)
            || !isPolarThetaLabel(text, radius)) {
            continue;
        }
        double angle = std::atan2(position[1], position[0]);
        double c = std::cos(angle);
        double s = std::sin(angle);
        double anchor = radius + gapPixels / scale;
        double x = anchor * c;
        double y = anchor * s;
        if (std::fabs(x - position[0]) > 1e-12 * anchor
            || std::fabs(y - position[1]) > 1e-12 * anchor) {
            text->setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, x, y, position[2]);
        }
        std::wstring horizontal = std::fabs(c) < centreLimit
            ? GO_PROPERTY_VALUE_CENTER_STR
            : (c > 0 ? GO_PROPERTY_VALUE_LEFT_STR : GO_PROPERTY_VALUE_RIGHT_STR);
        std::wstring vertical = std::fabs(s) <= middleLimit
            ? GO_PROPERTY_VALUE_MIDDLE_STR
            : (s > 0 ? GO_PROPERTY_VALUE_BOTTOM_STR : GO_PROPERTY_VALUE_TOP_STR);
        if (!text->stringCheck(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, horizontal)) {
            text->setRestrictedStringDefault(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, horizontal);
        }
        if (!text->stringCheck(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, vertical)) {
            text->setRestrictedStringDefault(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, vertical);
        }
    }
}
//=============================================================================
// Grid lines, ticks and labels that polarRefresh builds for a polar axes.
static bool
isPolarGridObject(GraphicsObject* object)
{
    return object && object->findProperty(GO_TAG_PROPERTY_NAME_STR, false)
        && object->findStringProperty(GO_TAG_PROPERTY_NAME_STR) == L"__nelson_polar_grid__";
}
//=============================================================================
// Handles of polar titles/subtitles that their hidden axes turned off.
static std::unordered_set<int64>&
polarTitlesHiddenByAxes()
{
    static std::unordered_set<int64> handles;
    return handles;
}
//=============================================================================
// Room above a polar box for the top theta label and the title stack, in pixels.
// The title takes 1.29 em, a subtitle 1.385 em more (measured on the reference).
static double
polarTitleTopMargin(GOAxis* axis, double boxWidth, double boxHeight)
{
    auto textPixels = [axis](const std::wstring& name) {
        GOText* text = axisTextObject(axis, name);
        if (!text || !axisTextHasString(axis, name)) {
            return 0.;
        }
        double pixels = polarPointsToPixels(fontSizeInPoints(text));
        return std::isfinite(pixels) ? pixels : 0.;
    };
    double titlePixels = textPixels(GO_TITLE_PROPERTY_NAME_STR);
    double subtitlePixels = textPixels(GO_SUBTITLE_PROPERTY_NAME_STR);
    double stack
        = titlePixels > 0. ? 1.29 * titlePixels + 1.385 * subtitlePixels : 1.29 * subtitlePixels;
    double fontPixels = polarPointsToPixels(fontSizeInPoints(axis));
    if (!std::isfinite(fontPixels)) {
        fontPixels = 0.;
    }
    double labelPixels
        = POLAR_THETA_LABEL_GAP_BASE + POLAR_THETA_LABEL_GAP_EM * fontPixels + 1.14 * fontPixels;
    // A disc limited by the box width leaves free room above it.
    double slack = std::max(0., (boxHeight - std::min(boxWidth, boxHeight)) * 0.5);
    return std::max(0., labelPixels + 0.55 + stack - slack);
}
//=============================================================================
static void
syncAxisLabelColors(GOAxis* axis)
{
    // An axis color always paints its label, even one colored by the user.
    const std::pair<const wchar_t*, const wchar_t*> labels[] = {
        { GO_X_LABEL_PROPERTY_NAME_STR, GO_X_COLOR_PROPERTY_NAME_STR },
        { GO_Y_LABEL_PROPERTY_NAME_STR, GO_Y_COLOR_PROPERTY_NAME_STR },
        { GO_Z_LABEL_PROPERTY_NAME_STR, GO_Z_COLOR_PROPERTY_NAME_STR },
    };
    for (const auto& entry : labels) {
        GOText* text = axisTextObject(axis, entry.first);
        if (!text || !axis->hasChanged(entry.second)) {
            continue;
        }
        std::vector<double> color = axis->findVectorDoubleProperty(entry.second);
        if (color.size() == 3) {
            text->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, color[0], color[1], color[2]);
        }
    }
}
//=============================================================================
static void
markAxisTextFontWritten(GOText* text, const std::wstring& name, const std::wstring& modeName)
{
    markTextFontChanged(text, name);
    if (!modeName.empty()) {
        text->setRestrictedStringDefault(modeName, GO_PROPERTY_VALUE_AUTO_STR);
        markTextFontChanged(text, modeName);
    }
}
//=============================================================================
static void
syncAxisTextFont(GOAxis* axis, GOText* text, const AxisTextFontSync& sync, double fontSize,
    const std::wstring& fontWeight)
{
    bool written = false;
    // The size is written in the text's own font units.
    std::wstring axisUnits = axis->findStringProperty(GO_FONT_UNITS_PROPERTY_NAME_STR);
    std::wstring textUnits = text->findStringProperty(GO_FONT_UNITS_PROPERTY_NAME_STR);
    if (axisUnits != textUnits && (sync.size || sync.autoSize)) {
        fontSize
            = convertFontSizeUnits(fontSize, axisUnits, textUnits, fontUnitsReferenceHeight(axis));
    }
    bool autoSize = sync.autoSize && text->isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)
        && text->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR) != fontSize;
    if (sync.size || autoSize) {
        text->setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, fontSize);
        markAxisTextFontWritten(
            text, GO_FONT_SIZE_PROPERTY_NAME_STR, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
        written = true;
    }
    if (sync.weight || (sync.autoWeight && text->isAuto(GO_FONT_WEIGHT_MODE_PROPERTY_NAME_STR))) {
        text->setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, fontWeight);
        markAxisTextFontWritten(
            text, GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_FONT_WEIGHT_MODE_PROPERTY_NAME_STR);
        written = true;
    }
    if (sync.name) {
        text->setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR,
            axis->findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
        markAxisTextFontWritten(
            text, GO_FONT_NAME_PROPERTY_NAME_STR, GO_FONT_NAME_MODE_PROPERTY_NAME_STR);
        written = true;
    }
    if (sync.angle) {
        text->setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR,
            axis->findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
        markAxisTextFontWritten(
            text, GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_FONT_ANGLE_MODE_PROPERTY_NAME_STR);
        written = true;
    }
    if (written) {
        // Apply it now; the written mode tells the text this is not a user value.
        text->updateState();
    }
}
//=============================================================================
static void
convertAxisFontUnits(GOAxis* axis)
{
    auto* units = static_cast<GOFontUnitsProperty*>(
        axis->findProperty(GO_FONT_UNITS_PROPERTY_NAME_STR, false));
    auto* size = dynamic_cast<GOFontSizeProperty*>(
        axis->findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR, false));
    if (!units || !size) {
        return;
    }
    std::wstring from = units->takePendingFrom();
    if (from.empty()) {
        return;
    }
    std::wstring to = units->data();
    double height = fontUnitsReferenceHeight(axis);
    // A FontSize given after FontUnits in the same set is already in the new units.
    if (size->getSetOrder() < units->getSetOrder()) {
        size->data(convertFontSizeUnits(size->data(), from, to, height));
    }
    bool is3D = !axis->is2D();
    for (const std::wstring& name :
        { GO_TITLE_PROPERTY_NAME_STR, GO_SUBTITLE_PROPERTY_NAME_STR, GO_X_LABEL_PROPERTY_NAME_STR,
            GO_Y_LABEL_PROPERTY_NAME_STR, GO_Z_LABEL_PROPERTY_NAME_STR }) {
        GOText* text = axisTextObject(axis, name);
        if (!text) {
            continue;
        }
        // Ruler labels keep points until the user sets their font units.
        bool rulerLabel = name == GO_X_LABEL_PROPERTY_NAME_STR
            || name == GO_Y_LABEL_PROPERTY_NAME_STR
            || (is3D && name == GO_Z_LABEL_PROPERTY_NAME_STR);
        if (rulerLabel && text->isAuto(GO_FONT_UNITS_MODE_PROPERTY_NAME_STR)) {
            continue;
        }
        std::wstring textUnits = text->findStringProperty(GO_FONT_UNITS_PROPERTY_NAME_STR);
        double textSize = text->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
        text->setScalarDoubleDefault(
            GO_FONT_SIZE_PROPERTY_NAME_STR, convertFontSizeUnits(textSize, textUnits, to, height));
        text->setRestrictedStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, to);
        // Marking the modes too keeps them as they are.
        markTextFontChanged(text, GO_FONT_SIZE_PROPERTY_NAME_STR);
        markTextFontChanged(text, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
        markTextFontChanged(text, GO_FONT_UNITS_PROPERTY_NAME_STR);
        markTextFontChanged(text, GO_FONT_UNITS_MODE_PROPERTY_NAME_STR);
        text->updateState();
    }
}
//=============================================================================

//=============================================================================
static double
autoFontSizeForBox(double baseFontSize, double widthPixels, double heightPixels)
{
    // The height counts as a 1.25x wider box; the smaller side drives the shrink.
    double size = std::min(widthPixels, heightPixels * AUTO_FONT_HEIGHT_TO_WIDTH);
    double ratio = (size - AUTO_FONT_MIN_WIDTH) / (AUTO_FONT_FULL_WIDTH - AUTO_FONT_MIN_WIDTH);
    ratio = std::clamp(ratio, 0.0, 1.0);
    double scale = AUTO_FONT_MIN_SCALE + (1.0 - AUTO_FONT_MIN_SCALE) * ratio;
    // Rounded to the nearest half point.
    return std::round(baseFontSize * scale * 2.0) / 2.0;
}
//=============================================================================
static void
followAutoFontSize(GOAxis* axis, double previousFontSize, double fontSize)
{
    // Title and labels still sized from the previous axes size follow the new one.
    const std::pair<const wchar_t*, const wchar_t*> texts[] = {
        { GO_TITLE_PROPERTY_NAME_STR, GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR },
        { GO_SUBTITLE_PROPERTY_NAME_STR, GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR },
        { GO_X_LABEL_PROPERTY_NAME_STR, GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR },
        { GO_Y_LABEL_PROPERTY_NAME_STR, GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR },
        { GO_Z_LABEL_PROPERTY_NAME_STR, GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR },
    };
    for (const auto& entry : texts) {
        GOText* text = axisTextObject(axis, entry.first);
        if (!text || !text->isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
            continue;
        }
        double multiplier = axis->findScalarDoubleProperty(entry.second);
        double current = text->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
        if (std::fabs(current - previousFontSize * multiplier) > 1e-9) {
            continue;
        }
        text->setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, fontSize * multiplier);
        markAxisTextFontWritten(
            text, GO_FONT_SIZE_PROPERTY_NAME_STR, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
        text->updateState();
    }
}
//=============================================================================
static std::vector<double>
uiAxesViewportPosition(const std::vector<double>& position, double scaleX, double scaleY)
{
    if (position.size() != 4) {
        return position;
    }
    double x = position[0] + UI_AXES_LEFT_INSET * scaleX;
    double y = position[1] + UI_AXES_BOTTOM_INSET * scaleY;
    double width = position[2] - (UI_AXES_LEFT_INSET + UI_AXES_RIGHT_INSET) * scaleX;
    double height = position[3] - (UI_AXES_BOTTOM_INSET + UI_AXES_TOP_INSET) * scaleY;
    if (width <= 1.0 || height <= 1.0) {
        return position;
    }
    return { x, y, width, height };
}
//=============================================================================
bool
GOAxis::applyAutoFontSize()
{
    // uiaxes keep their size whatever the box.
    if (usesUiAxesLayout() || !isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)
        || hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)
        || !stringCheck(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR)) {
        return false;
    }
    std::vector<double> box = getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR);
    if (box.size() < 4 || !std::isfinite(box[2]) || !std::isfinite(box[3])) {
        return false;
    }
    // A subplot grid cell or an outer box counts as the default plot box inside it.
    std::vector<double> grid = findVectorDoubleProperty(GO_SUBPLOT_GRID_INTERNAL_PROPERTY_NAME_STR);
    if (grid.size() == 2 && grid[0] >= 1 && grid[1] >= 1) {
        double parentWidth = 0;
        double parentHeight = 0;
        graphicsParentSizeInPixels(this, getParentFigure(), parentWidth, parentHeight);
        box[2] = DEFAULT_PLOT_BOX_WIDTH * parentWidth / grid[1];
        box[3] = DEFAULT_PLOT_BOX_HEIGHT * parentHeight / grid[0];
    } else if (stringCheck(
                   GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR)) {
        // Room taken by outside legends and colorbars does not count.
        std::vector<double> outer;
        if (axesLayoutBaseOuterPosition(this, outer)) {
            outer = layoutUnitsToPixels(outer);
        } else {
            outer = getPropertyVectorAsPixels(GO_OUTER_POSITION_PROPERTY_NAME_STR);
        }
        if (outer.size() >= 4 && std::isfinite(outer[2]) && std::isfinite(outer[3])) {
            box[2] = DEFAULT_PLOT_BOX_WIDTH * outer[2];
            box[3] = DEFAULT_PLOT_BOX_HEIGHT * outer[3];
        }
    }
    // The auto size starts from the user default (groot) or the factory size.
    double baseFontSize = AXES_FACTORY_FONT_SIZE;
    GORoot* root = getGraphicsRootObject();
    ArrayOf userDefault;
    if (!isPolarAxes() && root && root->getUserDefaultProperty(L"DefaultAxesFontSize", userDefault)
        && userDefault.isNumeric() && userDefault.isScalar()) {
        double value = userDefault.asDoubleScalar();
        if (std::isfinite(value) && value > 0) {
            baseFontSize = value;
        }
    }
    double fontSize = autoFontSizeForBox(baseFontSize, box[2], box[3]);
    double previousFontSize = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    if (fontSize == previousFontSize) {
        return false;
    }
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, fontSize);
    followAutoFontSize(this, previousFontSize, fontSize);
    return true;
}
//=============================================================================
std::vector<double>
GOAxis::getPropertyVectorAsPixels(const std::wstring& name)
{
    GOFourVectorProperty* hp = static_cast<GOFourVectorProperty*>(findProperty(name));
    return (unitsReinterpret(hp->data()));
}
//=============================================================================
// Insets of the outer box on both sides of a plot box, along one direction: the loose
// inset or the tight one when it is larger. A shared loose inset is a share of the outer
// box itself, so the outer size is solved from the box size.
static void
outerInsetsAroundBox(bool shared, double boxSize, double parentSize, double lowShare,
    double highShare, double tightLow, double tightHigh, double& low, double& high)
{
    if (!shared) {
        low = std::max(lowShare * parentSize, tightLow);
        high = std::max(highShare * parentSize, tightHigh);
        return;
    }
    double outerSize = boxSize / (1.0 - lowShare - highShare);
    low = lowShare * outerSize;
    high = highShare * outerSize;
    if (tightLow > low) {
        // the tight inset wins on the low side: the loose share applies to the other one
        outerSize = (boxSize + tightLow) / (1.0 - highShare);
        low = tightLow;
        high = std::max(highShare * outerSize, tightHigh);
    } else if (tightHigh > high) {
        outerSize = (boxSize + tightHigh) / (1.0 - lowShare);
        high = tightHigh;
        low = std::max(lowShare * outerSize, tightLow);
    }
}
//=============================================================================
// The room an outside colorbar takes depends on a 2-D or 3-D view, and a hidden
// figure is never painted: its colorbars follow a view change here.
static void
updatePeerColorBars(GOAxis* axis)
{
    int64 holderHandle = HANDLE_UNASSIGNED_OBJECT;
    GraphicsObject* holder = axesDecorationHolder(axis, holderHandle);
    auto* children = holder ? dynamic_cast<GOGObjectsProperty*>(
                                  holder->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
                            : nullptr;
    if (!children) {
        return;
    }
    const int64 axisHandle = findGraphicsObjectHandle(axis);
    const std::vector<int64> handles(children->data());
    for (int64 handle : handles) {
        auto* colorBar = dynamic_cast<GOColorBar*>(findGraphicsObject(handle, false));
        if (colorBar && colorBar->getPeerAxesHandle() == axisHandle) {
            colorBar->updateState();
        }
    }
}
//=============================================================================

} // namespace Nelson
//=============================================================================
