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

//=============================================================================
// The auto up-vector is derived from View (azimuth/elevation) only. Setting CameraPosition manually
// does not update View, so a camera posed straight along an axis (e.g. CameraPosition [0 0 40],
// target at the origin) can leave the candidate up collinear with the viewing direction. That gives
// a degenerate camera basis, so the up is replaced in place by a world axis that is not aligned
// with the view direction.
//
// The axes are tried in z, y, x order: z stays up unless the look is straight along z, which falls
// back to y. The y fallback is oriented by the look direction (forward_z < 0 for a camera above the
// target), so a top view reports [0 1 0] like view(2) and a bottom view [0 -1 0] like view(0, -90).
static void
resolveDegenerateAutoUpVector(std::vector<double>& cameraUp,
    const std::vector<double>& cameraCenter, const std::vector<double>& cameraPosition)
{
    double forward[3] = { cameraCenter[0] - cameraPosition[0], cameraCenter[1] - cameraPosition[1],
        cameraCenter[2] - cameraPosition[2] };
    double forwardNorm
        = std::sqrt(forward[0] * forward[0] + forward[1] * forward[1] + forward[2] * forward[2]);
    if (forwardNorm <= 0) {
        return;
    }
    forward[0] /= forwardNorm;
    forward[1] /= forwardNorm;
    forward[2] /= forwardNorm;
    double upNorm = std::sqrt(
        cameraUp[0] * cameraUp[0] + cameraUp[1] * cameraUp[1] + cameraUp[2] * cameraUp[2]);
    double cosAngle = (upNorm > 0)
        ? (cameraUp[0] * forward[0] + cameraUp[1] * forward[1] + cameraUp[2] * forward[2]) / upNorm
        : 0.0;
    if (upNorm > 0 && std::abs(cosAngle) <= 0.9999) {
        return;
    }
    static const double candidateAxes[3][3]
        = { { 0.0, 0.0, 1.0 }, { 0.0, 1.0, 0.0 }, { 1.0, 0.0, 0.0 } };
    for (const auto& axis : candidateAxes) {
        double dot = axis[0] * forward[0] + axis[1] * forward[1] + axis[2] * forward[2];
        if (std::abs(dot) <= 0.9999) {
            cameraUp[0] = axis[0];
            cameraUp[1] = axis[1];
            cameraUp[2] = axis[2];
            // Orient the y fallback by the look direction: a camera below the
            // target (forward_z > 0) reports [0 -1 0], matching view(0, -90).
            if (axis[1] != 0.0 && forward[2] > 0.0) {
                cameraUp[1] = -cameraUp[1];
            }
            return;
        }
    }
}

static double
norm3(const std::vector<double>& value)
{
    return std::sqrt(value[0] * value[0] + value[1] * value[1] + value[2] * value[2]);
}

static double
dot3(const std::vector<double>& a, const std::vector<double>& b)
{
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

static std::vector<double>
cross3(const std::vector<double>& a, const std::vector<double>& b)
{
    return { a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0] };
}

static bool
normalize3(std::vector<double>& value)
{
    double n = norm3(value);
    if (!std::isfinite(n) || n <= 0) {
        return false;
    }
    value[0] /= n;
    value[1] /= n;
    value[2] /= n;
    return true;
}

static void
expandProjectionRange(double& amin, double& amax)
{
    double range = amax - amin;
    if (!std::isfinite(range) || range <= 0) {
        return;
    }
    rerange(amin, amax, range * (1.0 + PROJECTION_CLIP_MARGIN));
}

static void
expandDepthProjectionRange(double& amin, double& amax)
{
    double range = amax - amin;
    if (!std::isfinite(range) || range <= 0) {
        return;
    }
    rerange(amin, amax, range * DEPTH_PROJECTION_CLIP_EXPANSION);
}

static double
computeAutoCameraViewAngle(const std::vector<double>& limits,
    const std::vector<double>& cameraPosition, const std::vector<double>& cameraTarget,
    const std::vector<double>& cameraUp, const std::vector<double>& dataAspectRatio,
    const std::vector<double>& axisPosition)
{
    std::vector<double> dar = dataAspectRatio;
    if (dar.size() < 3 || !std::isfinite(dar[0]) || !std::isfinite(dar[1]) || !std::isfinite(dar[2])
        || dar[0] <= 0 || dar[1] <= 0 || dar[2] <= 0) {
        dar = { 1.0, 1.0, 1.0 };
    }
    std::vector<double> scaledLimits = { limits[0] / dar[0], limits[1] / dar[0], limits[2] / dar[1],
        limits[3] / dar[1], limits[4] / dar[2], limits[5] / dar[2] };
    std::vector<double> scaledCameraPosition
        = { cameraPosition[0] / dar[0], cameraPosition[1] / dar[1], cameraPosition[2] / dar[2] };
    std::vector<double> scaledCameraTarget
        = { cameraTarget[0] / dar[0], cameraTarget[1] / dar[1], cameraTarget[2] / dar[2] };
    std::vector<double> scaledCameraUp
        = { cameraUp[0] / dar[0], cameraUp[1] / dar[1], cameraUp[2] / dar[2] };

    std::vector<double> forward = { scaledCameraTarget[0] - scaledCameraPosition[0],
        scaledCameraTarget[1] - scaledCameraPosition[1],
        scaledCameraTarget[2] - scaledCameraPosition[2] };
    double distance = norm3(forward);
    if (!std::isfinite(distance) || distance <= 0 || !normalize3(forward)) {
        return DEFAULT_CAMERA_VIEW_ANGLE;
    }
    std::vector<double> right = cross3(forward, scaledCameraUp);
    if (!normalize3(right)) {
        return DEFAULT_CAMERA_VIEW_ANGLE;
    }
    std::vector<double> up = cross3(right, forward);
    if (!normalize3(up)) {
        return DEFAULT_CAMERA_VIEW_ANGLE;
    }

    double xMin = std::numeric_limits<double>::infinity();
    double xMax = -std::numeric_limits<double>::infinity();
    double yMin = std::numeric_limits<double>::infinity();
    double yMax = -std::numeric_limits<double>::infinity();
    for (int ix = 0; ix < 2; ++ix) {
        for (int iy = 0; iy < 2; ++iy) {
            for (int iz = 0; iz < 2; ++iz) {
                std::vector<double> point = { ix == 0 ? scaledLimits[0] : scaledLimits[1],
                    iy == 0 ? scaledLimits[2] : scaledLimits[3],
                    iz == 0 ? scaledLimits[4] : scaledLimits[5] };
                std::vector<double> relative = { point[0] - scaledCameraPosition[0],
                    point[1] - scaledCameraPosition[1], point[2] - scaledCameraPosition[2] };
                double x = dot3(relative, right);
                double y = dot3(relative, up);
                xMin = std::min(xMin, x);
                xMax = std::max(xMax, x);
                yMin = std::min(yMin, y);
                yMax = std::max(yMax, y);
            }
        }
    }
    double xSpan = xMax - xMin;
    double ySpan = yMax - yMin;
    double width = 1.0;
    double height = 1.0;
    if (axisPosition.size() >= 4 && std::isfinite(axisPosition[2]) && std::isfinite(axisPosition[3])
        && axisPosition[2] > 0 && axisPosition[3] > 0) {
        width = axisPosition[2];
        height = axisPosition[3];
    }
    double span = GOAxis::viewAngleSpansWidth(xSpan, ySpan, width, height) ? xSpan : ySpan;
    return GOAxis::cameraViewAngleForSpan(span, distance, DEFAULT_CAMERA_VIEW_ANGLE);
}

void
GOAxis::setupProjection(RenderInterface& gc, bool updateLimits)
{
    GOThreeVectorProperty* cameraPositionVector
        = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR));
    GOThreeVectorProperty* cameraTargetVector
        = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR));
    GOThreeVectorProperty* cameraUpVector
        = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR));

    std::vector<double> dar(findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR));
    if (dar.size() < 3 || !std::isfinite(dar[0]) || !std::isfinite(dar[1]) || !std::isfinite(dar[2])
        || dar[0] <= 0 || dar[1] <= 0 || dar[2] <= 0) {
        dar = { 1.0, 1.0, 1.0 };
    }
    std::vector<double> scale = { 1.0 / dar[0], 1.0 / dar[1], 1.0 / dar[2] };

    if (renderedAs2D()) {
        gc.lookAt(cameraPositionVector->data()[0] * scale[0],
            cameraPositionVector->data()[1] * scale[1], cameraPositionVector->data()[2] * scale[2],
            cameraTargetVector->data()[0] * scale[0], cameraTargetVector->data()[1] * scale[1],
            cameraTargetVector->data()[2] * scale[2], cameraUpVector->data()[0] * scale[0],
            cameraUpVector->data()[1] * scale[1], cameraUpVector->data()[2] * scale[2]);
    } else {
        std::vector<double> cameraPosition = cameraPositionVector->data();
        std::vector<double> cameraTarget = cameraTargetVector->data();
        std::vector<double> cameraUp = cameraUpVector->data();
        cameraPosition = mapCamera(cameraPosition);
        cameraTarget = mapCamera(cameraTarget);
        if (stringCheck(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            cameraUp[0] = -cameraUp[0];
        }
        if (stringCheck(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            cameraUp[1] = -cameraUp[1];
        }
        if (stringCheck(GO_Z_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            cameraUp[2] = -cameraUp[2];
        }

        gc.lookAt(cameraPosition[0] * scale[0], cameraPosition[1] * scale[1],
            cameraPosition[2] * scale[2], cameraTarget[0] * scale[0], cameraTarget[1] * scale[1],
            cameraTarget[2] * scale[2], cameraUp[0] * scale[0], cameraUp[1] * scale[1],
            cameraUp[2] * scale[2]);
    }

    gc.scale(scale[0], scale[1], scale[2]);
    std::vector<double> limits(getAxisLimits());
    double xvals[8];
    double yvals[8];
    double zValues[8];
    gc.mapPoint(limits[0], limits[2], limits[4], xvals[0], yvals[0], zValues[0]);
    gc.mapPoint(limits[0], limits[2], limits[5], xvals[1], yvals[1], zValues[1]);
    gc.mapPoint(limits[0], limits[3], limits[4], xvals[2], yvals[2], zValues[2]);
    gc.mapPoint(limits[0], limits[3], limits[5], xvals[3], yvals[3], zValues[3]);
    gc.mapPoint(limits[1], limits[2], limits[4], xvals[4], yvals[4], zValues[4]);
    gc.mapPoint(limits[1], limits[2], limits[5], xvals[5], yvals[5], zValues[5]);
    gc.mapPoint(limits[1], limits[3], limits[4], xvals[6], yvals[6], zValues[6]);
    gc.mapPoint(limits[1], limits[3], limits[5], xvals[7], yvals[7], zValues[7]);
    double xmin, xmax, ymin, ymax, zmin, zmax;
    minMaxVector(xvals, 8, xmin, xmax);
    minMaxVector(yvals, 8, ymin, ymax);
    minMaxVector(zValues, 8, zmin, zmax);
    if (zmin == zmax) {
        zmin = zmax - 1;
        zmax = zmax + 1;
    }
    // Depth span of the real data box along the view axis, captured before the
    // aspect-ratio rerange below rewrites zmin/zmax to fit the plot box. The
    // perspective near/far planes must bound the actual geometry: the reranged
    // span can fall short of it (clipping away the far half of a near object),
    // so both projections widen their depth range to it.
    double zMappedMinRaw = zmin;
    double zMappedMaxRaw = zmax;
    std::vector<double> position(getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR));
    // Which viewport dimension the data box fills, decided on the unpadded box: the camera view
    // angle spans that extent (see cameraViewAngleForSpan).
    bool angleSpansWidth = viewAngleSpansWidth(xmax - xmin, ymax - ymin, position[2], position[3]);
    bool manualAspect
        = stringCheck(GO_PLOT_BOX_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
        || stringCheck(GO_DATA_ASPECT_RATIO_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    // A manually set CameraViewAngle also makes the view isotropic (equal x and y screen scale,
    // square pixels), so an image drawn under camva renders round instead of stretched to the axes
    // box.
    bool manualViewAngle
        = stringCheck(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    if (manualAspect || manualViewAngle) {
        double xratio = (xmax - xmin) / position[2];
        double yratio = (ymax - ymin) / position[3];
        double maxratio = std::max(xratio, yratio);
        rerange(xmin, xmax, maxratio * position[2]);
        rerange(ymin, ymax, maxratio * position[3]);
        rerange(zmin, zmax, maxratio * position[3]);
        if (updateLimits && manualAspect) {
            setAxisLimits(limits);
        }
    }
    if (!renderedAs2D()
        && stringCheck(GO_PROJECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PERSPECTIVE_STR)
        && setupPerspectiveProjection(
            gc, zMappedMinRaw, zMappedMaxRaw, angleSpansWidth, position)) {
        return;
    }
    if (!renderedAs2D()) {
        expandProjectionRange(xmin, xmax);
        expandProjectionRange(ymin, ymax);
    }
    // The depth range must still hold the whole data box: the aspect rerange above may
    // shrink it, cutting a 2D patch drawn at ZLim(2) out of the view.
    zmin = std::min(zmin, zMappedMinRaw);
    zmax = std::max(zmax, zMappedMaxRaw);
    expandDepthProjectionRange(zmin, zmax);
    setupOrthographicProjection(gc, scale, position, { xmin, xmax, ymin, ymax, zmin, zmax },
        DEFAULT_CAMERA_VIEW_ANGLE, angleSpansWidth);
}

void
GOAxis::updateCamera()
{
    GOThreeVectorProperty* plotBoxAspectRatio = static_cast<GOThreeVectorProperty*>(
        findProperty(GO_PLOT_BOX_ASPECT_RATIO_PROPERTY_NAME_STR));

    double azimuth;
    double elevation;
    getNormalizedView(azimuth, elevation);

    double azimuthRadians = azimuth * M_PI / 180.;
    double elevationRadians = elevation * M_PI / 180.;
    std::vector<double> limits(getAxisLimits());

    if (hasChanged(GO_CAMERA_TARGET_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR);
    }

    std::vector<double> cameraCenter(3);
    if (isAuto(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR)) {
        cameraCenter[0] = (limits[0] + limits[1]) / 2.0;
        cameraCenter[1] = (limits[2] + limits[3]) / 2.0;
        cameraCenter[2] = (limits[4] + limits[5]) / 2.0;

        GOThreeVectorProperty* cameraTargetProperty
            = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR));
        cameraTargetProperty->value(cameraCenter[0], cameraCenter[1], cameraCenter[2]);
    } else {
        GOThreeVectorProperty* cameraTargetProperty
            = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR));
        cameraCenter = cameraTargetProperty->data();
    }

    if (hasChanged(GO_CAMERA_POSITION_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR)) {
        std::vector<double> cameraPosition(3);
        double xmax = std::max(
            std::abs(limits[0] - cameraCenter[0]), std::abs(limits[1] - cameraCenter[0]));
        double ymax = std::max(
            std::abs(limits[2] - cameraCenter[1]), std::abs(limits[3] - cameraCenter[1]));
        double zmax = std::max(
            std::abs(limits[4] - cameraCenter[2]), std::abs(limits[5] - cameraCenter[2]));
        std::vector<double> dar(findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR));
        if (dar.size() < 3 || !std::isfinite(dar[0]) || !std::isfinite(dar[1])
            || !std::isfinite(dar[2]) || dar[0] <= 0 || dar[1] <= 0 || dar[2] <= 0) {
            dar = { 1.0, 1.0, 1.0 };
        }
        // Camera distance: ten times the half diagonal of the data box in aspect-scaled units.
        // The straight top and bottom views use the same distance (a cube for an automatic
        // aspect, the real box for a manual one) with exactly zero lateral offsets.
        double r = 10.0
            * std::sqrt(std::pow(xmax / dar[0], 2) + std::pow(ymax / dar[1], 2)
                + std::pow(zmax / dar[2], 2));
        if (azimuth == 0. && (elevation == 90. || elevation == -90.)) {
            cameraPosition[0] = (limits[0] + limits[1]) / 2.0;
            cameraPosition[1] = (limits[2] + limits[3]) / 2.0;
            cameraPosition[2] = cameraCenter[2] + ((elevation > 0) ? 1.0 : -1.0) * r * dar[2];
        } else {
            cameraPosition[0] = std::cos(elevationRadians) * std::sin(azimuthRadians) * r * dar[0]
                + cameraCenter[0];
            cameraPosition[1] = -std::cos(elevationRadians) * std::cos(azimuthRadians) * r * dar[1]
                + cameraCenter[1];
            cameraPosition[2] = std::sin(elevationRadians) * r * dar[2] + cameraCenter[2];
        }
        if (stringCheck(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            cameraPosition[0] = 2 * cameraCenter[0] - cameraPosition[0];
        }
        if (stringCheck(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            cameraPosition[1] = 2 * cameraCenter[1] - cameraPosition[1];
        }
        if (stringCheck(GO_Z_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            cameraPosition[2] = 2 * cameraCenter[2] - cameraPosition[2];
        }

        GOThreeVectorProperty* cameraPositionProperty = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR));
        cameraPositionProperty->value(cameraPosition[0], cameraPosition[1], cameraPosition[2]);
    }

    if (hasChanged(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR);
    }
    if (isAuto(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR)) {
        GOThreeVectorProperty* cameraUpProperty = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR));
        std::vector<double> cameraUp(3);

        if (elevation == 90 || elevation == -90) {
            if (azimuth == 0. || (abs(elevationRadians - M_PI / 2) < .001)) {
                cameraUp[0] = 0;
                cameraUp[1] = (elevation > 0) ? 1 : -1;
                cameraUp[2] = 0;
            } else {
                cameraUp[0] = ((elevation > 0) ? -1 : 1) * sin(azimuth * M_PI / 180.0)
                    * (limits[1] - limits[0]) / plotBoxAspectRatio->data()[0];
                cameraUp[1] = ((elevation > 0) ? 1 : -1) * cos(azimuth * M_PI / 180.0)
                    * (limits[3] - limits[2]) / plotBoxAspectRatio->data()[1];
            }
        } else {
            cameraUp[0] = 0;
            cameraUp[1] = 0;
            cameraUp[2] = 1;
        }

        GOThreeVectorProperty* cameraPositionProperty = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR));
        resolveDegenerateAutoUpVector(cameraUp, cameraCenter, cameraPositionProperty->data());

        cameraUpProperty->value(cameraUp[0], cameraUp[1], cameraUp[2]);
    }
    if (hasChanged(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR)) {
        toManual(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR);
    }
    // A manual view angle (set by hand or frozen by 'axis vis3d') is kept as given,
    // whatever the limits do afterwards.
    if (isAuto(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR)) {
        GOThreeVectorProperty* cameraPositionProperty = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR));
        GOThreeVectorProperty* cameraTargetProperty
            = static_cast<GOThreeVectorProperty*>(findProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR));
        GOThreeVectorProperty* cameraUpProperty = static_cast<GOThreeVectorProperty*>(
            findProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR));
        GOScalarProperty* cameraViewAngleProperty
            = static_cast<GOScalarProperty*>(findProperty(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR));
        std::vector<double> axisPosition(getPropertyVectorAsPixels(GO_POSITION_PROPERTY_NAME_STR));
        cameraViewAngleProperty->data(computeAutoCameraViewAngle(limits,
            cameraPositionProperty->data(), cameraTargetProperty->data(), cameraUpProperty->data(),
            findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR), axisPosition));
        clearChanged(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
