//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include "GOAxis.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "RenderInterface.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
void
GOAxis::setupOrthographicProjection(RenderInterface& gc, const std::vector<double>& scale,
    const std::vector<double>& position, std::vector<double> bounds, double defaultViewAngle,
    bool angleSpansWidth)
{
    double& xmin = bounds[0];
    double& xmax = bounds[1];
    double& ymin = bounds[2];
    double& ymax = bounds[3];
    const double zmin = bounds[4], zmax = bounds[5];
    const auto cameraPosition = findVectorDoubleProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR);
    const auto cameraTarget = findVectorDoubleProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR);
    // A manual target defines the screen center. Fitting the translated data
    // bounds around their own center would cancel a lateral camera pan.
    if (!isAuto(GO_CAMERA_TARGET_MODE_PROPERTY_NAME_STR)) {
        xmax = (xmax - xmin) * 0.5;
        xmin = -xmax;
        ymax = (ymax - ymin) * 0.5;
        ymin = -ymax;
    }
    // Orthographic camera-view-angle zoom: a manual CameraViewAngle scales the visible extent
    // relative to the data-fitting auto angle (a smaller angle zooms in, e.g. camva(4) on an
    // image). The perspective path returned above. Auto mode (the default) leaves the box intact.
    if (stringCheck(GO_CAMERA_VIEW_ANGLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        double manualAngle = findScalarDoubleProperty(GO_CAMERA_VIEW_ANGLE_PROPERTY_NAME_STR);
        // Baseline auto angle from the box about to be projected (after the aspect-ratio rerange
        // and the 3D range padding), so a manual angle equal to it leaves the view untouched (zoom
        // == 1). The raw data span would not match the reranged box of an 'axis equal' image.
        double distanceX = (cameraPosition[0] - cameraTarget[0]) * scale[0];
        double distanceY = (cameraPosition[1] - cameraTarget[1]) * scale[1];
        double distanceZ = (cameraPosition[2] - cameraTarget[2]) * scale[2];
        double cameraDistance
            = std::sqrt(distanceX * distanceX + distanceY * distanceY + distanceZ * distanceZ);
        double boxSpan = angleSpansWidth ? (xmax - xmin) : (ymax - ymin);
        double autoAngle = cameraViewAngleForSpan(boxSpan, cameraDistance, defaultViewAngle);
        if (std::isfinite(manualAngle) && manualAngle > 0 && manualAngle < 180
            && std::isfinite(autoAngle) && autoAngle > 0 && autoAngle < 180) {
            double zoom = std::tan(manualAngle * M_PI / 360.0) / std::tan(autoAngle * M_PI / 360.0);
            if (std::isfinite(zoom) && zoom > 0) {
                double xCenter = 0.5 * (xmin + xmax);
                double yCenter = 0.5 * (ymin + ymax);
                double xHalf = 0.5 * (xmax - xmin) * zoom;
                double yHalf = 0.5 * (ymax - ymin) * zoom;
                xmin = xCenter - xHalf;
                xmax = xCenter + xHalf;
                ymin = yCenter - yHalf;
                ymax = yCenter + yHalf;
            }
        }
    }
    gc.project(xmin, xmax, ymin, ymax, -zmax, -zmin);
    gc.viewport(position[0], position[1], position[2], position[3]);
    if (stringCheck(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        gc.clipToEntireFrame();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
