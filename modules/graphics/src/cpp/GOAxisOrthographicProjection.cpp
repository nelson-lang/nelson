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
#include <limits>
#include <vector>
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
    finishProjectionViewport(gc, position, xmin, xmax, ymin, ymax, -zmax, -zmin, false);
}
//=============================================================================
// Shifts and stretches a projection range so the same data lands on the same pixels
// once the viewport moves from [p0, p0 + pw] to [v0, v0 + vw].
static void
remapProjectionRange(double& lo, double& hi, double p0, double pw, double v0, double vw)
{
    const double span = hi - lo;
    lo = lo + (v0 - p0) * span / pw;
    hi = lo + span * vw / pw;
}
//=============================================================================
void
GOAxis::finishProjectionViewport(RenderInterface& gc, const std::vector<double>& position,
    double left, double right, double bottom, double top, double nearDist, double farDist,
    bool perspective)
{
    auto install = [&]() {
        if (perspective) {
            gc.projectPerspective(left, right, bottom, top, nearDist, farDist);
        } else {
            gc.project(left, right, bottom, top, nearDist, farDist);
        }
    };
    install();
    gc.viewport(position[0], position[1], position[2], position[3]);
    // Pixel box of the data limits under this projection: it exceeds the axes rectangle
    // when a manual camera angle zooms in, and the reference still draws that overflow.
    std::vector<double> limits(getAxisLimits());
    double bx0 = std::numeric_limits<double>::infinity();
    double by0 = bx0;
    double bx1 = -bx0;
    double by1 = -bx0;
    for (int corner = 0; corner < 8 && limits.size() >= 6; ++corner) {
        double px = 0;
        double py = 0;
        double depth = 0;
        bool clipped = false;
        gc.toPixels(limits[(corner & 1) ? 1 : 0], limits[(corner & 2) ? 3 : 2],
            limits[(corner & 4) ? 5 : 4], px, py, depth, clipped);
        // clipped also flags corners outside the viewport, the ones that matter here;
        // only a corner behind the camera (depth sentinel 2) has no pixel position.
        if (depth > 1.5 || !std::isfinite(px) || !std::isfinite(py)) {
            continue;
        }
        bx0 = std::min(bx0, px);
        bx1 = std::max(bx1, px);
        by0 = std::min(by0, py);
        by1 = std::max(by1, py);
    }
    const double x0 = position[0];
    const double y0 = position[1];
    const double x1 = position[0] + position[2];
    const double y1 = position[1] + position[3];
    const bool overflow = std::isfinite(bx0) && std::isfinite(by0) && position[2] > 0
        && position[3] > 0
        && (bx0 < x0 - 0.5 || by0 < y0 - 0.5 || bx1 > x1 + 0.5 || by1 > y1 + 0.5);
    if (overflow) {
        // The viewport grows to the overflow kept inside the frame, never below the axes box.
        const double vx0 = std::min(x0, std::max(0.0, bx0));
        const double vy0 = std::min(y0, std::max(0.0, by0));
        const double vx1 = std::max(x1, std::min(gc.width(), bx1));
        const double vy1 = std::max(y1, std::min(gc.height(), by1));
        remapProjectionRange(left, right, x0, position[2], vx0, vx1 - vx0);
        remapProjectionRange(bottom, top, y0, position[3], vy0, vy1 - vy0);
        install();
        gc.viewport(vx0, vy0, vx1 - vx0, vy1 - vy0);
    }
    if (stringCheck(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        gc.clipToEntireFrame();
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
