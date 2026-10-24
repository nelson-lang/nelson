//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebFigureBridgeExports.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include <Eigen/Geometry>
#include <cmath>
//=============================================================================
namespace {
// A pan makes the camera position manual. Orbit that position around its
// translated target instead of updating only View, which manual cameras ignore.
void
setInteractiveView(Nelson::GOAxis* axis, double azimuth, double elevation)
{
    axis->navigationReferenceView();
    if (!axis->isAuto(GO_CAMERA_POSITION_MODE_PROPERTY_NAME_STR)) {
        const auto position = axis->findVectorDoubleProperty(GO_CAMERA_POSITION_PROPERTY_NAME_STR);
        const auto target = axis->findVectorDoubleProperty(GO_CAMERA_TARGET_PROPERTY_NAME_STR);
        const auto aspect = axis->findVectorDoubleProperty(GO_DATA_ASPECT_RATIO_PROPERTY_NAME_STR);
        Eigen::Vector3d scale = Eigen::Vector3d::Ones();
        const std::wstring directions[] = { GO_X_DIR_PROPERTY_NAME_STR, GO_Y_DIR_PROPERTY_NAME_STR,
            GO_Z_DIR_PROPERTY_NAME_STR };
        for (int k = 0; k < 3; ++k) {
            if (aspect.size() == 3 && std::isfinite(aspect[k]) && aspect[k] > 0) {
                scale[k] = aspect[k];
            }
            if (axis->stringCheck(directions[k], GO_PROPERTY_VALUE_REVERSE_STR)) {
                scale[k] = -scale[k];
            }
        }
        Eigen::Vector3d offset;
        for (int k = 0; k < 3; ++k) {
            offset[k] = (position[k] - target[k]) / scale[k];
        }
        double previousAzimuth = 0, previousElevation = 0;
        axis->getView(previousAzimuth, previousElevation);
        const double radians = std::acos(-1.0) / 180.0;
        const double actualAzimuth = std::atan2(offset[0], -offset[1]);
        const Eigen::Quaterniond rotation
            = Eigen::AngleAxisd(
                  actualAzimuth + (azimuth - previousAzimuth) * radians, Eigen::Vector3d::UnitZ())
            * Eigen::AngleAxisd(
                -(elevation - previousElevation) * radians, Eigen::Vector3d::UnitX())
            * Eigen::AngleAxisd(-actualAzimuth, Eigen::Vector3d::UnitZ());
        offset = (rotation * offset).cwiseProduct(scale);
        axis->setThreeVectorDefault(GO_CAMERA_POSITION_PROPERTY_NAME_STR, target[0] + offset[0],
            target[1] + offset[1], target[2] + offset[2]);
        if (!axis->isAuto(GO_CAMERA_UP_VECTOR_MODE_PROPERTY_NAME_STR)) {
            const auto up = axis->findVectorDoubleProperty(GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR);
            const Eigen::Vector3d rotatedUp
                = (rotation * Eigen::Vector3d(up[0] / scale[0], up[1] / scale[1], up[2] / scale[2]))
                      .cwiseProduct(scale);
            axis->setThreeVectorDefault(
                GO_CAMERA_UP_VECTOR_PROPERTY_NAME_STR, rotatedUp[0], rotatedUp[1], rotatedUp[2]);
        }
    }
    axis->setView(azimuth, elevation);
    axis->updateViewState();
}
//=============================================================================
} // namespace
//=============================================================================
extern "C"
{
    // Push an interactive rotation back onto the figure's axes so the graphics
    // object stays in sync: the web client sends azimuth/elevation (degrees)
    // during a drag, and this sets View and recomputes the camera. Returns false if the figure
    // or an axes could not be found.
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiSetAxesView(int64_t handle, int64_t axesHandle, double azimuth, double elevation)
    {
        Nelson::GOFigureHost* win = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (win == nullptr) {
            return false;
        }
        Nelson::GOFigure* fig = win->hostFigure();
        if (fig == nullptr) {
            return false;
        }
        if (axesHandle > 0) {
            auto* axis = dynamic_cast<Nelson::GOAxis*>(
                Nelson::findGraphicsObject(static_cast<Nelson::int64>(axesHandle), false));
            if (axis == nullptr || axis->getParentFigure() != fig) {
                return false;
            }
            setInteractiveView(axis, azimuth, elevation);
            return true;
        }
        auto* childProp = dynamic_cast<Nelson::GOGObjectsProperty*>(
            fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (childProp == nullptr) {
            return false;
        }
        bool done = false;
        for (Nelson::int64 h : childProp->data()) {
            // Zero keeps the legacy whole-figure behavior.
            auto* ax = dynamic_cast<Nelson::GOAxis*>(Nelson::findGraphicsObject(h, false));
            if (ax != nullptr && ax->isType(GO_PROPERTY_VALUE_AXES_STR)) {
                setInteractiveView(ax, azimuth, elevation);
                done = true;
            }
        }
        return done;
    }
    //=============================================================================
}
//=============================================================================
