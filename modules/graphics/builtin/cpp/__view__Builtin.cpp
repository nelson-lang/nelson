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
#include <chrono>
#include <cmath>
#include <string>
#include <unordered_map>
#include "__view__Builtin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "WebDesktopBridge.hpp"
#include "WebFigureAnimation.hpp"
#include "WebFigureBridgeExports.hpp"
#include "WebFigureFramePush.hpp"
#include "WebFigureRenderMutex.hpp"
#include "axesBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
pushLimitedWebViewFrame(int64_t handle)
{
    // Same shared per-figure throttle as drawNow and the headless repaint
    // path, so a view() loop interleaved with drawnow never double-renders.
    constexpr long long minimumFrameIntervalUs = 16000;
    if (webFigureFramePushedWithin(handle, minimumFrameIntervalUs)) {
        return;
    }
    WebDesktopBridge* bridge = WebDesktopBridge::getInstance();
    if (pushWebFigureAnimationFrame(bridge, handle)) {
        noteWebFigureFramePushed(handle);
    }
}
//=============================================================================
static void
xyzToAzimuthElevation(double x, double y, double z, double& azimuth, double& elevation)
{
    double hypotxy = hypot(x, y);
    elevation = atan2(z, hypotxy);
    azimuth = atan2(y, x);
    azimuth *= (180.0 / M_PI);
    if (azimuth != 0.) {
        azimuth += 90;
    }
    elevation *= (180.0 / M_PI);
}
//=============================================================================
ArrayOfVector
__view__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    // view(2),
    // view(3)
    // view([0 90]),
    // view([0 90 0])
    // view(az, el)
    // view(ax, ...)
    ArrayOfVector retval;

    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 2);

    double azimuth = std::nan("");
    double elevation = std::nan("");

    if (!argIn[0].isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectS",
            L"Expected graphics object(s).");
    }
    int64 handle = argIn[0].asGraphicsObjectScalar();

    bool isAxis = false;
    GraphicsObject* fp = nullptr;
    if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
        if (fp->getType() == GO_PROPERTY_VALUE_AXES_STR) {
            isAxis = true;
        }
    }
    if (!isAxis) {
        Error(L"Nelson:graphics:expectedAxes", L"Axes graphic object expected.");
    }

    switch (argIn.size()) {
    case 2: {
        if (argIn[1].isScalar()) {
            indexType dim = argIn[1].asScalarIndex(false, true, false);
            if (dim == 2) {
                azimuth = 0;
                elevation = 90;
            } else if (dim == 3) {
                azimuth = -37.5;
                elevation = 30;
            } else {
                Error(L"Nelson:view:invalidScalar", L"Single scalar argument must be 2 or 3.");
            }
        } else if (argIn[1].isVector()) {
            double* dp = (double*)argIn[1].dataPointer();
            if (argIn[1].elementCount() == 2) {
                azimuth = dp[0];
                elevation = dp[1];
            } else if (argIn[1].elementCount() == 3) {
                xyzToAzimuthElevation(dp[0], dp[1], dp[2], azimuth, elevation);
            } else {
                Error(L"Nelson:view:invalidVector", L"Vector [az, el] or [x, y, z] expected.");
            }
        }
    } break;
    case 3: {
        azimuth = argIn[1].asDoubleScalar();
        elevation = argIn[2].asDoubleScalar();
    } break;
    case 4: {
        double x = argIn[1].asDoubleScalar();
        double y = argIn[2].asDoubleScalar();
        double z = argIn[3].asDoubleScalar();

        xyzToAzimuthElevation(x, y, z, azimuth, elevation);
    } break;
    default: {
    } break;
    }

    GOAxis* axis = (GOAxis*)fp;
    if (argIn.size() >= 2) {
        WebDesktopBridge* bridge = WebDesktopBridge::getInstance();
        if (bridge->hasWebFigureFrameHook()) {
            const std::lock_guard<std::recursive_mutex> lock(webFigureRenderMutex());
            axis->setView(azimuth, elevation);
            axis->updateViewState();
            GOFigure* figure = axis->getParentFigure();
            if (figure != nullptr
                && figure->stringCheck(
                    GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
                int64_t figureHandle = findGraphicsObjectHandle(figure);
                nlsWebGuiMarkFigureViewOnlyFrame(figureHandle);
                pushLimitedWebViewFrame(figureHandle);
            }
        } else {
            axis->setView(azimuth, elevation);
            bool repaintRequested = axis->updateViewState();
            GOFigure* figure = axis->getParentFigure();
            if (figure != nullptr
                && figure->stringCheck(GO_DRAW_LATER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)
                && !repaintRequested) {
                figure->repaint();
            }
        }
    }

    axis->getView(azimuth, elevation);
    retval << ArrayOf::doubleScalar(azimuth);
    retval << ArrayOf::doubleScalar(elevation);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
