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
#include <cmath>
#include "__view__Builtin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GraphicsObject.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOAxis.hpp"
#include "GOFiguresManager.hpp"
#include "axesBuiltin.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static void
xyzToAzimuthElevation(double x, double y, double z, double& azimuth, double& elevation)
{
    double hypotxy = hypot(x, y);
    // Calculate elevation angle
    elevation = atan2(z, hypotxy);
    // Calculate azimuth angle
    azimuth = atan2(y, x);
    // Convert angles from radians to degrees
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

    double azimuth = std::nan("NaN");
    double elevation = std::nan("NaN");

    if (!argIn[0].isGraphicsObject()) {
        Error(_W("Expected graphics object(s)."));
    }
    int64 handle = argIn[0].getContentAsGraphicsObjectScalar();

    bool isAxis = false;
    GraphicsObject* fp = nullptr;
    if (handle >= HANDLE_OFFSET_OBJECT) {
        fp = findGraphicsObject(handle);
        if (fp->getType() == GO_PROPERTY_VALUE_AXES_STR) {
            isAxis = true;
        }
    }
    if (!isAxis) {
        Error(_W("Axes graphic object expected."));
    }

    switch (argIn.size()) {
    case 2: {
        if (argIn[1].isScalar()) {
            indexType dim = argIn[1].getContentAsScalarIndex(false, true, false);
            if (dim == 2) {
                azimuth = 0;
                elevation = 90;
            } else if (dim == 3) {
                azimuth = -37.5;
                elevation = 30;
            } else {
                Error(_W("Single scalar argument must be 2 or 3."));
            }
        } else if (argIn[1].isVector()) {
            double* dp = (double*)argIn[1].getDataPointer();
            if (argIn[1].getElementCount() == 2) {
                azimuth = dp[0];
                elevation = dp[1];
            } else if (argIn[1].getElementCount() == 3) {
                xyzToAzimuthElevation(dp[0], dp[1], dp[2], azimuth, elevation);
            } else {
                Error(_W("Vector [az, el] or [x, y, z] expected."));
            }
        }
    } break;
    case 3: {
        azimuth = argIn[1].getContentAsDoubleScalar();
        elevation = argIn[2].getContentAsDoubleScalar();
    } break;
    case 4: {
        double x = argIn[1].getContentAsDoubleScalar();
        double y = argIn[2].getContentAsDoubleScalar();
        double z = argIn[3].getContentAsDoubleScalar();

        xyzToAzimuthElevation(x, y, z, azimuth, elevation);
    } break;
    default: {
    } break;
    }

    GOAxis* axis = (GOAxis*)fp;
    if (argIn.size() >= 2) {
        axis->setView(azimuth, elevation);
        axis->getParentFigure()->setRenderingStateInvalid(true);
        axis->updateState();
    }

    axis->getView(azimuth, elevation);
    retval << ArrayOf::doubleConstructor(azimuth);
    retval << ArrayOf::doubleConstructor(elevation);
    return retval;
}
//=============================================================================
}
//=============================================================================
