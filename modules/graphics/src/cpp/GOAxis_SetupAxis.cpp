//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// GOAxis axis setup, coordinate flip and font update methods.
// Split out of GOAxis.cpp to reduce that translation unit. Behaviour is
// unchanged: these are the same GOAxis member definitions, moved verbatim.
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
void
GOAxis::setupAxis(RenderInterface& gc)
{
    std::vector<double> limits(getAxisLimits());
    computeAxisEdges(gc, limits);
    double x1, y1, x2, y2;
    gc.toPixels(limits[0], x1pos[1], x1pos[2], x1, y1);
    gc.toPixels(limits[1], x1pos[1], x1pos[2], x2, y2);
    xvisible = (fabs(x1 - x2) > 2) || (fabs(y1 - y2) > 2);
    gc.toPixels(y1pos[0], limits[2], y1pos[2], x1, y1);
    gc.toPixels(y1pos[0], limits[3], y1pos[2], x2, y2);
    yvisible = (fabs(x1 - x2) > 2) || (fabs(y1 - y2) > 2);
    gc.toPixels(z1pos[0], z1pos[1], limits[4], x1, y1);
    gc.toPixels(z1pos[0], z1pos[1], limits[5], x2, y2);
    zvisible = (fabs(x1 - x2) > 2) || (fabs(y1 - y2) > 2);
}
//=============================================================================
void
GOAxis::computeAxisEdges(RenderInterface& gc, const std::vector<double>& limits)
{
    std::vector<double> model;
    gc.getModelviewMatrix(model);
    auto flipXValue = [&limits](double t) { return t == limits[0] ? limits[1] : limits[0]; };
    auto flipYValue = [&limits](double t) { return t == limits[2] ? limits[3] : limits[2]; };
    auto flipZValue = [&limits](double t) { return t == limits[4] ? limits[5] : limits[4]; };
    if ((static_cast<GOTopBottomProperty*>(findProperty(GO_X_AXIS_LOCATION_PROPERTY_NAME_STR)))
            ->isEqual(GO_PROPERTY_VALUE_BOTTOM_STR)) {
        x1pos[2] = limits[4];
    } else {
        x1pos[2] = limits[5];
    }
    if ((static_cast<GOLeftRightProperty*>(findProperty(GO_Y_AXIS_LOCATION_PROPERTY_NAME_STR)))
            ->isEqual(GO_PROPERTY_VALUE_LEFT_STR)) {
        y1pos[2] = limits[4];
    } else {
        y1pos[2] = limits[5];
    }
    if ((model[10] > 0) && (model[6] > 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[3];
        } else {
            x1pos[1] = limits[2];
        }
    } else if ((model[10] > 0) && (model[6] <= 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[2];
        } else {
            x1pos[1] = limits[3];
        }
    } else if ((model[10] <= 0) && (model[6] > 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[2];
        } else {
            x1pos[1] = limits[3];
        }
    } else if ((model[10] <= 0) && (model[6] <= 0)) {
        if (x1pos[2] == limits[4]) {
            x1pos[1] = limits[3];
        } else {
            x1pos[1] = limits[2];
        }
    }
    x2pos[1] = flipYValue(x1pos[1]);
    x2pos[2] = x1pos[2];
    if ((model[10] > 0) && (model[2] > 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[1];
        } else {
            y1pos[0] = limits[0];
        }
    } else if ((model[10] <= 0) && (model[2] > 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[0];
        } else {
            y1pos[0] = limits[1];
        }
    } else if ((model[10] > 0) && (model[2] <= 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[0];
        } else {
            y1pos[0] = limits[1];
        }
    } else if ((model[10] <= 0) && (model[2] <= 0)) {
        if (y1pos[2] == limits[4]) {
            y1pos[0] = limits[1];
        } else {
            y1pos[0] = limits[0];
        }
    }
    y2pos[0] = flipXValue(y1pos[0]);
    y2pos[2] = y1pos[2];
    if (model[6] > 0) {
        z1pos[0] = limits[1];
    } else {
        z1pos[0] = limits[0];
    }
    if (model[2] > 0) {
        z1pos[1] = limits[2];
    } else {
        z1pos[1] = limits[3];
    }
    if ((model[10] >= 0) && (model[6] >= 0) && (model[2] >= 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[3];
    } else if ((model[10] >= 0) && (model[6] >= 0) && (model[2] < 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[3];
    } else if ((model[10] >= 0) && (model[6] < 0) && (model[2] >= 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[2];
    } else if ((model[10] >= 0) && (model[6] < 0) && (model[2] < 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[2];
    } else if ((model[10] < 0) && (model[6] >= 0) && (model[2] >= 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[2];
    } else if ((model[10] < 0) && (model[6] >= 0) && (model[2] < 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[2];
    } else if ((model[10] < 0) && (model[6] < 0) && (model[2] >= 0)) {
        z2pos[0] = limits[0];
        z2pos[1] = limits[3];
    } else if ((model[10] < 0) && (model[6] < 0) && (model[2] < 0)) {
        z2pos[0] = limits[1];
        z2pos[1] = limits[3];
    }
    if ((model[2] == 0) && (model[6] == 0)) {
        x2pos[1] = flipYValue(x1pos[1]);
        x2pos[2] = x1pos[2];
        y2pos[0] = flipXValue(y1pos[0]);
    }
    if ((model[6] == 0) && (model[10] == 0)) {
        y2pos[0] = y1pos[0];
        y2pos[2] = flipZValue(y1pos[2]);
        z2pos[0] = z1pos[0];
        z2pos[1] = flipYValue(z1pos[1]);
    }
    if ((model[2] == 0) && (model[10] == 0)) {
        x2pos[1] = x1pos[1];
        x2pos[2] = flipZValue(x1pos[2]);
        z2pos[0] = flipXValue(z1pos[0]);
        z2pos[1] = z1pos[1];
    }
}
//=============================================================================
double
GOAxis::flipZ(double t)
{
    std::vector<double> limits(getAxisLimits());
    if (t == limits[4]) {
        return limits[5];
    }
    return limits[4];
}
//=============================================================================
double
GOAxis::flipY(double t)
{
    std::vector<double> limits(getAxisLimits());
    if (t == limits[2]) {
        return limits[3];
    }
    return limits[2];
}
//=============================================================================
double
GOAxis::flipX(double t)
{
    std::vector<double> limits(getAxisLimits());
    if (t == limits[0]) {
        return limits[1];
    }
    return limits[0];
}
//=============================================================================
void
GOAxis::updateAxisFont()
{
    GOStringProperty* fontname
        = static_cast<GOStringProperty*>(findProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
    GOFontAngleProperty* fontangle
        = static_cast<GOFontAngleProperty*>(findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
    GOFontWeightProperty* fontweight
        = static_cast<GOFontWeightProperty*>(findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
    GOScalarProperty* fontsize
        = static_cast<GOScalarProperty*>(findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
    GOFontUnitsProperty* fontUnits
        = static_cast<GOFontUnitsProperty*>(findProperty(GO_FONT_UNITS_PROPERTY_NAME_STR));
    const bool sizeInPixels = fontUnits->isEqual(GO_PROPERTY_VALUE_PIXELS_STR);
    m_font = makeGOFontSpec(resolveRootFontName(fontname->data()), sizeInPixels,
        sizeInPixels ? fontsize->data() : fontSizeInPoints(this), fontangle->data(),
        fontweight->data());
    setScalarDoubleDefault(GO_TEXT_HEIGHT_PROPERTY_NAME_STR,
        std::lround(FontMetrics::lineHeightPixels(
            m_font.family, fontSpecPixelSize(m_font), m_font.bold, m_font.italic)));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
