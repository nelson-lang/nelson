//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// GOAxis view-state and zoom/pan interaction methods, split out of GOAxis.cpp
// to reduce that translation unit. Behaviour is unchanged: these are the same
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
GOAxis::updateViewState()
{
    constexpr unsigned requiredWarmupUpdates = 2;
    if (m_viewStateWarmupCount >= requiredWarmupUpdates) {
        updateInteractionState();
        return true;
    }
    GOFigure* fig = getParentFigure();
    if (fig) {
        fig->setRenderingStateInvalid(true);
    }
    updateState();
    ++m_viewStateWarmupCount;
    return false;
}
//=============================================================================
void
GOAxis::invalidateViewState()
{
    m_viewStateWarmupCount = 0;
}
//=============================================================================
void
GOAxis::markViewStateReady()
{
    constexpr unsigned requiredWarmupUpdates = 2;
    m_viewStateWarmupCount = requiredWarmupUpdates;
}
//=============================================================================
void
GOAxis::resetView()
{
    setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    updateState();
}
//=============================================================================
void
GOAxis::zoom(double scaleFactor)
{
    if (zoomXOn) {
        GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_X_LIM_PROPERTY_NAME_STR);
        double range = (hp->data()[1] - hp->data()[0]) / 2;
        double mean = (hp->data()[1] + hp->data()[0]) / 2;
        setTwoVectorDefault(
            GO_X_LIM_PROPERTY_NAME_STR, mean - range * scaleFactor, mean + range * scaleFactor);
        setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    if (zoomYOn) {
        GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        double range = (hp->data()[1] - hp->data()[0]) / 2;
        double mean = (hp->data()[1] + hp->data()[0]) / 2;
        setTwoVectorDefault(
            GO_Y_LIM_PROPERTY_NAME_STR, mean - range * scaleFactor, mean + range * scaleFactor);
        setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    updateState();
}
//=============================================================================
void
GOAxis::pan(double stepX, double stepY)
{
    std::vector<double> position(getClientAreaAsPixels());
    if (panXOn) {
        std::vector<double> xLim = findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        double delx = stepX * (xLim[1] - xLim[0]) / position[2];
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, xLim[0] + delx, xLim[1] + delx);
        setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    if (panYOn) {
        std::vector<double> yLim = findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        double dely = stepY * (yLim[1] - yLim[0]) / position[3];
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, yLim[0] + dely, yLim[1] + dely);
        setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    if (panXOn || panYOn) {
        updateState();
    };
}
//=============================================================================
void
GOAxis::zoomXMode()
{
    zoomXOn = true;
    zoomYOn = false;
}
//=============================================================================
void
GOAxis::zoomYMode()
{
    zoomYOn = true;
    zoomXOn = false;
}
//=============================================================================
void
GOAxis::zoomOn()
{
    zoomXOn = true;
    zoomYOn = true;
}
//=============================================================================
void
GOAxis::zoomOff()
{
    zoomXOn = false;
    zoomYOn = false;
}
//=============================================================================
void
GOAxis::zoomOut()
{
    if (!zoomXStore.empty()) {
        setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, zoomXStore[0], zoomXStore[1]);
        setRestrictedStringDefault(GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    if (!zoomYStore.empty()) {
        setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, zoomYStore[0], zoomYStore[1]);
        setRestrictedStringDefault(GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    updateState();
}
//=============================================================================
void
GOAxis::zoomReset()
{
    GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_X_LIM_PROPERTY_NAME_STR);
    zoomXStore = hp->data();
    hp = (GOVectorProperty*)findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
    zoomYStore = hp->data();
}
//=============================================================================
void
GOAxis::panXMode()
{
    panXOn = true;
    panYOn = false;
}
//=============================================================================
void
GOAxis::panYMode()
{
    panXOn = false;
    panYOn = true;
}
//=============================================================================
void
GOAxis::panOn()
{
    panXOn = true;
    panYOn = true;
}
//=============================================================================
void
GOAxis::panOff()
{
    panXOn = false;
    panYOn = false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
