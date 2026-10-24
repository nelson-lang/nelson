//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include "UITicksHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
tickLabelOf(double value);
static double
niceStepOf(double span, int intervals);
//=============================================================================
bool
uiTicksSyncModes(GraphicsObject* go)
{
    bool majorTicksChanged = go->hasChanged(GO_MAJOR_TICKS_PROPERTY_NAME_STR);
    bool minorTicksChanged = go->hasChanged(GO_MINOR_TICKS_PROPERTY_NAME_STR);
    if (majorTicksChanged) {
        go->setRestrictedStringDefault(
            GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    if (minorTicksChanged) {
        go->setRestrictedStringDefault(
            GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    if (go->hasChanged(GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR)) {
        go->setRestrictedStringDefault(
            GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    }
    return go->hasChanged(GO_LIMITS_PROPERTY_NAME_STR) || majorTicksChanged || minorTicksChanged;
}
//=============================================================================
void
uiTicksUpdateAuto(GraphicsObject* go, bool autoMinorTicks, int majorIntervals)
{
    std::vector<double> limits(go->findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
    if (limits.size() != 2 || limits[1] <= limits[0]) {
        return;
    }
    double niceStep = niceStepOf(limits[1] - limits[0], majorIntervals);
    GOVectorProperty* majorProperty
        = static_cast<GOVectorProperty*>(go->findProperty(GO_MAJOR_TICKS_PROPERTY_NAME_STR));
    if (go->stringCheck(GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR)) {
        std::vector<double> major;
        for (double tick = limits[0]; tick <= limits[1] + niceStep * 1e-9; tick += niceStep) {
            major.push_back(tick);
        }
        majorProperty->data(major);
    }
    if (go->stringCheck(GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR)) {
        std::vector<double> major = majorProperty->data();
        wstringVector labels;
        labels.reserve(major.size());
        for (double tick : major) {
            labels.push_back(tickLabelOf(tick));
        }
        static_cast<GOStringVectorProperty*>(
            go->findProperty(GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR))
            ->data(labels);
    }
    if (autoMinorTicks
        && go->stringCheck(GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR)) {
        std::vector<double> minor;
        double minorStep = niceStep / 5.0;
        for (double tick = limits[0]; tick <= limits[1] + minorStep * 1e-9; tick += minorStep) {
            minor.push_back(tick);
        }
        static_cast<GOVectorProperty*>(go->findProperty(GO_MINOR_TICKS_PROPERTY_NAME_STR))
            ->data(minor);
    }
}
//=============================================================================
void
uiDisplayMajorTicks(double minimum, double maximum, bool majorAuto,
    const std::vector<double>& majorProperty, const std::vector<std::wstring>& majorLabelProperty,
    double pixelExtent, double divisor, std::vector<double>& outTicks,
    std::vector<std::wstring>& outLabels)
{
    if (!majorAuto || maximum <= minimum || pixelExtent <= 0.0 || divisor <= 0.0) {
        outTicks = majorProperty;
        outLabels = majorLabelProperty;
        return;
    }
    int intervals = std::max(1, static_cast<int>(std::lround(pixelExtent / divisor)));
    double step = niceStepOf(maximum - minimum, intervals);
    outTicks.clear();
    outLabels.clear();
    for (double tick = minimum; tick <= maximum + step * 1e-9; tick += step) {
        outTicks.push_back(tick);
        outLabels.push_back(tickLabelOf(tick));
    }
}
//=============================================================================
std::wstring
tickLabelOf(double value)
{
    wchar_t buffer[64];
    swprintf(buffer, 64, L"%g", value);
    return buffer;
}
//=============================================================================
// "nice" major step producing about 'intervals' intervals
double
niceStepOf(double span, int intervals)
{
    double roughStep = span / intervals;
    double magnitude = std::pow(10.0, std::floor(std::log10(roughStep)));
    double residual = roughStep / magnitude;
    return (residual <= 1) ? magnitude
        : (residual <= 2)  ? 2 * magnitude
        : (residual <= 5)  ? 5 * magnitude
                           : 10 * magnitude;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
