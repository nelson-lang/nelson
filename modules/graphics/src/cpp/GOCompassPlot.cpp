//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOCompassPlot.hpp"
#include "GOChartDecorators.hpp"
#include "GOAxis.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOStringProperty.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
static void
appendSegment(std::vector<double>& xs, std::vector<double>& ys, std::vector<double>& zs, double x0,
    double y0, double z0, double x1, double y1, double z1)
{
    xs.insert(xs.end(), { x0, x1, std::numeric_limits<double>::quiet_NaN() });
    ys.insert(ys.end(), { y0, y1, std::numeric_limits<double>::quiet_NaN() });
    zs.insert(zs.end(), { z0, z1, std::numeric_limits<double>::quiet_NaN() });
}
//=============================================================================
static void
appendCompassHead(std::vector<double>& xs, std::vector<double>& ys, std::vector<double>& zs,
    double theta, double radius)
{
    if (!std::isfinite(radius) || radius == 0.) {
        return;
    }
    double tipX = radius * std::cos(theta);
    double tipY = radius * std::sin(theta);
    double length = std::abs(radius);
    double ux = tipX / length;
    double uy = tipY / length;
    double sx = -uy;
    double sy = ux;
    double headLength = 0.12 * length;
    double headWidth = 0.45 * headLength;
    double baseX = tipX - headLength * ux;
    double baseY = tipY - headLength * uy;
    appendSegment(xs, ys, zs, baseX + headWidth * sx, baseY + headWidth * sy, 0., tipX, tipY, 0.);
    appendSegment(xs, ys, zs, baseX - headWidth * sx, baseY - headWidth * sy, 0., tipX, tipY, 0.);
}
//=============================================================================
static void
appendCompassSegments(GraphicsObject* object, std::vector<double>& xs, std::vector<double>& ys,
    std::vector<double>& zs)
{
    std::vector<double> theta(object->findVectorDoubleProperty(GO_THETA_DATA_PROPERTY_NAME_STR));
    std::vector<double> radius(object->findVectorDoubleProperty(GO_R_DATA_PROPERTY_NAME_STR));
    if (theta.size() != radius.size()) {
        return;
    }
    for (size_t k = 0; k < theta.size(); ++k) {
        double angle = theta[k];
        double value = radius[k];
        if (std::isfinite(value) && value < 0.) {
            angle += 3.141592653589793238462643383279502884;
            value = -value;
        }
        double x = value * std::cos(angle);
        double y = value * std::sin(angle);
        appendSegment(xs, ys, zs, 0., 0., 0., x, y, 0.);
        appendCompassHead(xs, ys, zs, angle, value);
    }
}
//=============================================================================
static void
finiteMinMax(const std::vector<double>& values, double& low, double& high)
{
    for (double value : values) {
        if (!std::isfinite(value)) {
            continue;
        }
        low = std::min(low, value);
        high = std::max(high, value);
    }
}
//=============================================================================
GOCompassPlot::GOCompassPlot() : GOLineSeries()
{
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_VARIABLE_PROPERTY_NAME_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COMPASS_PLOT_STR);
    findProperty(GO_SOURCE_TABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    setStringDefault(GO_THETA_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_R_VARIABLE_PROPERTY_NAME_STR, L"");
    sortProperties();
    clearAllChanged();
}
//=============================================================================
std::wstring
GOCompassPlot::getType()
{
    return GO_PROPERTY_VALUE_COMPASS_PLOT_STR;
}
//=============================================================================
void
GOCompassPlot::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOAxis* axis = static_cast<GOAxis*>(getParentAxis());
    if (!axis) {
        return;
    }
    auto* color = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (color->isNone()) {
        return;
    }
    std::vector<double> xs;
    std::vector<double> ys;
    std::vector<double> zs;
    appendCompassSegments(this, xs, ys, zs);
    std::vector<double> mx;
    std::vector<double> my;
    std::vector<double> mz;
    axis->reMap(xs, ys, zs, mx, my, mz);
    gc.color(color->data());
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
    gc.lineSeries(mx, my, mz);
}
//=============================================================================
std::vector<double>
GOCompassPlot::getLimits()
{
    std::vector<double> xs;
    std::vector<double> ys;
    std::vector<double> zs;
    appendCompassSegments(this, xs, ys, zs);
    if (xs.empty()) {
        return {};
    }
    double xmin = 0.;
    double xmax = 0.;
    double ymin = 0.;
    double ymax = 0.;
    finiteMinMax(xs, xmin, xmax);
    finiteMinMax(ys, ymin, ymax);
    return { xmin, xmax, ymin, ymax, 0., 0., 0., 0., 0., 0. };
}
//=============================================================================
} // namespace Nelson
//=============================================================================
