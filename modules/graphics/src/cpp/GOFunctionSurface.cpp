//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFunctionSurface.hpp"
#include "ContourGenerator.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "NelsonConfiguration.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
makeGridData(indexType density, const std::vector<double>& range, bool xData)
{
    double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, density * density));
    ArrayOf data(NLS_DOUBLE, Dimensions(density, density), dp);
    double step = density > 1 ? (range[1] - range[0]) / static_cast<double>(density - 1) : 0;
    for (indexType col = 0; col < density; ++col) {
        for (indexType row = 0; row < density; ++row) {
            indexType idx = row + col * density;
            indexType source = xData ? col : row;
            dp[idx] = range[0] + step * static_cast<double>(source);
        }
    }
    return data;
}
//=============================================================================
static void
setArrayData(GraphicsObject* object, const std::wstring& name, const ArrayOf& value)
{
    GOArrayOfProperty* property = static_cast<GOArrayOfProperty*>(object->findProperty(name));
    property->data(value);
    property->clearModified();
}
//=============================================================================
static FunctionDef*
functionDefinitionFromProperty(const ArrayOf& functionValue)
{
    if (!functionValue.isFunctionHandle()) {
        return nullptr;
    }
    function_handle fh = functionValue.asFunctionHandle();
    return reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
}
//=============================================================================
static ArrayOf
evaluateSurfaceFunction(FunctionDef* funcDef, const ArrayOf& xData, const ArrayOf& yData)
{
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (!funcDef || !eval) {
        return ArrayOf::empty();
    }

    ArrayOfVector inputs;
    inputs << xData;
    inputs << yData;
    ArrayOfVector outputs = funcDef->evaluateFunction(eval, inputs, 1);
    if (outputs.empty()) {
        return ArrayOf::empty();
    }
    ArrayOf zData(outputs[0]);
    if (zData.isNumeric() && zData.isScalar()) {
        double value = zData.asDoubleScalar();
        double* dp
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, xData.elementCount()));
        std::fill(dp, dp + xData.elementCount(), value);
        return ArrayOf(NLS_DOUBLE, xData.dimensions(), dp);
    }
    if (zData.isNumeric() && zData.dimensions().equals(xData.dimensions())) {
        zData.promoteType(NLS_DOUBLE);
        return zData;
    }
    return ArrayOf::empty();
}
//=============================================================================
static ArrayOf
evaluateSurfaceFunctionByPoint(FunctionDef* funcDef, const ArrayOf& xData, const ArrayOf& yData)
{
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
    if (!funcDef || !eval) {
        return ArrayOf::empty();
    }

    ArrayOf xValues(xData);
    ArrayOf yValues(yData);
    xValues.promoteType(NLS_DOUBLE);
    yValues.promoteType(NLS_DOUBLE);
    const double* xp = static_cast<const double*>(xValues.dataPointer());
    const double* yp = static_cast<const double*>(yValues.dataPointer());
    double* zp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, xData.elementCount()));
    ArrayOf zData(NLS_DOUBLE, xData.dimensions(), zp);
    for (indexType k = 0; k < xData.elementCount(); ++k) {
        ArrayOfVector inputs;
        inputs << ArrayOf::doubleScalar(xp[k]);
        inputs << ArrayOf::doubleScalar(yp[k]);
        ArrayOfVector outputs = funcDef->evaluateFunction(eval, inputs, 1);
        if (outputs.empty() || !outputs[0].isNumeric() || !outputs[0].isScalar()) {
            return ArrayOf::empty();
        }
        zp[k] = outputs[0].asDoubleScalar();
    }
    return zData;
}
//=============================================================================
static ArrayOf
sampleSurfaceFunction(const ArrayOf& functionValue, const ArrayOf& xData, const ArrayOf& yData)
{
    FunctionDef* funcDef = functionDefinitionFromProperty(functionValue);
    ArrayOf zData;
    try {
        zData = evaluateSurfaceFunction(funcDef, xData, yData);
    } catch (...) {
        zData = ArrayOf::empty();
    }
    if (!zData.isEmpty()) {
        return zData;
    }
    try {
        return evaluateSurfaceFunctionByPoint(funcDef, xData, yData);
    } catch (...) {
        return ArrayOf::empty();
    }
}
//=============================================================================
GOFunctionSurface::GOFunctionSurface()
{
    registerProperty(new GOArrayOfProperty, GO_FUNCTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarPositiveIntegerValueProperty, GO_MESH_DENSITY_PROPERTY_NAME_STR);
    // ShowContours reports as a plain 'on'/'off' char, not an OnOffSwitchState enum.
    registerProperty(new GORestrictedStringProperty(std::vector<std::wstring> {
                         GO_PROPERTY_VALUE_OFF_STR, GO_PROPERTY_VALUE_ON_STR }),
        GO_SHOW_CONTOURS_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_X_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_Y_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_RANGE_MODE_PROPERTY_NAME_STR);
    setScalarPositiveIntegerValueDefault(GO_MESH_DENSITY_PROPERTY_NAME_STR, 35);
    setRestrictedStringDefault(GO_SHOW_CONTOURS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setTwoVectorDefault(GO_X_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_X_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_Y_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_Y_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INTERP_STR);
    sortProperties();
}
//=============================================================================
GOFunctionSurface::~GOFunctionSurface() = default;
//=============================================================================
std::wstring
GOFunctionSurface::getType()
{
    return GO_PROPERTY_VALUE_FUNCTION_SURFACE_STR;
}
//=============================================================================
void
GOFunctionSurface::updateState()
{
    if (hasChanged(GO_X_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_RANGE_MODE_PROPERTY_NAME_STR);
    }

    std::vector<std::wstring> samplingProperties
        = { GO_FUNCTION_PROPERTY_NAME_STR, GO_X_RANGE_PROPERTY_NAME_STR,
              GO_X_RANGE_MODE_PROPERTY_NAME_STR, GO_Y_RANGE_PROPERTY_NAME_STR,
              GO_Y_RANGE_MODE_PROPERTY_NAME_STR, GO_MESH_DENSITY_PROPERTY_NAME_STR };
    std::vector<std::wstring> dataProperties
        = { GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR };
    bool samplingChanged = hasChanged(samplingProperties);
    bool dataChanged = hasChanged(dataProperties);
    if (samplingChanged && !dataChanged) {
        auto* densityProperty = static_cast<GOScalarPositiveIntegerValueProperty*>(
            findProperty(GO_MESH_DENSITY_PROPERTY_NAME_STR));
        indexType density = static_cast<indexType>(densityProperty->data());
        std::vector<double> xrange = findVectorDoubleProperty(GO_X_RANGE_PROPERTY_NAME_STR);
        std::vector<double> yrange = findVectorDoubleProperty(GO_Y_RANGE_PROPERTY_NAME_STR);
        if (density >= 2 && xrange.size() == 2 && yrange.size() == 2) {
            ArrayOf xData = makeGridData(density, xrange, true);
            ArrayOf yData = makeGridData(density, yrange, false);
            ArrayOf zData = sampleSurfaceFunction(
                findArrayOfProperty(GO_FUNCTION_PROPERTY_NAME_STR), xData, yData);
            if (!zData.isEmpty()) {
                setArrayData(this, GO_X_DATA_PROPERTY_NAME_STR, xData);
                setArrayData(this, GO_Y_DATA_PROPERTY_NAME_STR, yData);
                setArrayData(this, GO_Z_DATA_PROPERTY_NAME_STR, zData);
            }
        }
    }
    if (samplingChanged) {
        clearChanged(samplingProperties);
    }
    GOSurface::updateState();
}
//=============================================================================
// Map a contour level to a colormap colour through the axis CLim, like the
// per-level colouring of the projected contour lines. Falls back to black when no
// axis/figure/colormap is available.
static void
selectContourLevelColor(GraphicsObject* object, RenderInterface& gc, double level)
{
    const std::vector<double> black { 0.0, 0.0, 0.0, 1.0 };
    GOAxis* axis = object->getParentAxis();
    GOFigure* figure = object->getParentFigure();
    if (!axis || !figure) {
        gc.color(black);
        return;
    }
    std::vector<double> colorMap(figure->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
    std::vector<double> cLim(axis->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
    int colorMapLength = static_cast<int>(colorMap.size() / 3);
    if (cLim.size() < 2 || colorMapLength <= 0) {
        gc.color(black);
        return;
    }
    double cLimMin = std::min(cLim[0], cLim[1]);
    double cLimMax = std::max(cLim[0], cLim[1]);
    int idx = 0;
    if (cLimMax > cLimMin) {
        idx = static_cast<int>((level - cLimMin) / (cLimMax - cLimMin) * (colorMapLength - 1));
    }
    idx = std::min(colorMapLength - 1, std::max(0, idx));
    gc.color(std::vector<double> {
        colorMap[3 * idx], colorMap[3 * idx + 1], colorMap[3 * idx + 2], 1.0 });
}
//=============================================================================
void
GOFunctionSurface::paintMe(RenderInterface& gc)
{
    GOSurface::paintMe(gc);
    if (!stringCheck(GO_SHOW_CONTOURS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        return;
    }
    ArrayOf zData(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (!zData.isNumeric() || zData.isEmpty()) {
        return;
    }
    ArrayOf xData(findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR));
    ArrayOf yData(findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    xData.promoteType(NLS_DOUBLE);
    yData.promoteType(NLS_DOUBLE);
    zData.promoteType(NLS_DOUBLE);
    std::vector<double> levels = defaultContourLevels(zData);
    double zMin = ArrayOfMin(zData);
    for (double level : levels) {
        selectContourLevelColor(this, gc, level);
        contourLineSet lines = generateContourLines(zData, level, xData, yData);
        for (const contourLine& line : lines) {
            for (size_t k = 1; k < line.size(); ++k) {
                gc.line(line[k - 1].x, line[k - 1].y, zMin, line[k].x, line[k].y, zMin);
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
