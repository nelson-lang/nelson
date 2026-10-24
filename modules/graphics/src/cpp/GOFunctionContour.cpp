//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFunctionContour.hpp"
#include "Evaluator.hpp"
#include "FunctionDef.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOColorInterpProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarPositiveIntegerValueProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "ContourGenerator.hpp"
#include "NelsonConfiguration.hpp"
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
makeGridData(indexType density, const std::vector<double>& range, bool xData)
{
    double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, density * density));
    ArrayOf data(NLS_DOUBLE, Dimensions(density, density), dp);
    double step = density > 1 ? (range[1] - range[0]) / static_cast<double>(density - 1) : 0.;
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
evaluateFunction(FunctionDef* funcDef, const ArrayOf& xData, const ArrayOf& yData)
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
evaluateFunctionByPoint(FunctionDef* funcDef, const ArrayOf& xData, const ArrayOf& yData)
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
sampleContourFunction(const ArrayOf& functionValue, const ArrayOf& xData, const ArrayOf& yData)
{
    FunctionDef* funcDef = functionDefinitionFromProperty(functionValue);
    ArrayOf zData;
    try {
        zData = evaluateFunction(funcDef, xData, yData);
    } catch (...) {
        zData = ArrayOf::empty();
    }
    if (!zData.isEmpty()) {
        return zData;
    }
    try {
        return evaluateFunctionByPoint(funcDef, xData, yData);
    } catch (...) {
        return ArrayOf::empty();
    }
}
//=============================================================================
static void
copyProperty(GraphicsObject* object, const std::wstring& source, const std::wstring& target)
{
    GOGenericProperty* sourceProperty = object->findProperty(source);
    GOGenericProperty* targetProperty = object->findProperty(target);
    targetProperty->set(sourceProperty->get());
    targetProperty->clearModified();
}
//=============================================================================
GOFunctionContour::GOFunctionContour() : GOContour()
{
    registerProperty(new GOArrayOfProperty, GO_FUNCTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_FILL_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_LINE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarPositiveIntegerValueProperty, GO_MESH_DENSITY_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_X_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_Y_RANGE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_RANGE_MODE_PROPERTY_NAME_STR);
    setRestrictedStringDefault(GO_FILL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringColorDefault(
        GO_LINE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    setScalarPositiveIntegerValueDefault(GO_MESH_DENSITY_PROPERTY_NAME_STR, 71);
    setTwoVectorDefault(GO_X_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_X_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwoVectorDefault(GO_Y_RANGE_PROPERTY_NAME_STR, -5, 5);
    setStringDefault(GO_Y_RANGE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    sortProperties();
}
//=============================================================================
GOFunctionContour::~GOFunctionContour() = default;
//=============================================================================
std::wstring
GOFunctionContour::getType()
{
    return GO_PROPERTY_VALUE_FUNCTION_CONTOUR_STR;
}
//=============================================================================
void
GOFunctionContour::updateState()
{
    if (hasChanged(GO_X_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_X_RANGE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_RANGE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_RANGE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LINE_COLOR_PROPERTY_NAME_STR)) {
        copyProperty(this, GO_LINE_COLOR_PROPERTY_NAME_STR, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    }
    if (stringCheck(GO_FILL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        setRestrictedStringColorDefault(
            GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    } else {
        setRestrictedStringColorDefault(
            GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, 0, 0, 0);
    }

    std::vector<std::wstring> samplingProperties
        = { GO_FUNCTION_PROPERTY_NAME_STR, GO_X_RANGE_PROPERTY_NAME_STR,
              GO_X_RANGE_MODE_PROPERTY_NAME_STR, GO_Y_RANGE_PROPERTY_NAME_STR,
              GO_Y_RANGE_MODE_PROPERTY_NAME_STR, GO_MESH_DENSITY_PROPERTY_NAME_STR };
    std::vector<std::wstring> dataProperties
        = { GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR, GO_Z_DATA_PROPERTY_NAME_STR };
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
            ArrayOf zData = sampleContourFunction(
                findArrayOfProperty(GO_FUNCTION_PROPERTY_NAME_STR), xData, yData);
            if (!zData.isEmpty()) {
                setArrayData(this, GO_X_DATA_PROPERTY_NAME_STR, xData);
                setArrayData(this, GO_Y_DATA_PROPERTY_NAME_STR, yData);
                setArrayData(this, GO_Z_DATA_PROPERTY_NAME_STR, zData);
                toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
                toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
            }
        }
    }
    if (samplingChanged) {
        clearChanged(samplingProperties);
    }
    bool autoLevelList
        = stringCheck(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR)
        && !hasChanged(GO_LEVEL_LIST_PROPERTY_NAME_STR);
    if (autoLevelList) {
        std::vector<double> levels
            = defaultContourLevels(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
        auto* levelProperty
            = static_cast<GOVectorProperty*>(findProperty(GO_LEVEL_LIST_PROPERTY_NAME_STR));
        levelProperty->data(levels);
        levelProperty->clearModified();
        toManual(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    }
    GOContour::updateState();
    if (autoLevelList) {
        toAuto(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
std::vector<double>
GOFunctionContour::getLimits()
{
    // GOContour takes its extent from the level lines it drew. A function contour
    // frames the sampled XRange/YRange instead, even when the levels cover only a
    // small disc (a gaussian) or there is no level line at all (a constant function).
    std::vector<double> limits = GOContour::getLimits();
    if (limits.size() < 4) {
        return limits;
    }
    std::vector<double> xRange = findVectorDoubleProperty(GO_X_RANGE_PROPERTY_NAME_STR);
    std::vector<double> yRange = findVectorDoubleProperty(GO_Y_RANGE_PROPERTY_NAME_STR);
    if (xRange.size() == 2 && std::isfinite(xRange[0]) && std::isfinite(xRange[1])) {
        limits[0] = xRange[0];
        limits[1] = xRange[1];
    }
    if (yRange.size() == 2 && std::isfinite(yRange[0]) && std::isfinite(yRange[1])) {
        limits[2] = yRange[0];
        limits[3] = yRange[1];
    }
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
