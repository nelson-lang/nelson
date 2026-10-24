//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOStackedLineChart.hpp"
#include "GOChartDecorators.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GOVectorProperty.hpp"
#include <algorithm>
#include <cmath>
#include <cwctype>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::vector<GraphicsObject*>
getStackedAxes(GOStackedLineChart* chart);
//=============================================================================
static std::vector<GraphicsObject*>
getTypedChildren(GraphicsObject* parent, const std::wstring& type);
//=============================================================================
static bool
setObjectProperty(GraphicsObject* object, const std::wstring& propertyName, const ArrayOf& value);
//=============================================================================
static void
setTextObjectString(GraphicsObject* object, const std::wstring& value);
//=============================================================================
static void
updateObjects(const std::vector<GraphicsObject*>& objects);
//=============================================================================
static bool
arrayValuesEqual(const ArrayOf& currentValue, const ArrayOf& newValue);
//=============================================================================
static bool
doubleVectorsEqual(const std::vector<double>& currentValue, const std::vector<double>& newValue);
//=============================================================================
static void
applyGridVisible(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
applyXLimits(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
applyFontProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
applyTopLevelLineProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
syncLinePropertiesCache(
    GOStackedLineChart* chart, const std::wstring& propertyName, const ArrayOf& value);
//=============================================================================
static std::string
linePropertyFieldName(const std::wstring& propertyName);
//=============================================================================
static void
applyAxesProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
applyLineProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
applyLineProperty(GraphicsObject* line, const std::string& fieldName,
    const std::wstring& propertyName, const ArrayOf& value, size_t lineIndex, size_t lineCount);
//=============================================================================
static void
applyPlotType(GOStackedLineChart* chart, GraphicsObject* line, const ArrayOf& value,
    size_t lineIndex, size_t lineCount);
//=============================================================================
static void
applyDisplayLabels(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
applyTitle(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static void
applyXLabel(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes);
//=============================================================================
static bool
isUsableStructArray(const ArrayOf& value);
//=============================================================================
static ArrayOf
getStructFieldValue(const ArrayOf& value, const std::string& fieldName, size_t index);
//=============================================================================
static std::wstring
getTextElement(const ArrayOf& value, size_t index);
//=============================================================================
static ArrayOf
lineSpecificPropertyValue(
    const ArrayOf& value, const std::string& fieldName, size_t lineIndex, size_t lineCount);
//=============================================================================
static ArrayOf
numericLineSpecificPropertyValue(
    const ArrayOf& value, const std::string& fieldName, size_t lineIndex, size_t lineCount);
//=============================================================================
static ArrayOf
textLineSpecificPropertyValue(const ArrayOf& value, size_t lineIndex, size_t lineCount);
//=============================================================================
static void
restoreOriginalLineData(GOStackedLineChart* chart, GraphicsObject* line);
//=============================================================================
static void
applyStairsLineData(GOStackedLineChart* chart, GraphicsObject* line);
//=============================================================================
static ArrayOf
stairsLineVector(const ArrayOf& xData, const ArrayOf& yData, bool useXData);
//=============================================================================
static bool
isColorLineProperty(const std::string& fieldName);
//=============================================================================
static bool
isScalarLineProperty(const std::string& fieldName);
//=============================================================================
GOStackedLineChart::GOStackedLineChart()
{
    registerProperties();
    registerCommonProperties(this);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_GRID_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_EVENTS_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_LEGEND_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_LEGEND_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HORIZONTAL_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1, -1, -1);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setThreeVectorDefault(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 8);
    initializeCommonProperties(this);
}
//=============================================================================
std::wstring
GOStackedLineChart::getType()
{
    return GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR;
}
//=============================================================================
void
GOStackedLineChart::registerProperties()
{
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DISPLAY_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DISPLAY_VARIABLES_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_EVENTS_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COMBINE_MATCHING_NAMES_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_LEGEND_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_LEGEND_ORIENTATION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_LEGEND_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_LINE_PROPERTIES_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_AXES_PROPERTIES_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_GRID_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOSourceTableProperty(true), GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOStackedLineChart::updateState()
{
    std::vector<GraphicsObject*> axes = getStackedAxes(this);
    applyGridVisible(this, axes);
    applyXLimits(this, axes);
    applyFontProperties(this, axes);
    applyTopLevelLineProperties(this, axes);
    applyAxesProperties(this, axes);
    applyLineProperties(this, axes);
    applyDisplayLabels(this, axes);
    applyTitle(this, axes);
    applyXLabel(this, axes);
}
//=============================================================================
void
GOStackedLineChart::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOGObjectsProperty* children
        = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return;
    }
    for (int64 handle : children->data()) {
        if (handle == -1 || isDeletedGraphicsObject(handle)) {
            continue;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child) {
            child->paintMe(gc);
        }
    }
}
//=============================================================================
void
GOStackedLineChart::preserveLineData(GraphicsObject* line)
{
    if (!line || m_originalLineData.find(line) != m_originalLineData.end()) {
        return;
    }
    GOGenericProperty* xProperty = line->findProperty(GO_X_DATA_PROPERTY_NAME_STR, false);
    GOGenericProperty* yProperty = line->findProperty(GO_Y_DATA_PROPERTY_NAME_STR, false);
    if (xProperty && yProperty) {
        m_originalLineData[line] = std::make_pair(xProperty->get(), yProperty->get());
    }
}
//=============================================================================
std::pair<ArrayOf, ArrayOf>
GOStackedLineChart::originalLineData(GraphicsObject* line)
{
    preserveLineData(line);
    auto iterator = m_originalLineData.find(line);
    if (iterator == m_originalLineData.end()) {
        return std::make_pair(ArrayOf::empty(), ArrayOf::empty());
    }
    return iterator->second;
}
//=============================================================================
static std::vector<GraphicsObject*>
getStackedAxes(GOStackedLineChart* chart)
{
    std::vector<GraphicsObject*> axes;
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        chart->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return axes;
    }
    for (int64 handle : children->data()) {
        if (handle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(handle)) {
            continue;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child && child->isType(GO_PROPERTY_VALUE_AXES_STR)) {
            axes.push_back(child);
        }
    }
    return axes;
}
//=============================================================================
static std::vector<GraphicsObject*>
getTypedChildren(GraphicsObject* parent, const std::wstring& type)
{
    std::vector<GraphicsObject*> objects;
    GOGObjectsProperty* children = dynamic_cast<GOGObjectsProperty*>(
        parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return objects;
    }
    for (int64 handle : children->data()) {
        if (handle < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(handle)) {
            continue;
        }
        GraphicsObject* child = findGraphicsObject(handle, false);
        if (child && child->isType(type)) {
            objects.push_back(child);
        }
    }
    return objects;
}
//=============================================================================
static bool
setObjectProperty(GraphicsObject* object, const std::wstring& propertyName, const ArrayOf& value)
{
    GOGenericProperty* property = object->findProperty(propertyName, false);
    if (property) {
        if (arrayValuesEqual(property->get(), value)) {
            return false;
        }
        property->set(value);
        return true;
    }
    return false;
}
//=============================================================================
static void
setTextObjectString(GraphicsObject* object, const std::wstring& value)
{
    setObjectProperty(object, GO_STRING_PROPERTY_NAME_STR, ArrayOf::characterArray(value));
}
//=============================================================================
static void
applyGridVisible(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    if (!chart->hasChanged(GO_GRID_VISIBLE_PROPERTY_NAME_STR)) {
        return;
    }
    std::wstring value = chart->findStringProperty(GO_GRID_VISIBLE_PROPERTY_NAME_STR);
    ArrayOf gridValue = ArrayOf::characterArray(value);
    for (GraphicsObject* axis : axes) {
        setObjectProperty(axis, GO_X_GRID_PROPERTY_NAME_STR, gridValue);
        setObjectProperty(axis, GO_Y_GRID_PROPERTY_NAME_STR, gridValue);
        setObjectProperty(axis, GO_Z_GRID_PROPERTY_NAME_STR, gridValue);
    }
    chart->clearChanged(GO_GRID_VISIBLE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
applyXLimits(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    if (!chart->hasChanged(GO_X_LIMITS_PROPERTY_NAME_STR)) {
        return;
    }
    ArrayOf value = chart->findArrayOfProperty(GO_X_LIMITS_PROPERTY_NAME_STR);
    if (!value.isEmpty()) {
        std::vector<GraphicsObject*> changedAxes;
        for (GraphicsObject* axis : axes) {
            if (setObjectProperty(axis, GO_X_LIM_PROPERTY_NAME_STR, value)) {
                changedAxes.push_back(axis);
            }
        }
        updateObjects(changedAxes);
    }
    chart->clearChanged(GO_X_LIMITS_PROPERTY_NAME_STR);
}
//=============================================================================
static void
applyFontProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    const std::vector<std::wstring> fields { GO_FONT_NAME_PROPERTY_NAME_STR,
        GO_FONT_SIZE_PROPERTY_NAME_STR };
    for (const auto& field : fields) {
        if (!chart->hasChanged(field)) {
            continue;
        }
        GOGenericProperty* property = chart->findProperty(field, false);
        if (property) {
            ArrayOf value = property->get();
            std::vector<GraphicsObject*> changedObjects;
            for (GraphicsObject* axis : axes) {
                if (setObjectProperty(axis, field, value)) {
                    changedObjects.push_back(axis);
                }
                std::vector<GraphicsObject*> textObjects
                    = getTypedChildren(axis, GO_PROPERTY_VALUE_TEXT_STR);
                for (GraphicsObject* textObject : textObjects) {
                    setObjectProperty(textObject, field, value);
                }
            }
            updateObjects(changedObjects);
        }
        chart->clearChanged(field);
    }
}
//=============================================================================
static void
applyTopLevelLineProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    const std::vector<std::wstring> fields { GO_COLOR_PROPERTY_NAME_STR,
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_LINE_STYLE_PROPERTY_NAME_STR, GO_LINE_WIDTH_PROPERTY_NAME_STR,
        GO_MARKER_PROPERTY_NAME_STR, GO_MARKER_SIZE_PROPERTY_NAME_STR };
    for (const auto& field : fields) {
        if (!chart->hasChanged(field)) {
            continue;
        }
        GOGenericProperty* property = chart->findProperty(field, false);
        if (property) {
            ArrayOf value = property->get();
            for (GraphicsObject* axis : axes) {
                std::vector<GraphicsObject*> lines
                    = getTypedChildren(axis, GO_PROPERTY_VALUE_LINE_STR);
                for (GraphicsObject* line : lines) {
                    setObjectProperty(line, field, value);
                }
            }
            syncLinePropertiesCache(chart, field, value);
        }
        chart->clearChanged(field);
    }
}
//=============================================================================
static void
applyAxesProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    if (!chart->hasChanged(GO_AXES_PROPERTIES_PROPERTY_NAME_STR)) {
        return;
    }
    ArrayOf properties = chart->findArrayOfProperty(GO_AXES_PROPERTIES_PROPERTY_NAME_STR);
    if (!isUsableStructArray(properties)) {
        chart->clearChanged(GO_AXES_PROPERTIES_PROPERTY_NAME_STR);
        return;
    }
    size_t count = std::min<size_t>((size_t)properties.elementCount(), axes.size());
    std::vector<GraphicsObject*> changedAxes;
    for (size_t k = 0; k < count; ++k) {
        ArrayOf yLimits = getStructFieldValue(properties, "YLimits", k);
        if (!yLimits.isEmpty()) {
            if (setObjectProperty(axes[k], GO_Y_LIM_PROPERTY_NAME_STR, yLimits)) {
                changedAxes.push_back(axes[k]);
            }
        }
        ArrayOf yScale = getStructFieldValue(properties, "YScale", k);
        if (!yScale.isEmpty()) {
            if (setObjectProperty(axes[k], GO_Y_SCALE_PROPERTY_NAME_STR, yScale)) {
                changedAxes.push_back(axes[k]);
            }
        }
    }
    updateObjects(changedAxes);
    chart->clearChanged(GO_AXES_PROPERTIES_PROPERTY_NAME_STR);
}
//=============================================================================
static void
applyLineProperties(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    if (!chart->hasChanged(GO_LINE_PROPERTIES_PROPERTY_NAME_STR)) {
        return;
    }
    ArrayOf properties = chart->findArrayOfProperty(GO_LINE_PROPERTIES_PROPERTY_NAME_STR);
    if (!isUsableStructArray(properties)) {
        chart->clearChanged(GO_LINE_PROPERTIES_PROPERTY_NAME_STR);
        return;
    }
    const std::vector<std::pair<std::string, std::wstring>> fields {
        { "Color", GO_COLOR_PROPERTY_NAME_STR },
        { "MarkerFaceColor", GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR },
        { "MarkerEdgeColor", GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR },
        { "LineStyle", GO_LINE_STYLE_PROPERTY_NAME_STR },
        { "LineWidth", GO_LINE_WIDTH_PROPERTY_NAME_STR }, { "Marker", GO_MARKER_PROPERTY_NAME_STR },
        { "MarkerSize", GO_MARKER_SIZE_PROPERTY_NAME_STR }
    };
    size_t count = std::min<size_t>((size_t)properties.elementCount(), axes.size());
    for (size_t k = 0; k < count; ++k) {
        std::vector<GraphicsObject*> lines = getTypedChildren(axes[k], GO_PROPERTY_VALUE_LINE_STR);
        for (size_t lineIndex = 0; lineIndex < lines.size(); ++lineIndex) {
            for (const auto& field : fields) {
                ArrayOf value = getStructFieldValue(properties, field.first, k);
                if (!value.isEmpty()) {
                    applyLineProperty(lines[lineIndex], field.first, field.second, value, lineIndex,
                        lines.size());
                }
            }
            applyPlotType(chart, lines[lineIndex], getStructFieldValue(properties, "PlotType", k),
                lineIndex, lines.size());
        }
    }
    chart->clearChanged(GO_LINE_PROPERTIES_PROPERTY_NAME_STR);
}
//=============================================================================
static void
applyDisplayLabels(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    if (!chart->hasChanged(GO_DISPLAY_LABELS_PROPERTY_NAME_STR)) {
        return;
    }
    ArrayOf labels = chart->findArrayOfProperty(GO_DISPLAY_LABELS_PROPERTY_NAME_STR);
    size_t count = std::min<size_t>((size_t)labels.elementCount(), axes.size());
    for (size_t k = 0; k < count; ++k) {
        std::vector<GraphicsObject*> textObjects
            = getTypedChildren(axes[k], GO_PROPERTY_VALUE_TEXT_STR);
        if (!textObjects.empty()) {
            setTextObjectString(textObjects[0], getTextElement(labels, k));
        }
    }
    chart->clearChanged(GO_DISPLAY_LABELS_PROPERTY_NAME_STR);
}
//=============================================================================
static void
applyTitle(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    if (!chart->hasChanged(GO_TITLE_PROPERTY_NAME_STR) || axes.empty()) {
        return;
    }
    GOGObjectsProperty* title = dynamic_cast<GOGObjectsProperty*>(
        axes[0]->findProperty(GO_TITLE_PROPERTY_NAME_STR, false));
    if (title && !title->data().empty()) {
        GraphicsObject* textObject = findGraphicsObject(title->data()[0], false);
        if (textObject) {
            setTextObjectString(textObject, chart->findStringProperty(GO_TITLE_PROPERTY_NAME_STR));
        }
    }
    chart->clearChanged(GO_TITLE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
applyXLabel(GOStackedLineChart* chart, const std::vector<GraphicsObject*>& axes)
{
    if (!chart->hasChanged(GO_X_LABEL_PROPERTY_NAME_STR) || axes.empty()) {
        return;
    }
    GraphicsObject* bottomAxis = axes.back();
    GOGObjectsProperty* xLabel = dynamic_cast<GOGObjectsProperty*>(
        bottomAxis->findProperty(GO_X_LABEL_PROPERTY_NAME_STR, false));
    if (xLabel && !xLabel->data().empty()) {
        GraphicsObject* textObject = findGraphicsObject(xLabel->data()[0], false);
        if (textObject) {
            setTextObjectString(
                textObject, chart->findStringProperty(GO_X_LABEL_PROPERTY_NAME_STR));
        }
    }
    chart->clearChanged(GO_X_LABEL_PROPERTY_NAME_STR);
}
//=============================================================================
static void
updateObjects(const std::vector<GraphicsObject*>& objects)
{
    std::vector<GraphicsObject*> updated;
    for (GraphicsObject* object : objects) {
        if (!object || std::find(updated.begin(), updated.end(), object) != updated.end()) {
            continue;
        }
        object->updateState();
        updated.push_back(object);
    }
}
//=============================================================================
static bool
arrayValuesEqual(const ArrayOf& currentValue, const ArrayOf& newValue)
{
    if (currentValue.isEmpty() || newValue.isEmpty()) {
        return currentValue.isEmpty() && newValue.isEmpty();
    }
    if (!currentValue.dimensions().equals(newValue.dimensions())) {
        return false;
    }
    try {
        if (currentValue.isNumeric() && newValue.isNumeric() && !currentValue.isComplex()
            && !newValue.isComplex()) {
            return doubleVectorsEqual(currentValue.asDoubleVector(), newValue.asDoubleVector());
        }
        if ((currentValue.isRowVectorCharacterArray() || currentValue.isScalarStringArray())
            && (newValue.isRowVectorCharacterArray() || newValue.isScalarStringArray())) {
            return currentValue.asWideString() == newValue.asWideString();
        }
    } catch (...) {
        return false;
    }
    return false;
}
//=============================================================================
static bool
doubleVectorsEqual(const std::vector<double>& currentValue, const std::vector<double>& newValue)
{
    if (currentValue.size() != newValue.size()) {
        return false;
    }
    for (size_t k = 0; k < currentValue.size(); ++k) {
        if (std::isnan(currentValue[k]) && std::isnan(newValue[k])) {
            continue;
        }
        if (std::abs(currentValue[k] - newValue[k]) > 1e-12) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static bool
isUsableStructArray(const ArrayOf& value)
{
    return (value.isStruct() || value.isClassdefType()) && value.elementCount() > 0;
}
//=============================================================================
static ArrayOf
getStructFieldValue(const ArrayOf& value, const std::string& fieldName, size_t index)
{
    if (!value.isField(fieldName) || index >= (size_t)value.elementCount()) {
        return ArrayOf::empty();
    }
    ArrayOfVector values = value.fieldAsList(fieldName);
    if (index >= values.size()) {
        return ArrayOf::empty();
    }
    return values[index];
}
//=============================================================================
static std::wstring
getTextElement(const ArrayOf& value, size_t index)
{
    if (value.isCell()) {
        const ArrayOf* cells = static_cast<const ArrayOf*>(value.dataPointer());
        if (index < (size_t)value.elementCount()) {
            return cells[index].asWideString();
        }
        return L"";
    }
    if (value.isStringArray() && value.elementCount() > 1) {
        const ArrayOf* strings = static_cast<const ArrayOf*>(value.dataPointer());
        if (index < (size_t)value.elementCount()) {
            return strings[index].asWideString();
        }
        return L"";
    }
    return value.asWideString();
}
//=============================================================================
static void
syncLinePropertiesCache(
    GOStackedLineChart* chart, const std::wstring& propertyName, const ArrayOf& value)
{
    std::string fieldName = linePropertyFieldName(propertyName);
    if (fieldName.empty()) {
        return;
    }
    ArrayOf lineProperties = chart->findArrayOfProperty(GO_LINE_PROPERTIES_PROPERTY_NAME_STR);
    if (!isUsableStructArray(lineProperties) || !lineProperties.isField(fieldName)) {
        return;
    }
    ArrayOfVector values = lineProperties.fieldAsList(fieldName);
    for (size_t k = 0; k < values.size(); ++k) {
        values[k] = value;
    }
    lineProperties.setFieldAsList(fieldName, values);
    GOGenericProperty* property = chart->findProperty(GO_LINE_PROPERTIES_PROPERTY_NAME_STR, false);
    if (property) {
        property->set(lineProperties);
    }
}
//=============================================================================
static std::string
linePropertyFieldName(const std::wstring& propertyName)
{
    if (propertyName == GO_COLOR_PROPERTY_NAME_STR) {
        return "Color";
    }
    if (propertyName == GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR) {
        return "MarkerFaceColor";
    }
    if (propertyName == GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR) {
        return "MarkerEdgeColor";
    }
    if (propertyName == GO_LINE_STYLE_PROPERTY_NAME_STR) {
        return "LineStyle";
    }
    if (propertyName == GO_LINE_WIDTH_PROPERTY_NAME_STR) {
        return "LineWidth";
    }
    if (propertyName == GO_MARKER_PROPERTY_NAME_STR) {
        return "Marker";
    }
    if (propertyName == GO_MARKER_SIZE_PROPERTY_NAME_STR) {
        return "MarkerSize";
    }
    return "";
}
//=============================================================================
static void
applyLineProperty(GraphicsObject* line, const std::string& fieldName,
    const std::wstring& propertyName, const ArrayOf& value, size_t lineIndex, size_t lineCount)
{
    ArrayOf lineValue = lineSpecificPropertyValue(value, fieldName, lineIndex, lineCount);
    if (!lineValue.isEmpty()) {
        setObjectProperty(line, propertyName, lineValue);
    }
}
//=============================================================================
static void
applyPlotType(GOStackedLineChart* chart, GraphicsObject* line, const ArrayOf& value,
    size_t lineIndex, size_t lineCount)
{
    ArrayOf lineValue = lineSpecificPropertyValue(value, "PlotType", lineIndex, lineCount);
    if (lineValue.isEmpty()) {
        return;
    }
    chart->preserveLineData(line);
    std::wstring plotType = lineValue.asWideString();
    std::transform(plotType.begin(), plotType.end(), plotType.begin(), ::towlower);
    if (plotType == L"scatter") {
        restoreOriginalLineData(chart, line);
        setObjectProperty(line, GO_LINE_STYLE_PROPERTY_NAME_STR, ArrayOf::characterArray(L"none"));
        GOGenericProperty* markerProperty = line->findProperty(GO_MARKER_PROPERTY_NAME_STR, false);
        if (markerProperty && markerProperty->get().asWideString() == L"none") {
            setObjectProperty(line, GO_MARKER_PROPERTY_NAME_STR, ArrayOf::characterArray(L"o"));
        }
    } else if (plotType == L"stairs") {
        applyStairsLineData(chart, line);
    } else if (plotType == L"plot") {
        restoreOriginalLineData(chart, line);
    }
}
//=============================================================================
static ArrayOf
lineSpecificPropertyValue(
    const ArrayOf& value, const std::string& fieldName, size_t lineIndex, size_t lineCount)
{
    if (lineCount <= 1 || value.isScalar() || value.isRowVectorCharacterArray()
        || value.isScalarStringArray()) {
        return value;
    }
    if (value.isNumeric() && !value.isComplex()) {
        return numericLineSpecificPropertyValue(value, fieldName, lineIndex, lineCount);
    }
    if (value.isCell() || value.isStringArray()) {
        return textLineSpecificPropertyValue(value, lineIndex, lineCount);
    }
    return value;
}
//=============================================================================
static ArrayOf
numericLineSpecificPropertyValue(
    const ArrayOf& value, const std::string& fieldName, size_t lineIndex, size_t lineCount)
{
    std::vector<double> values = value.asDoubleVector();
    if (isColorLineProperty(fieldName) && value.rows() == (indexType)lineCount
        && value.columns() == 3) {
        std::vector<double> rgb { values[lineIndex], values[lineIndex + lineCount],
            values[lineIndex + (2 * lineCount)] };
        return ArrayOf::doubleRowVector(rgb);
    }
    if (isScalarLineProperty(fieldName) && value.elementCount() == (indexType)lineCount) {
        return ArrayOf::doubleScalar(values[lineIndex]);
    }
    return value;
}
//=============================================================================
static ArrayOf
textLineSpecificPropertyValue(const ArrayOf& value, size_t lineIndex, size_t lineCount)
{
    if (value.elementCount() != (indexType)lineCount) {
        return value;
    }
    const ArrayOf* elements = static_cast<const ArrayOf*>(value.dataPointer());
    if (lineIndex >= (size_t)value.elementCount()) {
        return ArrayOf::empty();
    }
    if (value.isCell()) {
        return elements[lineIndex];
    }
    return ArrayOf::characterArray(elements[lineIndex].asWideString());
}
//=============================================================================
static void
restoreOriginalLineData(GOStackedLineChart* chart, GraphicsObject* line)
{
    std::pair<ArrayOf, ArrayOf> originalData = chart->originalLineData(line);
    if (!originalData.first.isEmpty()) {
        setObjectProperty(line, GO_X_DATA_PROPERTY_NAME_STR, originalData.first);
    }
    if (!originalData.second.isEmpty()) {
        setObjectProperty(line, GO_Y_DATA_PROPERTY_NAME_STR, originalData.second);
    }
}
//=============================================================================
static void
applyStairsLineData(GOStackedLineChart* chart, GraphicsObject* line)
{
    std::pair<ArrayOf, ArrayOf> originalData = chart->originalLineData(line);
    if (originalData.first.isEmpty() || originalData.second.isEmpty()) {
        return;
    }
    ArrayOf xStairs = stairsLineVector(originalData.first, originalData.second, true);
    ArrayOf yStairs = stairsLineVector(originalData.first, originalData.second, false);
    if (!xStairs.isEmpty() && !yStairs.isEmpty()) {
        setObjectProperty(line, GO_X_DATA_PROPERTY_NAME_STR, xStairs);
        setObjectProperty(line, GO_Y_DATA_PROPERTY_NAME_STR, yStairs);
    }
}
//=============================================================================
static ArrayOf
stairsLineVector(const ArrayOf& xData, const ArrayOf& yData, bool useXData)
{
    if (!xData.isNumeric() || !yData.isNumeric() || xData.isComplex() || yData.isComplex()
        || xData.elementCount() != yData.elementCount()) {
        return ArrayOf::empty();
    }
    std::vector<double> x = xData.asDoubleVector();
    std::vector<double> y = yData.asDoubleVector();
    if (x.empty()) {
        return ArrayOf::empty();
    }
    std::vector<double> values;
    values.reserve((x.size() * 2) - 1);
    values.push_back(useXData ? x[0] : y[0]);
    for (size_t k = 1; k < x.size(); ++k) {
        if (useXData) {
            values.push_back(x[k]);
            values.push_back(x[k]);
        } else {
            values.push_back(y[k - 1]);
            values.push_back(y[k]);
        }
    }
    return ArrayOf::doubleRowVector(values);
}
//=============================================================================
static bool
isColorLineProperty(const std::string& fieldName)
{
    return fieldName == "Color" || fieldName == "MarkerFaceColor" || fieldName == "MarkerEdgeColor";
}
//=============================================================================
static bool
isScalarLineProperty(const std::string& fieldName)
{
    return fieldName == "LineWidth" || fieldName == "MarkerSize";
}
//=============================================================================
} // namespace Nelson
//=============================================================================
