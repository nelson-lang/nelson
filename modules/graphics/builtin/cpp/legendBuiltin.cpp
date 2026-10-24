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
#include "legendBuiltin.hpp"
#include "axesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "GOAxis.hpp"
#include "GOCallbackProperty.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOLegend.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
static std::wstring
lowerString(const std::wstring& value)
{
    std::wstring lowered(value);
    StringHelpers::to_lower(lowered);
    return lowered;
}
//=============================================================================
static bool
isStringScalar(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static std::wstring
stringValue(const ArrayOf& value)
{
    if (!isStringScalar(value)) {
        Error(L"Nelson:legend:expectedString", L"Expected a string scalar.");
    }
    return value.asWideString();
}
//=============================================================================
static GraphicsObject*
objectFromHandle(int64 handle)
{
    if (handle >= HANDLE_OFFSET_OBJECT) {
        return findGraphicsObject(handle, false);
    }
    if (handle != HANDLE_ROOT_OBJECT && handle != 0) {
        return static_cast<GraphicsObject*>(findGOFigure(handle));
    }
    return nullptr;
}
//=============================================================================
static int64
currentAxesHandle()
{
    int64 currentFigureID = getCurrentFigure();
    if (currentFigureID == NO_FIGURE) {
        currentFigureID = createNewFigure();
    }
    GOFigure* fig = getCurrentGOFigure();
    if (!fig) {
        Error(L"Nelson:graphics:invalidFigureHandle", L"Invalid figure handle.");
    }
    int64 currentAxes = fig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
    if (currentAxes < HANDLE_OFFSET_OBJECT || isDeletedGraphicsObject(currentAxes)) {
        ArrayOfVector args;
        ArrayOfVector out = axesBuiltin(1, args);
        currentAxes = out[0].asGraphicsObjectScalar();
    }
    return currentAxes;
}
//=============================================================================
static GOFigure*
figureForPeer(GraphicsObject* peer, int64& figureHandle)
{
    GOFigure* fig = peer ? peer->getParentFigure() : nullptr;
    if (!fig) {
        Error(L"Nelson:graphics:targetNotInFigure", L"Target must belong to a figure.");
    }
    figureHandle = findGraphicsObjectHandle(fig);
    if (figureHandle == -1) {
        Error(L"Nelson:graphics:targetNotInFigure", L"Target must belong to a figure.");
    }
    return fig;
}
//=============================================================================
static void
appendFigureChild(GOFigure* fig, int64 childHandle)
{
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> values = children->data();
    if (std::find(values.begin(), values.end(), childHandle) == values.end()) {
        values.push_back(childHandle);
        children->data(values);
    }
}
//=============================================================================
static bool
isAxesLike(GraphicsObject* object)
{
    return object
        && (object->isType(GO_PROPERTY_VALUE_AXES_STR)
            || object->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR));
}
//=============================================================================
static bool
isLegendableChild(GraphicsObject* child)
{
    if (!child) {
        return false;
    }
    if (child->haveProperty(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR)
        && lowerString(child->findStringProperty(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR))
            == GO_PROPERTY_VALUE_OFF_STR) {
        return false;
    }
    const std::wstring type = child->getType();
    return type == GO_PROPERTY_VALUE_LINE_STR || type == GO_PROPERTY_VALUE_STAIR_STR
        || type == GO_PROPERTY_VALUE_ANIMATED_LINE_STR || type == GO_PROPERTY_VALUE_SCATTER_STR
        || type == GO_PROPERTY_VALUE_COMPASS_PLOT_STR || type == GO_PROPERTY_VALUE_PATCH_STR
        || type == GO_PROPERTY_VALUE_SURFACE_STR || type == GO_PROPERTY_VALUE_CONTOUR_STR
        || type == GO_PROPERTY_VALUE_HGGROUP_STR || type == GO_PROPERTY_VALUE_HGTRANSFORM_STR
        || type == GO_PROPERTY_VALUE_IMAGE_STR || type == GO_PROPERTY_VALUE_BAR_STR
        || type == GO_PROPERTY_VALUE_BUBBLE_CHART_STR
        || type == GO_PROPERTY_VALUE_RAIN_CLOUD_PLOT_STR;
}
//=============================================================================
static std::vector<int64>
legendableChildren(GraphicsObject* peer)
{
    std::vector<int64> result;
    if (!peer) {
        return result;
    }
    GOGObjectsProperty* children = static_cast<GOGObjectsProperty*>(
        peer->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return result;
    }
    for (int64 childHandle : children->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (isLegendableChild(child)) {
            result.push_back(childHandle);
        }
    }
    return result;
}
//=============================================================================
static std::wstring
legendLabel(GraphicsObject* child, size_t index)
{
    std::wstring label;
    if (child && child->haveProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR)) {
        label = child->findStringProperty(GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    }
    if (label.empty()) {
        label = L"data " + std::to_wstring(index);
    }
    return label;
}
//=============================================================================
static std::vector<std::wstring>
defaultLabels(const std::vector<int64>& plotHandles)
{
    std::vector<std::wstring> labels;
    labels.reserve(plotHandles.size());
    for (size_t k = 0; k < plotHandles.size(); ++k) {
        labels.push_back(legendLabel(findGraphicsObject(plotHandles[k], false), k + 1));
    }
    return labels;
}
//=============================================================================
static std::vector<int64>
handlesFromGraphicsArray(const ArrayOf& value)
{
    if (!value.isGraphicsObject()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectHandleS",
            L"Expected graphics object handle(s).");
    }
    const nelson_handle* ptr = static_cast<const nelson_handle*>(value.dataPointer());
    std::vector<int64> handles;
    handles.reserve(value.elementCount());
    for (indexType k = 0; k < value.elementCount(); ++k) {
        handles.push_back(ptr[k]);
    }
    return handles;
}
//=============================================================================
static int64
parentAxesForPlot(int64 plotHandle)
{
    GraphicsObject* plot = objectFromHandle(plotHandle);
    if (!plot) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }
    GOGObjectsProperty* parent
        = static_cast<GOGObjectsProperty*>(plot->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parent || parent->data().empty()) {
        Error(L"Nelson:legend:plotNotInAxes", L"Plot object must belong to an axes.");
    }
    int64 parentHandle = parent->data()[0];
    GraphicsObject* parentObject = objectFromHandle(parentHandle);
    if (!isAxesLike(parentObject)) {
        Error(L"Nelson:legend:plotNotInAxes", L"Plot object must belong to an axes.");
    }
    return parentHandle;
}
//=============================================================================
static std::wstring
canonicalPropertyName(GraphicsObject* object, const std::wstring& requested)
{
    if (!object) {
        return requested;
    }
    if (object->haveProperty(requested)) {
        return requested;
    }
    std::wstring lowered = lowerString(requested);
    for (const auto& fieldName : object->fieldnames()) {
        if (lowerString(fieldName) == lowered) {
            return fieldName;
        }
    }
    return requested;
}
//=============================================================================
static bool
isLegendProperty(GraphicsObject* legend, const ArrayOf& value)
{
    if (!legend || !isStringScalar(value)) {
        return false;
    }
    return legend->haveProperty(canonicalPropertyName(legend, stringValue(value)));
}
//=============================================================================
static bool
isCommand(const std::wstring& value)
{
    static const std::vector<std::wstring> commands = { GO_PROPERTY_VALUE_OFF_STR, L"delete",
        L"hide", L"show", L"toggle", L"boxon", L"boxoff" };
    std::wstring token = lowerString(value);
    return std::find(commands.begin(), commands.end(), token) != commands.end();
}
//=============================================================================
static bool
isLocationToken(const std::wstring& value)
{
    static const std::vector<std::wstring> locations = { GO_PROPERTY_VALUE_NORTH_STR,
        GO_PROPERTY_VALUE_SOUTH_STR, GO_PROPERTY_VALUE_EAST_STR, GO_PROPERTY_VALUE_WEST_STR,
        L"northeast", L"northwest", L"southeast", L"southwest", GO_PROPERTY_VALUE_NORTHOUTSIDE_STR,
        GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR, GO_PROPERTY_VALUE_EASTOUTSIDE_STR,
        GO_PROPERTY_VALUE_WESTOUTSIDE_STR, L"northeastoutside", L"northwestoutside",
        L"southeastoutside", L"southwestoutside", L"best", L"bestoutside",
        GO_PROPERTY_VALUE_NONE_STR, L"layout", L"ne", L"nw", L"se", L"sw", L"n", L"s", L"e", L"w" };
    std::wstring token = lowerString(value);
    return std::find(locations.begin(), locations.end(), token) != locations.end();
}
//=============================================================================
static bool
isOrientationToken(const std::wstring& value)
{
    std::wstring token = lowerString(value);
    return token == GO_PROPERTY_VALUE_VERTICAL_STR || token == GO_PROPERTY_VALUE_HORIZONTAL_STR;
}
//=============================================================================
static std::vector<std::wstring>
labelsFromArray(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray() || value.isScalarStringArray()) {
        return { value.asWideString() };
    }
    if (value.isStringArray() || value.isCellArrayOfCharacterVectors()) {
        return value.asWideStringVector(false);
    }
    Error(L"Nelson:legend:invalidLabels",
        L"Labels must be character vectors, string arrays, or cell arrays of character vectors.");
    return {};
}
//=============================================================================
static int64
existingLegendHandle(GOFigure* fig, int64 peerHandle)
{
    if (!fig) {
        return HANDLE_UNASSIGNED_OBJECT;
    }
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return HANDLE_UNASSIGNED_OBJECT;
    }
    for (int64 child : children->data()) {
        GraphicsObject* object = findGraphicsObject(child, false);
        GOLegend* legend = dynamic_cast<GOLegend*>(object);
        if (legend && legend->getPeerAxesHandle() == peerHandle) {
            return child;
        }
    }
    return HANDLE_UNASSIGNED_OBJECT;
}
//=============================================================================
static GOLegend*
legendFromAxes(GraphicsObject* peer, int64& legendHandle)
{
    legendHandle = HANDLE_UNASSIGNED_OBJECT;
    if (!peer) {
        return nullptr;
    }
    GOGObjectsProperty* legendProperty
        = static_cast<GOGObjectsProperty*>(peer->findProperty(GO_LEGEND_PROPERTY_NAME_STR, false));
    if (legendProperty && !legendProperty->data().empty()) {
        int64 candidate = legendProperty->data()[0];
        GraphicsObject* object = findGraphicsObject(candidate, false);
        GOLegend* legend = dynamic_cast<GOLegend*>(object);
        if (legend) {
            legendHandle = candidate;
            return legend;
        }
    }
    int64 figureHandle = 0;
    GOFigure* fig = figureForPeer(peer, figureHandle);
    legendHandle = existingLegendHandle(fig, findGraphicsObjectHandle(peer));
    if (legendHandle >= HANDLE_OFFSET_OBJECT) {
        return dynamic_cast<GOLegend*>(findGraphicsObject(legendHandle, false));
    }
    return nullptr;
}
//=============================================================================
static void
setAxesLegend(GraphicsObject* peer, int64 legendHandle)
{
    if (!peer) {
        return;
    }
    GOGObjectsProperty* legendProperty
        = static_cast<GOGObjectsProperty*>(peer->findProperty(GO_LEGEND_PROPERTY_NAME_STR, false));
    if (legendProperty) {
        if (legendHandle >= HANDLE_OFFSET_OBJECT) {
            legendProperty->data({ legendHandle });
        } else {
            legendProperty->data({});
        }
    }
}
//=============================================================================
static void
deleteLegendForPeer(GraphicsObject* peer)
{
    int64 legendHandle = HANDLE_UNASSIGNED_OBJECT;
    GOLegend* legend = legendFromAxes(peer, legendHandle);
    if (legend && legendHandle >= HANDLE_OFFSET_OBJECT) {
        setAxesLegend(peer, HANDLE_UNASSIGNED_OBJECT);
        deleteGraphicsObject(legendHandle, true, true);
    }
}
//=============================================================================
static ArrayOf
graphicsHandleVector(const std::vector<int64>& handles)
{
    Dimensions dims(handles.size(), 1);
    nelson_handle* ptr = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, dims.elementCount(), stringVector(), false));
    for (indexType k = 0; k < dims.elementCount(); ++k) {
        ptr[k] = handles[k];
    }
    return ArrayOf(NLS_GO_HANDLE, dims, ptr);
}
//=============================================================================
static ArrayOf
emptyGraphicsHandleVector()
{
    Dimensions dims(0, 1);
    nelson_handle* ptr = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, 0, stringVector(), false));
    return ArrayOf(NLS_GO_HANDLE, dims, ptr);
}
//=============================================================================
static void
filterEmptyLabels(std::vector<int64>& plotHandles, std::vector<std::wstring>& labels)
{
    std::vector<int64> filteredHandles;
    std::vector<std::wstring> filteredLabels;
    size_t count = std::min(plotHandles.size(), labels.size());
    filteredHandles.reserve(count);
    filteredLabels.reserve(count);
    for (size_t k = 0; k < count; ++k) {
        if (!labels[k].empty()) {
            filteredHandles.push_back(plotHandles[k]);
            filteredLabels.push_back(labels[k]);
        }
    }
    plotHandles.swap(filteredHandles);
    labels.swap(filteredLabels);
}
//=============================================================================
static void
applyProperties(GOLegend* legend, const ArrayOfVector& props)
{
    ArrayOfVector values(props);
    while (values.size() >= 2) {
        std::wstring requested = stringValue(values[0]);
        std::wstring propname = canonicalPropertyName(legend, requested);
        try {
            if (!legend->haveProperty(propname)) {
                Error(
                    L"Nelson:legend:unrecognizedProperty", L"Unrecognized property: %s", requested);
            }
            if (!legend->isWritable(propname)) {
                Error(L"Nelson:legend:readOnlyProperty", L"Property is read-only: %s", propname);
            }
            legend->findProperty(propname)->set(values[1]);
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propname)
                    + L"\n" + e.what(),
                L"Nelson:legend:propertyError");
        }
        values.pop_front();
        values.pop_front();
    }
}
//=============================================================================
static int64
createOrUpdateLegend(int64 peerHandle, std::vector<int64> plotHandles,
    std::vector<std::wstring> labels, const ArrayOfVector& props)
{
    GraphicsObject* peer = findGraphicsObject(peerHandle, false);
    if (!isAxesLike(peer)) {
        Error(L"Nelson:graphics:targetMustBeAxes", L"Target must be an axes.");
    }
    filterEmptyLabels(plotHandles, labels);
    int64 figureHandle = 0;
    GOFigure* fig = figureForPeer(peer, figureHandle);
    int64 legendHandle = HANDLE_UNASSIGNED_OBJECT;
    GOLegend* legend = legendFromAxes(peer, legendHandle);
    bool isNewLegend = false;
    if (!legend) {
        legend = new GOLegend;
        legendHandle = assignGraphicsObject(legend);
        legend->setPeerAxesHandle(peerHandle);
        legend->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, figureHandle);
        legend->createTitle(legendHandle);
        legend->createLayoutOptions(legendHandle, L"");
        appendFigureChild(fig, legendHandle);
        setAxesLegend(peer, legendHandle);
        isNewLegend = true;
    }
    legend->setPlotHandles(plotHandles);
    legend->setLabels(labels);
    applyProperties(legend, props);
    legend->updateState();

    if (isNewLegend) {
        GOCallbackProperty* createCallback = static_cast<GOCallbackProperty*>(
            legend->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR, false));
        if (createCallback) {
            createCallback->executeNow(legend);
        }
    }
    fig->setRenderingStateInvalid(true);
    if (!fig->isDrawLater()) {
        fig->repaint();
    }
    return legendHandle;
}
//=============================================================================
static void
executeLegendCommand(GraphicsObject* peer, const std::wstring& command)
{
    int64 legendHandle = HANDLE_UNASSIGNED_OBJECT;
    GOLegend* legend = legendFromAxes(peer, legendHandle);
    std::wstring token = lowerString(command);
    if (token == GO_PROPERTY_VALUE_OFF_STR || token == L"delete") {
        deleteLegendForPeer(peer);
        return;
    }
    if (!legend) {
        return;
    }
    if (token == L"hide") {
        legend->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)
            ->set(ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR));
    } else if (token == L"show") {
        legend->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)
            ->set(ArrayOf::characterArray(GO_PROPERTY_VALUE_ON_STR));
    } else if (token == L"toggle") {
        std::wstring visible = legend->findStringProperty(GO_VISIBLE_PROPERTY_NAME_STR);
        legend->findProperty(GO_VISIBLE_PROPERTY_NAME_STR)
            ->set(ArrayOf::characterArray(visible == GO_PROPERTY_VALUE_ON_STR
                    ? GO_PROPERTY_VALUE_OFF_STR
                    : GO_PROPERTY_VALUE_ON_STR));
    } else if (token == L"boxon") {
        legend->findProperty(GO_BOX_PROPERTY_NAME_STR)
            ->set(ArrayOf::characterArray(GO_PROPERTY_VALUE_ON_STR));
    } else if (token == L"boxoff") {
        legend->findProperty(GO_BOX_PROPERTY_NAME_STR)
            ->set(ArrayOf::characterArray(GO_PROPERTY_VALUE_OFF_STR));
    }
    legend->updateState();
    GOFigure* fig = legend->getParentFigure();
    if (fig) {
        fig->setRenderingStateInvalid(true);
        if (!fig->isDrawLater()) {
            fig->repaint();
        }
    }
}
//=============================================================================
static void
parseTarget(const ArrayOfVector& argIn, size_t& index, int64& peerHandle, GraphicsObject*& peer,
    std::vector<int64>& explicitPlots)
{
    peerHandle = currentAxesHandle();
    peer = findGraphicsObject(peerHandle, false);
    if (argIn.empty() || !argIn[0].isGraphicsObject()) {
        return;
    }
    std::vector<int64> handles = handlesFromGraphicsArray(argIn[0]);
    if (handles.empty()) {
        explicitPlots = handles;
        index = 1;
        return;
    }
    GraphicsObject* firstObject = objectFromHandle(handles[0]);
    if (handles.size() == 1 && isAxesLike(firstObject)) {
        peerHandle = handles[0];
        peer = firstObject;
        index = 1;
        return;
    }
    if (handles.size() == 1 && firstObject && firstObject->isType(GO_PROPERTY_VALUE_LEGEND_STR)) {
        GOLegend* legend = static_cast<GOLegend*>(firstObject);
        peerHandle = legend->getPeerAxesHandle();
        peer = findGraphicsObject(peerHandle, false);
        index = 1;
        return;
    }
    for (int64 handle : handles) {
        GraphicsObject* object = objectFromHandle(handle);
        if (!isLegendableChild(object)) {
            Error(L"Nelson:legend:expectedAxesOrPlot", L"Expected axes or plot object handle(s).");
        }
    }
    explicitPlots = handles;
    peerHandle = parentAxesForPlot(handles[0]);
    peer = findGraphicsObject(peerHandle, false);
    index = 1;
}
//=============================================================================
static void
parsePlotHandles(
    const ArrayOfVector& argIn, size_t& index, int64 peerHandle, std::vector<int64>& explicitPlots)
{
    if (index >= argIn.size() || !argIn[index].isGraphicsObject()) {
        return;
    }
    std::vector<int64> handles = handlesFromGraphicsArray(argIn[index]);
    for (int64 handle : handles) {
        GraphicsObject* object = objectFromHandle(handle);
        if (!isLegendableChild(object)) {
            Error(L"Nelson:legend:expectedPlot", L"Expected plot object handle(s).");
        }
        if (parentAxesForPlot(handle) != peerHandle) {
            Error(L"Nelson:legend:plotNotInTargetAxes",
                L"Plot object must belong to the target axes.");
        }
    }
    explicitPlots = handles;
    ++index;
}
//=============================================================================
static void
parseInputs(const ArrayOfVector& argIn, size_t index, GOLegend* propertyProbe,
    std::vector<std::wstring>& labels, ArrayOfVector& props)
{
    while (index < argIn.size()) {
        const ArrayOf& current = argIn[index];
        if (isLegendProperty(propertyProbe, current)) {
            break;
        }
        if (!isStringScalar(current) && !current.isStringArray()
            && !current.isCellArrayOfCharacterVectors()) {
            Error(L"Nelson:legend:invalidArgument", L"Invalid legend argument.");
        }
        if ((current.isStringArray() && !current.isScalarStringArray())
            || current.isCellArrayOfCharacterVectors()) {
            std::vector<std::wstring> values = labelsFromArray(current);
            labels.insert(labels.end(), values.begin(), values.end());
            ++index;
            continue;
        }
        std::wstring token = stringValue(current);
        if (labels.empty() && index + 1 == argIn.size() && isLocationToken(token)) {
            props.push_back(ArrayOf::characterArray(GO_LOCATION_PROPERTY_NAME_STR));
            props.push_back(ArrayOf::characterArray(token));
            ++index;
            continue;
        }
        if (labels.empty() && index + 1 == argIn.size() && isOrientationToken(token)) {
            props.push_back(ArrayOf::characterArray(GO_ORIENTATION_PROPERTY_NAME_STR));
            props.push_back(ArrayOf::characterArray(token));
            ++index;
            continue;
        }
        labels.push_back(token);
        ++index;
    }
    while (index < argIn.size()) {
        if (index + 1 >= argIn.size()) {
            Error(L"Nelson:graphics:nameValuePairsExpected",
                L"Name-value arguments must occur in pairs.");
        }
        props.push_back(argIn[index++]);
        props.push_back(argIn[index++]);
    }
}
//=============================================================================
static ArrayOf
labelCellRow(const std::vector<std::wstring>& labels)
{
    Dimensions dims(1, labels.size());
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount(), stringVector(), false));
    for (indexType k = 0; k < dims.elementCount(); ++k) {
        elements[k] = ArrayOf::characterArray(labels[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, elements);
}
//=============================================================================
static ArrayOfVector
legendReturn(int nLhs, int64 legendHandle, const std::vector<int64>& plotHandles,
    const std::vector<std::wstring>& labels)
{
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << graphicsObjectHandle(legendHandle);
    }
    if (nLhs > 1) {
        retval << emptyGraphicsHandleVector();
    }
    if (nLhs > 2) {
        retval << graphicsHandleVector(plotHandles);
    }
    if (nLhs > 3) {
        retval << labelCellRow(labels);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
legendBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 4);

    size_t index = 0;
    int64 peerHandle = HANDLE_UNASSIGNED_OBJECT;
    GraphicsObject* peer = nullptr;
    std::vector<int64> explicitPlots;
    parseTarget(argIn, index, peerHandle, peer, explicitPlots);
    if (!isAxesLike(peer)) {
        Error(L"Nelson:graphics:targetMustBeAxes", L"Target must be an axes.");
    }
    parsePlotHandles(argIn, index, peerHandle, explicitPlots);

    if (index + 1 == argIn.size() && isStringScalar(argIn[index])
        && isCommand(stringValue(argIn[index]))) {
        executeLegendCommand(peer, stringValue(argIn[index]));
        ArrayOfVector retval;
        if (nLhs > 0) {
            int64 legendHandle = HANDLE_UNASSIGNED_OBJECT;
            GOLegend* legend = legendFromAxes(peer, legendHandle);
            if (legend) {
                retval << graphicsObjectHandle(legendHandle);
            } else {
                retval << ArrayOf::empty();
            }
        }
        return retval;
    }

    GOLegend propertyProbe;
    std::vector<std::wstring> labels;
    ArrayOfVector props;
    parseInputs(argIn, index, &propertyProbe, labels, props);

    std::vector<int64> plotHandles
        = explicitPlots.empty() ? legendableChildren(peer) : explicitPlots;
    if (labels.empty()) {
        labels = defaultLabels(plotHandles);
    }
    if (labels.size() < plotHandles.size()) {
        plotHandles.resize(labels.size());
    } else if (labels.size() > plotHandles.size()) {
        labels.resize(plotHandles.size());
    }
    filterEmptyLabels(plotHandles, labels);

    int64 legendHandle = createOrUpdateLegend(peerHandle, plotHandles, labels, props);
    return legendReturn(nLhs, legendHandle, plotHandles, labels);
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
