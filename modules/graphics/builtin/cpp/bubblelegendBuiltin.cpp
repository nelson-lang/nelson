//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "bubblelegendBuiltin.hpp"
#include "axesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "GOAxis.hpp"
#include "GOBubbleLegend.hpp"
#include "GOCallbackProperty.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringHelpers.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson::GraphicsGateway {
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
static std::wstring
lowerString(const std::wstring& value)
{
    std::wstring lowered(value);
    StringHelpers::to_lower(lowered);
    return lowered;
}
//=============================================================================
static GraphicsObject*
objectFromHandle(int64 handle)
{
    if (handle >= HANDLE_OFFSET_OBJECT) {
        return findGraphicsObject(handle, false);
    }
    if (handle >= 0) {
        return static_cast<GraphicsObject*>(findGOFigure(handle));
    }
    return nullptr;
}
//=============================================================================
static bool
isAxes(GraphicsObject* object)
{
    return object && object->isType(GO_PROPERTY_VALUE_AXES_STR);
}
//=============================================================================
static bool
isFigure(GraphicsObject* object)
{
    return object && object->isType(GO_PROPERTY_VALUE_FIGURE_STR);
}
//=============================================================================
static int64
graphicsHandleValue(const ArrayOf& value, const std::wstring& name)
{
    if (!value.isGraphicsObject()) {
        Error(
            L"Nelson:bubblelegend:expectedGraphicsObject", L"%s must be a graphics object.", name);
    }
    return value.asGraphicsObjectScalar();
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
static int64
targetAxes(const ArrayOfVector& argIn, size_t& index)
{
    int64 peerHandle = currentAxesHandle();
    if (!argIn.empty() && argIn[0].isGraphicsObject()) {
        int64 candidate = argIn[0].asGraphicsObjectScalar();
        if (isAxes(objectFromHandle(candidate))) {
            peerHandle = candidate;
            index = 1;
        }
    }
    return peerHandle;
}
//=============================================================================
static std::wstring
canonicalPropertyName(GraphicsObject* object, const std::wstring& requested)
{
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
isBubbleLegendProperty(GraphicsObject* object, const ArrayOf& value)
{
    return object && isStringScalar(value)
        && object->haveProperty(canonicalPropertyName(object, stringValue(value)));
}
//=============================================================================
static bool
isSpecialBubbleLegendProperty(const std::wstring& name)
{
    std::wstring lowered = lowerString(name);
    return lowered == L"axes" || lowered == L"parent";
}
//=============================================================================
static void
applyProperties(GOBubbleLegend* legend, const ArrayOfVector& props)
{
    ArrayOfVector values(props);
    while (values.size() >= 2) {
        std::wstring requested = stringValue(values[0]);
        std::wstring propname = canonicalPropertyName(legend, requested);
        try {
            if (!legend->haveProperty(propname)) {
                Error(L"Nelson:bubblelegend:unrecognizedProperty", L"Unrecognized property: %s",
                    requested);
            }
            if (!legend->isWritable(propname)) {
                Error(L"Nelson:bubblelegend:readOnlyProperty", L"Property is read-only: %s",
                    propname);
            }
            legend->findProperty(propname)->set(values[1]);
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propname)
                    + L"\n" + e.what(),
                L"Nelson:bubblelegend:propertyError");
        }
        values.pop_front();
        values.pop_front();
    }
}
//=============================================================================
static ArrayOf
graphicsHandle(int64 handle)
{
    Dimensions dims(1, 1);
    nelson_handle* ptr = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, dims.elementCount(), stringVector(), false));
    ptr[0] = handle;
    return ArrayOf(NLS_GO_HANDLE, dims, ptr);
}
//=============================================================================
ArrayOfVector
bubblelegendBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 13);
    nargoutcheck(nLhs, 0, 1);
    size_t index = 0;
    int64 peerHandle = targetAxes(argIn, index);
    std::wstring title;
    GOBubbleLegend probe;
    if (((argIn.size() - index) % 2) == 1) {
        if (!isStringScalar(argIn[index])) {
            Error(L"Nelson:bubblelegend:invalidTitle", L"Invalid bubble legend title.");
        }
        title = stringValue(argIn[index]);
        index++;
    }
    ArrayOfVector props;
    int64 parentHandle = HANDLE_UNASSIGNED_OBJECT;
    while (index < argIn.size()) {
        if (index + 1 >= argIn.size()) {
            Error(L"Nelson:graphics:nameValueArgumentsExpected", L"Name-value arguments expected.");
        }
        if (!isStringScalar(argIn[index])) {
            Error(L"Nelson:bubblelegend:invalidPropertyName",
                L"Property name must be a string scalar.");
        }
        std::wstring requested = stringValue(argIn[index]);
        if (isSpecialBubbleLegendProperty(requested)) {
            int64 handle = graphicsHandleValue(argIn[index + 1], requested);
            GraphicsObject* object = objectFromHandle(handle);
            std::wstring lowered = lowerString(requested);
            if (lowered == L"axes") {
                if (!isAxes(object)) {
                    Error(L"Nelson:bubblelegend:invalidAxes", L"Axes must be an axes.");
                }
                peerHandle = handle;
            } else {
                if (!isFigure(object)) {
                    Error(L"Nelson:graphics:parentMustBeFigure", L"Parent must be a figure.");
                }
                parentHandle = handle;
            }
            index += 2;
            continue;
        }
        if (!isBubbleLegendProperty(&probe, argIn[index])) {
            Error(L"Nelson:bubblelegend:unrecognizedProperty", L"Unrecognized property: %s",
                requested);
        }
        props.push_back(argIn[index]);
        props.push_back(argIn[index + 1]);
        index += 2;
    }
    GraphicsObject* peer = findGraphicsObject(peerHandle, false);
    if (!isAxes(peer)) {
        Error(L"Nelson:graphics:targetMustBeAxes", L"Target must be an axes.");
    }
    int64 figureHandle = 0;
    GOFigure* fig = figureForPeer(peer, figureHandle);
    if (parentHandle >= HANDLE_OFFSET_OBJECT) {
        figureHandle = parentHandle;
        fig = dynamic_cast<GOFigure*>(objectFromHandle(parentHandle));
        if (!fig) {
            Error(L"Nelson:graphics:parentMustBeFigure", L"Parent must be a figure.");
        }
    }
    GOBubbleLegend* legend = new GOBubbleLegend;
    int64 legendHandle = assignGraphicsObject(legend);
    legend->setPeerAxesHandle(peerHandle);
    legend->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, figureHandle);
    legend->createTitle(legendHandle, title);
    legend->createLayoutOptions(legendHandle, L"");
    appendFigureChild(fig, legendHandle);
    applyProperties(legend, props);
    legend->updateState();
    GOCallbackProperty* createCallback = static_cast<GOCallbackProperty*>(
        legend->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR, false));
    if (createCallback) {
        createCallback->executeNow(legend);
    }
    fig->setRenderingStateInvalid(true);
    if (!fig->isDrawLater()) {
        fig->repaint();
    }
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << graphicsHandle(legendHandle);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
