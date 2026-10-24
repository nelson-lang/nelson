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
#include "colorbarBuiltin.hpp"
#include "axesBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "GOAxis.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorBar.hpp"
#include "GOFigure.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOText.hpp"
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
static bool
isDeleteCommand(const std::wstring& value)
{
    std::wstring token = lowerString(value);
    return token == GO_PROPERTY_VALUE_OFF_STR || token == L"delete" || token == L"hide";
}
//=============================================================================
static bool
isLocationToken(const std::wstring& value)
{
    static const std::vector<std::wstring> locations
        = { GO_PROPERTY_VALUE_NORTH_STR, GO_PROPERTY_VALUE_SOUTH_STR, GO_PROPERTY_VALUE_EAST_STR,
              GO_PROPERTY_VALUE_WEST_STR, GO_PROPERTY_VALUE_NORTHOUTSIDE_STR,
              GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR, GO_PROPERTY_VALUE_EASTOUTSIDE_STR,
              GO_PROPERTY_VALUE_WESTOUTSIDE_STR, GO_PROPERTY_VALUE_MANUAL_STR };
    std::wstring token = lowerString(value);
    return std::find(locations.begin(), locations.end(), token) != locations.end();
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
static std::vector<int64>
colorBarsForPeer(GOFigure* fig, int64 peerHandle)
{
    std::vector<int64> result;
    if (!fig) {
        return result;
    }
    GOGObjectsProperty* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return result;
    }
    for (int64 child : children->data()) {
        GraphicsObject* object = findGraphicsObject(child, false);
        GOColorBar* colorBar = dynamic_cast<GOColorBar*>(object);
        if (colorBar && colorBar->getPeerAxesHandle() == peerHandle) {
            result.push_back(child);
        }
    }
    return result;
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
createLabelForColorBar(int64 colorBarHandle, GOColorBar* colorBar)
{
    return colorBar->createLabel(colorBarHandle);
}
//=============================================================================
static void
deleteColorBars(const std::vector<int64>& handles)
{
    for (int64 handle : handles) {
        if (handle >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(handle)) {
            deleteGraphicsObject(handle, true, true);
        }
    }
}
//=============================================================================
static void
applyProperties(GOColorBar* colorBar, const ArrayOfVector& props)
{
    ArrayOfVector values(props);
    while (values.size() >= 2) {
        std::wstring propname(values[0].asWideString());
        try {
            if (!colorBar->isWritable(propname) && colorBar->haveProperty(propname)) {
                Error(L"Nelson:colorbar:readOnlyProperty", L"Property is read-only: %s", propname);
            }
            colorBar->findProperty(propname)->set(values[1]);
        } catch (const NelsonException& e) {
            RaiseError(formatI18nMessage(
                           Nelson::resolveMessageTemplate(
                               L"Nelson:graphics:gotErrorProperty", L"Got error for property: %s"),
                           propname)
                    + L"\n" + e.what(),
                L"Nelson:colorbar:propertyError");
        }
        values.pop_front();
        values.pop_front();
    }
}
//=============================================================================
static ArrayOfVector
emptyReturn(int nLhs)
{
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << ArrayOf::empty();
    }
    return retval;
}
//=============================================================================
static int64
createColorBar(int64 peerHandle, const std::wstring& location, const ArrayOfVector& props)
{
    GraphicsObject* peer = findGraphicsObject(peerHandle, false);
    if (!peer
        || (!peer->isType(GO_PROPERTY_VALUE_AXES_STR)
            && !peer->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR))) {
        Error(L"Nelson:graphics:targetMustBeAxes", L"Target must be an axes.");
    }
    int64 figureHandle = 0;
    GOFigure* fig = figureForPeer(peer, figureHandle);
    std::vector<int64> oldColorBars = colorBarsForPeer(fig, peerHandle);

    GOColorBar* colorBar = nullptr;
    int64 colorBarHandle = 0;
    for (auto it = oldColorBars.begin(); it != oldColorBars.end(); ++it) {
        GraphicsObject* oldObject = findGraphicsObject(*it, false);
        colorBar = dynamic_cast<GOColorBar*>(oldObject);
        if (colorBar) {
            colorBarHandle = *it;
            oldColorBars.erase(it);
            break;
        }
    }

    const bool reusedColorBar = colorBar != nullptr;
    if (!reusedColorBar) {
        colorBar = new GOColorBar;
        colorBarHandle = assignGraphicsObject(colorBar);
        colorBar->setGoProperty(GO_PARENT_PROPERTY_NAME_STR, figureHandle);
        createLabelForColorBar(colorBarHandle, colorBar);
        appendFigureChild(fig, colorBarHandle);
        colorBar->createLayoutOptions(colorBarHandle, L"");
    }
    colorBar->setPeerAxesHandle(peerHandle);

    if (!location.empty()) {
        colorBar->findProperty(GO_LOCATION_PROPERTY_NAME_STR)
            ->set(ArrayOf::characterArray(location));
    }
    applyProperties(colorBar, props);
    if (colorBar->findStringProperty(GO_LOCATION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_MANUAL_STR
        && !colorBar->hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        colorBar->setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    }
    deleteColorBars(oldColorBars);
    colorBar->applyLocationToPeerAxes();
    colorBar->clearChanged(GO_LOCATION_PROPERTY_NAME_STR);
    colorBar->updateState();

    GOCallbackProperty* createCallback
        = static_cast<GOCallbackProperty*>(colorBar->findProperty(GO_CREATE_FCN_PROPERTY_NAME_STR));
    if (createCallback && !reusedColorBar) {
        createCallback->executeNow(colorBar);
    }
    fig->setRenderingStateInvalid(true);
    if (!fig->isDrawLater()) {
        fig->repaint();
    }
    return colorBarHandle;
}
//=============================================================================
static void
parseTarget(const ArrayOfVector& argIn, size_t& index, int64& targetHandle, GraphicsObject*& target)
{
    if (argIn.empty()) {
        targetHandle = currentAxesHandle();
        target = findGraphicsObject(targetHandle, false);
        return;
    }
    if (isStringScalar(argIn[0]) && lowerString(argIn[0].asWideString()) == L"peer") {
        if (argIn.size() < 2 || !argIn[1].isGraphicsObject()) {
            Error(L"Nelson:colorbar:expectedAxesAfterPeer", L"Expected axes handle after 'peer'.");
        }
        targetHandle = argIn[1].asGraphicsObjectScalar();
        target = objectFromHandle(targetHandle);
        index = 2;
        return;
    }
    if (argIn[0].isGraphicsObject()) {
        targetHandle = argIn[0].asGraphicsObjectScalar();
        target = objectFromHandle(targetHandle);
        index = 1;
        return;
    }
    targetHandle = currentAxesHandle();
    target = findGraphicsObject(targetHandle, false);
}
//=============================================================================
static std::wstring
parseLocation(const ArrayOfVector& argIn, size_t& index)
{
    if (index >= argIn.size() || !isStringScalar(argIn[index])) {
        return {};
    }
    std::wstring token = lowerString(argIn[index].asWideString());
    if (token == GO_PROPERTY_VALUE_HORIZONTAL_STR) {
        ++index;
        return GO_PROPERTY_VALUE_SOUTHOUTSIDE_STR;
    }
    if (token == GO_PROPERTY_VALUE_VERTICAL_STR) {
        ++index;
        return GO_PROPERTY_VALUE_EASTOUTSIDE_STR;
    }
    if (isLocationToken(token)) {
        ++index;
        return token;
    }
    return {};
}
//=============================================================================
ArrayOfVector
colorbarBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0);
    nargoutcheck(nLhs, 0, 1);

    size_t index = 0;
    int64 targetHandle = 0;
    GraphicsObject* target = nullptr;
    parseTarget(argIn, index, targetHandle, target);
    if (!target) {
        Error(L"Nelson:graphics:invalidHandle", L"Invalid handle.");
    }

    if (index < argIn.size() && isStringScalar(argIn[index])
        && isDeleteCommand(argIn[index].asWideString())) {
        if (GOColorBar* colorBar = dynamic_cast<GOColorBar*>(target)) {
            deleteGraphicsObject(targetHandle, true, true);
        } else {
            GOFigure* fig = target->getParentFigure();
            deleteColorBars(colorBarsForPeer(fig, targetHandle));
        }
        return emptyReturn(nLhs);
    }

    if (GOColorBar* colorBar = dynamic_cast<GOColorBar*>(target)) {
        ArrayOfVector retval;
        if (nLhs > 0) {
            retval << graphicsObjectHandle(targetHandle);
        }
        return retval;
    }

    if (!target->isType(GO_PROPERTY_VALUE_AXES_STR)
        && !target->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR)) {
        Error(L"Nelson:graphics:targetMustBeAxes", L"Target must be an axes.");
    }

    std::wstring location = parseLocation(argIn, index);
    ArrayOfVector props;
    while (index < argIn.size()) {
        props.push_back(argIn[index++]);
    }
    if (props.size() % 2 != 0) {
        Error(L"Nelson:graphics:nameValuePairsExpected",
            L"Name-value arguments must occur in pairs.");
    }

    int64 handle = createColorBar(targetHandle, location, props);
    ArrayOfVector retval;
    if (nLhs > 0) {
        retval << graphicsObjectHandle(handle);
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
