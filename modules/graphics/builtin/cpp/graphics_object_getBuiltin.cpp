//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "graphics_object_getBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "GraphicsObject.hpp"
#include "GOAxis.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORoot.hpp"
#include "GOStringOnOffProperty.hpp"
#include "FindObj.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "ClassdefParser.hpp"
#include "FunctionDef.hpp"
#include "Context.hpp"
#include <algorithm>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
// A pure on/off property reports a nelson.lang.OnOffSwitchState enumeration instead of a
// plain 'on'/'off' char. The instance comes from evaluating the class member accessor,
// which needs an Evaluator. Without one (or if the class cannot be loaded) the char value
// is returned unchanged. get() is not on the rendering path, so the extra call costs nothing
// while drawing.
static ArrayOf
onOffSwitchStateValue(Evaluator* eval, bool isOn)
{
    static const std::string className = "nelson.lang.OnOffSwitchState";
    if (eval != nullptr && classdefLoadClass(className)) {
        const std::string member = className + (isOn ? ".on" : ".off");
        FunctionDefPtr funcDef = nullptr;
        if (eval->getContext()->lookupFunction(member, funcDef) && funcDef != nullptr) {
            ArrayOfVector result = funcDef->evaluateFunction(eval, ArrayOfVector(), 1);
            if (!result.empty()) {
                return result[0];
            }
        }
    }
    return ArrayOf::characterArray(isOn ? GO_PROPERTY_VALUE_ON_STR : GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
static ArrayOf
onOffPropertyValue(Evaluator* eval, GOGenericProperty* property, const ArrayOf& value)
{
    if (dynamic_cast<GOOnOffProperty*>(property) == nullptr || !value.isRowVectorCharacterArray()) {
        return value;
    }
    const std::wstring text = value.asWideString();
    if (text == GO_PROPERTY_VALUE_ON_STR) {
        return onOffSwitchStateValue(eval, true);
    }
    if (text == GO_PROPERTY_VALUE_OFF_STR) {
        return onOffSwitchStateValue(eval, false);
    }
    return value;
}
//=============================================================================
static bool
isTiledLayoutSubplotTextFontSizeMode(GraphicsObject* fp, const std::wstring& propname)
{
    if (!fp || !fp->isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)
        || !StringHelpers::iequals(propname, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
        return false;
    }
    int64 parentHandle = fp->findGoProperty(GO_PARENT_PROPERTY_NAME_STR);
    GraphicsObject* parent = findGraphicsObject(parentHandle, false);
    return parent && parent->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)
        && fp->findProperty(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR, false);
}
//=============================================================================
static void
checkPropertyAppliesToObject(GraphicsObject* fp, const std::wstring& propname)
{
    GOAxis* axis = dynamic_cast<GOAxis*>(fp);
    if (axis && GOAxis::isPolarAxesOnlyProperty(propname) && !axis->isPolarAxes()) {
        Error(L"Nelson:graphics:unrecognizedProperty", L"Unrecognized property: %s", propname);
    }
    if (fp->isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
        if (isTiledLayoutSubplotTextFontSizeMode(fp, propname)) {
            return;
        }
        wstringVector fieldnames = fp->getVisibleFieldnames();
        if (std::find(fieldnames.begin(), fieldnames.end(), propname) == fieldnames.end()) {
            Error(L"Nelson:graphics:unrecognizedProperty", L"Unrecognized property: %s", propname);
        }
    }
}
//=============================================================================
// Children are stored back to front, the painting order. A container that stacks them
// reports the top object first, so the list is reversed on the way out. The stored list
// (GOGObjectsProperty::data()) is left untouched for internal consumers.
static ArrayOf
reverseGraphicsObjectArray(const ArrayOf& children)
{
    if (children.dataClass() != NLS_GO_HANDLE || children.elementCount() < 2) {
        return children;
    }
    const auto* handles = static_cast<const nelson_handle*>(children.dataPointer());
    indexType count = children.elementCount();
    auto* dp = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_GO_HANDLE, count);
    ArrayOf ret = ArrayOf(NLS_GO_HANDLE, Dimensions(count, 1), dp);
    for (indexType k = 0; k < count; ++k) {
        dp[k] = handles[count - 1 - k];
    }
    return ret;
}
//=============================================================================
// Children with HandleVisibility 'off' are excluded from the Children property (only
// allchild/findall expose them). The stored order and the raw handle list
// (GOGObjectsProperty::data()) are left untouched.
static ArrayOf
filterHandleVisibleChildren(const ArrayOf& children)
{
    if (children.dataClass() != NLS_GO_HANDLE || children.isEmpty()) {
        return children;
    }
    const auto* handles = static_cast<const nelson_handle*>(children.dataPointer());
    indexType count = children.elementCount();
    std::vector<int64> kept;
    kept.reserve(count);
    for (indexType k = 0; k < count; ++k) {
        GraphicsObject* child = graphicsObjectFromHandle(handles[k], false);
        if (child && isHandleVisible(child)) {
            kept.push_back(static_cast<int64>(handles[k]));
        }
    }
    if (static_cast<indexType>(kept.size()) == count) {
        return children;
    }
    if (kept.empty()) {
        Dimensions emptyDims(0, 0);
        return ArrayOf::emptyGraphicsObject(
            std::string("mat") + "lab" + ".graphics.GraphicsPlaceholder", emptyDims);
    }
    auto* dp = (nelson_handle*)ArrayOf::allocateArrayOf(NLS_GO_HANDLE, kept.size());
    ArrayOf ret = ArrayOf(NLS_GO_HANDLE, Dimensions(kept.size(), 1), dp);
    for (size_t i = 0; i < kept.size(); ++i) {
        dp[i] = kept[i];
    }
    return ret;
}
//=============================================================================
static ArrayOf
propertyValueForUser(
    Evaluator* eval, GraphicsObject* fp, GOGenericProperty* property, const std::wstring& propname)
{
    ArrayOf value = property->get();
    if (StringHelpers::iequals(propname, GO_CHILDREN_PROPERTY_NAME_STR)) {
        if (stacksChildren(fp)) {
            value = reverseGraphicsObjectArray(value);
        }
        value = filterHandleVisibleChildren(value);
        return value;
    }
    return onOffPropertyValue(eval, property, value);
}
//=============================================================================
// Properties stored as plain values: reading them never needs the deferred axes state.
static bool
isAxesStateIndependentProperty(const std::wstring& name)
{
    return name == GO_COLOR_ORDER_PROPERTY_NAME_STR || name == GO_COLOR_ORDER_INDEX_PROPERTY_NAME_STR
        || name == GO_LINE_STYLE_ORDER_PROPERTY_NAME_STR
        || name == GO_LINE_STYLE_ORDER_INDEX_PROPERTY_NAME_STR
        || name == GO_USER_DATA_PROPERTY_NAME_STR || name == GO_TAG_PROPERTY_NAME_STR;
}
//=============================================================================
ArrayOfVector
Nelson::GraphicsGateway::graphics_object_getBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    ArrayOf paramGo1 = argIn[0];
    if (paramGo1.dataClass() != NLS_GO_HANDLE) {
        Error(L"Nelson:graphics:expectedGraphicsObject:graphicsObjectExpected",
            L"graphics_object expected.");
    }
    Dimensions dims1 = paramGo1.dimensions();
    auto* ptrGO1 = (nelson_handle*)paramGo1.dataPointer();
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        if (!paramGo1.isScalar()) {
            Error(L"Nelson:graphics:expectedScalarGraphicsObject",
                L"Scalar graphics object expected.");
        }
        GraphicsObject* fp = graphicsObjectFromHandle(ptrGO1[0], true);
        // The attach path defers the computed state (limits, ticks, DataLimits), so flush it
        // before any property is read back.
        if (GOAxis* axis = dynamic_cast<GOAxis*>(fp)) {
            axis->updateStateIfDirty();
        }
        wstringVector fieldnames = fp->getVisibleFieldnames();
        ArrayOfVector values;
        values.reserve(fieldnames.size());
        for (const auto& name : fieldnames) {
            values << propertyValueForUser(eval, fp, fp->findProperty(name), name);
        }
        retval << ArrayOf::structScalar(fieldnames, values);
        return retval;
    }
    std::wstring requestedPropname = argIn[1].asWideString();
    for (size_t k = 0; k < dims1.elementCount(); k++) {
        auto handle = ptrGO1[k];
        GraphicsObject* fp = graphicsObjectFromHandle(handle, true);
        if (GOAxis* axis = dynamic_cast<GOAxis*>(fp)) {
            if (!isAxesStateIndependentProperty(requestedPropname)) {
                axis->updateStateIfDirty();
            }
        }
        if (handle == HANDLE_ROOT_OBJECT) {
            GORoot* root = static_cast<GORoot*>(fp);
            ArrayOf defaultValue;
            if (root->getDefaultProperty(requestedPropname, defaultValue)) {
                retval << defaultValue;
                continue;
            }
        }
        // Resolves an abbreviated name (get(gcf,'Pos')) like set() does. Each object has its
        // own property set, so this runs per handle.
        std::wstring propname = fp->resolvePartialPropertyName(requestedPropname);
        checkPropertyAppliesToObject(fp, propname);
        retval << propertyValueForUser(eval, fp, fp->findProperty(propname), propname);
    }
    return retval;
}
//=============================================================================
