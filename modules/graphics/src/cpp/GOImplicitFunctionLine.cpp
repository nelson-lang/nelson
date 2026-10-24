//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOImplicitFunctionLine.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOSymbolProperty.hpp"
#include <vector>
//=============================================================================
namespace Nelson {
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
GOImplicitFunctionLine::GOImplicitFunctionLine() : GOFunctionContour()
{
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    // Line-series marker/color properties. The implicit function line is built
    // through the contour chain, which does not register them, so add them here.
    registerProperty(new GOAutoManualProperty, GO_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MARKER_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MARKER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, -1, -1, -1);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1, -1, -1);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR))
        ->setValue(1);
    setScalarPositiveIntegerValueDefault(GO_MESH_DENSITY_PROPERTY_NAME_STR, 151);
    setVectorDoubleDefault(GO_LEVEL_LIST_PROPERTY_NAME_STR, { 0. });
    setStringDefault(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringColorDefault(
        GO_LINE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0.066, 0.443, 0.745);
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, 0.066, 0.443, 0.745);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    sortProperties();
}
//=============================================================================
GOImplicitFunctionLine::~GOImplicitFunctionLine() = default;
//=============================================================================
std::wstring
GOImplicitFunctionLine::getType()
{
    return GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_LINE_STR;
}
//=============================================================================
void
GOImplicitFunctionLine::updateState()
{
    setVectorDoubleDefault(GO_LEVEL_LIST_PROPERTY_NAME_STR, { 0. });
    setStringDefault(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    if (hasChanged(GO_COLOR_PROPERTY_NAME_STR)) {
        copyProperty(this, GO_COLOR_PROPERTY_NAME_STR, GO_LINE_COLOR_PROPERTY_NAME_STR);
        copyProperty(this, GO_COLOR_PROPERTY_NAME_STR, GO_EDGE_COLOR_PROPERTY_NAME_STR);
        clearChanged(GO_COLOR_PROPERTY_NAME_STR);
    }
    GOFunctionContour::updateState();
}
//=============================================================================
std::vector<double>
GOImplicitFunctionLine::getLimits()
{
    // Frames the solution set it drew, not the sampling range: x^2 + y^2 - 4 keeps
    // the limits on the circle of radius 2, not on [-5 5]. The range-based extent of
    // GOFunctionContour is skipped on purpose.
    return GOContour::getLimits();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
