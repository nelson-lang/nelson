//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOCommonProperties.hpp"
#include <algorithm>
#include "NelsonSettingsScope.hpp"
#include "GraphicsEnvironment.hpp"
#include "ArrayOf.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAxesToolbarProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOChartDecorators.hpp"
#include "GOMappingModeProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOThemeProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOWindowStyleProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOFigure.hpp"
#include "GORoot.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
    bool writable = true, bool visible = true)
{
    if (go->haveProperty(name)) {
        delete property;
        return;
    }
    go->registerProperty(property, name, writable, visible);
}
//=============================================================================
static void
addArray(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOArrayOfProperty, name);
}
//=============================================================================
static void
addAutoManual(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOAutoManualProperty, name);
}
//=============================================================================
static void
addCallback(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOCallbackProperty, name);
}
//=============================================================================
static void
addFourVector(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOFourVectorProperty, name);
}
//=============================================================================
static void
addGo(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOGObjectsProperty, name);
}
//=============================================================================
static void
addLayoutOptions(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOLayoutOptionsProperty, name);
}
//=============================================================================
static void
addAnnotationEventData(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData), name);
}
//=============================================================================
static void
addDataTipTemplate(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate), name);
}
//=============================================================================
static void
addSourceTable(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOSourceTableProperty, name);
}
//=============================================================================
static void
addOnOff(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOOnOffProperty, name);
}
//=============================================================================
static void
addPickableParts(GraphicsObject* go)
{
    addProperty(go, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
}
//=============================================================================
static void
addScalar(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOScalarProperty, name);
}
//=============================================================================
static void
addSeriesIndex(GraphicsObject* go)
{
    addProperty(go, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
}
//=============================================================================
static void
addString(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOStringProperty, name);
}
//=============================================================================
static void
addTheme(GraphicsObject* go)
{
    addProperty(go, new GOThemeProperty, GO_THEME_PROPERTY_NAME_STR);
}
//=============================================================================
static void
addLimitMethod(GraphicsObject* go, const std::wstring& name)
{
    static const wchar_t* limitMethodValues[] = { GO_PROPERTY_VALUE_TICKALIGNED_STR,
        GO_PROPERTY_VALUE_TIGHT_STR, GO_PROPERTY_VALUE_PADDED_STR, nullptr };
    addProperty(go, new GORestrictedStringProperty(limitMethodValues), name);
}
//=============================================================================
static void
addTwoVector(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOTwoVectorProperty, name);
}
//=============================================================================
static void
addUnits(GraphicsObject* go, const std::wstring& name)
{
    addProperty(go, new GOUnitsProperty, name);
}
//=============================================================================
static void
setPointerShapeCData(GraphicsObject* go)
{
    Dimensions dims(16, 16);
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dims.elementCount()));
    std::fill(data, data + dims.elementCount(), 1.0);
    go->findProperty(GO_POINTER_SHAPE_C_DATA_PROPERTY_NAME_STR)
        ->set(ArrayOf(NLS_DOUBLE, dims, data));
}
//=============================================================================
static void
registerInteractionProperties(GraphicsObject* go, bool withClipping)
{
    addAnnotationEventData(go, GO_ANNOTATION_PROPERTY_NAME_STR);
    addCallback(go, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    if (withClipping) {
        addOnOff(go, GO_CLIPPING_PROPERTY_NAME_STR);
    }
    addGo(go, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addProperty(go, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addOnOff(go, GO_HIT_TEST_PROPERTY_NAME_STR);
    addPickableParts(go);
    addOnOff(go, GO_SELECTED_PROPERTY_NAME_STR);
    addOnOff(go, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerRootProperties(GraphicsObject* go)
{
    addString(go, GO_FIXED_WIDTH_FONT_NAME_PROPERTY_NAME_STR);
    addProperty(go, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addProperty(go, new GOArrayOfProperty, GO_MONITOR_POSITIONS_PROPERTY_NAME_STR, false);
    addOnOff(go, GO_SHOW_HIDDEN_HANDLES_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerFigureProperties(GraphicsObject* go)
{
    addOnOff(go, GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR);
    addOnOff(go, GO_CLIPPING_PROPERTY_NAME_STR);
    addGo(go, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addString(go, GO_CURRENT_CHARACTER_PROPERTY_NAME_STR);
    addTwoVector(go, GO_CURRENT_POINT_PROPERTY_NAME_STR);
    addOnOff(go, GO_DOCK_CONTROLS_PROPERTY_NAME_STR);
    addString(go, GO_FILE_NAME_PROPERTY_NAME_STR);
    addString(go, GO_ICON_PROPERTY_NAME_STR);
    addFourVector(go, GO_INNER_POSITION_PROPERTY_NAME_STR);
    addOnOff(go, GO_INTEGER_HANDLE_PROPERTY_NAME_STR);
    addOnOff(go, GO_INVERT_HARDCOPY_PROPERTY_NAME_STR);
    addFourVector(go, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    addString(go, GO_PAPER_ORIENTATION_PROPERTY_NAME_STR);
    addFourVector(go, GO_PAPER_POSITION_PROPERTY_NAME_STR);
    addAutoManual(go, GO_PAPER_POSITION_MODE_PROPERTY_NAME_STR);
    addTwoVector(go, GO_PAPER_SIZE_PROPERTY_NAME_STR);
    addString(go, GO_PAPER_TYPE_PROPERTY_NAME_STR);
    addUnits(go, GO_PAPER_UNITS_PROPERTY_NAME_STR);
    addString(go, GO_POINTER_PROPERTY_NAME_STR);
    addArray(go, GO_POINTER_SHAPE_C_DATA_PROPERTY_NAME_STR);
    addTwoVector(go, GO_POINTER_SHAPE_HOT_SPOT_PROPERTY_NAME_STR);
    addString(go, GO_RENDERER_PROPERTY_NAME_STR);
    addAutoManual(go, GO_RENDERER_MODE_PROPERTY_NAME_STR);
    addOnOff(go, GO_SCROLLABLE_PROPERTY_NAME_STR);
    addString(go, GO_SELECTION_TYPE_PROPERTY_NAME_STR);
    addTheme(go);
    addCallback(go, GO_THEME_CHANGED_FCN_PROPERTY_NAME_STR);
    addAutoManual(go, GO_THEME_MODE_PROPERTY_NAME_STR);
    addUnits(go, GO_UNITS_PROPERTY_NAME_STR);
    addCallback(go, GO_WINDOW_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    addCallback(go, GO_WINDOW_BUTTON_MOTION_FCN_PROPERTY_NAME_STR);
    addCallback(go, GO_WINDOW_BUTTON_UP_FCN_PROPERTY_NAME_STR);
    addCallback(go, GO_WINDOW_KEY_PRESS_FCN_PROPERTY_NAME_STR);
    addCallback(go, GO_WINDOW_KEY_RELEASE_FCN_PROPERTY_NAME_STR);
    addCallback(go, GO_WINDOW_SCROLL_WHEEL_FCN_PROPERTY_NAME_STR);
    addProperty(go, new GOWindowStyleProperty, GO_WINDOW_STYLE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerAxesProperties(GraphicsObject* go)
{
    addString(go, GO_ALPHA_SCALE_PROPERTY_NAME_STR);
    addString(go, GO_BOX_STYLE_PROPERTY_NAME_STR);
    addProperty(
        go, new GOTwoVectorProperty, GO_BUBBLE_LIMITS_INTERNAL_PROPERTY_NAME_STR, true, false);
    addProperty(go, new GOAutoManualProperty, GO_BUBBLE_LIMITS_MODE_INTERNAL_PROPERTY_NAME_STR,
        true, false);
    addProperty(
        go, new GOTwoVectorProperty, GO_BUBBLE_SIZE_RANGE_INTERNAL_PROPERTY_NAME_STR, true, false);
    addCallback(go, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    addString(go, GO_CLIPPING_STYLE_PROPERTY_NAME_STR);
    addString(go, GO_COLOR_SCALE_PROPERTY_NAME_STR);
    addGo(go, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addArray(go, GO_CURRENT_POINT_PROPERTY_NAME_STR);
    addAutoManual(go, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    addOnOff(go, GO_FONT_SMOOTHING_PROPERTY_NAME_STR);
    addAutoManual(go, GO_GRID_ALPHA_MODE_PROPERTY_NAME_STR);
    addAutoManual(go, GO_GRID_COLOR_MODE_PROPERTY_NAME_STR);
    addScalar(go, GO_GRID_LINE_WIDTH_PROPERTY_NAME_STR);
    addAutoManual(go, GO_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR);
    addFourVector(go, GO_INNER_POSITION_PROPERTY_NAME_STR);
    addArray(go, GO_INTERACTION_OPTIONS_PROPERTY_NAME_STR);
    addArray(go, GO_INTERACTIONS_PROPERTY_NAME_STR);
    // hidden flag read by the figure navigation; defaults to 'on'
    addProperty(go, new GOOnOffProperty, GO_DEFAULT_INTERACTIVITY_INTERNAL_PROPERTY_NAME_STR, true,
        false);
    addScalar(go, GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR);
    addGo(go, GO_LEGEND_PROPERTY_NAME_STR);
    addString(go, GO_LINE_STYLE_CYCLING_METHOD_PROPERTY_NAME_STR);
    addScalar(go, GO_MINOR_GRID_ALPHA_PROPERTY_NAME_STR);
    addAutoManual(go, GO_MINOR_GRID_ALPHA_MODE_PROPERTY_NAME_STR);
    addProperty(go, new GOColorProperty, GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR);
    addAutoManual(go, GO_MINOR_GRID_COLOR_MODE_PROPERTY_NAME_STR);
    addScalar(go, GO_MINOR_GRID_LINE_WIDTH_PROPERTY_NAME_STR);
    addAutoManual(go, GO_MINOR_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR);
    addScalar(go, GO_NEXT_SERIES_INDEX_PROPERTY_NAME_STR);
    addPickableParts(go);
    addString(go, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    addString(go, GO_SORT_METHOD_PROPERTY_NAME_STR);
    addGo(go, GO_SUBTITLE_PROPERTY_NAME_STR);
    addString(go, GO_SUBTITLE_FONT_WEIGHT_PROPERTY_NAME_STR);
    addString(go, GO_TITLE_FONT_WEIGHT_PROPERTY_NAME_STR);
    addScalar(go, GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR);
    addString(go, GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    addProperty(go, new GOAxesToolbarProperty, GO_TOOLBAR_PROPERTY_NAME_STR);
    addGo(go, GO_X_AXIS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_X_COLOR_MODE_PROPERTY_NAME_STR);
    addLimitMethod(go, GO_X_LIMIT_METHOD_PROPERTY_NAME_STR);
    addOnOff(go, GO_X_MINOR_TICK_PROPERTY_NAME_STR);
    addScalar(go, GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    addAutoManual(go, GO_X_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    addGo(go, GO_Y_AXIS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Y_COLOR_MODE_PROPERTY_NAME_STR);
    addLimitMethod(go, GO_Y_LIMIT_METHOD_PROPERTY_NAME_STR);
    addOnOff(go, GO_Y_MINOR_TICK_PROPERTY_NAME_STR);
    addScalar(go, GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Y_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
    addGo(go, GO_Z_AXIS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Z_COLOR_MODE_PROPERTY_NAME_STR);
    addLimitMethod(go, GO_Z_LIMIT_METHOD_PROPERTY_NAME_STR);
    addOnOff(go, GO_Z_MINOR_TICK_PROPERTY_NAME_STR);
    addScalar(go, GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Z_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerLineProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addOnOff(go, GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR);
    addOnOff(go, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_COLOR_MODE_PROPERTY_NAME_STR);
    addString(go, GO_LINE_JOIN_PROPERTY_NAME_STR);
    addAutoManual(go, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    addArray(go, GO_MARKER_INDICES_PROPERTY_NAME_STR);
    addAutoManual(go, GO_MARKER_MODE_PROPERTY_NAME_STR);
    addString(go, GO_R_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_R_VARIABLE_PROPERTY_NAME_STR);
    addScalar(go, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    addString(go, GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_THETA_VARIABLE_PROPERTY_NAME_STR);
    addDataTipTemplate(go, GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);
    addSourceTable(go, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
addAnimatedLineSeriesIndex(GraphicsObject* go)
{
    addSeriesIndex(go);
}
//=============================================================================
static void
registerAnimatedLineProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addAnimatedLineSeriesIndex(go);
}
//=============================================================================
static void
registerScatterProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addProperty(go, new GOMappingModeProperty, GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR);
    addAutoManual(go, GO_ALPHA_DATA_MODE_PROPERTY_NAME_STR);
    addString(go, GO_ALPHA_VARIABLE_PROPERTY_NAME_STR);
    addString(go, GO_C_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_COLOR_VARIABLE_PROPERTY_NAME_STR);
    addDataTipTemplate(go, GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);
    addSeriesIndex(go);
    addAutoManual(go, GO_SIZE_DATA_MODE_PROPERTY_NAME_STR);
    addString(go, GO_SIZE_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_SIZE_VARIABLE_PROPERTY_NAME_STR);
    addSourceTable(go, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    addString(go, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_X_JITTER_PROPERTY_NAME_STR);
    addString(go, GO_X_JITTER_DIRECTION_PROPERTY_NAME_STR);
    addScalar(go, GO_X_JITTER_WIDTH_PROPERTY_NAME_STR);
    addAutoManual(go, GO_X_JITTER_WIDTH_MODE_PROPERTY_NAME_STR);
    addString(go, GO_X_VARIABLE_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    addString(go, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_Y_JITTER_PROPERTY_NAME_STR);
    addString(go, GO_Y_JITTER_DIRECTION_PROPERTY_NAME_STR);
    addScalar(go, GO_Y_JITTER_WIDTH_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Y_JITTER_WIDTH_MODE_PROPERTY_NAME_STR);
    addString(go, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Z_DATA_MODE_PROPERTY_NAME_STR);
    addString(go, GO_Z_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_Z_JITTER_PROPERTY_NAME_STR);
    addString(go, GO_Z_JITTER_DIRECTION_PROPERTY_NAME_STR);
    addScalar(go, GO_Z_JITTER_WIDTH_PROPERTY_NAME_STR);
    addAutoManual(go, GO_Z_JITTER_WIDTH_MODE_PROPERTY_NAME_STR);
    addString(go, GO_Z_VARIABLE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerSurfaceProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addOnOff(go, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR);
    addString(go, GO_C_DATA_SOURCE_PROPERTY_NAME_STR);
    addDataTipTemplate(go, GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);
    addString(go, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addArray(go, GO_FACE_NORMALS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_FACE_NORMALS_MODE_PROPERTY_NAME_STR);
    addAutoManual(go, GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR);
    addString(go, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_Z_DATA_SOURCE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerPatchProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addOnOff(go, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_C_DATA_MODE_PROPERTY_NAME_STR);
    addArray(go, GO_FACE_NORMALS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_FACE_NORMALS_MODE_PROPERTY_NAME_STR);
    addArray(go, GO_FACE_VERTEX_ALPHA_DATA_PROPERTY_NAME_STR);
    addAutoManual(go, GO_FACE_VERTEX_C_DATA_MODE_PROPERTY_NAME_STR);
    addString(go, GO_LINE_JOIN_PROPERTY_NAME_STR);
    addSeriesIndex(go);
    addAutoManual(go, GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerRectangleProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addSeriesIndex(go);
}
//=============================================================================
static void
registerConstantLineProperties(GraphicsObject* go)
{
    // A ConstantLine has no Clipping property; it always spans the axes.
    registerInteractionProperties(go, false);
    addSeriesIndex(go);
}
//=============================================================================
static void
registerImageProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addString(go, GO_INTERPOLATION_PROPERTY_NAME_STR);
    addString(go, GO_MAX_RENDERED_RESOLUTION_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerLightProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, false);
}
//=============================================================================
static void
registerTextProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addOnOff(go, GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR);
    addAutoManual(go, GO_COLOR_MODE_PROPERTY_NAME_STR);
    addOnOff(go, GO_EDITING_PROPERTY_NAME_STR);
    addArray(go, GO_INTERACTIONS_PROPERTY_NAME_STR);
    addSeriesIndex(go);
}
//=============================================================================
static void
registerContourProperties(GraphicsObject* go)
{
    registerInteractionProperties(go, true);
    addDataTipTemplate(go, GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR);
    addString(go, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    addString(go, GO_Z_DATA_SOURCE_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerUIControlProperties(GraphicsObject* go)
{
    addGo(go, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addFourVector(go, GO_EXTENT_PROPERTY_NAME_STR);
    addFourVector(go, GO_INNER_POSITION_PROPERTY_NAME_STR);
    addFourVector(go, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    addString(go, GO_TAG_PROPERTY_NAME_STR);
}
//=============================================================================
static void
registerStackedLineChartProperties(GraphicsObject* go)
{
    addProperty(go, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addFourVector(go, GO_INNER_POSITION_PROPERTY_NAME_STR);
    addLayoutOptions(go, GO_LAYOUT_PROPERTY_NAME_STR);
    addFourVector(go, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    addFourVector(go, GO_POSITION_PROPERTY_NAME_STR);
    addString(go, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    addUnits(go, GO_UNITS_PROPERTY_NAME_STR);
}
//=============================================================================
static void
setString(GraphicsObject* go, const std::wstring& name, const std::wstring& value)
{
    GOGenericProperty* property = go->findProperty(name, false);
    if (property) {
        property->set(ArrayOf::characterArray(value));
        property->clearModified();
    }
}
//=============================================================================
static void
setScalar(GraphicsObject* go, const std::wstring& name, double value)
{
    if (go->findProperty(name, false)) {
        go->setScalarDoubleDefault(name, value);
        go->findProperty(name)->clearModified();
    }
}
//=============================================================================
static void
setSeriesIndex(GraphicsObject* go, double value)
{
    GOGenericProperty* property = go->findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR, false);
    if (!property) {
        return;
    }
    if (auto* seriesIndex = dynamic_cast<GOSeriesIndexProperty*>(property)) {
        seriesIndex->setValue(value);
        seriesIndex->clearModified();
        return;
    }
    setScalar(go, GO_SERIES_INDEX_PROPERTY_NAME_STR, value);
}
//=============================================================================
static void
setSeriesIndexNone(GraphicsObject* go)
{
    auto* seriesIndex = dynamic_cast<GOSeriesIndexProperty*>(
        go->findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR, false));
    if (seriesIndex) {
        seriesIndex->setNone();
        seriesIndex->clearModified();
    }
}
//=============================================================================
static void
setTwo(GraphicsObject* go, const std::wstring& name, double x, double y)
{
    if (go->findProperty(name, false)) {
        go->setTwoVectorDefault(name, x, y);
        go->findProperty(name)->clearModified();
    }
}
//=============================================================================
static void
setFour(GraphicsObject* go, const std::wstring& name, double x, double y, double z, double w)
{
    if (go->findProperty(name, false)) {
        go->setFourVectorDefault(name, x, y, z, w);
        go->findProperty(name)->clearModified();
    }
}
//=============================================================================
static void
initializeInteractionProperties(GraphicsObject* go, bool withClipping)
{
    if (withClipping) {
        setString(go, GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    }
    setString(go, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setString(go, GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setString(go, GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setString(go, GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setString(go, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
}
//=============================================================================
static void
initializeRootProperties(GraphicsObject* go)
{
    setString(go, GO_FIXED_WIDTH_FONT_NAME_PROPERTY_NAME_STR, L"Courier New");
    setString(go, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setString(go, GO_SHOW_HIDDEN_HANDLES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    std::vector<double> screen = go->findVectorDoubleProperty(GO_SCREEN_SIZE_PROPERTY_NAME_STR);
    if (screen.size() == 4) {
        GOArrayOfProperty* property = static_cast<GOArrayOfProperty*>(
            go->findProperty(GO_MONITOR_POSITIONS_PROPERTY_NAME_STR, false));
        if (property) {
            property->data(ArrayOf::doubleRowVector(screen));
        }
    }
}
//=============================================================================
static std::wstring
defaultFigureWindowStyle()
{
    // desktop preference: new figures open docked into the desktop as tabs.
    // Ignored when no desktop hosts the docks (CLI, advanced CLI).
    if (GraphicsEnvironment::desktopPrefersDockedFigures()) {
        return GO_PROPERTY_VALUE_DOCKED_STR;
    }
    return GO_PROPERTY_VALUE_NORMAL_STR;
}
//=============================================================================
static void
initializeFigureProperties(GraphicsObject* go)
{
    setString(go, GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setString(go, GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setTwo(go, GO_CURRENT_POINT_PROPERTY_NAME_STR, 0, 0);
    setString(go, GO_DOCK_CONTROLS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    std::vector<double> defaultPosition = defaultFigurePosition();
    setFour(go, GO_INNER_POSITION_PROPERTY_NAME_STR, defaultPosition[0], defaultPosition[1],
        defaultPosition[2], defaultPosition[3]);
    setString(go, GO_INTEGER_HANDLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setString(go, GO_INVERT_HARDCOPY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setFour(go, GO_OUTER_POSITION_PROPERTY_NAME_STR, defaultPosition[0], defaultPosition[1],
        defaultPosition[2], defaultPosition[3]);
    setString(go, GO_PAPER_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PORTRAIT_STR);
    setFour(go, GO_PAPER_POSITION_PROPERTY_NAME_STR, -0.863854846666666, 8.01052034333333,
        22.7277083333333, 13.6789583333333);
    setString(go, GO_PAPER_POSITION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwo(go, GO_PAPER_SIZE_PROPERTY_NAME_STR, 20.99999864, 29.69999902);
    setString(go, GO_PAPER_TYPE_PROPERTY_NAME_STR, L"A4");
    setString(go, GO_PAPER_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTIMETERS_STR);
    setString(go, GO_POINTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ARROW_STR);
    setPointerShapeCData(go);
    setTwo(go, GO_POINTER_SHAPE_HOT_SPOT_PROPERTY_NAME_STR, 1, 1);
    setString(go, GO_RENDERER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OPENGL_STR);
    setString(go, GO_RENDERER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_SCROLLABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setString(go, GO_SELECTION_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setString(go, GO_THEME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LIGHT_STR);
    setString(go, GO_THEME_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR);
    setString(go, GO_WINDOW_STYLE_PROPERTY_NAME_STR, defaultFigureWindowStyle());
}
//=============================================================================
static void
initializeAxesProperties(GraphicsObject* go)
{
    setString(go, GO_ALPHA_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setString(go, GO_BOX_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BACK_STR);
    setTwo(go, GO_BUBBLE_LIMITS_INTERNAL_PROPERTY_NAME_STR, 0, 1);
    setString(go, GO_BUBBLE_LIMITS_MODE_INTERNAL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setTwo(go, GO_BUBBLE_SIZE_RANGE_INTERNAL_PROPERTY_NAME_STR, 3, 50);
    setString(go, GO_CLIPPING_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_3DBOX_STR);
    setString(go, GO_COLOR_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setString(go, GO_FONT_SIZE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_FONT_SMOOTHING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setString(go, GO_GRID_ALPHA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_GRID_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalar(go, GO_GRID_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setString(go, GO_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setFour(go, GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setScalar(go, GO_LABEL_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR, 1.1);
    setString(go, GO_LINE_STYLE_CYCLING_METHOD_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AFTERCOLOR_STR);
    setScalar(go, GO_MINOR_GRID_ALPHA_PROPERTY_NAME_STR, 0.15);
    setString(go, GO_MINOR_GRID_ALPHA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    if (go->findProperty(GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR, false)) {
        go->setThreeVectorDefault(GO_MINOR_GRID_COLOR_PROPERTY_NAME_STR, 0.1, 0.1, 0.1);
    }
    setString(go, GO_MINOR_GRID_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalar(go, GO_MINOR_GRID_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setString(go, GO_MINOR_GRID_LINE_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalar(go, GO_NEXT_SERIES_INDEX_PROPERTY_NAME_STR, 1);
    setString(go, GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setString(go, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    setString(go, GO_SORT_METHOD_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CHILDORDER_STR);
    setString(go, GO_SUBTITLE_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setString(go, GO_TITLE_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOLD_STR);
    setScalar(go, GO_TITLE_FONT_SIZE_MULTIPLIER_PROPERTY_NAME_STR, 1.1);
    setString(go, GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
    setString(go, GO_X_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_X_LIMIT_METHOD_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TICKALIGNED_STR);
    setString(go, GO_X_MINOR_TICK_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setScalar(go, GO_X_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, 0);
    setString(go, GO_X_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_Y_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_Y_LIMIT_METHOD_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TICKALIGNED_STR);
    setString(go, GO_Y_MINOR_TICK_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setScalar(go, GO_Y_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, 0);
    setString(go, GO_Y_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_Z_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_Z_LIMIT_METHOD_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TICKALIGNED_STR);
    setString(go, GO_Z_MINOR_TICK_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setScalar(go, GO_Z_TICK_LABEL_ROTATION_PROPERTY_NAME_STR, 0);
    setString(go, GO_Z_TICK_LABEL_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
}
//=============================================================================
static void
initializeSeriesProperties(GraphicsObject* go, bool withClipping)
{
    initializeInteractionProperties(go, withClipping);
    setString(go, GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setString(go, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setString(go, GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_LINE_JOIN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ROUND_STR);
    setString(go, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_MARKER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setString(go, GO_R_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setSeriesIndex(go, 1);
    setString(go, GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
}
//=============================================================================
static void
initializeAnimatedLineProperties(GraphicsObject* go)
{
    initializeInteractionProperties(go, true);
    GOSeriesIndexProperty* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(go->findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    if (seriesIndex) {
        seriesIndex->setNone();
        seriesIndex->clearModified();
    }
}
//=============================================================================
static void
initializeDataSourceProperties(GraphicsObject* go)
{
    setString(go, GO_C_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setString(go, GO_X_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setString(go, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setString(go, GO_Z_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
}
//=============================================================================
static void
initializeByType(GraphicsObject* go, const std::wstring& type)
{
    if (type == GO_PROPERTY_VALUE_ROOT_STR) {
        initializeRootProperties(go);
    } else if (type == GO_PROPERTY_VALUE_FIGURE_STR) {
        initializeFigureProperties(go);
    } else if (type == GO_PROPERTY_VALUE_AXES_STR) {
        initializeAxesProperties(go);
    } else if (type == GO_PROPERTY_VALUE_LINE_STR || type == GO_PROPERTY_VALUE_STAIR_STR) {
        initializeSeriesProperties(go, true);
    } else if (type == GO_PROPERTY_VALUE_ANIMATED_LINE_STR) {
        initializeAnimatedLineProperties(go);
    } else if (type == GO_PROPERTY_VALUE_SCATTER_STR
        || type == GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
        initializeSeriesProperties(go, true);
        initializeDataSourceProperties(go);
        setString(go, GO_ALPHA_DATA_MAPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
        setString(go, GO_ALPHA_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setSeriesIndex(go, 1);
        setString(go, GO_SIZE_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_SIZE_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
        setString(go, GO_X_JITTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
        setString(go, GO_X_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
        setScalar(go, GO_X_JITTER_WIDTH_PROPERTY_NAME_STR, 0.9);
        setString(go, GO_X_JITTER_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_Z_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_Y_JITTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
        setString(go, GO_Y_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
        setScalar(go, GO_Y_JITTER_WIDTH_PROPERTY_NAME_STR, 0.9);
        setString(go, GO_Y_JITTER_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_Z_JITTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
        setString(go, GO_Z_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
        setScalar(go, GO_Z_JITTER_WIDTH_PROPERTY_NAME_STR, 0.9);
        setString(go, GO_Z_JITTER_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    } else if (type == GO_PROPERTY_VALUE_SURFACE_STR
        || type == GO_PROPERTY_VALUE_FUNCTION_SURFACE_STR) {
        initializeSeriesProperties(go, true);
        initializeDataSourceProperties(go);
        setString(go, GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
        setString(go, GO_FACE_NORMALS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    } else if (type == GO_PROPERTY_VALUE_PATCH_STR) {
        initializeSeriesProperties(go, true);
        setSeriesIndexNone(go);
        setString(go, GO_C_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_FACE_NORMALS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_FACE_VERTEX_C_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setString(go, GO_LINE_JOIN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MITER_STR);
        setString(go, GO_VERTEX_NORMALS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    } else if (type == GO_PROPERTY_VALUE_RECTANGLE_STR) {
        initializeInteractionProperties(go, true);
        setSeriesIndexNone(go);
    } else if (type == GO_PROPERTY_VALUE_CONSTANTLINE_STR) {
        initializeInteractionProperties(go, false);
        setSeriesIndexNone(go);
    } else if (type == GO_PROPERTY_VALUE_IMAGE_STR) {
        initializeInteractionProperties(go, true);
        setString(go, GO_INTERPOLATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NEAREST_STR);
        setString(go, GO_MAX_RENDERED_RESOLUTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    } else if (type == GO_PROPERTY_VALUE_LIGHT_STR) {
        initializeInteractionProperties(go, false);
    } else if (type == GO_PROPERTY_VALUE_TEXT_STR) {
        initializeSeriesProperties(go, true);
        setSeriesIndexNone(go);
        setString(go, GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
        setString(go, GO_EDITING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    } else if (type == GO_PROPERTY_VALUE_CONTOUR_STR) {
        initializeInteractionProperties(go, true);
        initializeDataSourceProperties(go);
    } else if (type == GO_PROPERTY_VALUE_HGGROUP_STR || type == GO_PROPERTY_VALUE_HGTRANSFORM_STR) {
        initializeInteractionProperties(go, false);
    } else if (type == GO_PROPERTY_VALUE_UICONTROL_STR) {
        setFour(go, GO_EXTENT_PROPERTY_NAME_STR, 0, 0, 0, 0);
        setFour(go, GO_INNER_POSITION_PROPERTY_NAME_STR, 20, 20, 60, 20);
        setFour(go, GO_OUTER_POSITION_PROPERTY_NAME_STR, 20, 20, 60, 20);
        setString(go, GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    } else if (type == GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR) {
        setString(go, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
        setFour(go, GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
        setFour(go, GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
        setFour(go, GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
        setString(
            go, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
        setString(go, GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    }
}
//=============================================================================
void
registerCommonProperties(GraphicsObject* go)
{
    if (!go) {
        return;
    }
    std::wstring type = go->getType();
    if (type == GO_PROPERTY_VALUE_ROOT_STR) {
        registerRootProperties(go);
    } else if (type == GO_PROPERTY_VALUE_FIGURE_STR) {
        registerFigureProperties(go);
    } else if (type == GO_PROPERTY_VALUE_AXES_STR) {
        registerAxesProperties(go);
    } else if (type == GO_PROPERTY_VALUE_LINE_STR || type == GO_PROPERTY_VALUE_STAIR_STR) {
        registerLineProperties(go);
    } else if (type == GO_PROPERTY_VALUE_ANIMATED_LINE_STR) {
        registerAnimatedLineProperties(go);
    } else if (type == GO_PROPERTY_VALUE_SCATTER_STR
        || type == GO_PROPERTY_VALUE_BUBBLE_CHART_STR) {
        registerScatterProperties(go);
    } else if (type == GO_PROPERTY_VALUE_SURFACE_STR
        || type == GO_PROPERTY_VALUE_FUNCTION_SURFACE_STR) {
        registerSurfaceProperties(go);
    } else if (type == GO_PROPERTY_VALUE_PATCH_STR) {
        registerPatchProperties(go);
    } else if (type == GO_PROPERTY_VALUE_RECTANGLE_STR) {
        registerRectangleProperties(go);
    } else if (type == GO_PROPERTY_VALUE_CONSTANTLINE_STR) {
        registerConstantLineProperties(go);
    } else if (type == GO_PROPERTY_VALUE_IMAGE_STR) {
        registerImageProperties(go);
    } else if (type == GO_PROPERTY_VALUE_LIGHT_STR) {
        registerLightProperties(go);
    } else if (type == GO_PROPERTY_VALUE_TEXT_STR) {
        registerTextProperties(go);
    } else if (type == GO_PROPERTY_VALUE_CONTOUR_STR) {
        registerContourProperties(go);
    } else if (type == GO_PROPERTY_VALUE_HGGROUP_STR || type == GO_PROPERTY_VALUE_HGTRANSFORM_STR) {
        registerInteractionProperties(go, false);
    } else if (type == GO_PROPERTY_VALUE_UICONTROL_STR) {
        registerUIControlProperties(go);
    } else if (type == GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
        addGo(go, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    } else if (type == GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR) {
        registerStackedLineChartProperties(go);
    }
}
//=============================================================================
void
initializeCommonProperties(GraphicsObject* go)
{
    if (!go) {
        return;
    }
    initializeByType(go, go->getType());
    if (go->getType() != GO_PROPERTY_VALUE_ROOT_STR) {
        GORoot* root = getGraphicsRootObject();
        root->rememberFactoryDefaults(go);
        root->applyDefaultProperties(go);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
