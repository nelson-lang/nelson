//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOStem.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "GOVectorProperty.hpp"
//=============================================================================
// Internal, non-visible property holding the delegated renderer group state.
// Kept local so the shared property-name header does not force a module-wide
// recompile.
#define GO_RENDERER_STATE_PROPERTY_NAME_STR L"RendererState"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
addStemProperty(
    GraphicsObject* go, GOGenericProperty* property, const std::wstring& name, bool writable = true)
{
    go->registerProperty(property, name, writable);
}
//=============================================================================
GOStem::GOStem()
{
    addStemProperty(this, new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOVectorProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoManualProperty, GO_Z_DATA_MODE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_Z_VARIABLE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_Z_DATA_SOURCE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::Baseline),
        GO_BASE_LINE_PROPERTY_NAME_STR, false);
    addStemProperty(this, new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoManualProperty, GO_COLOR_MODE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoManualProperty, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addStemProperty(this, new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoManualProperty, GO_MARKER_MODE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    addStemProperty(this, new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    addStemProperty(this, new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    addStemProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    addStemProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addStemProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addStemProperty(this, new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    addStemProperty(this, new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addStemProperty(this, new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    addStemProperty(this, new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    addStemProperty(this, new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    addStemProperty(this, new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    addStemProperty(this, new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    addStemProperty(this, new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    addStemProperty(this, new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    addStemProperty(this, new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    addStemProperty(this, new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    addStemProperty(this, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    addStemProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addStemProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR, false);
    addStemProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    addStemProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR, false);
    addStemProperty(this, new GOScalarProperty, GO_BASE_VALUE_PROPERTY_NAME_STR);
    addStemProperty(this, new GOOnOffProperty, GO_SHOW_BASE_LINE_PROPERTY_NAME_STR);
    // Hidden slot for the handles of the delegated renderer group (the hggroup that
    // paints the stems and markers). Non-visible, so it stays out of
    // properties()/fieldnames()/display and UserData stays free for the user.
    registerProperty(new GOArrayOfProperty, GO_RENDERER_STATE_PROPERTY_NAME_STR, true, false);

    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Z_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_X_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Z_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setRestrictedStringDefault(GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, L"o");
    setRestrictedStringDefault(GO_MARKER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6.);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, -1., -1., -1.);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1., -1., -1.);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_STEM_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setScalarDoubleDefault(GO_BASE_VALUE_PROPERTY_NAME_STR, 0.);
    setRestrictedStringDefault(GO_SHOW_BASE_LINE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR))
        ->setValue(1.);
    clearAllChanged();
    sortProperties();
}
//=============================================================================
std::wstring
GOStem::getType()
{
    return GO_PROPERTY_VALUE_STEM_STR;
}
//=============================================================================
void
GOStem::paintMe(RenderInterface& gc)
{
}
//=============================================================================
} // namespace Nelson
//=============================================================================
