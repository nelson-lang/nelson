//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GORuler.hpp"
#include "GOAxis.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOHelpers.hpp"
#include "GOColorProperty.hpp"
#include "GOLinearLogProperty.hpp"
#include "GONormalReverseProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOInOutProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOCommonProperties.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Ruler properties that mirror a side's Y state, paired with the writer the
// axes uses to push an edit back. Label is intentionally excluded: it shares the
// side's YLabel text object handle, so editing it edits the axes directly.
static const wchar_t* RULER_MAPPED_PROPERTIES[] = { GO_LIMITS_PROPERTY_NAME_STR,
    GO_LIMITS_MODE_PROPERTY_NAME_STR, GO_COLOR_PROPERTY_NAME_STR, GO_SCALE_PROPERTY_NAME_STR,
    GO_DIRECTION_PROPERTY_NAME_STR, GO_TICK_VALUES_PROPERTY_NAME_STR,
    GO_TICK_VALUES_MODE_PROPERTY_NAME_STR, GO_TICK_LABELS_PROPERTY_NAME_STR,
    GO_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_FONT_SIZE_PROPERTY_NAME_STR,
    GO_FONT_NAME_PROPERTY_NAME_STR, GO_FONT_WEIGHT_PROPERTY_NAME_STR,
    GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_TICK_DIRECTION_PROPERTY_NAME_STR,
    GO_TICK_LENGTH_PROPERTY_NAME_STR, GO_FONT_UNITS_PROPERTY_NAME_STR,
    GO_LINE_WIDTH_PROPERTY_NAME_STR, GO_TICK_DIR_PROPERTY_NAME_STR, GO_GRID_COLOR_PROPERTY_NAME_STR,
    GO_GRID_ALPHA_PROPERTY_NAME_STR, GO_BOX_PROPERTY_NAME_STR, GO_UNITS_PROPERTY_NAME_STR,
    GO_TICK_LABEL_FORMAT_PROPERTY_NAME_STR };
//=============================================================================
GORuler::GORuler()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GORuler::~GORuler() = default;
//=============================================================================
std::wstring
GORuler::getType()
{
    return (m_valueKind == RULER_VALUE_DATETIME) ? GO_PROPERTY_VALUE_DATETIME_RULER_STR
                                                 : GO_PROPERTY_VALUE_NUMERIC_RULER_STR;
}
//=============================================================================
std::string
GORuler::getClassName()
{
    return (m_valueKind == RULER_VALUE_DATETIME) ? GO_CLASS_NAME_DATETIME_RULER_STR
                                                 : GO_CLASS_NAME_NUMERIC_RULER_STR;
}
//=============================================================================
void
GORuler::setValueKind(int valueKind)
{
    m_valueKind = (valueKind == RULER_VALUE_DATETIME) ? RULER_VALUE_DATETIME : RULER_VALUE_NUMERIC;
    // Keeps Type in step with getType() so get(ruler,'Type') reports the current kind.
    GOStringProperty* typeProperty
        = static_cast<GOStringProperty*>(findProperty(GO_TYPE_PROPERTY_NAME_STR, false));
    if (typeProperty) {
        typeProperty->data(getType());
        clearChanged(GO_TYPE_PROPERTY_NAME_STR);
    }
}
//=============================================================================
void
GORuler::constructProperties()
{
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GONormalReverseProperty, GO_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LIMITS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOLinearLogProperty, GO_SCALE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_TICK_VALUES_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TICK_VALUES_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty(true), GO_TICK_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TICK_LABELS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    // Font, TickDir and TickLength mirror the parent axes.
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOInOutProperty, GO_TICK_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_TICK_LENGTH_PROPERTY_NAME_STR);
    // Shared parent axes state. The plotly export reads it from the ruler on the yyaxis path.
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOInOutProperty, GO_TICK_DIR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_GRID_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_GRID_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    // Type reports the ruler kind (numericruler / datetimeruler); read-only, kept
    // in step with getType() by setValueKind().
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    // Date/datetime display format. Empty on a numeric ruler; the parent axes
    // fills it when the side is in datetime mode, and a user edit maps back to the
    // axes *TickLabelFormat.
    registerProperty(new GOStringProperty, GO_TICK_LABEL_FORMAT_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GORuler::setupDefaults()
{
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setRestrictedStringDefault(GO_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setTwoVectorDefault(GO_LIMITS_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_LIMITS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINEAR_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_TICK_VALUES_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_TICK_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_IN_STR);
    setTwoVectorDefault(GO_TICK_LENGTH_PROPERTY_NAME_STR, 0.01, 0.025);
    setRestrictedStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_TICK_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_IN_STR);
    setThreeVectorDefault(GO_GRID_COLOR_PROPERTY_NAME_STR, 0.15, 0.15, 0.15);
    setScalarDoubleDefault(GO_GRID_ALPHA_PROPERTY_NAME_STR, 0.15);
    setRestrictedStringDefault(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_TICK_LABEL_FORMAT_PROPERTY_NAME_STR, L"");
}
//=============================================================================
void
GORuler::paintMe(RenderInterface& gc)
{
    // The parent axes renders both rulers; a ruler never paints itself.
}
//=============================================================================
void
GORuler::linkToAxis(int64 axisHandle, int side, int axisKind)
{
    m_axisHandle = axisHandle;
    m_side = side;
    m_axisKind = axisKind;
    std::vector<int64> parent = { axisHandle };
    static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR))->data(parent);
}
//=============================================================================
void
GORuler::updateState()
{
    if (m_axisHandle == -1) {
        return;
    }
    GOAxis* axis = dynamic_cast<GOAxis*>(findGraphicsObject(m_axisHandle, false));
    if (!axis) {
        return;
    }
    bool changed = false;
    for (const wchar_t* name : RULER_MAPPED_PROPERTIES) {
        if (hasChanged(name)) {
            axis->applyRulerSideProperty(m_axisKind, m_side, name, findProperty(name)->get());
            clearChanged(name);
            changed = true;
        }
    }
    if (changed) {
        axis->updateState();
        GOFigure* fig = axis->getParentFigure();
        if (fig) {
            fig->setRenderingStateInvalid(true);
            if (!fig->isDrawLater()) {
                fig->repaint();
            }
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
