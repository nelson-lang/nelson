//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOChartDecorators.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOBaseline::GOBaseline()
{
    registerProperties();
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.15, 0.15, 0.15);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
}
//=============================================================================
std::wstring
GOBaseline::getType()
{
    return L"baseline";
}
//=============================================================================
std::string
GOBaseline::getClassName()
{
    return GO_CLASS_NAME_BASELINE_STR;
}
//=============================================================================
void
GOBaseline::registerProperties()
{
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    sortProperties();
}
//=============================================================================
void
GOBaseline::paintMe(RenderInterface& gc)
{
    // The owning chart draws its own baseline; this carrier never paints.
}
//=============================================================================
GOLegendEntry::GOLegendEntry()
{
    registerProperties();
    setRestrictedStringDefault(GO_ICON_DISPLAY_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
}
//=============================================================================
std::wstring
GOLegendEntry::getType()
{
    return L"legendentry";
}
//=============================================================================
std::string
GOLegendEntry::getClassName()
{
    return GO_CLASS_NAME_LEGEND_ENTRY_STR;
}
//=============================================================================
void
GOLegendEntry::registerProperties()
{
    static const wchar_t* iconDisplayStyleValues[] = { GO_PROPERTY_VALUE_ON_STR,
        GO_PROPERTY_VALUE_OFF_STR, GO_PROPERTY_VALUE_CHILDREN_STR, nullptr };
    registerProperty(new GORestrictedStringProperty(iconDisplayStyleValues),
        GO_ICON_DISPLAY_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    sortProperties();
}
//=============================================================================
void
GOLegendEntry::paintMe(RenderInterface& gc)
{
}
//=============================================================================
GOAnnotationEventData::GOAnnotationEventData()
{
    registerProperties();
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
}
//=============================================================================
std::wstring
GOAnnotationEventData::getType()
{
    return L"annotation";
}
//=============================================================================
std::string
GOAnnotationEventData::getClassName()
{
    return GO_CLASS_NAME_ANNOTATION_EVENTDATA_STR;
}
//=============================================================================
void
GOAnnotationEventData::registerProperties()
{
    // LegendInformation is the one meaningful field: its IconDisplayStyle drives
    // whether the owning object appears in a legend.
    registerProperty(new GOLazyDecoratorProperty(GODecoratorKind::LegendEntry),
        GO_LEGEND_INFORMATION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    sortProperties();
}
//=============================================================================
void
GOAnnotationEventData::paintMe(RenderInterface& gc)
{
}
//=============================================================================
GODataTipTemplate::GODataTipTemplate()
{
    registerProperties();
    setRestrictedStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10.0);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
}
//=============================================================================
std::wstring
GODataTipTemplate::getType()
{
    return L"datatiptemplate";
}
//=============================================================================
std::string
GODataTipTemplate::getClassName()
{
    return GO_CLASS_NAME_DATATIP_TEMPLATE_STR;
}
//=============================================================================
void
GODataTipTemplate::registerProperties()
{
    static const wchar_t* interpreterValues[] = { GO_PROPERTY_VALUE_TEX_STR,
        GO_PROPERTY_VALUE_LATEX_STR, GO_PROPERTY_VALUE_NONE_STR, nullptr };
    // DataTipRows is the ordered list of rows shown in a data tip. It starts empty
    // and is populated on demand by the data-cursor machinery.
    registerProperty(new GOGObjectsProperty, GO_DATA_TIP_ROWS_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringProperty(interpreterValues), GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    sortProperties();
}
//=============================================================================
void
GODataTipTemplate::paintMe(RenderInterface& gc)
{
}
//=============================================================================
static GraphicsObject*
createDecorator(GODecoratorKind kind)
{
    switch (kind) {
    case GODecoratorKind::Baseline:
        return new GOBaseline;
    case GODecoratorKind::AnnotationEventData:
        return new GOAnnotationEventData;
    case GODecoratorKind::LegendEntry:
        return new GOLegendEntry;
    case GODecoratorKind::DataTipTemplate:
        return new GODataTipTemplate;
    }
    return new GOBaseline;
}
//=============================================================================
GOLazyDecoratorProperty::GOLazyDecoratorProperty(GODecoratorKind kind) : m_kind(kind) { }
//=============================================================================
ArrayOf
GOLazyDecoratorProperty::get()
{
    if (_data.empty()) {
        GraphicsObject* decorator = createDecorator(m_kind);
        int64 handle = assignGraphicsObject(decorator);
        std::vector<int64> handles;
        handles.push_back(handle);
        data(handles);
    }
    return GOGObjectsProperty::get();
}
//=============================================================================
GOLayoutOptionsProperty::GOLayoutOptionsProperty() { }
//=============================================================================
ArrayOf
GOLayoutOptionsProperty::get()
{
    if (_data.empty()) {
        Dimensions dims(0, 0);
        return ArrayOf::emptyGraphicsObject(GO_CLASS_NAME_LAYOUT_OPTIONS_STR, dims);
    }
    return GOGObjectsProperty::get();
}
//=============================================================================
GOSourceTableProperty::GOSourceTableProperty(bool asTimetable) : m_asTimetable(asTimetable) { }
//=============================================================================
ArrayOf
GOSourceTableProperty::get()
{
    ArrayOf stored = GOArrayOfProperty::get();
    if (stored.isEmpty()) {
        ArrayOf emptyTable = ArrayOf::table(ArrayOfVector(), stringVector(), stringVector());
        if (m_asTimetable) {
            // Empty row times: timetable::size reads timeLength(RowTimes), which
            // falls back to length() on a plain empty value -> 0 rows.
            return ArrayOf::timetable(emptyTable, ArrayOf::empty());
        }
        return emptyTable;
    }
    return stored;
}
//=============================================================================
ArrayOf
GOStyleConfigurationsProperty::get()
{
    ArrayOf stored = GOArrayOfProperty::get();
    if (stored.isEmpty()) {
        // A 0x3 table: the Target / Style / TargetIndex columns an addStyle call would fill. Empty
        // columns keep the height at 0 while the three variable names give the width.
        ArrayOfVector columns;
        columns.push_back(ArrayOf::empty(0, 1));
        columns.push_back(ArrayOf::empty(0, 1));
        columns.push_back(ArrayOf::empty(0, 1));
        stringVector variableNames = { "Target", "Style", "TargetIndex" };
        return ArrayOf::table(columns, variableNames, stringVector());
    }
    return stored;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
