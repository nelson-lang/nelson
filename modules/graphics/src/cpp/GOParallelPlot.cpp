//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOParallelPlot.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOParallelPlot::GOParallelPlot() : GOGroup()
{
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COORDINATE_VARIABLES_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_GROUP_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_GROUP_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COORDINATE_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COORDINATE_TICK_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DATA_NORMALIZATION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    // LineStyle/MarkerStyle report as a string scalar, not a char array.
    registerProperty(new GOStringVectorProperty(false, true), GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_LINE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOStringVectorProperty(false, true), GO_MARKER_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_JITTER_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_LEGEND_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_COORDINATE_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DATA_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_LEGEND_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    registerProperty(new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    sortProperties();

    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    findProperty(GO_SOURCE_TABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_COORDINATE_VARIABLES_PROPERTY_NAME_STR)
        ->set(ArrayOf::emptyCell(Dimensions(0, 0)));
    setStringDefault(GO_GROUP_VARIABLE_PROPERTY_NAME_STR, L"");
    findProperty(GO_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_GROUP_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_COORDINATE_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_COORDINATE_TICK_LABELS_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setStringDefault(GO_DATA_NORMALIZATION_PROPERTY_NAME_STR, L"range");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    findProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)->set(ArrayOf::characterArray(L"-"));
    setScalarDoubleDefault(GO_LINE_ALPHA_PROPERTY_NAME_STR, 0.7);
    findProperty(GO_MARKER_STYLE_PROPERTY_NAME_STR)->set(ArrayOf::characterArray(L"none"));
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setScalarDoubleDefault(GO_JITTER_PROPERTY_NAME_STR, 0.1);
    setStringDefault(GO_LEGEND_TITLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_COORDINATE_LABEL_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_DATA_LABEL_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_LEGEND_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setStringDefault(GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    clearAllChanged();
}
//=============================================================================
std::wstring
GOParallelPlot::getType()
{
    return GO_PROPERTY_VALUE_PARALLEL_PLOT_STR;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
