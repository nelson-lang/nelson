//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOHeatmapChart.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOHeatmapChart::GOHeatmapChart() : GOGroup()
{
    registerProperty(new GOArrayOfProperty, GO_COLOR_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLOR_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DISPLAY_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DISPLAY_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DISPLAY_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DISPLAY_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLOR_DISPLAY_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_COLOR_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_COLOR_METHOD_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_COLOR_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_COLORBAR_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_GRID_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_CELL_LABEL_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_CELL_LABEL_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_MISSING_DATA_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FONT_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_SCALED_STR,
                         GO_PROPERTY_VALUE_LOG_STR, GO_PROPERTY_VALUE_DIRECT_STR }),
        GO_COLOR_SCALING_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_MISSING_DATA_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_OUTERPOSITION_STR,
                         GO_PROPERTY_VALUE_INNERPOSITION_STR }),
        GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_LIMITS_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_Y_LIMITS_PROPERTY_NAME_STR, false);
    sortProperties();

    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    findProperty(GO_COLOR_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_X_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_Y_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_X_DISPLAY_LABELS_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_Y_DISPLAY_LABELS_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_X_DISPLAY_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_Y_DISPLAY_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_COLOR_DISPLAY_DATA_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    findProperty(GO_SOURCE_TABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_X_LABEL_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_LABEL_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_X_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_Y_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_COLOR_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_COLOR_METHOD_PROPERTY_NAME_STR, L"none");
    setTwoVectorDefault(GO_COLOR_LIMITS_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_COLORBAR_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_GRID_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_CELL_LABEL_FORMAT_PROPERTY_NAME_STR, L"%0.4g");
    setRestrictedStringColorDefault(
        GO_CELL_LABEL_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, 0, 0, 0);
    setStringDefault(GO_MISSING_DATA_LABEL_PROPERTY_NAME_STR, L"NaN");
    setThreeVectorDefault(
        GO_FONT_COLOR_PROPERTY_NAME_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0.13, 0.11, 0.775, 0.815);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, 1, 1);
    setRestrictedStringDefault(GO_COLOR_SCALING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALED_STR);
    setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setThreeVectorDefault(GO_MISSING_DATA_COLOR_PROPERTY_NAME_STR, 0.129411764705882,
        0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(
        GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    findProperty(GO_X_LIMITS_PROPERTY_NAME_STR)->set(ArrayOf::emptyCell(Dimensions(1, 0)));
    findProperty(GO_Y_LIMITS_PROPERTY_NAME_STR)->set(ArrayOf::emptyCell(Dimensions(1, 0)));
    clearAllChanged();
}
//=============================================================================
std::wstring
GOHeatmapChart::getType()
{
    return GO_PROPERTY_VALUE_HEATMAP_STR;
}
//=============================================================================
static ArrayOf
firstLastCell(const ArrayOf& data)
{
    // XLimits/YLimits report the first and last category as a 1x2 cell.
    // An empty axis reports an empty cell.
    if (!data.isCell() || data.isEmpty()) {
        return ArrayOf::emptyCell(Dimensions(1, 0));
    }
    const ArrayOf* elements = static_cast<const ArrayOf*>(data.dataPointer());
    indexType n = data.elementCount();
    ArrayOf* limits = static_cast<ArrayOf*>(ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, 2));
    limits[0] = elements[0];
    limits[1] = elements[n - 1];
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 2), limits);
}
//=============================================================================
void
GOHeatmapChart::updateState()
{
    findProperty(GO_X_LIMITS_PROPERTY_NAME_STR)
        ->set(firstLastCell(findProperty(GO_X_DATA_PROPERTY_NAME_STR)->get()));
    findProperty(GO_Y_LIMITS_PROPERTY_NAME_STR)
        ->set(firstLastCell(findProperty(GO_Y_DATA_PROPERTY_NAME_STR)->get()));
    GOGroup::updateState();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
