//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "GOUITable.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOChartDecorators.hpp"
#include "GOColorProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOEnableProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUITable::GOUITable()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUITable::getType()
{
    return GO_PROPERTY_VALUE_UITABLE_STR;
}
//=============================================================================
std::string
GOUITable::getClassName()
{
    return GO_CLASS_NAME_UI_TABLE_STR;
}
//=============================================================================
void
GOUITable::updateState()
{
    if (hasChanged(GO_DATA_PROPERTY_NAME_STR)) {
        GOArrayOfProperty* data
            = static_cast<GOArrayOfProperty*>(findProperty(GO_DATA_PROPERTY_NAME_STR));
        static_cast<GOArrayOfProperty*>(findProperty(GO_DISPLAY_DATA_PROPERTY_NAME_STR))
            ->data(data->data());
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUITable::constructProperties()
{
    registerUIComponentCommonProperties(true, false, true);
    // Enable reports as a plain 'on'/'off'/'inactive' char here, not an OnOffSwitchState
    // enum (legacy component).
    registerProperty(new GOEnableProperty, GO_ENABLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_COLUMN_REARRANGEABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DISPLAY_SELECTION_PROPERTY_NAME_STR, false);
    registerProperty(new GOStyleConfigurationsProperty, GO_STYLE_CONFIGURATIONS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DISPLAY_DATA_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_COLUMN_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_ROW_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLUMN_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLUMN_EDITABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLUMN_SORTABLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLUMN_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ROW_STRIPING_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_SELECTION_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(
                         { L"cell", GO_PROPERTY_VALUE_ROW_STR, GO_PROPERTY_VALUE_COLUMN_STR }),
        GO_SELECTION_TYPE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_MULTISELECT_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CALLBACK_PROPERTY_NAME_STR, true, false);
    registerProperty(new GOCallbackProperty, L"CellEditCallback");
    registerProperty(new GOCallbackProperty, L"CellSelectionCallback");
    registerProperty(new GOCallbackProperty, GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CLICKED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DOUBLE_CLICKED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, L"DisplayDataChangedFcn");
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUITable::setupDefaults()
{
    setupUIComponentCommonDefaults(20, 20, 300, 300, true, false);
    GOArrayOfProperty* data
        = static_cast<GOArrayOfProperty*>(findProperty(GO_DATA_PROPERTY_NAME_STR));
    data->data(ArrayOf::empty(0, 0));
    static_cast<GOArrayOfProperty*>(findProperty(GO_DISPLAY_DATA_PROPERTY_NAME_STR))
        ->data(ArrayOf::empty(0, 0));
    static_cast<GOArrayOfProperty*>(findProperty(GO_COLUMN_NAME_PROPERTY_NAME_STR))
        ->data(ArrayOf::characterArray(L"numbered"));
    static_cast<GOArrayOfProperty*>(findProperty(GO_ROW_NAME_PROPERTY_NAME_STR))
        ->data(ArrayOf::characterArray(L"numbered"));
    static_cast<GOArrayOfProperty*>(findProperty(GO_COLUMN_WIDTH_PROPERTY_NAME_STR))
        ->data(ArrayOf::characterArray(GO_PROPERTY_VALUE_AUTO_STR));
    static_cast<GOArrayOfProperty*>(findProperty(GO_COLUMN_EDITABLE_PROPERTY_NAME_STR))
        ->data(ArrayOf(NLS_LOGICAL, Dimensions(0, 0), nullptr));
    static_cast<GOArrayOfProperty*>(findProperty(GO_COLUMN_SORTABLE_PROPERTY_NAME_STR))
        ->data(ArrayOf(NLS_LOGICAL, Dimensions(0, 0), nullptr));
    static_cast<GOArrayOfProperty*>(findProperty(GO_COLUMN_FORMAT_PROPERTY_NAME_STR))
        ->data(ArrayOf::emptyCell(Dimensions(1, 0)));
    setRestrictedStringDefault(GO_ROW_STRIPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(
        GO_COLUMN_REARRANGEABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    static_cast<GOArrayOfProperty*>(findProperty(GO_DISPLAY_SELECTION_PROPERTY_NAME_STR))
        ->data(ArrayOf::empty(0, 0));
    static_cast<GOArrayOfProperty*>(findProperty(GO_SELECTION_PROPERTY_NAME_STR))
        ->data(ArrayOf::empty(0, 0));
    setRestrictedStringDefault(GO_SELECTION_TYPE_PROPERTY_NAME_STR, L"cell");
    setRestrictedStringDefault(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, 0.129411764705882,
        0.129411764705882, 0.129411764705882);
    GOArrayOfProperty* backgroundColor
        = static_cast<GOArrayOfProperty*>(findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR));
    double backgroundValues[6] = { 1, 0.96078431372549, 1, 0.96078431372549, 1, 0.96078431372549 };
    ArrayOf backgroundMatrix = ArrayOf::doubleMatrix2d(2, 3);
    double* backgroundPointer = (double*)backgroundMatrix.dataPointer();
    for (int k = 0; k < 6; ++k) {
        backgroundPointer[k] = backgroundValues[k];
    }
    backgroundColor->data(backgroundMatrix);
    setStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 8);
}
//=============================================================================
bool
GOUITable::onUserCellEdited(int row, int column, const std::wstring& newText)
{
    if (isProgrammaticChange()) {
        return true;
    }
    GOArrayOfProperty* dataProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_DATA_PROPERTY_NAME_STR));
    ArrayOf data = dataProperty->data();
    indexType rows = data.rows();
    if (data.isEmpty() || row < 0 || column < 0 || (indexType)row >= rows
        || (indexType)column >= data.columns()) {
        return true;
    }
    ArrayOf previousData = data;
    ArrayOf newDataValue;
    bool numericParsed = false;
    double parsed = 0;
    try {
        size_t consumed = 0;
        parsed = std::stod(newText, &consumed);
        numericParsed = consumed == newText.size();
    } catch (...) {
        numericParsed = false;
    }
    if (data.isCell()) {
        ArrayOf* elements = (ArrayOf*)data.dataPointer();
        ArrayOf previousCellValue = elements[row + column * rows];
        if (previousCellValue.isNumeric() && numericParsed) {
            elements[row + column * rows] = ArrayOf::doubleScalar(parsed);
            newDataValue = ArrayOf::doubleScalar(parsed);
        } else {
            elements[row + column * rows] = ArrayOf::characterArray(newText);
            newDataValue = ArrayOf::characterArray(newText);
        }
        dataProperty->data(data);
    } else if (data.isNumeric() && numericParsed) {
        ArrayOf asDouble(data);
        asDouble.promoteType(NLS_DOUBLE);
        double* values = (double*)asDouble.dataPointer();
        values[row + column * rows] = parsed;
        dataProperty->data(asDouble);
        newDataValue = ArrayOf::doubleScalar(parsed);
    } else {
        // rejected edit: the caller reverts its display from Data
        return false;
    }
    static_cast<GOArrayOfProperty*>(findProperty(GO_DISPLAY_DATA_PROPERTY_NAME_STR))
        ->data(dataProperty->data());

    ArrayOf indices = ArrayOf::doubleRowVector({ (double)(row + 1), (double)(column + 1) });
    wstringVector fieldNames = { L"Indices", L"DisplayIndices", L"EditData", L"NewData" };
    ArrayOfVector fieldValues;
    fieldValues << indices;
    fieldValues << indices;
    fieldValues << ArrayOf::characterArray(newText);
    fieldValues << newDataValue;
    emitCallbackEvent(L"CellEditCallback", L"CellEditData", L"CellEdit", fieldNames, fieldValues);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
