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
#include <limits>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <sstream>
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOPositionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Text of a number with the shortest form, like %g.
static std::wstring
numberToText(double value)
{
    std::wostringstream stream;
    stream << value;
    return stream.str();
}
//=============================================================================
// ColorLimits must be a finite increasing two-element numeric vector.
class GOHeatmapColorLimitsProperty : public GOTwoVectorProperty
{
public:
    void
    set(ArrayOf arg) override
    {
        if (!arg.isNumeric() || arg.elementCount() != 2) {
            Error(L"Nelson:graphics:invalidColorLimits",
                L"ColorLimits must be a two-element increasing numeric vector.");
        }
        ArrayOf values(arg);
        values.promoteType(NLS_DOUBLE);
        const double* data = static_cast<const double*>(values.dataPointer());
        if (!(data[0] < data[1])) {
            Error(L"Nelson:graphics:invalidColorLimits",
                L"ColorLimits must be a two-element increasing numeric vector.");
        }
        GOTwoVectorProperty::set(arg);
    }
};
//=============================================================================
// A text property that refuses the empty string (CellLabelFormat).
class GOHeatmapNonEmptyStringProperty : public GOStringProperty
{
public:
    void
    set(ArrayOf arg) override
    {
        if (arg.isEmpty()) {
            Error(L"Nelson:graphics:emptyString", L"Value must not be empty.");
        }
        GOStringProperty::set(arg);
    }
};
//=============================================================================
// A text property that also accepts a numeric scalar, stored as its text (MissingDataLabel).
class GOHeatmapLabelProperty : public GOStringProperty
{
public:
    void
    set(ArrayOf arg) override
    {
        if (arg.isNumeric() && arg.isScalar()) {
            ArrayOf value(arg);
            value.promoteType(NLS_DOUBLE);
            std::wstring text = numberToText(static_cast<const double*>(value.dataPointer())[0]);
            GOStringProperty::set(ArrayOf::characterArray(text));
            return;
        }
        GOStringProperty::set(arg);
    }
};
//=============================================================================
// A strictly positive numeric scalar; text is refused instead of read as character codes.
class GOHeatmapFontSizeProperty : public GOScalarProperty
{
public:
    GOHeatmapFontSizeProperty()
        : GOScalarProperty(
            std::numeric_limits<double>::min(), std::numeric_limits<double>::infinity())
    {
    }
    void
    set(ArrayOf arg) override
    {
        if (!arg.isNumeric()) {
            Error(L"Nelson:validators:mustBeNumeric", L"%s must be numeric.", L"FontSize");
        }
        GOScalarProperty::set(arg);
    }
};
//=============================================================================
// Colormap is empty (default) or an Mx3 matrix: double/single in [0 1] or uint8.
class GOHeatmapColormapProperty : public GOArrayOfProperty
{
public:
    void
    set(ArrayOf arg) override
    {
        if (arg.isEmpty()) {
            GOArrayOfProperty::set(arg);
            return;
        }
        bool validType = arg.isNumeric() && !arg.isComplex();
        Dimensions dims = arg.dimensions();
        bool validSize = dims.length() == 2 && dims.columns() == 3;
        bool validRange = validType;
        if (validType && arg.dataClass() != NLS_UINT8) {
            ArrayOf values(arg);
            values.promoteType(NLS_DOUBLE);
            const double* data = static_cast<const double*>(values.dataPointer());
            for (indexType k = 0; k < values.elementCount() && validRange; ++k) {
                validRange = data[k] >= 0 && data[k] <= 1;
            }
        }
        if (!(validType && validSize && validRange)) {
            Error(L"Nelson:graphics:invalidColormap",
                L"Colormap must be an Mx3 array of type single or double in the range [0 1], or "
                L"an Mx3 array of type uint8.");
        }
        GOArrayOfProperty::set(arg);
    }
};
//=============================================================================
// Display labels must have as many elements as the displayed data (when it is known).
class GOHeatmapDisplayLabelsProperty : public GOArrayOfProperty
{
    GraphicsObject* _owner;
    std::wstring _referenceName;
    std::wstring _fallbackName;

public:
    GOHeatmapDisplayLabelsProperty(
        GraphicsObject* owner, const std::wstring& referenceName, const std::wstring& fallbackName)
        : _owner(owner), _referenceName(referenceName), _fallbackName(fallbackName)
    {
    }
    void
    set(ArrayOf arg) override
    {
        if (!arg.isEmpty()) {
            indexType expected = 0;
            GOGenericProperty* reference = _owner->findProperty(_referenceName, false);
            if (reference != nullptr) {
                expected = reference->get().elementCount();
            }
            if (expected == 0) {
                reference = _owner->findProperty(_fallbackName, false);
                if (reference != nullptr) {
                    expected = reference->get().elementCount();
                }
            }
            if (expected != 0 && arg.elementCount() != expected) {
                Error(L"Nelson:graphics:heatmapLabelsMismatch",
                    L"Display labels must contain the same number of elements as the displayed "
                    L"data.");
            }
        }
        GOArrayOfProperty::set(arg);
    }
};
//=============================================================================
GOHeatmapChart::GOHeatmapChart() : GOGroup()
{
    registerProperty(new GOArrayOfProperty, GO_COLOR_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapColormapProperty, GO_COLOR_MAP_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapDisplayLabelsProperty(
                         this, GO_X_DISPLAY_DATA_PROPERTY_NAME_STR, GO_X_DATA_PROPERTY_NAME_STR),
        GO_X_DISPLAY_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapDisplayLabelsProperty(
                         this, GO_Y_DISPLAY_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR),
        GO_Y_DISPLAY_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DISPLAY_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DISPLAY_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_COLOR_DISPLAY_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapLabelProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapLabelProperty, GO_X_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapLabelProperty, GO_Y_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_COLOR_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ L"none", L"count", L"mean", L"median", L"sum",
                         L"min", L"max" }),
        GO_COLOR_METHOD_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapColorLimitsProperty, GO_COLOR_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_COLORBAR_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_GRID_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapNonEmptyStringProperty, GO_CELL_LABEL_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_CELL_LABEL_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapLabelProperty, GO_MISSING_DATA_LABEL_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FONT_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOHeatmapFontSizeProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
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
    syncChartPositionUnits(this);
    findProperty(GO_X_LIMITS_PROPERTY_NAME_STR)
        ->set(firstLastCell(findProperty(GO_X_DATA_PROPERTY_NAME_STR)->get()));
    findProperty(GO_Y_LIMITS_PROPERTY_NAME_STR)
        ->set(firstLastCell(findProperty(GO_Y_DATA_PROPERTY_NAME_STR)->get()));
    GOGroup::updateState();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
