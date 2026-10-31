//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOWordCloudChart.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOPositionHelpers.hpp"
#include <cmath>
#include <limits>
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static std::wstring
    numberToText(double value)
    {
        std::wostringstream stream;
        stream << value;
        return stream.str();
    }
    //=============================================================================
    static const std::vector<std::wstring> SHAPE_VALUES = { L"oval", L"rectangle" };
    static const std::vector<std::wstring> POSITION_CONSTRAINT_VALUES
        = { L"outerposition", L"innerposition" };
    //=============================================================================
    // Color also takes an N-by-3 matrix: one RGB row per word.
    class GOWordCloudColorProperty : public GOAutoFlatColorProperty
    {
        ArrayOf m_matrix;

    public:
        void
        set(ArrayOf arg) override
        {
            m_matrix = ArrayOf();
            if (arg.isNumeric() && arg.dimensions().rows() > 1 && arg.dimensions().columns() == 3
                && arg.dimensions().length() == 2) {
                ArrayOf values(arg);
                values.promoteType(NLS_DOUBLE);
                const double* data = static_cast<const double*>(values.dataPointer());
                const indexType count = values.elementCount();
                for (indexType k = 0; k < count; ++k) {
                    if (!(data[k] >= 0. && data[k] <= 1.)) {
                        Error(L"Nelson:graphics:invalidColor",
                            L"Color values must be between 0 and 1.");
                    }
                }
                const indexType rows = values.dimensions().rows();
                GOAutoFlatColorProperty::set(ArrayOf::doubleRowVector(
                    std::vector<double> { data[0], data[rows], data[2 * rows] }));
                m_matrix = values;
                return;
            }
            GOAutoFlatColorProperty::set(arg);
        }
        ArrayOf
        get() override
        {
            if (!m_matrix.isEmpty() && isEqual(GO_PROPERTY_VALUE_COLORSPEC_STR)) {
                return m_matrix;
            }
            return GOAutoFlatColorProperty::get();
        }
    };
    //=============================================================================
    // Title also takes a number (stored as text) or a cell of lines (joined by newlines).
    class GOWordCloudTitleProperty : public GOStringProperty
    {
    public:
        void
        set(ArrayOf arg) override
        {
            if (arg.isNumeric() && arg.isScalar()) {
                ArrayOf value(arg);
                value.promoteType(NLS_DOUBLE);
                GOStringProperty::set(ArrayOf::characterArray(
                    numberToText(static_cast<const double*>(value.dataPointer())[0])));
                return;
            }
            if (arg.isCell()) {
                const ArrayOf* elements = static_cast<const ArrayOf*>(arg.dataPointer());
                std::wstring text;
                for (indexType k = 0; k < arg.elementCount(); ++k) {
                    if (!elements[k].isRowVectorCharacterArray()
                        && !elements[k].isScalarStringArray()) {
                        Error(L"Nelson:graphics:expectedStringProperty",
                            L"Expecting a string for property.");
                    }
                    text += (k > 0 ? std::wstring(1, wchar_t(10)) : std::wstring())
                        + elements[k].asWideString();
                }
                GOStringProperty::set(ArrayOf::characterArray(text));
                return;
            }
            GOStringProperty::set(arg);
        }
    };
    //=============================================================================
    // A numeric scalar that must be an integer (inf allowed for the upper bound).
    class GOWordCloudIntegerProperty : public GOScalarProperty
    {
    public:
        GOWordCloudIntegerProperty(double minimum, double maximum)
            : GOScalarProperty(minimum, maximum)
        {
        }
        void
        set(ArrayOf arg) override
        {
            if (!arg.isNumeric() || !arg.isScalar()) {
                Error(L"Nelson:validators:mustBeNumeric", L"Value must be a numeric scalar.");
            }
            ArrayOf value(arg);
            value.promoteType(NLS_DOUBLE);
            const double number = static_cast<const double*>(value.dataPointer())[0];
            if (std::isfinite(number) && number != std::floor(number)) {
                Error(L"Nelson:validators:mustBeInteger", L"Value must be an integer.");
            }
            GOScalarProperty::set(arg);
        }
    };
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true, bool visible = true)
    {
        go->registerProperty(property, name, writable, visible);
    }
    //=============================================================================
    static void
    setColorSpec(
        GraphicsObject* object, const std::wstring& propertyName, const std::vector<double>& color)
    {
        auto* property = static_cast<GOAutoFlatColorProperty*>(object->findProperty(propertyName));
        property->set(ArrayOf::doubleRowVector(color));
        property->clearModified();
    }
    //=============================================================================
} // namespace
//=============================================================================
GOWordCloudChart::GOWordCloudChart()
{
    addProperty(this, new GOStringVectorProperty(false, true), GO_WORD_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_SIZE_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOWordCloudIntegerProperty(1, std::numeric_limits<double>::infinity()),
        GO_MAX_DISPLAY_WORDS_PROPERTY_NAME_STR);
    addProperty(this,
        new GOScalarProperty(
            std::numeric_limits<double>::min(), std::numeric_limits<double>::infinity()),
        GO_SIZE_POWER_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOWordCloudColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_HIGHLIGHT_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(SHAPE_VALUES), GO_SHAPE_PROPERTY_NAME_STR);
    addProperty(this, new GOWordCloudIntegerProperty(0, std::numeric_limits<double>::infinity()),
        GO_LAYOUT_NUM_PROPERTY_NAME_STR);
    addProperty(this, new GOWordCloudTitleProperty, GO_TITLE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_TITLE_FONT_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOFourVectorProperty, GO_OUTER_POSITION_PROPERTY_NAME_STR);
    addProperty(this, new GOFourVectorProperty, GO_INNER_POSITION_PROPERTY_NAME_STR);
    addProperty(this, new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    addProperty(this, new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(POSITION_CONSTRAINT_VALUES),
        GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR);
    addProperty(this, new GOLayoutOptionsProperty, GO_LAYOUT_PROPERTY_NAME_STR);
    addProperty(this, new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_WORD_VARIABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_SIZE_VARIABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_BOX_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    addProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    addProperty(this, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    addProperty(this, new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    addProperty(this, new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    addProperty(this, new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    setScalarDoubleDefault(GO_MAX_DISPLAY_WORDS_PROPERTY_NAME_STR, 100.);
    setScalarDoubleDefault(GO_SIZE_POWER_PROPERTY_NAME_STR, 0.5);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
    setColorSpec(this, GO_COLOR_PROPERTY_NAME_STR,
        { 0.380392156862745, 0.380392156862745, 0.380392156862745 });
    setColorSpec(this, GO_HIGHLIGHT_COLOR_PROPERTY_NAME_STR,
        { 0.752941176470588, 0.298039215686275, 0.0431372549019608 });
    setRestrictedStringDefault(GO_SHAPE_PROPERTY_NAME_STR, L"oval");
    setScalarDoubleDefault(GO_LAYOUT_NUM_PROPERTY_NAME_STR, 1.);
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_TITLE_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0., 0., 1., 1.);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0., 0., 1., 1.);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0., 0., 1., 1.);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    setRestrictedStringDefault(
        GO_POSITION_CONSTRAINT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OUTERPOSITION_STR);
    findProperty(GO_SOURCE_TABLE_PROPERTY_NAME_STR)->set(ArrayOf::empty());
    setStringDefault(GO_WORD_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_SIZE_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_BOX_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_WORD_CLOUD_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOWordCloudChart::~GOWordCloudChart() = default;
//=============================================================================
std::wstring
GOWordCloudChart::getType()
{
    return GO_PROPERTY_VALUE_WORD_CLOUD_STR;
}
//=============================================================================
void
GOWordCloudChart::paintMe(RenderInterface& gc)
{
    (void)gc;
    updateState();
}
//=============================================================================
void
GOWordCloudChart::updateState()
{
    syncChartPositionUnits(this, false);
    bool outerPositionChanged = hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    bool positionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR);
    bool innerPositionChanged = hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
    syncOuterDrivenPositionTriplet(this);
    if (outerPositionChanged && !positionChanged && !innerPositionChanged) {
        std::vector<double> outer = findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
        if (outer.size() == 4) {
            setFourVectorDefault(
                GO_POSITION_PROPERTY_NAME_STR, outer[0], outer[1], outer[2], outer[3]);
            setFourVectorDefault(
                GO_INNER_POSITION_PROPERTY_NAME_STR, outer[0], outer[1], outer[2], outer[3]);
        }
    } else if (syncInnerDrivenPositionTriplet(this)) {
        std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() == 4) {
            setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, position[0], position[1],
                position[2], position[3]);
        }
    }
    clearAllChanged();
}
//=============================================================================
std::vector<double>
GOWordCloudChart::getLimits()
{
    return { 0., 1., 0., 1., 0., 0., 0., 1., 1., 1. };
}
//=============================================================================
} // namespace Nelson
//=============================================================================
