//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include <algorithm>
#include <cmath>
#include <sstream>
#include <tuple>
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOText.hpp"
#include "GOFontSpecHelpers.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOTiledChartLayout.hpp"
#include "GOHelpers.hpp"
#include "GORoot.hpp"
#include "GOList.hpp"
#include "GOAlignVertProperty.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorThreeDoubleProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "TexToUnicode.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
#include "GOTexLayout.hpp"
#include "RenderHelpers.hpp"
#include "GOSurfaceScale.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class GOTextStringProperty : public GOStringProperty
    {
    private:
        std::vector<std::wstring> _lines;

        std::wstring
        joinedLines() const
        {
            std::wstringstream stream;
            for (size_t k = 0; k < _lines.size(); ++k) {
                if (k != 0) {
                    stream << L"\n";
                }
                stream << _lines[k];
            }
            return stream.str();
        }

    public:
        ArrayOf
        get() override
        {
            if (_lines.size() <= 1) {
                return ArrayOf::characterArray(_data);
            }
            Dimensions dims(_lines.size(), 1);
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, _lines.size());
            ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, elements);
            for (indexType k = 0; k < dims.elementCount(); k++) {
                elements[k] = ArrayOf::characterArray(_lines[k]);
            }
            return result;
        }

        void
        set(ArrayOf arg) override
        {
            _lines.clear();
            uniformizeStringVector(arg, _lines);
            _data = joinedLines();
            GOGenericProperty::set(arg);
        }

        void
        data(const std::wstring& m) override
        {
            _lines = { m };
            GOStringProperty::data(m);
        }
    };
    //=============================================================================
    static const std::vector<std::wstring>&
    fontStatePropertyNames()
    {
        static const std::vector<std::wstring> names
            = { GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_FONT_NAME_PROPERTY_NAME_STR,
                  GO_FONT_SIZE_PROPERTY_NAME_STR, GO_FONT_SMOOTHING_PROPERTY_NAME_STR,
                  GO_FONT_UNITS_PROPERTY_NAME_STR, GO_FONT_WEIGHT_PROPERTY_NAME_STR };
        return names;
    }
    //=============================================================================
    static const std::vector<std::wstring>&
    textContentPropertyNames()
    {
        static const std::vector<std::wstring> names
            = { GO_INTERPRETER_PROPERTY_NAME_STR, GO_STRING_PROPERTY_NAME_STR };
        return names;
    }
    //=============================================================================
    static double
    subplotTextCompactFigure(GOText* text)
    {
        GOFigure* fig = text->getParentFigure();
        if (!fig) {
            return false;
        }
        std::vector<double> position = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() >= 4 && position[2] > 0 && position[3] > 0) {
            return position[3] <= 500;
        }
        return fig->getHeight() <= 500;
    }
    //=============================================================================
    static bool
    defaultSubplotFigureSize(GOFigure* fig)
    {
        if (!fig) {
            return false;
        }
        std::vector<double> defaultPosition = defaultFigurePosition();
        double defaultWidth = defaultPosition.size() >= 4 ? defaultPosition[2] : 1074.0;
        double defaultHeight = defaultPosition.size() >= 4 ? defaultPosition[3] : 647.0;
        std::vector<double> position = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() >= 4 && position[2] > 0 && position[3] > 0) {
            if (std::abs(position[2] - defaultWidth) <= 1.0
                && std::abs(position[3] - defaultHeight) <= 1.0) {
                return true;
            }
        }
        return std::abs(fig->getWidth() - defaultWidth) <= 1.0
            && std::abs(fig->getHeight() - defaultHeight) <= 1.0;
    }
    //=============================================================================
    static bool
    subplotTextDefaultFigure(GOText* text)
    {
        return defaultSubplotFigureSize(text->getParentFigure());
    }
    //=============================================================================
    static double
    subplotTextAutoFontSize(GOText* text)
    {
        if (subplotTextCompactFigure(text)) {
            return 14.5;
        }
        if (subplotTextDefaultFigure(text)) {
            return 15.0;
        }
        return 15.0;
    }
    //=============================================================================
    static bool
    subplotTextParentIsTiledLayout(GOText* text)
    {
        GOGObjectsProperty* parent = static_cast<GOGObjectsProperty*>(
            text->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!parent || parent->data().empty()) {
            return false;
        }
        int64 parentHandle = parent->data()[0];
        if (parentHandle < HANDLE_OFFSET_OBJECT) {
            return false;
        }
        GraphicsObject* parentObject = findGraphicsObject(parentHandle, false);
        return parentObject && parentObject->getType() == GO_PROPERTY_VALUE_TILED_LAYOUT_STR;
    }
    //=============================================================================
    static double
    subplotTextQtFontScale(GOText* text)
    {
        if (!text->isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
            return subplotTextCompactFigure(text) ? 1.216 : 1.234;
        }
        return 1.22;
    }
    //=============================================================================
    static int
    subplotTextQtFontStretch(GOText* text)
    {
        bool compact = subplotTextCompactFigure(text);
        if (text->isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
            return compact ? 108 : 101;
        }
        return compact ? 102 : 102;
    }
    //=============================================================================
    static double
    subplotTextXOffsetPixels(GOText* text)
    {
        bool compact = subplotTextCompactFigure(text);
        if (text->isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
            return compact ? 0.0 : -2.0;
        }
        if (subplotTextDefaultFigure(text)) {
            return -1.0;
        }
        return compact ? -1.0 : -2.0;
    }
    //=============================================================================
    static double
    subplotTextYOffsetPixels(GOText* text)
    {
        bool compact = subplotTextCompactFigure(text);
        if (text->isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)) {
            return compact ? 8.0 : -6.0;
        }
        if (subplotTextDefaultFigure(text)) {
            return 6.0;
        }
        return compact ? 1.0 : -1.0;
    }
    //=============================================================================
    static bool
    axisReferencesText(GOAxis* axis, const std::wstring& propertyName, GOText* text)
    {
        GOGObjectsProperty* property
            = static_cast<GOGObjectsProperty*>(axis->findProperty(propertyName, false));
        if (!property || property->data().empty()) {
            return false;
        }
        return property->data()[0] == findGraphicsObjectHandle(text);
    }
    //=============================================================================
    static bool
    axisTitleText(GOText* text)
    {
        GOAxis* axis = text->getParentAxis();
        return axis && axisReferencesText(axis, GO_TITLE_PROPERTY_NAME_STR, text);
    }
    //=============================================================================
    // An axis X/Y/Z label is positioned by drawTickLabels in normalized box
    // coordinates and painted by drawAxisLabels under a plain 0..1 ortho projection
    // over the outer position. Its position is not a data coordinate, so it must
    // map linearly whatever the axis scale (unlike title, subtitle and text(),
    // which are in data units).
    static bool
    axisLabelText(GOText* text)
    {
        GOAxis* axis = text->getParentAxis();
        return axis
            && (axisReferencesText(axis, GO_X_LABEL_PROPERTY_NAME_STR, text)
                || axisReferencesText(axis, GO_Y_LABEL_PROPERTY_NAME_STR, text)
                || axisReferencesText(axis, GO_Z_LABEL_PROPERTY_NAME_STR, text));
    }
    //=============================================================================
    static int
    axisTitleYOffsetPixels(GOText* text)
    {
        GOFigure* fig = text->getParentFigure();
        GOAxis* axis = text->getParentAxis();
        // Inline Notebook figures have a compact canvas after the toolbar
        // has consumed part of the panel height.  A 3-D title is projected
        // just above the box; without a small shared renderer offset its
        // glyphs can straddle the canvas edge and be clipped.  Keep this in
        // the common graphics path so WebView, WebAssembly and desktop use
        // the same layout rule.
        if (axis && !axis->is2D() && subplotTextCompactFigure(text)) {
            return -10;
        }
        if (!defaultSubplotFigureSize(fig)) {
            return 0;
        }
        return 4;
    }
    //=============================================================================
    static double
    axisTitleQtFontScale()
    {
        return 1.0;
    }
    //=============================================================================
    // The 'latex' interpreter has no full math layout engine yet and falls back to
    // the 'tex' pipeline. Strip one pair of surrounding inline-math delimiters ($...$
    // or $$...$$) so the symbols and scripts render instead of the dollar signs.
    // See docs/design/latex_math_interpreter.md.
    static std::wstring
    stripLatexMathDelimiters(const std::wstring& raw)
    {
        std::wstring s = raw;
        auto trimPair = [](std::wstring& v, const std::wstring& delim) {
            const size_t d = delim.size();
            if (v.size() >= 2 * d && v.compare(0, d, delim) == 0
                && v.compare(v.size() - d, d, delim) == 0) {
                v = v.substr(d, v.size() - 2 * d);
                return true;
            }
            return false;
        };
        if (!trimPair(s, L"$$")) {
            trimPair(s, L"$");
        }
        return s;
    }
    //=============================================================================
    static void
    markParentTitleLayoutChanged(GOText* text, bool updateTitlePosition)
    {
        GOAxis* axis = text->getParentAxis();
        if (!axis) {
            return;
        }
        if (!axisReferencesText(axis, GO_TITLE_PROPERTY_NAME_STR, text)
            && !axisReferencesText(axis, GO_SUBTITLE_PROPERTY_NAME_STR, text)) {
            return;
        }
        GOGenericProperty* property
            = axis->findProperty(GO_TITLE_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, false);
        if (property) {
            property->setModified(true);
        }
        if (updateTitlePosition) {
            axis->syncTitleTextLayout();
        }
        axis->rePackTitleArea();
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
GOText::getType()
{
    return GO_PROPERTY_VALUE_TEXT_STR;
}
//=============================================================================
wstringVector
GOText::getVisibleFieldnames()
{
    return {
        GO_COLOR_PROPERTY_NAME_STR,
        GO_COLOR_MODE_PROPERTY_NAME_STR,
        GO_POSITION_PROPERTY_NAME_STR,
        GO_STRING_PROPERTY_NAME_STR,
        GO_INTERPRETER_PROPERTY_NAME_STR,
        GO_EXTENT_PROPERTY_NAME_STR,
        GO_ROTATION_PROPERTY_NAME_STR,
        GO_FONT_NAME_PROPERTY_NAME_STR,
        GO_FONT_UNITS_PROPERTY_NAME_STR,
        GO_FONT_SIZE_PROPERTY_NAME_STR,
        GO_FONT_ANGLE_PROPERTY_NAME_STR,
        GO_FONT_WEIGHT_PROPERTY_NAME_STR,
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR,
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR,
        GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_LINE_STYLE_PROPERTY_NAME_STR,
        GO_LINE_WIDTH_PROPERTY_NAME_STR,
        GO_BACKGROUND_COLOR_PROPERTY_NAME_STR,
        GO_MARGIN_PROPERTY_NAME_STR,
        GO_EDITING_PROPERTY_NAME_STR,
        GO_CLIPPING_PROPERTY_NAME_STR,
        GO_FONT_SMOOTHING_PROPERTY_NAME_STR,
        GO_INTERACTIONS_PROPERTY_NAME_STR,
        GO_UNITS_PROPERTY_NAME_STR,
        GO_CHILDREN_PROPERTY_NAME_STR,
        GO_PARENT_PROPERTY_NAME_STR,
        GO_VISIBLE_PROPERTY_NAME_STR,
        GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR,
        GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR,
        GO_CONTEXT_MENU_PROPERTY_NAME_STR,
        GO_BUSY_ACTION_PROPERTY_NAME_STR,
        GO_BEING_DELETED_PROPERTY_NAME_STR,
        GO_INTERRUPTIBLE_PROPERTY_NAME_STR,
        GO_CREATE_FCN_PROPERTY_NAME_STR,
        GO_DELETE_FCN_PROPERTY_NAME_STR,
        GO_TYPE_PROPERTY_NAME_STR,
        GO_TAG_PROPERTY_NAME_STR,
        GO_USER_DATA_PROPERTY_NAME_STR,
        GO_SELECTED_PROPERTY_NAME_STR,
        GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR,
        GO_HIT_TEST_PROPERTY_NAME_STR,
        GO_PICKABLE_PARTS_PROPERTY_NAME_STR,
        GO_AFFECT_AUTO_LIMITS_PROPERTY_NAME_STR,
        GO_SERIES_INDEX_PROPERTY_NAME_STR,
    };
}
//=============================================================================
GOText::GOText()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOText::~GOText() { }
//=============================================================================
int
GOText::getTextHeightInPixels()
{
    updateState();
    // Qt reported QFontMetrics::boundingRect("|").height(), which equals
    // round(pixelSize * (hhea ascent + descent)) for the fonts and sizes the compat
    // tests use. Reproduce the same integers without QFontMetrics, so the
    // title/label spacing stays identical.
    return static_cast<int>(std::lround(
        FontMetrics::lineHeightPixels(fnt.family, fontSpecPixelSize(fnt), fnt.bold, fnt.italic)));
}
//=============================================================================
void
GOText::constructProperties()
{
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOFourVectorProperty, GO_BOUNDING_BOX_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOTextStringProperty, GO_STRING_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_EXTENT_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(
        new GOAutoManualProperty, GO_HORIZONTAL_ALIGNMENT_MODE_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOThreeVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_ROTATION_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_ROTATION_MODE_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOAlignVertProperty, GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(
        new GOAutoManualProperty, GO_VERTICAL_ALIGNMENT_MODE_PROPERTY_NAME_STR, false, false);
    registerProperty(new GOColorProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARGIN_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOTextInterpreterProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_FONT_SMOOTHING_PROPERTY_NAME_STR);

    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOText::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_ROTATION_PROPERTY_NAME_STR, 0);
    setRestrictedStringDefault(GO_ROTATION_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DATA_STR);
    setRestrictedStringDefault(
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MIDDLE_STR);
    setRestrictedStringDefault(
        GO_VERTICAL_ALIGNMENT_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
    setThreeVectorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_MARGIN_PROPERTY_NAME_STR, 3);
    setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
    setRestrictedStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
    setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 33.0 / 255.0, 33.0 / 255.0, 33.0 / 255.0);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
    setRestrictedStringDefault(GO_FONT_SMOOTHING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    fnt.antialias = true;
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
    // Text defaults to unclipped, unlike most objects: text annotations may draw
    // beyond the axes box. Override the common 'on' default.
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
void
GOText::paintMe(RenderInterface& gc)
{
    if (isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR) && isAuto(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR)
        && !subplotTextParentIsTiledLayout(this)) {
        GOScalarProperty* fontsize
            = static_cast<GOScalarProperty*>(findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
        fontsize->data(subplotTextAutoFontSize(this));
    }
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::vector<double> pos(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    std::vector<double> mapped;
    bool mappedInPixels = false;
    GOAxis* axis = getParentAxis();
    if (axis) {
        GOUnitsProperty* unitsProperty
            = static_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR));
        if (unitsProperty->isEqual(GO_PROPERTY_VALUE_NORMALIZED_STR)) {
            std::vector<double> axisPosition
                = axis->getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR);
            if (axisPosition.size() < 4 || pos.size() < 2) {
                return;
            }
            double z = (pos.size() >= 3) ? pos[2] : 0.0;
            mapped = { axisPosition[0] + pos[0] * axisPosition[2],
                axisPosition[1] + pos[1] * axisPosition[3], z };
            mappedInPixels = true;
        } else if (!unitsProperty->isEqual(GO_PROPERTY_VALUE_DATA_STR)) {
            std::vector<double> axisPosition
                = axis->getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR);
            if (axisPosition.size() < 4 || pos.size() < 2) {
                return;
            }
            std::wstring units = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
            auto pixel = offsetOnSurface(this, units, pos[0], pos[1], axisPosition[2],
                axisPosition[3], gc.width(), gc.height());
            double z = (pos.size() >= 3) ? pos[2] : 0.0;
            mapped = { axisPosition[0] + pixel.first, axisPosition[1] + pixel.second, z };
            mappedInPixels = true;
        } else {
            // Data-unit text must follow the axis scale: apply the log mapping on
            // log-scaled axes (forceLinearScale == false) like lines and other data
            // children. Forcing linear scale dropped titles and text() on semilogx,
            // semilogy and loglog axes off the projection.
            //
            // Axis X/Y/Z labels are the exception: they carry normalized box coordinates
            // and are painted under a plain 0..1 ortho projection, so they map linearly on
            // log axes too.
            mapped = axis->reMap(pos, axisLabelText(this));
        }
    } else {
        GOGObjectsProperty* parent
            = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (!parent || parent->data().empty()) {
            return;
        }
        int64 parentHandle = parent->data()[0];
        if (parentHandle < HANDLE_OFFSET_OBJECT) {
            GOFigure* fig = findGOFigure(parentHandle);
            if (!fig || pos.size() < 2) {
                return;
            }
            std::wstring units = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
            auto pixel = offsetOnSurface(
                this, units, pos[0], pos[1], gc.width(), gc.height(), gc.width(), gc.height());
            double z = (pos.size() >= 3) ? pos[2] : 0.0;
            mapped = { pixel.first, pixel.second, z };
            mappedInPixels = true;
        } else {
            GraphicsObject* parentObject = findGraphicsObject(parentHandle, false);
            if (!parentObject || parentObject->getType() != GO_PROPERTY_VALUE_TILED_LAYOUT_STR) {
                return;
            }
            std::vector<double> layoutPosition
                = parentObject->findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
            if (layoutPosition.size() < 4 || pos.size() < 2) {
                return;
            }

            // Convert the layout outer position to pixels, so the result does not depend
            // on the current projection/viewport (which may be an axes viewport).
            std::wstring units = parentObject->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
            double layoutX = layoutPosition[0];
            double layoutY = layoutPosition[1];
            double layoutW = layoutPosition[2];
            double layoutH = layoutPosition[3];
            std::tie(layoutX, layoutY) = offsetOnSurface(
                this, units, layoutX, layoutY, gc.width(), gc.height(), gc.width(), gc.height());
            std::tie(layoutW, layoutH) = offsetOnSurface(
                this, units, layoutW, layoutH, gc.width(), gc.height(), gc.width(), gc.height());

            double z = (pos.size() >= 3) ? pos[2] : 0.0;
            mapped = { layoutX + pos[0] * layoutW, layoutY + pos[1] * layoutH, z };
            mappedInPixels = true;
        }
    }
    int x, y;
    bool isAxisTitle = axisTitleText(this);
    if (mappedInPixels) {
        if (isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
            mapped[0] += subplotTextXOffsetPixels(this);
            mapped[1] += subplotTextYOffsetPixels(this);
        }
        x = (int)mapped[0];
        y = (int)mapped[1];
    } else {
        gc.toPixels(mapped[0], mapped[1], mapped[2], x, y);
    }
    if (isAxisTitle) {
        y += axisTitleYOffsetPixels(this);
    }
    gc.setupDirectDraw();
    FontSpec paintFont(fnt);
    if (isAxisTitle) {
        if (paintFont.pixelSize > 0) {
            paintFont.pixelSize *= axisTitleQtFontScale();
        } else {
            paintFont.pointSizeF *= axisTitleQtFontScale();
        }
    }
    double margin = pointsToPixels(findScalarDoubleProperty(GO_MARGIN_PROPERTY_NAME_STR));
    RenderInterface::AlignmentFlag xalign, yalign;
    GOAlignVertProperty* hv
        = static_cast<GOAlignVertProperty*>(findProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR));
    if (hv->isEqual(GO_PROPERTY_VALUE_TOP_STR)) {
        yalign = RenderInterface::Max;
    } else if (hv->isEqual(GO_PROPERTY_VALUE_MIDDLE_STR)) {
        yalign = RenderInterface::Mean;
    } else {
        yalign = RenderInterface::Min;
    }
    GOAlignHorizProperty* hh = static_cast<GOAlignHorizProperty*>(
        findProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR));
    if (hh->isEqual(GO_PROPERTY_VALUE_LEFT_STR)) {
        xalign = RenderInterface::Min;
    } else if (hh->isEqual(GO_PROPERTY_VALUE_CENTER_STR)) {
        xalign = RenderInterface::Mean;
    } else {
        xalign = RenderInterface::Max;
    }
    int textwidth;
    int textheight;
    int textxoffset;
    int textyoffset;
    TexBlockLayout blockLayout;
    if (texScript) {
        // Multi-run layout of true super/subscript runs. The background/edge box uses
        // the total block extent, like measureText's alignment model, so it hugs the
        // runs for every alignment.
        blockLayout = buildTexBlockLayout(texToRuns(text), paintFont);
        textwidth = static_cast<int>(std::lround(blockLayout.totalWidth));
        textheight = static_cast<int>(std::lround(blockLayout.topAbove + blockLayout.botBelow));
        textxoffset = 0;
        textyoffset = -textheight;
        if (xalign == RenderInterface::Mean) {
            textxoffset -= textwidth / 2;
        } else if (xalign == RenderInterface::Max) {
            textxoffset -= textwidth;
        }
        if (yalign == RenderInterface::Mean) {
            textyoffset += textheight / 2;
        } else if (yalign == RenderInterface::Min) {
            textyoffset += textheight;
        }
    } else {
        gc.measureText(
            text, paintFont, xalign, yalign, textwidth, textheight, textxoffset, textyoffset);
    }
    if (isAxisTitle && axis && !axis->is2D()) {
        // The projected 3-D title may leave the frame when the camera is
        // orbited to a side view.  This is independent of the CSS size of an
        // inline figure: the shared renderer still paints in the figure's
        // native pixel frame before the WebView/WebAssembly client scales it.
        // Clamp the measured anchor in that frame so every orientation keeps
        // the title visible, including the regular desktop renderer.
        const int margin = std::max(4, textheight / 3);
        const int minX = margin + textwidth / 2;
        const int maxX = std::max(minX, static_cast<int>(gc.width()) - margin - textwidth / 2);
        const int maxY = std::max(margin, static_cast<int>(gc.height()) - textheight - margin);
        if (xalign == RenderInterface::Mean) {
            if (x < minX || x > maxX) {
                x = (minX + maxX) / 2;
            }
        } else if (xalign == RenderInterface::Max) {
            if (x < margin + textwidth || x > static_cast<int>(gc.width()) - margin) {
                x = static_cast<int>(gc.width()) - margin;
            }
        } else if (x < margin || x > static_cast<int>(gc.width()) - margin - textwidth) {
            x = margin;
        }
        // A 3-D title is a figure decoration, not a projected data label.
        // Keep it in the upper title band for every camera orientation. This
        // avoids the title following the projected surface below the frame
        // after an orbit and matches the desktop renderer's stable layout.
        // The band sits above the Position box of the axes and the anchor follows that
        // box, not the camera: a subplot title stays with its subplot and does not drift
        // while the view pans; a full figure axes keeps the frame top as before.
        y = maxY;
        std::vector<double> axesBox
            = axis->getPropertyVectorAsPixels(gc, GO_POSITION_PROPERTY_NAME_STR);
        if (axesBox.size() >= 4 && std::isfinite(axesBox[0]) && std::isfinite(axesBox[1])
            && std::isfinite(axesBox[2]) && std::isfinite(axesBox[3])) {
            const int boxLeft = static_cast<int>(std::lround(axesBox[0]));
            const int boxWidth = static_cast<int>(std::lround(axesBox[2]));
            const int boxTop = static_cast<int>(std::lround(axesBox[1] + axesBox[3]));
            if (xalign == RenderInterface::Mean) {
                x = boxLeft + boxWidth / 2;
            } else if (xalign == RenderInterface::Max) {
                x = boxLeft + boxWidth;
            } else {
                x = boxLeft;
            }
            y = std::min(maxY, boxTop + textheight + margin);
        }
    }
    double rotation = findScalarDoubleProperty(GO_ROTATION_PROPERTY_NAME_STR);
    double costheta = cos(-rotation * M_PI / 180.0);
    double sintheta = sin(-rotation * M_PI / 180.0);
    int x1 = (int)(x + (textxoffset - margin) * costheta + (textyoffset - margin) * sintheta);
    int y1 = (int)(y - (textxoffset - margin) * sintheta + (textyoffset - margin) * costheta);
    double hdelx = (textwidth + 2 * margin) * costheta;
    double hdely = -(textwidth + 2 * margin) * sintheta;
    double vdelx = (textheight + 2 * margin) * sintheta;
    double vdely = (textheight + 2 * margin) * costheta;
    GOColorProperty* bc
        = static_cast<GOColorProperty*>(findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR));
    if (!bc->isNone()) {
        gc.color(bc->data());
        gc.quad(x1, y1, 0, x1 + hdelx, y1 + hdely, 0, x1 + hdelx + vdelx, y1 + hdely + vdely, 0,
            x1 + vdelx, y1 + vdely, 0);
    }
    GOColorProperty* ec
        = static_cast<GOColorProperty*>(findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR));
    if (!ec->isNone()) {
        gc.color(ec->data());
        gc.setLineStyle(
            (static_cast<GOLineStyleProperty*>(findProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)))
                ->data());
        gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
        gc.quadline(x1, y1, 0, x1 + hdelx, y1 + hdely, 0, x1 + hdelx + vdelx, y1 + hdely + vdely, 0,
            x1 + vdelx, y1 + vdely, 0);
    }
    GOColorProperty* tc = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (texScript) {
        drawTexBlockLayout(gc, x, y, blockLayout, tc->data(), xalign, yalign, rotation);
    } else {
        gc.putText(x, y, text, tc->data(), xalign, yalign, paintFont, rotation);
    }
    gc.releaseDirectDraw();
}
//=============================================================================
void
GOText::updateState()
{
    bool wasStateInitialized = stateInitialized;
    // A user change to Rotation / HorizontalAlignment / VerticalAlignment flips
    // the hidden <Name>Mode from auto to manual, which freezes that attribute
    // against the axes' automatic label-orientation layout. Setup-time defaults must
    // not count, so this runs only once the object is initialized. The flag is
    // cleared at once so an auto value written by the owning axes (which also marks
    // the property modified) is not mistaken for a user set.
    if (stateInitialized) {
        if (hasChanged(GO_ROTATION_PROPERTY_NAME_STR)) {
            toManual(GO_ROTATION_MODE_PROPERTY_NAME_STR);
            clearChanged(GO_ROTATION_PROPERTY_NAME_STR);
        }
        if (hasChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)) {
            toManual(GO_HORIZONTAL_ALIGNMENT_MODE_PROPERTY_NAME_STR);
            clearChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
        }
        if (hasChanged(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)) {
            toManual(GO_VERTICAL_ALIGNMENT_MODE_PROPERTY_NAME_STR);
            clearChanged(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
        }
    }
    const std::vector<std::wstring>& fontProperties = fontStatePropertyNames();
    const std::vector<std::wstring>& textProperties = textContentPropertyNames();
    bool subplotTextFontModeChanged = isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)
        && hasChanged(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    bool fontChanged
        = !stateInitialized || hasChanged(fontProperties) || subplotTextFontModeChanged;
    bool textChanged = !stateInitialized || hasChanged(textProperties);
    if (!fontChanged && !textChanged) {
        return;
    }

    if (isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR) && hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)) {
        toManual(GO_FONT_SIZE_MODE_PROPERTY_NAME_STR);
    }

    if (fontChanged) {
        if (!stateInitialized || hasChanged(GO_FONT_NAME_PROPERTY_NAME_STR)) {
            GOStringProperty* fontname
                = static_cast<GOStringProperty*>(findProperty(GO_FONT_NAME_PROPERTY_NAME_STR));
            fnt.family = resolveRootFontName(fontname->data());
        }
        if (!stateInitialized || hasChanged(GO_FONT_SIZE_PROPERTY_NAME_STR)
            || hasChanged(GO_FONT_UNITS_PROPERTY_NAME_STR) || subplotTextFontModeChanged) {
            GOScalarProperty* fontsize
                = static_cast<GOScalarProperty*>(findProperty(GO_FONT_SIZE_PROPERTY_NAME_STR));
            GOFontUnitsProperty* fontUnits
                = static_cast<GOFontUnitsProperty*>(findProperty(GO_FONT_UNITS_PROPERTY_NAME_STR));
            double fontSize = fontsize->data();
            if (isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
                fontSize *= subplotTextQtFontScale(this);
            }
            if (fontUnits->isEqual(GO_PROPERTY_VALUE_PIXELS_STR)) {
                fnt.pixelSize = std::round(fontSize);
                fnt.pointSizeF = 0;
            } else {
                fnt.pointSizeF = goFontPointsToPointSize(fontSize);
                fnt.pixelSize = 0;
            }
            if (isType(GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
                fnt.stretch = subplotTextQtFontStretch(this) / 100.0;
            }
        }
        if (!stateInitialized || hasChanged(GO_FONT_SMOOTHING_PROPERTY_NAME_STR)) {
            GOOnOffProperty* fontSmoothingProperty
                = static_cast<GOOnOffProperty*>(findProperty(GO_FONT_SMOOTHING_PROPERTY_NAME_STR));
            fnt.antialias = fontSmoothingProperty->asBool();
        }
        if (!stateInitialized || hasChanged(GO_FONT_ANGLE_PROPERTY_NAME_STR)) {
            GOFontAngleProperty* fontangle
                = static_cast<GOFontAngleProperty*>(findProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR));
            fnt.italic = fontangle->isEqual(GO_PROPERTY_VALUE_ITALIC_STR)
                || fontangle->isEqual(GO_PROPERTY_VALUE_OBLIQUE_STR);
        }
        if (!stateInitialized || hasChanged(GO_FONT_WEIGHT_PROPERTY_NAME_STR)) {
            GOFontWeightProperty* fontweight = static_cast<GOFontWeightProperty*>(
                findProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR));
            int weight = FontSpec::WeightNormal;
            if (fontweight->isEqual(GO_PROPERTY_VALUE_BOLD_STR)) {
                weight = FontSpec::WeightBold;
            }
            if (fontweight->isEqual(GO_PROPERTY_VALUE_LIGHT_STR)) {
                weight = FontSpec::WeightLight;
            }
            if (fontweight->isEqual(GO_PROPERTY_VALUE_DEMI_STR)) {
                weight = FontSpec::WeightDemiBold;
            }
            fnt.setWeight(weight);
        }
    }
    if (textChanged) {
        GOStringProperty* txt
            = static_cast<GOStringProperty*>(findProperty(GO_STRING_PROPERTY_NAME_STR));
        GOTextInterpreterProperty* textInterpreter = static_cast<GOTextInterpreterProperty*>(
            findProperty(GO_INTERPRETER_PROPERTY_NAME_STR));
        texScript = false;
        const TEXT_INTERPRETER_FORMAT interpreter = textInterpreter->getAsEnum();
        if (interpreter == TEXT_INTERPRETER_FORMAT::TEX_MARKUP
            || interpreter == TEXT_INTERPRETER_FORMAT::LATEX_MARKUP) {
            // 'latex' falls back to the 'tex' pipeline after stripping surrounding $...$
            // delimiters.
            const std::wstring raw = (interpreter == TEXT_INTERPRETER_FORMAT::LATEX_MARKUP)
                ? stripLatexMathDelimiters(txt->data())
                : txt->data();
            // Lay out true raised/lowered runs only for single-line strings. A multi-line
            // string keeps the Unicode-glyph approximation, since the run layout does not
            // model line breaks.
            if (texHasScript(raw) && raw.find(L'\n') == std::wstring::npos) {
                texScript = true;
                text = raw;
            } else {
                text = texToUnicode(raw);
            }
        } else {
            text = txt->data();
        }
    }
    int textWidth = 0;
    int textHeight = 0;
    double descent = goFontDescent(fnt);
    if (texScript) {
        TexBlockLayout layout = buildTexBlockLayout(texToRuns(text), fnt);
        textWidth = static_cast<int>(std::lround(layout.totalWidth));
        textHeight = static_cast<int>(std::lround(layout.topAbove + layout.botBelow));
        descent = layout.botBelow;
    } else {
        measureGOText(text, fnt, textWidth, textHeight);
    }
    // An empty string occupies nothing (Qt reported [0 -1 0 0] here).
    const bool emptyBox = text.empty() || textWidth <= 0;
    double left = 0.;
    double bottom = emptyBox ? 0. : -descent;
    double width = emptyBox ? 0. : textWidth;
    double height = emptyBox ? 0. : textHeight;
    setFourVectorDefault(GO_BOUNDING_BOX_PROPERTY_NAME_STR, left, bottom, width, height);
    setFourVectorDefault(GO_EXTENT_PROPERTY_NAME_STR, left, bottom, width, height);
    stateInitialized = true;
    clearChanged(fontProperties);
    clearChanged(textProperties);
    // Sweep the orientation flags (marked modified by the setup-time defaults on
    // the first pass) so a later pass does not read them as a user change. Genuine
    // user changes are caught and cleared by the auto->manual block at the top of
    // updateState.
    clearChanged(GO_ROTATION_PROPERTY_NAME_STR);
    clearChanged(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    clearChanged(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    // A title/subtitle string appearing or disappearing changes the title's
    // vertical offset (it shifts up for a subtitle), so recompute the title layout
    // on text changes too.
    markParentTitleLayoutChanged(this, wasStateInitialized && (fontChanged || textChanged));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
