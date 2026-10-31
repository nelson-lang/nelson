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
#include <limits>
#include <sstream>
#include "GOAnnotation.hpp"
#include "GOFontSpecHelpers.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOTextInterpreterProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "TexToUnicode.hpp"
#include "GOSurfaceScale.hpp"
#include "UnitsConversionHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // Annotations draw in dark grey, not pure black.
    constexpr double ANNOTATION_FOREGROUND_COLOR = 33.0 / 255.0;
    //=============================================================================
    static const wchar_t* HEAD_STYLE_DICT[] = { L"plain", L"ellipse", L"vback1", L"vback2",
        L"vback3", L"cback1", L"cback2", L"cback3", L"fourstar", L"rectangle", L"diamond", L"rose",
        L"hypocycloid", L"astroid", L"deltoid", L"none", 0 };
    //=============================================================================
    static const wchar_t* TEXT_ARROW_VERTICAL_DICT[] = { GO_PROPERTY_VALUE_TOP_STR, L"cap",
        GO_PROPERTY_VALUE_MIDDLE_STR, L"baseline", GO_PROPERTY_VALUE_BOTTOM_STR, 0 };
    //=============================================================================
    static const wchar_t* TEXT_BOX_VERTICAL_DICT[] = { GO_PROPERTY_VALUE_TOP_STR,
        GO_PROPERTY_VALUE_MIDDLE_STR, GO_PROPERTY_VALUE_BOTTOM_STR, 0 };
    //=============================================================================
    static const wchar_t* ANNOTATION_UNITS_DICT[]
        = { GO_PROPERTY_VALUE_NORMALIZED_STR, GO_PROPERTY_VALUE_INCHES_STR,
              GO_PROPERTY_VALUE_CENTIMETERS_STR, GO_PROPERTY_VALUE_CHARACTERS_STR,
              GO_PROPERTY_VALUE_POINTS_STR, GO_PROPERTY_VALUE_PIXELS_STR, 0 };
    //=============================================================================
    class GOAnnotationUnitsProperty : public GORestrictedStringProperty
    {
    public:
        GOAnnotationUnitsProperty() : GORestrictedStringProperty(ANNOTATION_UNITS_DICT) { }
        void
        set(ArrayOf arg) override
        {
            if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
                if (arg.asWideString() == GO_PROPERTY_VALUE_DATA_STR) {
                    Error(L"Nelson:graphics:illegalSelection", L"Illegal selection for property.");
                }
            }
            GORestrictedStringProperty::set(arg);
        }
    };
    //=============================================================================
    class GOPositiveScalarProperty : public GOScalarProperty
    {
    public:
        void
        set(ArrayOf arg) override
        {
            double value = arg.asDoubleScalar();
            if (!std::isfinite(value) || value <= 0) {
                Error(L"Nelson:annotation:expectedPositiveScalar",
                    L"Positive scalar value expected.");
            }
            GOScalarProperty::set(arg);
        }
    };
    //=============================================================================
    class GONonnegativeScalarProperty : public GOScalarProperty
    {
    public:
        void
        set(ArrayOf arg) override
        {
            double value = arg.asDoubleScalar();
            if (!std::isfinite(value) || value < 0) {
                Error(L"Nelson:annotation:expectedNonnegativeScalar",
                    L"Nonnegative scalar value expected.");
            }
            GOScalarProperty::set(arg);
        }
    };
    //=============================================================================
    bool
    isLineLike(GOAnnotationKind kind)
    {
        return kind == GOAnnotationKind::Line || kind == GOAnnotationKind::Arrow
            || kind == GOAnnotationKind::DoubleArrow || kind == GOAnnotationKind::TextArrow;
    }
    //=============================================================================
    bool
    isShape(GOAnnotationKind kind)
    {
        return kind == GOAnnotationKind::Rectangle || kind == GOAnnotationKind::Ellipse;
    }
    //=============================================================================
    bool
    hasText(GOAnnotationKind kind)
    {
        return kind == GOAnnotationKind::TextArrow || kind == GOAnnotationKind::TextBox;
    }
    //=============================================================================
    bool
    isDrawableColor(const std::vector<double>& color)
    {
        return color.size() >= 3 && color[0] >= 0 && color[1] >= 0 && color[2] >= 0;
    }
    //=============================================================================
    std::vector<double>
    withAlpha(std::vector<double> color, double alpha)
    {
        if (isDrawableColor(color)) {
            if (color.size() < 4) {
                color.push_back(std::clamp(alpha, 0.0, 1.0));
            } else {
                color[3] = std::clamp(alpha, 0.0, 1.0);
            }
        }
        return color;
    }
    //=============================================================================
    double
    safePositive(double value, double fallback)
    {
        return std::isfinite(value) && value > 0 ? value : fallback;
    }
    //=============================================================================
    std::vector<double>
    rotatePoint(double x, double y, double cx, double cy, double angleDegrees)
    {
        double theta = angleDegrees * M_PI / 180.0;
        double ct = std::cos(theta);
        double st = std::sin(theta);
        double dx = x - cx;
        double dy = y - cy;
        return { cx + dx * ct - dy * st, cy + dx * st + dy * ct };
    }
    //=============================================================================
    std::wstring
    textFromProperty(GraphicsObject* object)
    {
        wstringVector lines = object->findStringVectorProperty(GO_STRING_PROPERTY_NAME_STR);
        if (lines.empty()) {
            return L"";
        }
        std::wstring text = lines[0];
        for (size_t k = 1; k < lines.size(); ++k) {
            text += L"\n";
            text += lines[k];
        }
        GOTextInterpreterProperty* interpreter = static_cast<GOTextInterpreterProperty*>(
            object->findProperty(GO_INTERPRETER_PROPERTY_NAME_STR));
        if (interpreter && interpreter->getAsEnum() == TEXT_INTERPRETER_FORMAT::TEX_MARKUP) {
            return texToUnicode(text);
        }
        return text;
    }
    //=============================================================================
    std::vector<std::wstring>
    splitTextLines(const std::wstring& text)
    {
        std::vector<std::wstring> lines;
        std::wstringstream stream(text);
        std::wstring line;
        while (std::getline(stream, line)) {
            lines.push_back(line);
        }
        if (lines.empty()) {
            lines.push_back(L"");
        }
        return lines;
    }
    //=============================================================================
    std::vector<std::wstring>
    wrapTextLines(const std::wstring& text, const FontSpec& font, double width)
    {
        const double px = fontSpecPixelSize(font);
        std::vector<std::wstring> result;
        for (const auto& sourceLine : splitTextLines(text)) {
            std::wstringstream stream(sourceLine);
            std::wstring word;
            std::wstring line;
            while (stream >> word) {
                std::wstring candidate = line.empty() ? word : line + L" " + word;
                if (!line.empty()
                    && FontMetrics::textWidthPixels(
                           candidate, font.family, px, font.bold, font.italic)
                        > width) {
                    result.push_back(line);
                    line = word;
                } else {
                    line = candidate;
                }
            }
            result.push_back(line);
        }
        return result;
    }
    //=============================================================================
    void
    measureTextLines(
        const std::vector<std::wstring>& lines, const FontSpec& font, int& width, int& height)
    {
        const double px = fontSpecPixelSize(font);
        width = 0;
        for (const auto& line : lines) {
            width = std::max(width,
                static_cast<int>(std::lround(
                    FontMetrics::textWidthPixels(line, font.family, px, font.bold, font.italic))));
        }
        height = goFontLineHeight(font) * static_cast<int>(std::max<size_t>(lines.size(), 1));
    }
    //=============================================================================
    FontSpec
    annotationFont(GraphicsObject* object, GOFigure* figure)
    {
        FontSpec font;
        font.family = object->findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
        double size
            = safePositive(object->findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR), 10.0);
        std::wstring units = object->findStringProperty(GO_FONT_UNITS_PROPERTY_NAME_STR);
        if (units == GO_PROPERTY_VALUE_PIXELS_STR) {
            font.pixelSize = std::round(size);
        } else if (units == GO_PROPERTY_VALUE_NORMALIZED_STR && figure) {
            font.pixelSize = std::max(1, static_cast<int>(std::round(size * figure->getHeight())));
        } else if (units == GO_PROPERTY_VALUE_INCHES_STR) {
            font.pointSizeF = size * 72.0;
        } else if (units == GO_PROPERTY_VALUE_CENTIMETERS_STR) {
            font.pointSizeF = size * 72.0 / 2.54;
        } else {
            font.pointSizeF = size;
        }
        std::wstring weight = object->findStringProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR);
        if (weight == GO_PROPERTY_VALUE_BOLD_STR) {
            font.setWeight(FontSpec::WeightBold);
        }
        std::wstring angle = object->findStringProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR);
        font.italic
            = angle == GO_PROPERTY_VALUE_ITALIC_STR || angle == GO_PROPERTY_VALUE_OBLIQUE_STR;
        return font;
    }
    //=============================================================================
    RenderInterface::AlignmentFlag
    horizontalFlag(const std::wstring& value)
    {
        if (value == GO_PROPERTY_VALUE_CENTER_STR) {
            return RenderInterface::Mean;
        }
        if (value == GO_PROPERTY_VALUE_RIGHT_STR) {
            return RenderInterface::Max;
        }
        return RenderInterface::Min;
    }
    //=============================================================================
    RenderInterface::AlignmentFlag
    verticalFlag(const std::wstring& value)
    {
        if (value == GO_PROPERTY_VALUE_MIDDLE_STR || value == L"cap" || value == L"baseline") {
            return RenderInterface::Mean;
        }
        if (value == GO_PROPERTY_VALUE_BOTTOM_STR) {
            return RenderInterface::Min;
        }
        return RenderInterface::Max;
    }
    //=============================================================================
    void
    drawPolygon(RenderInterface& gc, const std::vector<std::vector<double>>& points,
        const std::vector<double>& faceColor, const std::vector<double>& edgeColor,
        double lineWidth, const std::wstring& lineStyle)
    {
        if (points.size() < 3) {
            return;
        }
        double cx = 0;
        double cy = 0;
        for (const auto& point : points) {
            cx += point[0];
            cy += point[1];
        }
        cx /= static_cast<double>(points.size());
        cy /= static_cast<double>(points.size());
        if (isDrawableColor(faceColor)) {
            if (!gc.polygonFill(points, faceColor)) {
                gc.color(faceColor);
                for (size_t k = 0; k < points.size(); ++k) {
                    const auto& p1 = points[k];
                    const auto& p2 = points[(k + 1) % points.size()];
                    gc.tri(cx, cy, 0, p1[0], p1[1], 0, p2[0], p2[1], 0);
                }
            }
        }
        if (isDrawableColor(edgeColor) && lineStyle != GO_PROPERTY_VALUE_NONE_STR
            && lineWidth > 0) {
            gc.color(edgeColor);
            gc.lineWidth(lineWidth);
            gc.setLineStyle(lineStyle);
            for (size_t k = 0; k < points.size(); ++k) {
                const auto& p1 = points[k];
                const auto& p2 = points[(k + 1) % points.size()];
                gc.line(p1[0], p1[1], p2[0], p2[1]);
            }
        }
    }
    //=============================================================================
    std::vector<std::vector<double>>
    ellipsePoints(double x, double y, double width, double height, double angleDegrees)
    {
        constexpr int segments = 72;
        std::vector<std::vector<double>> points;
        points.reserve(segments);
        double cx = x + width / 2.0;
        double cy = y + height / 2.0;
        for (int k = 0; k < segments; ++k) {
            double theta = 2.0 * M_PI * static_cast<double>(k) / static_cast<double>(segments);
            double px = cx + width * std::cos(theta) / 2.0;
            double py = cy + height * std::sin(theta) / 2.0;
            points.push_back(rotatePoint(px, py, x, y, angleDegrees));
        }
        return points;
    }
    //=============================================================================
    std::vector<std::vector<double>>
    rectanglePoints(double x, double y, double width, double height, double angleDegrees,
        bool rotateAroundCenter)
    {
        if (rotateAroundCenter) {
            double cx = x + width / 2.0;
            double cy = y + height / 2.0;
            return { rotatePoint(x, y, cx, cy, angleDegrees),
                rotatePoint(x + width, y, cx, cy, angleDegrees),
                rotatePoint(x + width, y + height, cx, cy, angleDegrees),
                rotatePoint(x, y + height, cx, cy, angleDegrees) };
        }
        return { rotatePoint(x, y, x, y, angleDegrees),
            rotatePoint(x + width, y, x, y, angleDegrees),
            rotatePoint(x + width, y + height, x, y, angleDegrees),
            rotatePoint(x, y + height, x, y, angleDegrees) };
    }
    //=============================================================================
    void
    drawArrowHead(RenderInterface& gc, double tipX, double tipY, double tailX, double tailY,
        double headLength, double headWidth, const std::wstring& style,
        const std::vector<double>& color, double lineWidth)
    {
        if (!isDrawableColor(color)) {
            return;
        }
        double dx = tipX - tailX;
        double dy = tipY - tailY;
        double len = std::hypot(dx, dy);
        if (len <= std::numeric_limits<double>::epsilon()) {
            return;
        }
        double ux = dx / len;
        double uy = dy / len;
        double nx = -uy;
        double ny = ux;
        headLength = safePositive(headLength, 10.0);
        headWidth = safePositive(headWidth, 10.0);
        double baseX = tipX - ux * headLength;
        double baseY = tipY - uy * headLength;
        if (style == GO_PROPERTY_VALUE_NONE_STR) {
            return;
        }
        auto pointAt = [&](double centerX, double centerY, double along, double across) {
            return std::vector<double> { centerX + ux * along + nx * across,
                centerY + uy * along + ny * across };
        };
        if (style == L"rectangle" || style == L"diamond" || style == L"fourstar" || style == L"rose"
            || style == L"hypocycloid" || style == L"astroid" || style == L"deltoid") {
            std::vector<std::vector<double>> points;
            double centerX = tipX - ux * headLength / 2.0;
            double centerY = tipY - uy * headLength / 2.0;
            if (style == L"rectangle") {
                points = { pointAt(centerX, centerY, headLength / 2.0, -headWidth / 2.0),
                    pointAt(centerX, centerY, headLength / 2.0, headWidth / 2.0),
                    pointAt(centerX, centerY, -headLength / 2.0, headWidth / 2.0),
                    pointAt(centerX, centerY, -headLength / 2.0, -headWidth / 2.0) };
            } else if (style == L"diamond") {
                points = { pointAt(centerX, centerY, headLength / 2.0, 0),
                    pointAt(centerX, centerY, 0, headWidth / 2.0),
                    pointAt(centerX, centerY, -headLength / 2.0, 0),
                    pointAt(centerX, centerY, 0, -headWidth / 2.0) };
            } else {
                int segments = (style == L"fourstar") ? 8 : 40;
                for (int k = 0; k < segments; ++k) {
                    double theta
                        = 2.0 * M_PI * static_cast<double>(k) / static_cast<double>(segments);
                    double radius = 1.0;
                    if (style == L"fourstar") {
                        radius = (k % 2 == 0) ? 1.0 : 0.38;
                    } else if (style == L"rose") {
                        radius = 0.35 + 0.65 * std::abs(std::sin(2.0 * theta));
                    } else if (style == L"hypocycloid" || style == L"astroid") {
                        radius = std::pow(std::max(0.05, std::abs(std::cos(2.0 * theta))), 0.35);
                    } else if (style == L"deltoid") {
                        radius = 0.70 + 0.30 * std::cos(3.0 * theta);
                    }
                    points.push_back(
                        pointAt(centerX, centerY, std::cos(theta) * headLength * radius / 2.0,
                            std::sin(theta) * headWidth * radius / 2.0));
                }
            }
            drawPolygon(gc, points, color, color, lineWidth, L"-");
            return;
        }
        if (style == L"ellipse") {
            constexpr int segments = 36;
            std::vector<std::vector<double>> points;
            points.reserve(segments);
            double centerX = tipX - ux * headLength / 2.0;
            double centerY = tipY - uy * headLength / 2.0;
            for (int k = 0; k < segments; ++k) {
                double theta = 2.0 * M_PI * static_cast<double>(k) / static_cast<double>(segments);
                double along = std::cos(theta) * headLength / 2.0;
                double across = std::sin(theta) * headWidth / 2.0;
                points.push_back(
                    { centerX + ux * along + nx * across, centerY + uy * along + ny * across });
            }
            drawPolygon(gc, points, color, color, lineWidth, L"-");
            return;
        }

        double notch = 0.0;
        if (style == L"vback1" || style == L"cback1") {
            notch = headLength * 0.20;
        } else if (style == L"vback2" || style == L"cback2") {
            notch = headLength * 0.35;
        } else if (style == L"vback3" || style == L"cback3") {
            notch = headLength * 0.50;
        }
        std::vector<std::vector<double>> points { { tipX, tipY },
            { baseX + nx * headWidth / 2.0, baseY + ny * headWidth / 2.0 } };
        if (style == L"cback1" || style == L"cback2" || style == L"cback3") {
            constexpr int curveSegments = 8;
            double controlX = baseX + ux * notch;
            double controlY = baseY + uy * notch;
            for (int k = 1; k < curveSegments; ++k) {
                double t = static_cast<double>(k) / static_cast<double>(curveSegments);
                double oneMinusT = 1.0 - t;
                double leftX = baseX + nx * headWidth / 2.0;
                double leftY = baseY + ny * headWidth / 2.0;
                double rightX = baseX - nx * headWidth / 2.0;
                double rightY = baseY - ny * headWidth / 2.0;
                points.push_back(
                    { oneMinusT * oneMinusT * leftX + 2 * oneMinusT * t * controlX + t * t * rightX,
                        oneMinusT * oneMinusT * leftY + 2 * oneMinusT * t * controlY
                            + t * t * rightY });
            }
        } else if (notch > 0) {
            points.push_back({ baseX + ux * notch, baseY + uy * notch });
        }
        points.push_back({ baseX - nx * headWidth / 2.0, baseY - ny * headWidth / 2.0 });
        drawPolygon(gc, points, color, color, lineWidth, L"-");
    }
    //=============================================================================
} // namespace
//=============================================================================
GOAnnotation::GOAnnotation(GOAnnotationKind kind) : m_kind(kind)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOAnnotation::getType()
{
    // The Type property reports the shape name (arrowshape, textboxshape, ...).
    // It is deliberately distinct from the constructor/factory name
    // (annotation('arrow', ...), annotation('textbox', ...)) so that dispatch by
    // Type stays unambiguous with the primitive 'line' and 'rectangle' objects.
    switch (m_kind) {
    case GOAnnotationKind::Line:
        return GO_PROPERTY_VALUE_LINE_SHAPE_STR;
    case GOAnnotationKind::Arrow:
        return GO_PROPERTY_VALUE_ARROW_SHAPE_STR;
    case GOAnnotationKind::DoubleArrow:
        return GO_PROPERTY_VALUE_DOUBLEARROW_SHAPE_STR;
    case GOAnnotationKind::TextArrow:
        return GO_PROPERTY_VALUE_TEXTARROW_SHAPE_STR;
    case GOAnnotationKind::Rectangle:
        return GO_PROPERTY_VALUE_RECTANGLE_SHAPE_STR;
    case GOAnnotationKind::Ellipse:
        return GO_PROPERTY_VALUE_ELLIPSE_SHAPE_STR;
    case GOAnnotationKind::TextBox:
        return GO_PROPERTY_VALUE_TEXTBOX_SHAPE_STR;
    }
    return GO_PROPERTY_VALUE_LINE_SHAPE_STR;
}
//=============================================================================
std::string
GOAnnotation::getClassName()
{
    switch (m_kind) {
    case GOAnnotationKind::Line:
        return GO_CLASS_NAME_SHAPE_LINE_STR;
    case GOAnnotationKind::Arrow:
        return GO_CLASS_NAME_SHAPE_ARROW_STR;
    case GOAnnotationKind::DoubleArrow:
        return GO_CLASS_NAME_SHAPE_DOUBLE_ARROW_STR;
    case GOAnnotationKind::TextArrow:
        return GO_CLASS_NAME_SHAPE_TEXT_ARROW_STR;
    case GOAnnotationKind::Rectangle:
        return GO_CLASS_NAME_SHAPE_RECTANGLE_STR;
    case GOAnnotationKind::Ellipse:
        return GO_CLASS_NAME_SHAPE_ELLIPSE_STR;
    case GOAnnotationKind::TextBox:
        return GO_CLASS_NAME_SHAPE_TEXTBOX_STR;
    }
    return GO_CLASS_NAME_SHAPE_LINE_STR;
}
//=============================================================================
void
GOAnnotation::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);

    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CONTEXT_MENU_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    registerProperty(new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);

    if (isLineLike(m_kind) || isShape(m_kind) || m_kind == GOAnnotationKind::TextBox) {
        registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
        registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    }
    if (isLineLike(m_kind)) {
        registerProperty(new GOTwoVectorProperty, GO_X_PROPERTY_NAME_STR);
        registerProperty(new GOTwoVectorProperty, GO_Y_PROPERTY_NAME_STR);
    }
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOAnnotationUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);

    if (m_kind == GOAnnotationKind::Arrow || m_kind == GOAnnotationKind::TextArrow) {
        registerProperty(
            new GORestrictedStringProperty(HEAD_STYLE_DICT), GO_HEAD_STYLE_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_HEAD_LENGTH_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_HEAD_WIDTH_PROPERTY_NAME_STR);
    }
    if (m_kind == GOAnnotationKind::DoubleArrow) {
        registerProperty(
            new GORestrictedStringProperty(HEAD_STYLE_DICT), GO_HEAD_1_STYLE_PROPERTY_NAME_STR);
        registerProperty(
            new GORestrictedStringProperty(HEAD_STYLE_DICT), GO_HEAD_2_STYLE_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_HEAD_1_LENGTH_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_HEAD_2_LENGTH_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_HEAD_1_WIDTH_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_HEAD_2_WIDTH_PROPERTY_NAME_STR);
    }
    if (isShape(m_kind)) {
        registerProperty(new GOColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
        registerProperty(new GOScalarProperty, GO_ROTATION_PROPERTY_NAME_STR);
        if (m_kind == GOAnnotationKind::Rectangle) {
            registerProperty(new GOScalarProperty(0, 1), GO_FACE_ALPHA_PROPERTY_NAME_STR);
        }
    }
    if (hasText(m_kind)) {
        // String reports as a cell (empty by default).
        registerProperty(new GOStringVectorProperty(true), GO_STRING_PROPERTY_NAME_STR);
        registerProperty(new GOTextInterpreterProperty, GO_INTERPRETER_PROPERTY_NAME_STR);
        registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_FONT_SIZE_PROPERTY_NAME_STR);
        registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
        registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
        registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
        registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    }
    if (m_kind == GOAnnotationKind::TextBox) {
        registerProperty(new GOOnOffProperty, GO_FIT_BOX_TO_TEXT_PROPERTY_NAME_STR);
        registerProperty(new GOColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
        registerProperty(new GOColorProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
        registerProperty(new GOScalarProperty(0, 1), GO_FACE_ALPHA_PROPERTY_NAME_STR);
        registerProperty(new GONonnegativeScalarProperty, GO_MARGIN_PROPERTY_NAME_STR);
        registerProperty(new GOScalarProperty, GO_ROTATION_PROPERTY_NAME_STR);
        registerProperty(new GORestrictedStringProperty(TEXT_BOX_VERTICAL_DICT),
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    }
    if (m_kind == GOAnnotationKind::TextArrow) {
        registerProperty(new GOScalarProperty, GO_TEXT_ROTATION_PROPERTY_NAME_STR);
        registerProperty(new GOColorProperty, GO_TEXT_COLOR_PROPERTY_NAME_STR);
        registerProperty(new GOColorProperty, GO_TEXT_EDGE_COLOR_PROPERTY_NAME_STR);
        registerProperty(new GOColorProperty, GO_TEXT_BACKGROUND_COLOR_PROPERTY_NAME_STR);
        registerProperty(new GOPositiveScalarProperty, GO_TEXT_LINE_WIDTH_PROPERTY_NAME_STR);
        registerProperty(new GONonnegativeScalarProperty, GO_TEXT_MARGIN_PROPERTY_NAME_STR);
        registerProperty(new GORestrictedStringProperty(TEXT_ARROW_VERTICAL_DICT),
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    }
    sortProperties();
}
//=============================================================================
void
GOAnnotation::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0.3, 0.3, 0.1, 0.1);
    setRestrictedStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMALIZED_STR);
    if (isLineLike(m_kind)) {
        setTwoVectorDefault(GO_X_PROPERTY_NAME_STR, 0.3, 0.4);
        setTwoVectorDefault(GO_Y_PROPERTY_NAME_STR, 0.3, 0.4);
    }
    if (haveProperty(GO_COLOR_PROPERTY_NAME_STR)) {
        setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, ANNOTATION_FOREGROUND_COLOR,
            ANNOTATION_FOREGROUND_COLOR, ANNOTATION_FOREGROUND_COLOR);
    }
    if (haveProperty(GO_LINE_STYLE_PROPERTY_NAME_STR)) {
        setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    }
    if (haveProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR)) {
        setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    }
    if (haveProperty(GO_HEAD_STYLE_PROPERTY_NAME_STR)) {
        setRestrictedStringDefault(GO_HEAD_STYLE_PROPERTY_NAME_STR, L"vback2");
        setScalarDoubleDefault(GO_HEAD_LENGTH_PROPERTY_NAME_STR, 10);
        setScalarDoubleDefault(GO_HEAD_WIDTH_PROPERTY_NAME_STR, 10);
    }
    if (m_kind == GOAnnotationKind::DoubleArrow) {
        setRestrictedStringDefault(GO_HEAD_1_STYLE_PROPERTY_NAME_STR, L"vback2");
        setRestrictedStringDefault(GO_HEAD_2_STYLE_PROPERTY_NAME_STR, L"vback2");
        setScalarDoubleDefault(GO_HEAD_1_LENGTH_PROPERTY_NAME_STR, 10);
        setScalarDoubleDefault(GO_HEAD_2_LENGTH_PROPERTY_NAME_STR, 10);
        setScalarDoubleDefault(GO_HEAD_1_WIDTH_PROPERTY_NAME_STR, 10);
        setScalarDoubleDefault(GO_HEAD_2_WIDTH_PROPERTY_NAME_STR, 10);
    }
    if (isShape(m_kind)) {
        setThreeVectorDefault(GO_FACE_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
        setScalarDoubleDefault(GO_ROTATION_PROPERTY_NAME_STR, 0);
        if (m_kind == GOAnnotationKind::Rectangle) {
            setScalarDoubleDefault(GO_FACE_ALPHA_PROPERTY_NAME_STR, 1);
        }
    }
    if (hasText(m_kind)) {
        static_cast<GOStringVectorProperty*>(findProperty(GO_STRING_PROPERTY_NAME_STR))
            ->data(wstringVector());
        setStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEX_STR);
        setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HELVETICA_STR);
        setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 10);
        setRestrictedStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
        setRestrictedStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
        setRestrictedStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
        setRestrictedStringDefault(
            GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
        setRestrictedStringDefault(
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR);
    }
    if (m_kind == GOAnnotationKind::TextBox) {
        setRestrictedStringDefault(GO_FIT_BOX_TO_TEXT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
        setThreeVectorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR, ANNOTATION_FOREGROUND_COLOR,
            ANNOTATION_FOREGROUND_COLOR, ANNOTATION_FOREGROUND_COLOR);
        setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
        setScalarDoubleDefault(GO_FACE_ALPHA_PROPERTY_NAME_STR, 1);
        setScalarDoubleDefault(GO_MARGIN_PROPERTY_NAME_STR, 5);
        setScalarDoubleDefault(GO_ROTATION_PROPERTY_NAME_STR, 0);
    }
    if (m_kind == GOAnnotationKind::TextArrow) {
        setRestrictedStringDefault(
            GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RIGHT_STR);
        setScalarDoubleDefault(GO_TEXT_ROTATION_PROPERTY_NAME_STR, 0);
        setThreeVectorDefault(GO_TEXT_COLOR_PROPERTY_NAME_STR, ANNOTATION_FOREGROUND_COLOR,
            ANNOTATION_FOREGROUND_COLOR, ANNOTATION_FOREGROUND_COLOR);
        setThreeVectorDefault(GO_TEXT_EDGE_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
        setThreeVectorDefault(GO_TEXT_BACKGROUND_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
        setScalarDoubleDefault(GO_TEXT_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
        setScalarDoubleDefault(GO_TEXT_MARGIN_PROPERTY_NAME_STR, 2);
    }
    clearAllChanged();
}
//=============================================================================
void
GOAnnotation::updateUnits()
{
    if (!hasChanged(GO_UNITS_PROPERTY_NAME_STR)) {
        return;
    }
    std::wstring previousUnits = m_units;
    std::wstring currentUnits = findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    if (previousUnits == currentUnits) {
        clearChanged(GO_UNITS_PROPERTY_NAME_STR);
        return;
    }
    GOFigure* fig = getParentFigure();
    double width = fig ? fig->getLayoutWidth() : 560;
    double height = fig ? fig->getLayoutHeight() : 420;
    std::vector<double> pos = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    convertPositionPixelOrigin(previousUnits, currentUnits, pos, width, height);
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, pos[0], pos[1], pos[2], pos[3]);
    if (isLineLike(m_kind)) {
        setTwoVectorDefault(GO_X_PROPERTY_NAME_STR, pos[0], pos[0] + pos[2]);
        setTwoVectorDefault(GO_Y_PROPERTY_NAME_STR, pos[1], pos[1] + pos[3]);
    }
    m_units = currentUnits;
    clearChanged(GO_UNITS_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOAnnotation::syncLinePosition()
{
    if (!isLineLike(m_kind) || m_updatingPosition) {
        return;
    }
    m_updatingPosition = true;
    bool positionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR);
    bool xChanged = hasChanged(GO_X_PROPERTY_NAME_STR);
    bool yChanged = hasChanged(GO_Y_PROPERTY_NAME_STR);
    if (positionChanged && !(xChanged || yChanged)) {
        std::vector<double> pos = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (pos.size() >= 4) {
            setTwoVectorDefault(GO_X_PROPERTY_NAME_STR, pos[0], pos[0] + pos[2]);
            setTwoVectorDefault(GO_Y_PROPERTY_NAME_STR, pos[1], pos[1] + pos[3]);
        }
    } else if (xChanged || yChanged) {
        std::vector<double> x = findVectorDoubleProperty(GO_X_PROPERTY_NAME_STR);
        std::vector<double> y = findVectorDoubleProperty(GO_Y_PROPERTY_NAME_STR);
        if (x.size() >= 2 && y.size() >= 2) {
            setFourVectorDefault(
                GO_POSITION_PROPERTY_NAME_STR, x[0], y[0], x[1] - x[0], y[1] - y[0]);
        }
    }
    clearChanged({ GO_X_PROPERTY_NAME_STR, GO_Y_PROPERTY_NAME_STR, GO_POSITION_PROPERTY_NAME_STR });
    m_updatingPosition = false;
}
//=============================================================================
void
GOAnnotation::updateState()
{
    updateUnits();
    syncLinePosition();
}
//=============================================================================
std::vector<double>
GOAnnotation::positionInPixels(RenderInterface& gc)
{
    return positionOnSurface(this, findStringProperty(GO_UNITS_PROPERTY_NAME_STR),
        findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR), 1.0, gc.width(), gc.height());
}
//=============================================================================
void
GOAnnotation::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    gc.setupDirectDraw();
    if (isLineLike(m_kind)) {
        paintLineLike(gc);
    } else if (isShape(m_kind)) {
        paintShape(gc);
    } else if (m_kind == GOAnnotationKind::TextBox) {
        paintTextBox(gc);
    }
    gc.releaseDirectDraw();
}
//=============================================================================
void
GOAnnotation::paintLineLike(RenderInterface& gc)
{
    std::vector<double> pos = positionInPixels(gc);
    double x1 = pos[0];
    double y1 = pos[1];
    double x2 = pos[0] + pos[2];
    double y2 = pos[1] + pos[3];
    std::vector<double> color = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    double lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    std::wstring lineStyle = findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR);
    if (isDrawableColor(color) && lineStyle != GO_PROPERTY_VALUE_NONE_STR && lineWidth > 0) {
        gc.color(color);
        gc.lineWidth(lineWidth);
        gc.setLineStyle(lineStyle);
        gc.line(x1, y1, x2, y2);
    }
    if (m_kind == GOAnnotationKind::Arrow || m_kind == GOAnnotationKind::TextArrow) {
        drawArrowHead(gc, x2, y2, x1, y1,
            findScalarDoubleProperty(GO_HEAD_LENGTH_PROPERTY_NAME_STR),
            findScalarDoubleProperty(GO_HEAD_WIDTH_PROPERTY_NAME_STR),
            findStringProperty(GO_HEAD_STYLE_PROPERTY_NAME_STR), color, lineWidth);
    } else if (m_kind == GOAnnotationKind::DoubleArrow) {
        drawArrowHead(gc, x1, y1, x2, y2,
            findScalarDoubleProperty(GO_HEAD_1_LENGTH_PROPERTY_NAME_STR),
            findScalarDoubleProperty(GO_HEAD_1_WIDTH_PROPERTY_NAME_STR),
            findStringProperty(GO_HEAD_1_STYLE_PROPERTY_NAME_STR), color, lineWidth);
        drawArrowHead(gc, x2, y2, x1, y1,
            findScalarDoubleProperty(GO_HEAD_2_LENGTH_PROPERTY_NAME_STR),
            findScalarDoubleProperty(GO_HEAD_2_WIDTH_PROPERTY_NAME_STR),
            findStringProperty(GO_HEAD_2_STYLE_PROPERTY_NAME_STR), color, lineWidth);
    }
    if (m_kind != GOAnnotationKind::TextArrow) {
        return;
    }
    std::wstring text = textFromProperty(this);
    if (text.empty()) {
        return;
    }
    FontSpec font = annotationFont(this, getParentFigure());
    int textWidth = 0;
    int textHeight = 0;
    int textXOffset = 0;
    int textYOffset = 0;
    gc.measureText(text, font, RenderInterface::Min, RenderInterface::Max, textWidth, textHeight,
        textXOffset, textYOffset);
    double margin = findScalarDoubleProperty(GO_TEXT_MARGIN_PROPERTY_NAME_STR);
    double boxX = x1;
    double boxY = y1 - textHeight - margin * 2;
    double boxW = textWidth + margin * 2;
    double boxH = textHeight + margin * 2;
    std::vector<double> background
        = findVectorDoubleProperty(GO_TEXT_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    std::vector<double> edge = findVectorDoubleProperty(GO_TEXT_EDGE_COLOR_PROPERTY_NAME_STR);
    if (isDrawableColor(background)) {
        drawPolygon(gc, rectanglePoints(boxX, boxY, boxW, boxH, 0, true), background, {}, 0,
            GO_PROPERTY_VALUE_NONE_STR);
    }
    if (isDrawableColor(edge)) {
        drawPolygon(gc, rectanglePoints(boxX, boxY, boxW, boxH, 0, true), {}, edge,
            findScalarDoubleProperty(GO_TEXT_LINE_WIDTH_PROPERTY_NAME_STR), L"-");
    }
    gc.putText(boxX + margin, boxY + boxH - margin, text,
        findVectorDoubleProperty(GO_TEXT_COLOR_PROPERTY_NAME_STR),
        horizontalFlag(findStringProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR)),
        verticalFlag(findStringProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR)), font,
        findScalarDoubleProperty(GO_TEXT_ROTATION_PROPERTY_NAME_STR));
}
//=============================================================================
void
GOAnnotation::paintShape(RenderInterface& gc)
{
    std::vector<double> pos = positionInPixels(gc);
    double angle = findScalarDoubleProperty(GO_ROTATION_PROPERTY_NAME_STR);
    std::vector<std::vector<double>> points = (m_kind == GOAnnotationKind::Ellipse)
        ? ellipsePoints(pos[0], pos[1], pos[2], pos[3], angle)
        : rectanglePoints(pos[0], pos[1], pos[2], pos[3], angle, false);
    std::vector<double> face = findVectorDoubleProperty(GO_FACE_COLOR_PROPERTY_NAME_STR);
    if (m_kind == GOAnnotationKind::Rectangle) {
        face = withAlpha(face, findScalarDoubleProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
    }
    drawPolygon(gc, points, face, findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR),
        findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR),
        findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
}
//=============================================================================
void
GOAnnotation::paintTextBox(RenderInterface& gc)
{
    std::vector<double> pos = positionInPixels(gc);
    std::wstring rawText = textFromProperty(this);
    FontSpec font = annotationFont(this, getParentFigure());
    double margin = findScalarDoubleProperty(GO_MARGIN_PROPERTY_NAME_STR);
    bool fitBox = stringCheck(GO_FIT_BOX_TO_TEXT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    std::vector<std::wstring> lines = fitBox
        ? splitTextLines(rawText)
        : wrapTextLines(rawText, font, std::max(1.0, pos[2] - 2.0 * margin));
    int textWidth = 0;
    int textHeight = 0;
    measureTextLines(lines, font, textWidth, textHeight);
    if (stringCheck(GO_FIT_BOX_TO_TEXT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        double top = pos[1] + pos[3];
        pos[2] = textWidth + margin * 2;
        pos[3] = textHeight + margin * 2;
        pos[1] = top - pos[3];
    }
    double angle = findScalarDoubleProperty(GO_ROTATION_PROPERTY_NAME_STR);
    std::vector<std::vector<double>> points
        = rectanglePoints(pos[0], pos[1], pos[2], pos[3], angle, false);
    std::vector<double> background
        = withAlpha(findVectorDoubleProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR),
            findScalarDoubleProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
    drawPolygon(gc, points, background, findVectorDoubleProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR),
        findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR),
        findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
    if (!rawText.empty()) {
        double textX = pos[0] + margin;
        std::wstring horizontal = findStringProperty(GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
        if (horizontal == GO_PROPERTY_VALUE_CENTER_STR) {
            textX = pos[0] + pos[2] / 2.0;
        } else if (horizontal == GO_PROPERTY_VALUE_RIGHT_STR) {
            textX = pos[0] + pos[2] - margin;
        }
        double textY = pos[1] + pos[3] - margin;
        std::wstring vertical = findStringProperty(GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
        if (vertical == GO_PROPERTY_VALUE_MIDDLE_STR) {
            textY = pos[1] + (pos[3] + textHeight) / 2.0;
        } else if (vertical == GO_PROPERTY_VALUE_BOTTOM_STR) {
            textY = pos[1] + margin + textHeight;
        }
        const int lineHeight = goFontLineHeight(font);
        std::vector<double> color = findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
        RenderInterface::AlignmentFlag xFlag = horizontalFlag(horizontal);
        for (size_t k = 0; k < lines.size(); ++k) {
            gc.putText(textX, textY - static_cast<double>(k) * lineHeight, lines[k], color, xFlag,
                RenderInterface::Max, font, angle);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
