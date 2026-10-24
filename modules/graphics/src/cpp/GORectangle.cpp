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
#include "GORectangle.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GORestrictedStringColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOCommonProperties.hpp"
#include "RGBAColorData.hpp"
#include "RenderInterface.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static const wchar_t* RECTANGLE_COLOR_DICT[]
        = { GO_PROPERTY_VALUE_NONE_STR, GO_PROPERTY_VALUE_COLORSPEC_STR, nullptr };
    //=============================================================================
    // Curvature accepts a scalar or a 1x2 vector, each element in the range [0, 1].
    class GOCurvatureProperty : public GOArrayOfProperty
    {
    public:
        void
        set(ArrayOf arg) override
        {
            if (!arg.isNumeric() || !arg.isReal()) {
                Error(L"Nelson:rectangle:invalidCurvature",
                    L"Curvature must be a scalar or a 1x2 vector of type double in the range [0, "
                    L"1].");
            }
            std::vector<double> values = arg.asDoubleVector();
            if (values.size() != 1 && values.size() != 2) {
                Error(L"Nelson:rectangle:invalidCurvature",
                    L"Curvature must be a scalar or a 1x2 vector of type double in the range [0, "
                    L"1].");
            }
            for (double value : values) {
                if (!std::isfinite(value) || value < 0. || value > 1.) {
                    Error(L"Nelson:rectangle:invalidCurvature",
                        L"Curvature must be a scalar or a 1x2 vector of type double in the range "
                        L"[0, 1].");
                }
            }
            GOArrayOfProperty::set(ArrayOf::doubleRowVector(values));
        }
    };
    //=============================================================================
} // namespace
//=============================================================================
GORectangle::GORectangle()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GORectangle::~GORectangle() = default;
//=============================================================================
std::wstring
GORectangle::getType()
{
    return GO_PROPERTY_VALUE_RECTANGLE_STR;
}
//=============================================================================
void
GORectangle::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCurvatureProperty, GO_CURVATURE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringColorProperty(RECTANGLE_COLOR_DICT), GO_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringColorProperty(RECTANGLE_COLOR_DICT), GO_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GORectangle::setupDefaults()
{
    setRestrictedStringDefault(
        GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RECTANGLE_STR);
    static_cast<GOArrayOfProperty*>(findProperty(GO_CURVATURE_PROPERTY_NAME_STR))
        ->data(ArrayOf::doubleRowVector(std::vector<double> { 0., 0. }));
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0., 0., 1., 1.);
    // FaceColor / EdgeColor default to dark grey (33/255).
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1., -1., -1.);
    setRestrictedStringColorDefault(GO_EDGE_COLOR_PROPERTY_NAME_STR,
        GO_PROPERTY_VALUE_COLORSPEC_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setStringDefault(GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_FACE_ALPHA_PROPERTY_NAME_STR, 1.);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    initializeCommonProperties(this);
    clearAllChanged();
}
//=============================================================================
void
GORectangle::updateState()
{
    static const std::vector<std::wstring> geometryProperties = { GO_POSITION_PROPERTY_NAME_STR,
        GO_CURVATURE_PROPERTY_NAME_STR, GO_FACE_COLOR_PROPERTY_NAME_STR,
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_FACE_ALPHA_PROPERTY_NAME_STR };
    if (hasChanged(GO_EDGE_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
    }
    if (m_faces.empty() || hasChanged(geometryProperties)) {
        limitsDirty = true;
        buildGeometry();
        clearChanged(geometryProperties);
    }
}
//=============================================================================
void
GORectangle::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    if (m_faces.empty()) {
        buildGeometry();
    }
    PatchRenderOptions options;
    options.lineWidth = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    options.lineStyle = findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR);
    gc.drawPatch(m_faces, options);
}
//=============================================================================
std::vector<double>
GORectangle::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    double x = position.size() > 0 ? position[0] : 0.;
    double y = position.size() > 1 ? position[1] : 0.;
    double w = position.size() > 2 ? position[2] : 1.;
    double h = position.size() > 3 ? position[3] : 1.;
    double left = std::min(x, x + w);
    double right = std::max(x, x + w);
    double bottom = std::min(y, y + h);
    double top = std::max(y, y + h);
    limits = { left, right, bottom, top, 0., 0., 1., 1., 1., 1. };
    limitsDirty = false;
    return limits;
}
//=============================================================================
void
GORectangle::buildGeometry()
{
    m_faces.clear();
    std::vector<double> position = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() < 4) {
        return;
    }
    double x = position[0];
    double y = position[1];
    double w = position[2];
    double h = position[3];
    double left = x;
    double right = x + w;
    double bottom = y;
    double top = y + h;
    double aw = std::abs(w);
    double ah = std::abs(h);

    // Curvature is stored as a GOArrayOfProperty (scalar or 1x2), so it must be
    // read through findArrayOfProperty rather than the GOVectorProperty accessor.
    ArrayOf curvatureData(findArrayOfProperty(GO_CURVATURE_PROPERTY_NAME_STR));
    curvatureData.promoteType(NLS_DOUBLE);
    std::vector<double> curvature = curvatureData.asDoubleVector();
    double rx = 0.;
    double ry = 0.;
    if (curvature.size() == 1) {
        // Scalar curvature yields circular corners sized by the shorter side.
        double radius = curvature[0] * std::min(aw, ah) / 2.;
        rx = radius;
        ry = radius;
    } else if (curvature.size() >= 2) {
        rx = curvature[0] * aw / 2.;
        ry = curvature[1] * ah / 2.;
    }
    rx = std::min(rx, aw / 2.);
    ry = std::min(ry, ah / 2.);
    double sx = (right >= left) ? 1. : -1.;
    double sy = (top >= bottom) ? 1. : -1.;

    Face face;
    if (stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        face.FaceColorMode = ColorMode::None;
    } else {
        face.FaceColorMode = ColorMode::ColorSpec;
        std::vector<double> color = static_cast<GORestrictedStringColorProperty*>(
            findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR))
                                        ->colorSpec();
        double alpha = findScalarDoubleProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR);
        face.FaceColor = RGBAColorData(color[0], color[1], color[2], alpha);
    }
    if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        face.EdgeColorMode = ColorMode::None;
    } else {
        face.EdgeColorMode = ColorMode::ColorSpec;
        std::vector<double> color = static_cast<GORestrictedStringColorProperty*>(
            findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR))
                                        ->colorSpec();
        face.EdgeColor = RGBAColorData(color[0], color[1], color[2], 1.);
    }

    if (rx <= 0. && ry <= 0.) {
        face.vertices.push_back(point(left, bottom, 0.));
        face.vertices.push_back(point(right, bottom, 0.));
        face.vertices.push_back(point(right, top, 0.));
        face.vertices.push_back(point(left, top, 0.));
    } else {
        constexpr int segments = 20;
        struct Corner
        {
            double cx;
            double cy;
            double startAngle;
        };
        // Counter-clockwise: bottom-right, top-right, top-left, bottom-left corners.
        const Corner corners[4] = { { right - sx * rx, bottom + sy * ry, -M_PI / 2. },
            { right - sx * rx, top - sy * ry, 0. }, { left + sx * rx, top - sy * ry, M_PI / 2. },
            { left + sx * rx, bottom + sy * ry, M_PI } };
        for (const Corner& corner : corners) {
            for (int k = 0; k <= segments; ++k) {
                double angle = corner.startAngle + (M_PI / 2.) * static_cast<double>(k) / segments;
                face.vertices.push_back(point(corner.cx + sx * rx * std::cos(angle),
                    corner.cy + sy * ry * std::sin(angle), 0.));
            }
        }
    }
    m_faces.push_back(std::move(face));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
