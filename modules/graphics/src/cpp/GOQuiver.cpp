//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOQuiver.hpp"
#include "GOChartDecorators.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOAxis.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOSymbolProperty.hpp"
#include "RenderHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    const std::vector<std::wstring> ALIGNMENT_VALUES { GO_PROPERTY_VALUE_TAIL_STR, L"head",
        L"center" };
    //=============================================================================
    struct NumericData
    {
        std::vector<double> values;
        size_t rows = 0;
        size_t columns = 0;
    };
    //=============================================================================
    struct ArrowData
    {
        std::vector<double> x;
        std::vector<double> y;
        std::vector<double> z;
        std::vector<double> u;
        std::vector<double> v;
        std::vector<double> w;
        size_t rows = 0;
        size_t columns = 0;
    };
    //=============================================================================
    struct PixelSegment
    {
        double x1;
        double y1;
        double x2;
        double y2;
    };
    //=============================================================================
    struct PixelPoint
    {
        double x;
        double y;
    };
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true)
    {
        go->registerProperty(property, name, writable);
    }
    //=============================================================================
    static NumericData
    numericData(GraphicsObject* object, const std::wstring& name)
    {
        ArrayOf value = object->findArrayOfProperty(name);
        NumericData data;
        if (value.isEmpty()) {
            return data;
        }
        if (!value.isNumeric() && !value.isLogical()) {
            Error(L"Nelson:validators:mustBeNumeric", L"%s must be numeric.", name);
        }
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        data.values = asDouble.asDoubleVector();
        data.rows = static_cast<size_t>(std::max<indexType>(1, asDouble.rows()));
        data.columns = static_cast<size_t>(std::max<indexType>(1, asDouble.columns()));
        return data;
    }
    //=============================================================================
    static size_t
    tarrows(const NumericData& u)
    {
        return std::max<size_t>(1, u.rows);
    }
    //=============================================================================
    static size_t
    tarcolumns(const NumericData& u)
    {
        return std::max<size_t>(1, u.columns);
    }
    //=============================================================================
    static double
    componentAt(const NumericData& data, size_t index, const std::wstring& name)
    {
        if (data.values.empty()) {
            Error(L"Nelson:quiver:mustNotBeEmpty", L"%s must not be empty.", name);
        }
        if (index >= data.values.size()) {
            Error(L"Nelson:quiver:sizeMismatch", L"%s must match UData size.", name);
        }
        return data.values[index];
    }
    //=============================================================================
    static double
    coordinateAt(const NumericData& data, size_t index, size_t rows, size_t columns, size_t count,
        bool useColumns)
    {
        if (data.values.empty()) {
            return 0.;
        }
        if (data.values.size() == 1) {
            return data.values[0];
        }
        if (data.values.size() == count) {
            return data.values[index];
        }
        if (data.values.size() == rows * columns) {
            return data.values[index];
        }
        size_t row = index % rows;
        size_t column = index / rows;
        if (useColumns && data.values.size() == columns) {
            return data.values[column];
        }
        if (!useColumns && data.values.size() == rows) {
            return data.values[row];
        }
        Error(L"Nelson:quiver:coordinateSizeMismatch",
            L"Coordinate data must match vector component size.");
        return 0.;
    }
    //=============================================================================
    static ArrowData
    currentArrowData(GraphicsObject* object)
    {
        NumericData x = numericData(object, GO_X_DATA_PROPERTY_NAME_STR);
        NumericData y = numericData(object, GO_Y_DATA_PROPERTY_NAME_STR);
        NumericData z = numericData(object, GO_Z_DATA_PROPERTY_NAME_STR);
        NumericData u = numericData(object, GO_U_DATA_PROPERTY_NAME_STR);
        NumericData v = numericData(object, GO_V_DATA_PROPERTY_NAME_STR);
        NumericData w = numericData(object, GO_W_DATA_PROPERTY_NAME_STR);
        size_t rows = tarrows(u);
        size_t columns = tarcolumns(u);
        size_t count = u.values.size();
        if (count == 0) {
            return {};
        }
        if (v.values.size() != count || (!w.values.empty() && w.values.size() != count)) {
            Error(
                "Nelson:dimagree:vectorComponentSizesMatch", "Vector component sizes must match.");
        }
        ArrowData data;
        data.rows = rows;
        data.columns = columns;
        data.x.resize(count);
        data.y.resize(count);
        data.z.resize(count);
        data.u.resize(count);
        data.v.resize(count);
        data.w.resize(count);
        for (size_t k = 0; k < count; ++k) {
            data.x[k] = coordinateAt(x, k, rows, columns, count, true);
            data.y[k] = coordinateAt(y, k, rows, columns, count, false);
            data.z[k] = z.values.empty() ? 0. : coordinateAt(z, k, rows, columns, count, false);
            data.u[k] = componentAt(u, k, GO_U_DATA_PROPERTY_NAME_STR);
            data.v[k] = componentAt(v, k, GO_V_DATA_PROPERTY_NAME_STR);
            data.w[k] = w.values.empty() ? 0. : componentAt(w, k, GO_W_DATA_PROPERTY_NAME_STR);
        }
        return data;
    }
    //=============================================================================
    static double
    minimumPositiveSpacing(std::vector<double> values)
    {
        values.erase(std::remove_if(values.begin(), values.end(),
                         [](double value) { return !std::isfinite(value); }),
            values.end());
        if (values.size() < 2) {
            return 1.;
        }
        std::sort(values.begin(), values.end());
        values.erase(std::unique(values.begin(), values.end()), values.end());
        double spacing = std::numeric_limits<double>::infinity();
        for (size_t k = 1; k < values.size(); ++k) {
            double delta = values[k] - values[k - 1];
            if (delta > 0.) {
                spacing = std::min(spacing, delta);
            }
        }
        return std::isfinite(spacing) ? spacing : 1.;
    }
    //=============================================================================
    static double
    finiteRange(const std::vector<double>& values)
    {
        double low = std::numeric_limits<double>::infinity();
        double high = -std::numeric_limits<double>::infinity();
        for (double value : values) {
            if (!std::isfinite(value)) {
                continue;
            }
            low = std::min(low, value);
            high = std::max(high, value);
        }
        if (!std::isfinite(low) || !std::isfinite(high)) {
            return 0.;
        }
        return high - low;
    }
    //=============================================================================
    static double
    computedScale(GraphicsObject* object, const ArrowData& data)
    {
        if (object->findStringProperty(GO_AUTO_SCALE_PROPERTY_NAME_STR)
            == GO_PROPERTY_VALUE_OFF_STR) {
            return 1.;
        }
        bool is3D = !numericData(object, GO_W_DATA_PROPERTY_NAME_STR).values.empty();
        double maxMagnitude = 0.;
        for (size_t k = 0; k < data.u.size(); ++k) {
            double magnitude
                = std::sqrt(data.u[k] * data.u[k] + data.v[k] * data.v[k] + data.w[k] * data.w[k]);
            if (std::isfinite(magnitude)) {
                maxMagnitude = std::max(maxMagnitude, magnitude);
            }
        }
        double factor = object->findScalarDoubleProperty(GO_AUTO_SCALE_FACTOR_PROPERTY_NAME_STR);
        if (maxMagnitude <= 0.) {
            return factor;
        }
        // Autoscale so the longest arrow spans a fixed fraction (AutoScaleFactor)
        // of the characteristic grid spacing. The spacing is derived per axis from
        // the data extent divided by the number of samples along that axis
        // (del = (rangeX / n)^2 + (rangeY / m)^2 [+ (rangeZ / max(m, n))^2]); the
        // longest scaled arrow then measures factor * sqrt(del). Vector-shaped
        // grids fall back to a square sample count.
        double rows = static_cast<double>(data.rows);
        double columns = static_cast<double>(data.columns);
        if (is3D && data.rows > 0 && data.u.size() > data.rows * data.columns) {
            columns = static_cast<double>(data.u.size()) / rows;
        }
        if (rows <= 1. || columns <= 1.) {
            double side = std::sqrt(static_cast<double>(data.u.size()));
            rows = side;
            columns = side;
        }
        double deltaX = (columns > 0.) ? finiteRange(data.x) / columns : 0.;
        double deltaY = (rows > 0.) ? finiteRange(data.y) / rows : 0.;
        double del = deltaX * deltaX + deltaY * deltaY;
        if (is3D) {
            double denominator = std::max(rows, columns);
            double deltaZ = (denominator > 0.) ? finiteRange(data.z) / denominator : 0.;
            del += deltaZ * deltaZ;
        }
        if (del <= 0.) {
            return factor;
        }
        return factor * std::sqrt(del) / maxMagnitude;
    }
    //=============================================================================
    static void
    setScaleFactor(GraphicsObject* object, double value)
    {
        auto* property = static_cast<GOScalarProperty*>(
            object->findProperty(GO_SCALE_FACTOR_PROPERTY_NAME_STR));
        property->data(value);
        property->clearModified();
    }
    //=============================================================================
    static void
    applyAlignment(
        GraphicsObject* object, double& x, double& y, double& z, double u, double v, double w)
    {
        std::wstring alignment = object->findStringProperty(GO_ALIGNMENT_PROPERTY_NAME_STR);
        double factor = 0.;
        if (alignment == L"center") {
            factor = 0.5;
        } else if (alignment == L"head") {
            factor = 1.;
        }
        x -= factor * u;
        y -= factor * v;
        z -= factor * w;
    }
    //=============================================================================
    static bool
    isDrawableColor(const std::vector<double>& color)
    {
        return color.size() >= 3 && color[0] >= 0.;
    }
    //=============================================================================
    static std::vector<double>
    resolvedRestrictedColor(
        GOAutoFlatColorProperty* property, const std::vector<double>& automaticColor)
    {
        if (!property || property->isEqual(GO_PROPERTY_VALUE_AUTO_STR)
            || property->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
            return automaticColor;
        }
        if (property->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return { -1., -1., -1. };
        }
        return property->colorSpec();
    }
    //=============================================================================
    static void
    appendSegment(std::vector<double>& xs, std::vector<double>& ys, std::vector<double>& zs,
        double x0, double y0, double z0, double x1, double y1, double z1)
    {
        xs.insert(xs.end(), { x0, x1, std::numeric_limits<double>::quiet_NaN() });
        ys.insert(ys.end(), { y0, y1, std::numeric_limits<double>::quiet_NaN() });
        zs.insert(zs.end(), { z0, z1, std::numeric_limits<double>::quiet_NaN() });
    }
    //=============================================================================
    static void
    appendArrowHead(std::vector<double>& xs, std::vector<double>& ys, std::vector<double>& zs,
        double x, double y, double z, double u, double v, double w, double maxHeadSize)
    {
        double length = std::sqrt(u * u + v * v + w * w);
        if (!std::isfinite(length) || length <= 0.) {
            return;
        }
        double ux = u / length;
        double uy = v / length;
        double uz = w / length;
        double sx = -uy;
        double sy = ux;
        double sz = 0.;
        double sideLength = std::sqrt(sx * sx + sy * sy + sz * sz);
        if (sideLength <= 0.) {
            sx = 1.;
            sy = 0.;
            sz = 0.;
            sideLength = 1.;
        }
        sx /= sideLength;
        sy /= sideLength;
        sz /= sideLength;
        double headLength = maxHeadSize * length;
        double headWidth = 0.464836597455402 * headLength;
        double tipX = x + u;
        double tipY = y + v;
        double tipZ = z + w;
        double baseX = tipX - headLength * ux;
        double baseY = tipY - headLength * uy;
        double baseZ = tipZ - headLength * uz;
        appendSegment(xs, ys, zs, baseX + headWidth * sx, baseY + headWidth * sy,
            baseZ + headWidth * sz, tipX, tipY, tipZ);
        appendSegment(xs, ys, zs, baseX - headWidth * sx, baseY - headWidth * sy,
            baseZ - headWidth * sz, tipX, tipY, tipZ);
    }
    //=============================================================================
    static void
    finiteMinMax(const std::vector<double>& values, double& low, double& high)
    {
        for (double value : values) {
            if (!std::isfinite(value)) {
                continue;
            }
            low = std::min(low, value);
            high = std::max(high, value);
        }
    }
    //=============================================================================
    static bool
    mapToPixel(
        GOAxis* axis, RenderInterface& gc, double x, double y, double z, double& px, double& py)
    {
        if (!axis || !std::isfinite(x) || !std::isfinite(y) || !std::isfinite(z)) {
            return false;
        }
        std::vector<double> xs { x };
        std::vector<double> ys { y };
        std::vector<double> zs { z };
        std::vector<double> mxs;
        std::vector<double> mys;
        std::vector<double> mzs;
        axis->reMap(xs, ys, zs, mxs, mys, mzs);
        if (mxs.empty()) {
            return false;
        }
        bool clipped = false;
        gc.toPixels(mxs[0], mys[0], mzs[0], px, py, clipped);
        return !clipped;
    }
    //=============================================================================
    static void
    appendPixelSegment(std::vector<PixelSegment>& segments, GOAxis* axis, RenderInterface& gc,
        double x1, double y1, double z1, double x2, double y2, double z2)
    {
        double px1 = 0.;
        double py1 = 0.;
        double px2 = 0.;
        double py2 = 0.;
        if (mapToPixel(axis, gc, x1, y1, z1, px1, py1)
            && mapToPixel(axis, gc, x2, y2, z2, px2, py2)) {
            segments.push_back({ px1, py1, px2, py2 });
        }
    }
    //=============================================================================
    static void
    appendPixelArrowHead(std::vector<PixelSegment>& segments, GOAxis* axis, RenderInterface& gc,
        double x, double y, double z, double u, double v, double w, double maxHeadSize)
    {
        std::vector<double> xs;
        std::vector<double> ys;
        std::vector<double> zs;
        appendArrowHead(xs, ys, zs, x, y, z, u, v, w, maxHeadSize);
        for (size_t k = 0; k + 1 < xs.size(); k += 3) {
            appendPixelSegment(
                segments, axis, gc, xs[k], ys[k], zs[k], xs[k + 1], ys[k + 1], zs[k + 1]);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
GOQuiver::GOQuiver()
{
    addProperty(this, new GOOnOffProperty, GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_AUTO_SCALE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_AUTO_SCALE_FACTOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_SCALE_FACTOR_PROPERTY_NAME_STR, false);
    addProperty(this, new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_MARKER_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_MAX_HEAD_SIZE_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SHOW_ARROW_HEAD_PROPERTY_NAME_STR);
    addProperty(
        this, new GORestrictedStringProperty(ALIGNMENT_VALUES), GO_ALIGNMENT_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_X_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Y_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Z_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_U_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_U_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_V_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_V_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_W_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_W_DATA_SOURCE_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    addProperty(this, new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    addProperty(this, new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
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
    addProperty(this, new GOOnOffProperty, GO_SELECTED_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_HIT_TEST_PROPERTY_NAME_STR);
    addProperty(this, new GOPickablePartsProperty, GO_PICKABLE_PARTS_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::AnnotationEventData),
        GO_ANNOTATION_PROPERTY_NAME_STR, false);
    addProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR, false);

    setRestrictedStringDefault(
        GO_ALIGN_VERTEX_CENTERS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_AUTO_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(GO_AUTO_SCALE_FACTOR_PROPERTY_NAME_STR, 0.9);
    setScalarDoubleDefault(GO_SCALE_FACTOR_PROPERTY_NAME_STR, 0.9);
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0.066, 0.443, 0.745);
    setRestrictedStringDefault(GO_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setRestrictedStringDefault(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_MARKER_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR, -1., -1., -1.);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1., -1., -1.);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6.);
    setScalarDoubleDefault(GO_MAX_HEAD_SIZE_PROPERTY_NAME_STR, 0.2);
    setRestrictedStringDefault(GO_SHOW_ARROW_HEAD_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TAIL_STR);
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setStringDefault(GO_X_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Z_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_U_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_V_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_W_DATA_SOURCE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUIVER_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setRestrictedStringDefault(GO_SELECTED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SELECTION_HIGHLIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VISIBLE_STR);
    setStringDefault(GO_DISPLAY_NAME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    auto* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    seriesIndex->setValue(1.);
    seriesIndex->clearModified();
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOQuiver::~GOQuiver() = default;
//=============================================================================
std::wstring
GOQuiver::getType()
{
    return GO_PROPERTY_VALUE_QUIVER_STR;
}
//=============================================================================
bool
GOQuiver::preservesAutoDataLimits()
{
    return true;
}
//=============================================================================
void
GOQuiver::updateState()
{
    if (hasChanged(GO_COLOR_PROPERTY_NAME_STR)) {
        toManual(GO_COLOR_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LINE_STYLE_PROPERTY_NAME_STR)) {
        toManual(GO_LINE_STYLE_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_MARKER_PROPERTY_NAME_STR)) {
        toManual(GO_MARKER_MODE_PROPERTY_NAME_STR);
    }
    bool dataChanged = hasChanged({ GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_Z_DATA_PROPERTY_NAME_STR, GO_U_DATA_PROPERTY_NAME_STR, GO_V_DATA_PROPERTY_NAME_STR,
        GO_W_DATA_PROPERTY_NAME_STR, GO_AUTO_SCALE_PROPERTY_NAME_STR,
        GO_AUTO_SCALE_FACTOR_PROPERTY_NAME_STR, GO_ALIGNMENT_PROPERTY_NAME_STR });
    if (dataChanged) {
        ArrowData data = currentArrowData(this);
        setScaleFactor(this, computedScale(this, data));
        limitsDirty = true;
    }
    clearChanged({ GO_COLOR_PROPERTY_NAME_STR, GO_LINE_STYLE_PROPERTY_NAME_STR,
        GO_MARKER_PROPERTY_NAME_STR, GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
        GO_Z_DATA_PROPERTY_NAME_STR, GO_U_DATA_PROPERTY_NAME_STR, GO_V_DATA_PROPERTY_NAME_STR,
        GO_W_DATA_PROPERTY_NAME_STR, GO_AUTO_SCALE_PROPERTY_NAME_STR,
        GO_AUTO_SCALE_FACTOR_PROPERTY_NAME_STR, GO_ALIGNMENT_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOQuiver::paintMe(RenderInterface& gc)
{
    updateState();
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOAxis* axis = static_cast<GOAxis*>(getParentAxis());
    if (!axis) {
        return;
    }
    ArrowData data = currentArrowData(this);
    if (data.u.empty()) {
        return;
    }
    GOColorProperty* colorProperty
        = static_cast<GOColorProperty*>(findProperty(GO_COLOR_PROPERTY_NAME_STR));
    if (colorProperty->isNone()) {
        return;
    }
    std::vector<double> xs;
    std::vector<double> ys;
    std::vector<double> zs;
    double scale = findScalarDoubleProperty(GO_SCALE_FACTOR_PROPERTY_NAME_STR);
    double maxHeadSize = findScalarDoubleProperty(GO_MAX_HEAD_SIZE_PROPERTY_NAME_STR);
    bool showHead
        = findStringProperty(GO_SHOW_ARROW_HEAD_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR;
    std::vector<double> color = colorProperty->data();
    double width = findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR);
    RenderInterface::SymbolType marker
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
    bool is2DQuiver = numericData(this, GO_W_DATA_PROPERTY_NAME_STR).values.empty();
    if (is2DQuiver) {
        std::vector<PixelSegment> segments;
        std::vector<PixelPoint> markerPoints;
        for (size_t k = 0; k < data.u.size(); ++k) {
            double u = data.u[k] * scale;
            double v = data.v[k] * scale;
            double w = data.w[k] * scale;
            double x = data.x[k];
            double y = data.y[k];
            double z = data.z[k];
            applyAlignment(this, x, y, z, u, v, w);
            appendPixelSegment(segments, axis, gc, x, y, z, x + u, y + v, z + w);
            if (showHead) {
                appendPixelArrowHead(segments, axis, gc, x, y, z, u, v, w, maxHeadSize);
            }
            double px = 0.;
            double py = 0.;
            if (mapToPixel(axis, gc, x, y, z, px, py)) {
                markerPoints.push_back({ px, py });
            }
        }
        std::vector<double> edgeColor
            = resolvedRestrictedColor(static_cast<GOAutoFlatColorProperty*>(
                                          findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR)),
                color);
        std::vector<double> faceColor
            = resolvedRestrictedColor(static_cast<GOAutoFlatColorProperty*>(
                                          findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR)),
                { -1., -1., -1. });
        double markerSize = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR);
        gc.setupDirectDraw();
        gc.color(color);
        gc.lineWidth(width);
        gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
        for (const auto& segment : segments) {
            gc.line(segment.x1, segment.y1, segment.x2, segment.y2);
        }
        if (marker != RenderInterface::None
            && (isDrawableColor(edgeColor) || isDrawableColor(faceColor))) {
            for (const auto& point : markerPoints) {
                DrawSymbol(
                    gc, marker, point.x, point.y, 0., markerSize, edgeColor, faceColor, width);
            }
        }
        gc.releaseDirectDraw();
        return;
    }
    for (size_t k = 0; k < data.u.size(); ++k) {
        double u = data.u[k] * scale;
        double v = data.v[k] * scale;
        double w = data.w[k] * scale;
        double x = data.x[k];
        double y = data.y[k];
        double z = data.z[k];
        applyAlignment(this, x, y, z, u, v, w);
        appendSegment(xs, ys, zs, x, y, z, x + u, y + v, z + w);
        if (showHead) {
            appendArrowHead(xs, ys, zs, x, y, z, u, v, w, maxHeadSize);
        }
    }
    std::vector<double> mx;
    std::vector<double> my;
    std::vector<double> mz;
    axis->reMap(xs, ys, zs, mx, my, mz);
    gc.color(color);
    gc.lineWidth(width);
    gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
    gc.lineSeries(mx, my, mz);

    if (marker == RenderInterface::None) {
        return;
    }
    std::vector<double> markerX = data.x;
    std::vector<double> markerY = data.y;
    std::vector<double> markerZ = data.z;
    for (size_t k = 0; k < markerX.size(); ++k) {
        double u = data.u[k] * scale;
        double v = data.v[k] * scale;
        double w = data.w[k] * scale;
        applyAlignment(this, markerX[k], markerY[k], markerZ[k], u, v, w);
    }
    axis->reMap(markerX, markerY, markerZ, mx, my, mz);
    std::vector<double> edgeColor = resolvedRestrictedColor(
        static_cast<GOAutoFlatColorProperty*>(findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR)),
        colorProperty->data());
    std::vector<double> faceColor = resolvedRestrictedColor(
        static_cast<GOAutoFlatColorProperty*>(findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR)),
        { -1., -1., -1. });
    if (!isDrawableColor(edgeColor) && !isDrawableColor(faceColor)) {
        return;
    }
    double markerSize = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR);
    gc.setupDirectDraw();
    std::vector<double> ux;
    std::vector<double> uy;
    ux.reserve(mx.size());
    uy.reserve(mx.size());
    for (size_t k = 0; k < mx.size(); ++k) {
        double px = 0.;
        double py = 0.;
        bool clipped = false;
        gc.toPixels(mx[k], my[k], mz[k], px, py, clipped);
        if (!clipped) {
            ux.push_back(px);
            uy.push_back(py);
        }
    }
    // One batched primitive (RenderWeb) instead of a marker glyph per arrow tail;
    // a backend that cannot batch (Qt/OpenGL) returns false and the loop runs.
    if (!DrawSymbolBatchUniform(gc, marker, ux, uy, markerSize, edgeColor, faceColor, width)) {
        for (size_t k = 0; k < ux.size(); ++k) {
            DrawSymbol(gc, marker, ux[k], uy[k], 0., markerSize, edgeColor, faceColor, width);
        }
    }
    gc.releaseDirectDraw();
}
//=============================================================================
std::vector<double>
GOQuiver::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    ArrowData data = currentArrowData(this);
    double scale = findScalarDoubleProperty(GO_SCALE_FACTOR_PROPERTY_NAME_STR);
    double xMin = std::numeric_limits<double>::infinity();
    double xMax = -std::numeric_limits<double>::infinity();
    double yMin = std::numeric_limits<double>::infinity();
    double yMax = -std::numeric_limits<double>::infinity();
    double zMin = std::numeric_limits<double>::infinity();
    double zMax = -std::numeric_limits<double>::infinity();
    finiteMinMax(data.x, xMin, xMax);
    finiteMinMax(data.y, yMin, yMax);
    finiteMinMax(data.z, zMin, zMax);
    double yAnchorMin = std::numeric_limits<double>::infinity();
    double yAnchorHigh = -std::numeric_limits<double>::infinity();
    finiteMinMax(data.y, yAnchorMin, yAnchorHigh);
    double yAnchorSpacing = minimumPositiveSpacing(data.y);
    bool showHead
        = findStringProperty(GO_SHOW_ARROW_HEAD_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR
        && findStringProperty(GO_AUTO_SCALE_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_ON_STR;
    double maxHeadSize = findScalarDoubleProperty(GO_MAX_HEAD_SIZE_PROPERTY_NAME_STR);
    for (size_t k = 0; k < data.u.size(); ++k) {
        double x = data.x[k];
        double y = data.y[k];
        double z = data.z[k];
        double u = data.u[k] * scale;
        double v = data.v[k] * scale;
        double w = data.w[k] * scale;
        applyAlignment(this, x, y, z, u, v, w);
        std::vector<double> xv { x, x + u };
        std::vector<double> yv { y, y + v };
        std::vector<double> zv { z, z + w };
        finiteMinMax(xv, xMin, xMax);
        finiteMinMax(yv, yMin, yMax);
        finiteMinMax(zv, zMin, zMax);
        if (showHead) {
            std::vector<double> headX;
            std::vector<double> headY;
            std::vector<double> headZ;
            appendArrowHead(headX, headY, headZ, x, y, z, u, v, w, maxHeadSize);
            finiteMinMax(headX, xMin, xMax);
            finiteMinMax(headY, yMin, yMax);
            finiteMinMax(headZ, zMin, zMax);
        }
    }
    if (numericData(this, GO_W_DATA_PROPERTY_NAME_STR).values.empty()
        && std::isfinite(yAnchorSpacing) && yAnchorSpacing > 0. && std::isfinite(yMin)
        && std::isfinite(yMax) && std::isfinite(yAnchorMin) && std::isfinite(yAnchorHigh)) {
        double halfSpacing = 0.5 * yAnchorSpacing;
        if (std::fabs(yMin - yAnchorMin) <= 1e-12) {
            yMin -= halfSpacing;
        }
        if (std::fabs(yMax - yAnchorHigh) <= 1e-12) {
            yMax += halfSpacing;
        }
    }
    if (!std::isfinite(xMin)) {
        xMin = yMin = zMin = 0.;
        xMax = yMax = zMax = 1.;
    }
    limits = { xMin, xMax, yMin, yMax, zMin, zMax, 0., 0., 0., 0. };
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
