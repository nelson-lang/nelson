//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOViolinPlot.hpp"
#include "GOChartDecorators.hpp"
#include "ViolinDensityHelpers.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOAxis.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOPickablePartsProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOSeriesIndexProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
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
    static const std::vector<std::wstring> ORIENTATION_VALUES = { L"vertical", L"horizontal" };
    static const std::vector<std::wstring> COLOR_GROUP_LAYOUT_VALUES = { L"grouped", L"overlaid" };
    static const std::vector<std::wstring> DENSITY_DIRECTION_VALUES
        = { L"both", L"positive", L"negative" };
    static const std::vector<std::wstring> DENSITY_SCALE_VALUES = { L"area", L"count", L"width" };
    //=============================================================================
    static void
    addProperty(GraphicsObject* go, GOGenericProperty* property, const std::wstring& name,
        bool writable = true, bool visible = true)
    {
        go->registerProperty(property, name, writable, visible);
    }
    //=============================================================================
    static std::vector<double>
    defaultSeriesColor()
    {
        return { 0.066, 0.443, 0.745 };
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
    static std::vector<double>
    colorValue(GraphicsObject* object, const std::wstring& propertyName)
    {
        auto* property = static_cast<GOAutoFlatColorProperty*>(object->findProperty(propertyName));
        if (property->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return {};
        }
        std::vector<double> value = property->colorSpec();
        if (value.size() < 3) {
            return defaultSeriesColor();
        }
        return { value[0], value[1], value[2] };
    }
    //=============================================================================
    static void
    appendAlpha(std::vector<double>& color, double alpha)
    {
        if (color.size() == 3) {
            color.push_back(alpha);
        } else if (color.size() >= 4) {
            color[3] = alpha;
        }
    }
    //=============================================================================
    static std::vector<double>
    numericVector(const ArrayOf& value)
    {
        if (value.isEmpty()) {
            return {};
        }
        if (!value.isNumeric() || value.isComplex()) {
            Error("Nelson:graphics:realNumericExpected", "Real numeric data expected.");
        }
        ArrayOf asDouble(value);
        asDouble.promoteType(NLS_DOUBLE);
        return asDouble.asDoubleVector();
    }
    //=============================================================================
    static std::vector<double>
    propertyVector(GraphicsObject* object, const std::wstring& name)
    {
        auto* property = static_cast<GOArrayOfProperty*>(object->findProperty(name));
        return numericVector(property->data());
    }
    //=============================================================================
    static bool
    finitePair(double x, double y)
    {
        return std::isfinite(x) && std::isfinite(y);
    }
    //=============================================================================
    static double
    sortedMedian(const std::vector<double>& sorted)
    {
        size_t mid = sorted.size() / 2;
        return sorted.size() % 2 == 1 ? sorted[mid] : 0.5 * (sorted[mid - 1] + sorted[mid]);
    }
    //=============================================================================
    static double
    median(const std::vector<double>& values)
    {
        if (values.empty()) {
            return 0.;
        }
        if (std::is_sorted(values.begin(), values.end())) {
            return sortedMedian(values);
        }
        std::vector<double> sorted(values);
        std::sort(sorted.begin(), sorted.end());
        return sortedMedian(sorted);
    }
    //=============================================================================
    static const int DENSITY_POINT_COUNT = 100;
    // The density is evaluated from min - 3h to max + 3h (h: bandwidth).
    static const double DENSITY_SUPPORT_BANDWIDTHS = 3.;
    // Samples farther than 8h from a center contribute less than exp(-32).
    static const double KERNEL_CUTOFF_BANDWIDTHS = 8.;
    //=============================================================================
    // Normal reference rule with a robust scale: sigma = MAD / 0.6745,
    // h = sigma * (4 / (3 n))^(1/5). Falls back to the data range when the
    // MAD is zero, and to 1 for constant data.
    static double
    violinBandwidth(const std::vector<double>& sorted)
    {
        double center = sortedMedian(sorted);
        std::vector<double> deviations;
        deviations.reserve(sorted.size());
        for (double value : sorted) {
            deviations.push_back(std::abs(value - center));
        }
        std::sort(deviations.begin(), deviations.end());
        double sigma = sortedMedian(deviations) / 0.6745;
        if (sigma <= 0.) {
            sigma = sorted.back() - sorted.front();
        }
        if (!std::isfinite(sigma) || sigma <= 0.) {
            return 1.;
        }
        return sigma * std::pow(4. / (3. * static_cast<double>(sorted.size())), 0.2);
    }
    //=============================================================================
    // Gaussian kernel density of sorted values at center; only the samples
    // within the kernel cutoff are visited.
    static double
    densityAt(const std::vector<double>& sorted, double center, double bandwidth)
    {
        double reach = KERNEL_CUTOFF_BANDWIDTHS * bandwidth;
        auto first = std::lower_bound(sorted.begin(), sorted.end(), center - reach);
        auto last = std::upper_bound(first, sorted.end(), center + reach);
        double total = 0.;
        for (auto it = first; it != last; ++it) {
            double u = (center - *it) / bandwidth;
            total += std::exp(-0.5 * u * u);
        }
        const double invSqrt2Pi = 0.3989422804014327;
        return invSqrt2Pi * total / (bandwidth * static_cast<double>(sorted.size()));
    }
    //=============================================================================
    static std::vector<double>
    densitiesAt(
        const std::vector<double>& sorted, double bandwidth, const std::vector<double>& centers)
    {
        std::vector<double> densities;
        densities.reserve(centers.size());
        for (double center : centers) {
            densities.push_back(sorted.empty() ? 0. : densityAt(sorted, center, bandwidth));
        }
        return densities;
    }
    //=============================================================================
    static std::vector<double>
    defaultCenters(const std::vector<double>& sorted, double bandwidth)
    {
        std::vector<double> centers;
        if (sorted.empty()) {
            return centers;
        }
        double minValue = sorted.front() - DENSITY_SUPPORT_BANDWIDTHS * bandwidth;
        double maxValue = sorted.back() + DENSITY_SUPPORT_BANDWIDTHS * bandwidth;
        centers.reserve(DENSITY_POINT_COUNT);
        for (int k = 0; k < DENSITY_POINT_COUNT; ++k) {
            double t = static_cast<double>(k) / static_cast<double>(DENSITY_POINT_COUNT - 1);
            centers.push_back(minValue + t * (maxValue - minValue));
        }
        return centers;
    }
    //=============================================================================
    static void
    violinDensityShape(const std::vector<double>& values, std::vector<double>& centers,
        std::vector<double>& widths)
    {
        std::vector<double> sorted(values);
        std::sort(sorted.begin(), sorted.end());
        double bandwidth = sorted.empty() ? 0. : violinBandwidth(sorted);
        centers = defaultCenters(sorted, bandwidth);
        widths = densitiesAt(sorted, bandwidth, centers);
    }
    //=============================================================================
    // EvaluationPoints and DensityValues: real floating-point arrays, a row
    // vector being stored as a column (one column per violin).
    class ViolinDensityProperty : public GOArrayOfProperty
    {
        bool finiteOnly;

    public:
        explicit ViolinDensityProperty(bool finite) : finiteOnly(finite) { }
        void
        set(ArrayOf value) override
        {
            if (!value.isEmpty()) {
                if (value.isSparse() || !(value.isDoubleType(true) || value.isSingleType(true))) {
                    Error(L"Nelson:graphics:floatingPointExpected",
                        L"Value must be a real floating-point array.");
                }
                if (finiteOnly) {
                    ArrayOf asDouble(value);
                    asDouble.promoteType(NLS_DOUBLE);
                    for (double element : asDouble.asDoubleVector()) {
                        if (!std::isfinite(element)) {
                            Error(L"Nelson:expectedFinite", L"%s must be finite.", L"Value");
                        }
                    }
                }
                if (value.isRowVector()) {
                    Dimensions dims(value.elementCount(), 1);
                    value.reshape(dims);
                }
            }
            GOArrayOfProperty::set(value);
        }
    };
    //=============================================================================
    struct ColumnMatrix
    {
        std::vector<double> values;
        size_t rows { 0 };
        size_t columns { 0 };
        std::vector<double>
        column(size_t k) const
        {
            auto first = values.begin() + static_cast<std::ptrdiff_t>(k * rows);
            return std::vector<double>(first, first + static_cast<std::ptrdiff_t>(rows));
        }
    };
    //=============================================================================
    static ColumnMatrix
    columnMatrix(GraphicsObject* object, const std::wstring& name)
    {
        ArrayOf value = static_cast<GOArrayOfProperty*>(object->findProperty(name))->data();
        ColumnMatrix matrix;
        matrix.values = numericVector(value);
        if (!matrix.values.empty()) {
            matrix.rows = static_cast<size_t>(value.rows());
            matrix.columns = matrix.values.size() / matrix.rows;
        }
        return matrix;
    }
    //=============================================================================
    static void
    publishColumns(GraphicsObject* object, const std::wstring& name,
        const std::vector<std::vector<double>>& columns)
    {
        size_t rows = columns.empty() ? 0 : columns.front().size();
        ArrayOf matrix = ArrayOf::empty();
        if (rows > 0) {
            matrix = ArrayOf::doubleMatrix2d(
                static_cast<indexType>(rows), static_cast<indexType>(columns.size()));
            double* data = static_cast<double*>(matrix.readWriteDataPointer());
            for (const std::vector<double>& column : columns) {
                data = std::copy(column.begin(), column.end(), data);
            }
        }
        auto* property = static_cast<GOArrayOfProperty*>(object->findProperty(name));
        property->data(matrix);
        object->clearChanged(name);
    }
    //=============================================================================
    static point
    mapPoint(GraphicsObject* object, const point& value)
    {
        auto* parent = static_cast<GOAxis*>(object->getParentAxis());
        if (!parent) {
            return value;
        }
        std::vector<double> xs = { value.x };
        std::vector<double> ys = { value.y };
        std::vector<double> zs = { value.z };
        std::vector<double> mappedXs;
        std::vector<double> mappedYs;
        std::vector<double> mappedZs;
        parent->reMap(xs, ys, zs, mappedXs, mappedYs, mappedZs);
        if (mappedXs.empty() || mappedYs.empty() || mappedZs.empty()) {
            return value;
        }
        return point(mappedXs[0], mappedYs[0], mappedZs[0]);
    }
    //=============================================================================
    static std::vector<point>
    mapPoints(GraphicsObject* object, const std::vector<point>& values)
    {
        auto* parent = static_cast<GOAxis*>(object->getParentAxis());
        if (!parent || values.empty()) {
            return values;
        }
        std::vector<double> xs;
        std::vector<double> ys;
        std::vector<double> zs;
        xs.reserve(values.size());
        ys.reserve(values.size());
        zs.reserve(values.size());
        for (const point& value : values) {
            xs.push_back(value.x);
            ys.push_back(value.y);
            zs.push_back(value.z);
        }
        std::vector<double> mappedXs;
        std::vector<double> mappedYs;
        std::vector<double> mappedZs;
        parent->reMap(xs, ys, zs, mappedXs, mappedYs, mappedZs);
        if (mappedXs.size() != values.size() || mappedYs.size() != values.size()
            || mappedZs.size() != values.size()) {
            return values;
        }
        std::vector<point> mapped;
        mapped.reserve(values.size());
        for (size_t k = 0; k < values.size(); ++k) {
            mapped.emplace_back(mappedXs[k], mappedYs[k], mappedZs[k]);
        }
        return mapped;
    }
    //=============================================================================
    static void
    drawFilledPolygon(RenderInterface& gc, const point& center, const std::vector<point>& vertices,
        const std::vector<double>& faceColor)
    {
        if (vertices.size() < 3) {
            return;
        }
        std::vector<std::vector<double>> polygon;
        polygon.reserve(vertices.size());
        for (const point& vertex : vertices) {
            polygon.push_back({ vertex.x, vertex.y });
        }
        gc.setLineStyle(GO_PROPERTY_VALUE_NONE_STR);
        if (!gc.polygonFill(polygon, faceColor)) {
            for (size_t k = 0; k < vertices.size(); ++k) {
                const point& a = vertices[k];
                const point& b = vertices[(k + 1) % vertices.size()];
                gc.tri(center.x, center.y, center.z, a.x, a.y, a.z, b.x, b.y, b.z);
            }
        }
        gc.setLineStyle(L"-");
    }
    //=============================================================================
    static point
    violinPoint(double position, double value, double width, bool horizontal)
    {
        return horizontal ? point(value, position + width, 0.) : point(position + width, value, 0.);
    }
    //=============================================================================
    static void
    drawViolin(RenderInterface& gc, GraphicsObject* object, double position,
        const std::vector<double>& values, const std::vector<double>& centers,
        const std::vector<double>& widths, const std::wstring& direction, bool horizontal,
        const std::vector<double>& faceColor, const std::vector<double>& edgeColor)
    {
        if (centers.empty()) {
            return;
        }
        std::vector<point> polygon;
        polygon.reserve(centers.size() * 2);
        for (size_t k = 0; k < centers.size(); ++k) {
            double offset = direction == GO_PROPERTY_VALUE_POSITIVE_STR ? 0. : -widths[k];
            polygon.push_back(violinPoint(position, centers[k], offset, horizontal));
        }
        for (size_t k = centers.size(); k-- > 0;) {
            double offset = direction == GO_PROPERTY_VALUE_NEGATIVE_STR ? 0. : widths[k];
            polygon.push_back(violinPoint(position, centers[k], offset, horizontal));
        }
        double medianValue = median(values);
        std::vector<point> mappedPolygon = mapPoints(object, polygon);
        if (faceColor.size() >= 3) {
            gc.color(faceColor);
            point center
                = horizontal ? point(medianValue, position, 0.) : point(position, medianValue, 0.);
            drawFilledPolygon(gc, mapPoint(object, center), mappedPolygon, faceColor);
        }
        if (edgeColor.size() >= 3) {
            gc.color(edgeColor);
            std::vector<double> xs;
            std::vector<double> ys;
            std::vector<double> zs;
            for (const point& vertex : mappedPolygon) {
                xs.push_back(vertex.x);
                ys.push_back(vertex.y);
                zs.push_back(vertex.z);
            }
            xs.push_back(mappedPolygon.front().x);
            ys.push_back(mappedPolygon.front().y);
            zs.push_back(mappedPolygon.front().z);
            gc.lineSeries(xs, ys, zs);
        }
    }
    //=============================================================================
    static void
    validatePositive(GraphicsObject* object, const std::wstring& name)
    {
        double value = object->findScalarDoubleProperty(name);
        if (!std::isfinite(value) || value <= 0.) {
            Error(L"Nelson:graphics:mustBePositiveFinite", L"%s must be a positive finite scalar.",
                name);
        }
    }
    //=============================================================================
    static void
    validateRange(GraphicsObject* object, const std::wstring& name, double low, double high)
    {
        double value = object->findScalarDoubleProperty(name);
        if (!std::isfinite(value) || value < low || value > high) {
            Error(L"Nelson:violinplot:outOfRange", L"%s is outside its valid range.", name);
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
GOViolinPlot::GOViolinPlot()
{
    addProperty(this, new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_X_VARIABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOStringProperty, GO_Y_VARIABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOSourceTableProperty, GO_SOURCE_TABLE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_FACE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty(0., 1.), GO_FACE_ALPHA_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoFlatColorProperty, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    addProperty(this, new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    addProperty(
        this, new GORestrictedStringProperty(ORIENTATION_VALUES), GO_ORIENTATION_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(COLOR_GROUP_LAYOUT_VALUES),
        GO_COLOR_GROUP_LAYOUT_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty(0., 1.), GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOScalarProperty, GO_DENSITY_WIDTH_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_DENSITY_WIDTH_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(DENSITY_DIRECTION_VALUES),
        GO_DENSITY_DIRECTION_PROPERTY_NAME_STR);
    addProperty(this, new GORestrictedStringProperty(DENSITY_SCALE_VALUES),
        GO_DENSITY_SCALE_PROPERTY_NAME_STR);
    addProperty(this, new ViolinDensityProperty(true), GO_EVALUATION_POINTS_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_EVALUATION_POINTS_MODE_PROPERTY_NAME_STR);
    addProperty(this, new ViolinDensityProperty(false), GO_DENSITY_VALUES_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_DENSITY_VALUES_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_FACE_COLOR_MODE_PROPERTY_NAME_STR);
    addProperty(this, new GOAutoManualProperty, GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR);
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
    addProperty(this, new GOSeriesIndexProperty, GO_SERIES_INDEX_PROPERTY_NAME_STR);
    addProperty(this, new GOLazyDecoratorProperty(GODecoratorKind::DataTipTemplate),
        GO_DATA_TIP_TEMPLATE_PROPERTY_NAME_STR, false);

    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setStringDefault(GO_X_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setStringDefault(GO_Y_VARIABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    setColorSpec(this, GO_FACE_COLOR_PROPERTY_NAME_STR, defaultSeriesColor());
    setColorSpec(this, GO_EDGE_COLOR_PROPERTY_NAME_STR, defaultSeriesColor());
    setScalarDoubleDefault(GO_FACE_ALPHA_PROPERTY_NAME_STR, 0.2);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
    setRestrictedStringDefault(GO_COLOR_GROUP_LAYOUT_PROPERTY_NAME_STR, L"grouped");
    setScalarDoubleDefault(GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR, 1.);
    setRestrictedStringDefault(
        GO_COLOR_GROUP_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_DENSITY_WIDTH_PROPERTY_NAME_STR, 0.9);
    setRestrictedStringDefault(GO_DENSITY_WIDTH_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_DENSITY_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
    setRestrictedStringDefault(
        GO_DENSITY_SCALE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AREA_SCALE_STR);
    setRestrictedStringDefault(
        GO_EVALUATION_POINTS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_DENSITY_VALUES_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_FACE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_EDGE_COLOR_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
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
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VIOLIN_PLOT_STR);
    setStringDefault(GO_TAG_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_EMPTY_STR);
    auto* seriesIndex
        = static_cast<GOSeriesIndexProperty*>(findProperty(GO_SERIES_INDEX_PROPERTY_NAME_STR));
    seriesIndex->setValue(1.);
    seriesIndex->clearModified();
    clearAllChanged();
    sortProperties();
}
//=============================================================================
GOViolinPlot::~GOViolinPlot() = default;
//=============================================================================
std::wstring
GOViolinPlot::getType()
{
    return GO_PROPERTY_VALUE_VIOLIN_PLOT_STR;
}
//=============================================================================
void
GOViolinPlot::updateState()
{
    validatePositive(this, GO_DENSITY_WIDTH_PROPERTY_NAME_STR);
    validatePositive(this, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    validateRange(this, GO_FACE_ALPHA_PROPERTY_NAME_STR, 0., 1.);
    validateRange(this, GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR, 0., 1.);
    refreshShapes();
    clearChanged({ GO_FACE_COLOR_PROPERTY_NAME_STR, GO_FACE_ALPHA_PROPERTY_NAME_STR,
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_LINE_STYLE_PROPERTY_NAME_STR,
        GO_LINE_WIDTH_PROPERTY_NAME_STR, GO_ORIENTATION_PROPERTY_NAME_STR,
        GO_COLOR_GROUP_LAYOUT_PROPERTY_NAME_STR, GO_COLOR_GROUP_WIDTH_PROPERTY_NAME_STR,
        GO_DENSITY_WIDTH_PROPERTY_NAME_STR, GO_DENSITY_DIRECTION_PROPERTY_NAME_STR,
        GO_DENSITY_SCALE_PROPERTY_NAME_STR });
}
//=============================================================================
void
GOViolinPlot::refreshShapes()
{
    const std::vector<std::wstring> inputs
        = { GO_X_DATA_PROPERTY_NAME_STR, GO_Y_DATA_PROPERTY_NAME_STR,
              GO_EVALUATION_POINTS_PROPERTY_NAME_STR, GO_EVALUATION_POINTS_MODE_PROPERTY_NAME_STR,
              GO_DENSITY_VALUES_PROPERTY_NAME_STR, GO_DENSITY_VALUES_MODE_PROPERTY_NAME_STR };
    if (!shapesDirty && !hasChanged(inputs)) {
        return;
    }
    if (hasChanged(GO_EVALUATION_POINTS_PROPERTY_NAME_STR)
        && !hasChanged(GO_EVALUATION_POINTS_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_EVALUATION_POINTS_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_DENSITY_VALUES_PROPERTY_NAME_STR)
        && !hasChanged(GO_DENSITY_VALUES_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_DENSITY_VALUES_MODE_PROPERTY_NAME_STR);
    }
    manualPoints = !isAuto(GO_EVALUATION_POINTS_MODE_PROPERTY_NAME_STR);
    bool manualDensities = !isAuto(GO_DENSITY_VALUES_MODE_PROPERTY_NAME_STR);
    ColumnMatrix points = columnMatrix(this, GO_EVALUATION_POINTS_PROPERTY_NAME_STR);
    ColumnMatrix densities = columnMatrix(this, GO_DENSITY_VALUES_PROPERTY_NAME_STR);
    std::vector<double> x = propertyVector(this, GO_X_DATA_PROPERTY_NAME_STR);
    std::vector<double> y = propertyVector(this, GO_Y_DATA_PROPERTY_NAME_STR);
    std::vector<double> positions = ViolinDensityHelpers::uniquePositions(x, y);
    if (positions.empty() && manualPoints) {
        // Precomputed densities without sample data: violins at 1, 2, ...
        for (size_t k = 0; k < points.columns; ++k) {
            positions.push_back(static_cast<double>(k + 1));
        }
    }
    // Manual evaluation points need one column per violin, manual densities
    // the size of the evaluation points; otherwise nothing is drawn.
    bool pointsMatch = !manualPoints || points.columns == positions.size();
    groups.clear();
    groups.resize(positions.size());
    std::vector<std::vector<double>> centerColumns;
    std::vector<std::vector<double>> densityColumns;
    for (size_t k = 0; k < positions.size(); ++k) {
        Group& group = groups[k];
        group.position = positions[k];
        group.values = ViolinDensityHelpers::valuesAtPosition(x, y, positions[k]);
        std::sort(group.values.begin(), group.values.end());
        group.bandwidth = group.values.empty() ? 0. : violinBandwidth(group.values);
        if (pointsMatch) {
            group.centers
                = manualPoints ? points.column(k) : defaultCenters(group.values, group.bandwidth);
            group.densities = densitiesAt(group.values, group.bandwidth, group.centers);
        }
        centerColumns.push_back(group.centers);
        densityColumns.push_back(group.densities);
    }
    if (!manualPoints) {
        publishColumns(this, GO_EVALUATION_POINTS_PROPERTY_NAME_STR, centerColumns);
        points = columnMatrix(this, GO_EVALUATION_POINTS_PROPERTY_NAME_STR);
    }
    bool densitiesMatch = densities.rows == points.rows && densities.columns == points.columns;
    for (size_t k = 0; k < groups.size(); ++k) {
        Group& group = groups[k];
        if (manualDensities) {
            group.densities
                = pointsMatch && densitiesMatch ? densities.column(k) : std::vector<double>();
        }
        group.maxDensity = 0.;
        for (double density : group.densities) {
            group.maxDensity = density > group.maxDensity ? density : group.maxDensity;
        }
    }
    if (!manualDensities) {
        publishColumns(this, GO_DENSITY_VALUES_PROPERTY_NAME_STR, densityColumns);
    }
    shapesDirty = false;
    clearChanged(inputs);
}
//=============================================================================
void
GOViolinPlot::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    updateState();
    refreshShapes();
    if (groups.empty()) {
        return;
    }
    double densityWidth = findScalarDoubleProperty(GO_DENSITY_WIDTH_PROPERTY_NAME_STR);
    bool horizontal
        = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    std::wstring direction = findStringProperty(GO_DENSITY_DIRECTION_PROPERTY_NAME_STR);
    std::wstring densityScale = findStringProperty(GO_DENSITY_SCALE_PROPERTY_NAME_STR);
    std::vector<double> faceColor = colorValue(this, GO_FACE_COLOR_PROPERTY_NAME_STR);
    appendAlpha(faceColor, findScalarDoubleProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
    std::vector<double> edgeColor = colorValue(this, GO_EDGE_COLOR_PROPERTY_NAME_STR);
    gc.lineWidth(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
    double globalMaxDensity = 0.;
    double maxCount = 0.;
    for (const Group& group : groups) {
        globalMaxDensity = std::max(globalMaxDensity, group.maxDensity);
        maxCount = std::max(maxCount, static_cast<double>(group.values.size()));
    }
    double halfWidth = densityWidth / 2.;
    std::vector<double> widths;
    for (const Group& group : groups) {
        double denominator = densityScale == L"width" ? group.maxDensity : globalMaxDensity;
        if (group.centers.empty() || group.centers.size() != group.densities.size()) {
            continue;
        }
        double countScale = densityScale == L"count" && maxCount > 0.
            ? static_cast<double>(group.values.size()) / maxCount
            : 1.;
        widths = group.densities;
        if (denominator > 0.) {
            for (double& width : widths) {
                width = halfWidth * width / denominator * countScale;
            }
        }
        drawViolin(gc, this, group.position, group.values, group.centers, widths, direction,
            horizontal, faceColor, edgeColor);
    }
}
//=============================================================================
std::vector<double>
GOViolinPlot::getLimits()
{
    refreshShapes();
    if (groups.empty()) {
        return { 0.5, 1.5, 0., 1., 0., 0., 0., 1., 1., 1. };
    }
    double densityHalf = findScalarDoubleProperty(GO_DENSITY_WIDTH_PROPERTY_NAME_STR) / 2.;
    double minPosition = groups.front().position - densityHalf;
    double maxPosition = groups.back().position + densityHalf;
    // Manual evaluation points: their range. Otherwise the data range padded
    // by the support of the widest kernel.
    double minValue = std::numeric_limits<double>::infinity();
    double maxValue = -std::numeric_limits<double>::infinity();
    double maxBandwidth = 0.;
    for (const Group& group : groups) {
        const std::vector<double>& range = manualPoints ? group.centers : group.values;
        for (double value : range) {
            minValue = std::min(minValue, value);
            maxValue = std::max(maxValue, value);
        }
        maxBandwidth = std::max(maxBandwidth, group.bandwidth);
    }
    if (!manualPoints) {
        minValue -= DENSITY_SUPPORT_BANDWIDTHS * maxBandwidth;
        maxValue += DENSITY_SUPPORT_BANDWIDTHS * maxBandwidth;
    }
    if (!std::isfinite(minValue) || !std::isfinite(maxValue)) {
        minValue = 0.;
        maxValue = 1.;
    }
    if (minValue == maxValue) {
        minValue -= 0.5;
        maxValue += 0.5;
    }
    bool horizontal
        = findStringProperty(GO_ORIENTATION_PROPERTY_NAME_STR) == GO_PROPERTY_VALUE_HORIZONTAL_STR;
    if (horizontal) {
        return { minValue, maxValue, minPosition, maxPosition, 0., 0., 0., 1., 1., 1. };
    }
    return { minPosition, maxPosition, minValue, maxValue, 0., 0., 0., 1., 1., 1. };
}
//=============================================================================
void
ViolinDensityHelpers::densityShape(
    const std::vector<double>& values, std::vector<double>& centers, std::vector<double>& widths)
{
    violinDensityShape(values, centers, widths);
}
//=============================================================================
std::vector<double>
ViolinDensityHelpers::uniquePositions(const std::vector<double>& x, const std::vector<double>& y)
{
    std::vector<double> positions;
    for (size_t k = 0; k < x.size() && k < y.size(); ++k) {
        if (finitePair(x[k], y[k])) {
            positions.push_back(x[k]);
        }
    }
    std::sort(positions.begin(), positions.end());
    positions.erase(std::unique(positions.begin(), positions.end()), positions.end());
    return positions;
}
//=============================================================================
std::vector<double>
ViolinDensityHelpers::valuesAtPosition(
    const std::vector<double>& x, const std::vector<double>& y, double position)
{
    std::vector<double> values;
    for (size_t k = 0; k < x.size() && k < y.size(); ++k) {
        if (finitePair(x[k], y[k]) && x[k] == position) {
            values.push_back(y[k]);
        }
    }
    return values;
}
//=============================================================================
void
ViolinDensityHelpers::drawViolin(RenderInterface& gc, GraphicsObject* object, double position,
    const std::vector<double>& values, const std::vector<double>& centers,
    const std::vector<double>& widths, const std::wstring& direction, bool horizontal,
    const std::vector<double>& faceColor, const std::vector<double>& edgeColor)
{
    Nelson::drawViolin(
        gc, object, position, values, centers, widths, direction, horizontal, faceColor, edgeColor);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
