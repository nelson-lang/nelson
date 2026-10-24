//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicsObjectFactory.hpp"
#include "GOAnimatedLine.hpp"
#include "GOAnnotation.hpp"
#include "GOArea.hpp"
#include "GOAxesToolbar.hpp"
#include "GOAxis.hpp"
#include "GOBar.hpp"
#include "GOBinscatter.hpp"
#include "GOBoxChart.hpp"
#include "GOBubbleChart.hpp"
#include "GOBubbleCloud.hpp"
#include "GOBubbleLegend.hpp"
#include "GOColorBar.hpp"
#include "GOCompassPlot.hpp"
#include "GOConstantLine.hpp"
#include "GOContextMenu.hpp"
#include "GOContour.hpp"
#include "GOErrorBar.hpp"
#include "GOFunctionContour.hpp"
#include "GOFunctionLine.hpp"
#include "GOImplicitFunctionLine.hpp"
#include "GOImplicitFunctionSurface.hpp"
#include "GOFunctionSurface.hpp"
#include "GOGroup.hpp"
#include "GOTransform.hpp"
#include "GOHeatmapChart.hpp"
#include "GOHistogram.hpp"
#include "GOHistogram2.hpp"
#include "GOImage.hpp"
#include "GOFigure.hpp"
#include "GOLegend.hpp"
#include "GOLight.hpp"
#include "GOLineSeries.hpp"
#include "GOMenu.hpp"
#include "GOPatch.hpp"
#include "GOPolarAxes.hpp"
#include "GOUIAxes.hpp"
#include "GORectangle.hpp"
#include "GOParameterizedFunctionLine.hpp"
#include "GOParallelPlot.hpp"
#include "GOPieChart.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOQuiver.hpp"
#include "GORoot.hpp"
#include "GOScatterHistogram.hpp"
#include "GOScatterSeries.hpp"
#include "GOStackedLineChart.hpp"
#include "GOStair.hpp"
#include "GOStem.hpp"
#include "GOSubplotText.hpp"
#include "GOSurface.hpp"
#include "GOText.hpp"
#include "GOTiledChartLayout.hpp"
#include "GORainCloudPlot.hpp"
#include "GOViolinPlot.hpp"
#include "GOWordCloudChart.hpp"
#include "StringHelpers.hpp"
#include "GOUIControl.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isType(const std::wstring& type, const std::wstring& expected)
{
    return StringHelpers::iequals(type, expected);
}
//=============================================================================
static GOAnnotationKind
annotationKindForType(const std::wstring& type)
{
    // Accept both the constructor name (annotation('arrow', ...)) and the shape
    // name reported by the Type property (arrowshape), so that objects rebuilt
    // from a serialized Type -- openfig, copyobj, reset -- resolve correctly.
    if (isType(type, GO_PROPERTY_VALUE_ARROW_STR)
        || isType(type, GO_PROPERTY_VALUE_ARROW_SHAPE_STR)) {
        return GOAnnotationKind::Arrow;
    }
    if (isType(type, GO_PROPERTY_VALUE_DOUBLEARROW_STR)
        || isType(type, GO_PROPERTY_VALUE_DOUBLEARROW_SHAPE_STR)) {
        return GOAnnotationKind::DoubleArrow;
    }
    if (isType(type, GO_PROPERTY_VALUE_TEXTARROW_STR)
        || isType(type, GO_PROPERTY_VALUE_TEXTARROW_SHAPE_STR)) {
        return GOAnnotationKind::TextArrow;
    }
    if (isType(type, GO_PROPERTY_VALUE_RECTANGLE_STR)
        || isType(type, GO_PROPERTY_VALUE_RECTANGLE_SHAPE_STR)) {
        return GOAnnotationKind::Rectangle;
    }
    if (isType(type, GO_PROPERTY_VALUE_ELLIPSE_STR)
        || isType(type, GO_PROPERTY_VALUE_ELLIPSE_SHAPE_STR)) {
        return GOAnnotationKind::Ellipse;
    }
    if (isType(type, GO_PROPERTY_VALUE_TEXTBOX_STR)
        || isType(type, GO_PROPERTY_VALUE_TEXTBOX_SHAPE_STR)) {
        return GOAnnotationKind::TextBox;
    }
    return GOAnnotationKind::Line;
}
//=============================================================================
static bool
isAnnotationType(const std::wstring& type)
{
    return isType(type, GO_PROPERTY_VALUE_ARROW_STR)
        || isType(type, GO_PROPERTY_VALUE_ARROW_SHAPE_STR)
        || isType(type, GO_PROPERTY_VALUE_DOUBLEARROW_STR)
        || isType(type, GO_PROPERTY_VALUE_DOUBLEARROW_SHAPE_STR)
        || isType(type, GO_PROPERTY_VALUE_TEXTARROW_STR)
        || isType(type, GO_PROPERTY_VALUE_TEXTARROW_SHAPE_STR)
        || isType(type, GO_PROPERTY_VALUE_RECTANGLE_STR)
        || isType(type, GO_PROPERTY_VALUE_RECTANGLE_SHAPE_STR)
        || isType(type, GO_PROPERTY_VALUE_ELLIPSE_STR)
        || isType(type, GO_PROPERTY_VALUE_ELLIPSE_SHAPE_STR)
        || isType(type, GO_PROPERTY_VALUE_TEXTBOX_STR)
        || isType(type, GO_PROPERTY_VALUE_TEXTBOX_SHAPE_STR)
        || isType(type, GO_PROPERTY_VALUE_LINE_SHAPE_STR);
}
//=============================================================================
static bool
isAnnotationLineType(const std::wstring& type, GraphicsObject* parent)
{
    return isType(type, GO_PROPERTY_VALUE_LINE_STR) && parent
        && parent->isType(GO_PROPERTY_VALUE_FIGURE_STR);
}
//=============================================================================
bool
canCreateGraphicsObjectForType(
    const std::wstring& type, GraphicsObject* parent, bool includeInternalObjects)
{
    if (isType(type, GO_PROPERTY_VALUE_AXES_STR) || isType(type, GO_PROPERTY_VALUE_POLAR_AXES_STR)
        || isType(type, GO_PROPERTY_VALUE_COLORBAR_STR)
        || isType(type, GO_PROPERTY_VALUE_UICONTEXTMENU_STR)
        || isType(type, GO_PROPERTY_VALUE_UIMENU_STR) || isType(type, GO_PROPERTY_VALUE_LEGEND_STR)
        || isType(type, GO_PROPERTY_VALUE_LINE_STR) || isType(type, GO_PROPERTY_VALUE_STAIR_STR)
        || isType(type, GO_PROPERTY_VALUE_ANIMATED_LINE_STR)
        || isType(type, GO_PROPERTY_VALUE_IMAGE_STR) || isType(type, GO_PROPERTY_VALUE_LIGHT_STR)
        || isType(type, GO_PROPERTY_VALUE_TEXT_STR)
        || isType(type, GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)
        || isType(type, GO_PROPERTY_VALUE_SURFACE_STR)
        || isType(type, GO_PROPERTY_VALUE_FUNCTION_LINE_STR)
        || isType(type, GO_PROPERTY_VALUE_PARAMETERIZED_FUNCTION_LINE_STR)
        || isType(type, GO_PROPERTY_VALUE_FUNCTION_SURFACE_STR)
        || isType(type, GO_PROPERTY_VALUE_FUNCTION_CONTOUR_STR)
        || isType(type, GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_LINE_STR)
        || isType(type, GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_SURFACE_STR)
        || isType(type, GO_PROPERTY_VALUE_PATCH_STR) || isType(type, GO_PROPERTY_VALUE_AREA_STR)
        || isType(type, GO_PROPERTY_VALUE_BAR_STR) || isType(type, GO_PROPERTY_VALUE_BINSCATTER_STR)
        || isType(type, GO_PROPERTY_VALUE_BOX_CHART_STR)
        || isType(type, GO_PROPERTY_VALUE_ERRORBAR_STR)
        || isType(type, GO_PROPERTY_VALUE_HISTOGRAM_STR)
        || isType(type, GO_PROPERTY_VALUE_HISTOGRAM2_STR)
        || isType(type, GO_PROPERTY_VALUE_PIE_CHART_STR)
        || isType(type, GO_PROPERTY_VALUE_DONUT_CHART_STR)
        || isType(type, GO_PROPERTY_VALUE_SCATTER_STR)
        || isType(type, GO_PROPERTY_VALUE_BUBBLE_CHART_STR)
        || isType(type, GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR)
        || isType(type, GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR)
        || isType(type, GO_PROPERTY_VALUE_QUIVER_STR) || isType(type, GO_PROPERTY_VALUE_CONTOUR_STR)
        || isType(type, GO_PROPERTY_VALUE_COMPASS_PLOT_STR)
        || isType(type, GO_PROPERTY_VALUE_HEATMAP_STR)
        || isType(type, GO_PROPERTY_VALUE_PARALLEL_PLOT_STR)
        || isType(type, GO_PROPERTY_VALUE_SCATTER_HISTOGRAM_STR)
        || isType(type, GO_PROPERTY_VALUE_STEM_STR)
        || isType(type, GO_PROPERTY_VALUE_RAIN_CLOUD_PLOT_STR)
        || isType(type, GO_PROPERTY_VALUE_VIOLIN_PLOT_STR)
        || isType(type, GO_PROPERTY_VALUE_WORD_CLOUD_STR)
        || isType(type, GO_PROPERTY_VALUE_HGGROUP_STR)
        || isType(type, GO_PROPERTY_VALUE_HGTRANSFORM_STR)
        || isType(type, GO_PROPERTY_VALUE_TILED_LAYOUT_STR)
        || isType(type, GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR)
        || isType(type, GO_PROPERTY_VALUE_CONSTANTLINE_STR) || isAnnotationType(type)
        || isAnnotationLineType(type, parent)) {
        return true;
    }
    return includeInternalObjects
        && (isType(type, GO_PROPERTY_VALUE_ROOT_STR) || isType(type, GO_PROPERTY_VALUE_FIGURE_STR)
            || isType(type, GO_PROPERTY_VALUE_AXES_TOOLBAR_STR)
            || isType(type, GO_PROPERTY_VALUE_UICONTROL_STR));
}
//=============================================================================
GraphicsObject*
newGraphicsObjectForType(
    const std::wstring& type, GraphicsObject* parent, bool includeInternalObjects)
{
    if (isType(type, GO_PROPERTY_VALUE_POLAR_AXES_STR)) {
        return new GOPolarAxes;
    }
    if (isType(type, GO_PROPERTY_VALUE_AXES_STR)) {
        return new GOAxis;
    }
    if (isType(type, GO_PROPERTY_VALUE_COLORBAR_STR)) {
        return new GOColorBar;
    }
    if (isType(type, GO_PROPERTY_VALUE_UICONTEXTMENU_STR)) {
        return new GOContextMenu;
    }
    if (isType(type, GO_PROPERTY_VALUE_UIMENU_STR)) {
        return new GOMenu;
    }
    if (isType(type, GO_PROPERTY_VALUE_LEGEND_STR)) {
        return new GOLegend;
    }
    if (isAnnotationLineType(type, parent)) {
        return new GOAnnotation(GOAnnotationKind::Line);
    }
    if (isType(type, GO_PROPERTY_VALUE_LINE_STR)) {
        return new GOLineSeries;
    }
    if (isType(type, GO_PROPERTY_VALUE_STAIR_STR)) {
        return new GOStair;
    }
    if (isType(type, GO_PROPERTY_VALUE_ANIMATED_LINE_STR)) {
        return new GOAnimatedLine;
    }
    if (isType(type, GO_PROPERTY_VALUE_IMAGE_STR)) {
        return new GOImage;
    }
    if (isType(type, GO_PROPERTY_VALUE_LIGHT_STR)) {
        return new GOLight;
    }
    if (isType(type, GO_PROPERTY_VALUE_TEXT_STR)) {
        return new GOText;
    }
    if (isType(type, GO_PROPERTY_VALUE_SUBPLOT_TEXT_STR)) {
        return new GOSubplotText;
    }
    if (isType(type, GO_PROPERTY_VALUE_SURFACE_STR)) {
        return new GOSurface;
    }
    if (isType(type, GO_PROPERTY_VALUE_FUNCTION_LINE_STR)) {
        return new GOFunctionLine;
    }
    if (isType(type, GO_PROPERTY_VALUE_PARAMETERIZED_FUNCTION_LINE_STR)) {
        return new GOParameterizedFunctionLine;
    }
    if (isType(type, GO_PROPERTY_VALUE_FUNCTION_SURFACE_STR)) {
        return new GOFunctionSurface;
    }
    if (isType(type, GO_PROPERTY_VALUE_FUNCTION_CONTOUR_STR)) {
        return new GOFunctionContour;
    }
    if (isType(type, GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_LINE_STR)) {
        return new GOImplicitFunctionLine;
    }
    if (isType(type, GO_PROPERTY_VALUE_IMPLICIT_FUNCTION_SURFACE_STR)) {
        return new GOImplicitFunctionSurface;
    }
    if (isType(type, GO_PROPERTY_VALUE_PATCH_STR)) {
        return new GOPatch;
    }
    if (isType(type, GO_PROPERTY_VALUE_AREA_STR)) {
        return new GOArea;
    }
    if (isType(type, GO_PROPERTY_VALUE_BAR_STR)) {
        return new GOBar;
    }
    if (isType(type, GO_PROPERTY_VALUE_BINSCATTER_STR)) {
        return new GOBinscatter;
    }
    if (isType(type, GO_PROPERTY_VALUE_BOX_CHART_STR)) {
        return new GOBoxChart;
    }
    if (isType(type, GO_PROPERTY_VALUE_ERRORBAR_STR)) {
        return new GOErrorBar;
    }
    if (isType(type, GO_PROPERTY_VALUE_HISTOGRAM_STR)) {
        return new GOHistogram;
    }
    if (isType(type, GO_PROPERTY_VALUE_HISTOGRAM2_STR)) {
        return new GOHistogram2;
    }
    if (isType(type, GO_PROPERTY_VALUE_HEATMAP_STR)) {
        return new GOHeatmapChart;
    }
    if (isType(type, GO_PROPERTY_VALUE_PARALLEL_PLOT_STR)) {
        return new GOParallelPlot;
    }
    if (isType(type, GO_PROPERTY_VALUE_SCATTER_HISTOGRAM_STR)) {
        return new GOScatterHistogram;
    }
    if (isType(type, GO_PROPERTY_VALUE_STEM_STR)) {
        return new GOStem;
    }
    if (isType(type, GO_PROPERTY_VALUE_RAIN_CLOUD_PLOT_STR)) {
        return new GORainCloudPlot;
    }
    if (isType(type, GO_PROPERTY_VALUE_VIOLIN_PLOT_STR)) {
        return new GOViolinPlot;
    }
    if (isType(type, GO_PROPERTY_VALUE_WORD_CLOUD_STR)) {
        return new GOWordCloudChart;
    }
    if (isType(type, GO_PROPERTY_VALUE_PIE_CHART_STR)) {
        return new GOPieChart;
    }
    if (isType(type, GO_PROPERTY_VALUE_DONUT_CHART_STR)) {
        return new GODonutChart;
    }
    if (isType(type, GO_PROPERTY_VALUE_SCATTER_STR)) {
        return new GOScatterSeries;
    }
    if (isType(type, GO_PROPERTY_VALUE_BUBBLE_CHART_STR)) {
        return new GOBubbleChart;
    }
    if (isType(type, GO_PROPERTY_VALUE_BUBBLE_CLOUD_STR)) {
        return new GOBubbleCloud;
    }
    if (isType(type, GO_PROPERTY_VALUE_BUBBLE_LEGEND_STR)) {
        return new GOBubbleLegend;
    }
    if (isType(type, GO_PROPERTY_VALUE_QUIVER_STR)) {
        return new GOQuiver;
    }
    if (isType(type, GO_PROPERTY_VALUE_COMPASS_PLOT_STR)) {
        return new GOCompassPlot;
    }
    if (isType(type, GO_PROPERTY_VALUE_CONTOUR_STR)) {
        return new GOContour;
    }
    if (isType(type, GO_PROPERTY_VALUE_HGGROUP_STR)) {
        return new GOGroup;
    }
    if (isType(type, GO_PROPERTY_VALUE_HGTRANSFORM_STR)) {
        return new GOTransform;
    }
    if (isType(type, GO_PROPERTY_VALUE_TILED_LAYOUT_STR)) {
        return new GOTiledChartLayout;
    }
    if (isType(type, GO_PROPERTY_VALUE_STACKED_LINE_CHART_STR)) {
        return new GOStackedLineChart;
    }
    if (isType(type, GO_PROPERTY_VALUE_CONSTANTLINE_STR)) {
        return new GOConstantLine;
    }
    if (isAnnotationType(type)) {
        return new GOAnnotation(annotationKindForType(type));
    }
    if (includeInternalObjects && isType(type, GO_PROPERTY_VALUE_ROOT_STR)) {
        return new GORoot;
    }
    if (includeInternalObjects && isType(type, GO_PROPERTY_VALUE_FIGURE_STR)) {
        return new GOFigure(nullptr, -1);
    }
    if (includeInternalObjects && isType(type, GO_PROPERTY_VALUE_AXES_TOOLBAR_STR)) {
        return new GOAxesToolbar;
    }
    if (includeInternalObjects && isType(type, GO_PROPERTY_VALUE_UICONTROL_STR)) {
        return new GOUIControl;
    }
    return nullptr;
}
//=============================================================================
GraphicsObject*
newGraphicsObjectForSource(GraphicsObject* source)
{
    if (!source) {
        return nullptr;
    }
    if (auto* annotation = dynamic_cast<GOAnnotation*>(source)) {
        // Resolve from the concrete source: the Type property reports the shape
        // name (arrowshape, ...), not the constructor name expected by the factory.
        return new GOAnnotation(annotation->getKind());
    }
    // The 'rectangle' type name is shared with annotation shapes, so the primitive
    // rectangle must be resolved from the concrete source object, not its type name.
    if (dynamic_cast<GORectangle*>(source)) {
        return new GORectangle;
    }
    // GOPolarAxes reports getType() == "axes" (like the historical polar-mode axes), so it
    // must be resolved from the concrete source object rather than its type name.
    if (dynamic_cast<GOPolarAxes*>(source)) {
        return new GOPolarAxes;
    }
    // GOUIAxes also reports getType() == "axes"; resolve it from the concrete source so a
    // cloned uiaxes keeps its dedicated class rather than degrading to a cartesian axes.
    if (dynamic_cast<GOUIAxes*>(source)) {
        return new GOUIAxes;
    }
    return newGraphicsObjectForType(source->getType());
}
//=============================================================================
bool
isGraphicsTreeLinkProperty(const std::wstring& name)
{
    return name == GO_PARENT_PROPERTY_NAME_STR || name == GO_CHILDREN_PROPERTY_NAME_STR;
}
//=============================================================================
bool
containsGraphicsRuntimeValue(const ArrayOf& value)
{
    if (value.isGraphicsObject() || value.isHandle() || value.isFunctionHandle()) {
        return true;
    }
    if (value.isCell()) {
        const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
        for (indexType k = 0; k < value.elementCount(); ++k) {
            if (containsGraphicsRuntimeValue(elements[k])) {
                return true;
            }
        }
    } else if (value.isStruct() || value.isClassType() || value.isClassdefType()) {
        const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
        indexType fieldCount = static_cast<indexType>(value.fieldNames().size());
        for (indexType k = 0; k < value.elementCount() * fieldCount; ++k) {
            if (containsGraphicsRuntimeValue(elements[k])) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
canCopyGraphicsProperty(GraphicsObject* source, GraphicsObject* target, const std::wstring& name)
{
    if (!source || !target || isGraphicsTreeLinkProperty(name) || !target->haveProperty(name)
        || !target->isWritable(name)) {
        return false;
    }
    GOGenericProperty* property = source->findProperty(name, false);
    return property && !containsGraphicsRuntimeValue(property->get());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
