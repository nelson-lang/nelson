//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScatterSeries.hpp"
#include "GOList.hpp"
#include "GraphicsObject.hpp"
#include "GOAxis.hpp"
#include "GOSymbolProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOAutoFlatColorProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "RenderHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOColorVectorProperty.hpp"
#include "GOScalarAlphaProperty.hpp"
#include "GOCommonProperties.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    static std::vector<double>
    defaultScatterColor()
    {
        return { 0, 0.4470, 0.7410 };
    }
    //=============================================================================
    static bool
    isTrueColorCData(const ArrayOf& cdata, size_t pointCount)
    {
        return cdata.is2D() && cdata.columns() == 3
            && cdata.rows() >= static_cast<indexType>(pointCount);
    }
    //=============================================================================
    static std::vector<double>
    trueColorAt(const double* ptrCData, indexType rows, size_t pointIndex)
    {
        return { ptrCData[pointIndex], ptrCData[pointIndex + rows],
            ptrCData[pointIndex + 2 * rows] };
    }
    //=============================================================================
    // Reports the CData range when the scatter is color-mapped (per-point scalar CData).
    // Follows the rendering decision in cDataColorAt so CLim matches what is drawn: an
    // empty CData, an Nx3 RGB list or a single RGB row are true-color and do not drive CLim.
    // Returns false and leaves cmin/cmax untouched in those cases.
    static bool
    mappedCDataRange(const ArrayOf& cdata, size_t pointCount, double& cmin, double& cmax)
    {
        if (cdata.isEmpty() || isTrueColorCData(cdata, pointCount) || cdata.elementCount() == 3) {
            return false;
        }
        cmin = ArrayOfMin(cdata);
        cmax = ArrayOfMax(cdata);
        return true;
    }
    //=============================================================================
    static void
    refreshCDataRange(
        const double* ptrCData, indexType elementCount, bool& dirty, double& minC, double& maxC)
    {
        if (!dirty) {
            return;
        }
        if (elementCount <= 0) {
            minC = 0.;
            maxC = 1.;
        } else {
            auto range = std::minmax_element(ptrCData, ptrCData + elementCount);
            minC = *range.first;
            maxC = *range.second;
        }
        dirty = false;
    }
    //=============================================================================
    static std::vector<double>
    mappedCDataColor(const double* ptrCData, indexType elementCount, size_t pointIndex,
        const std::vector<double>& colorMapData, bool& dirty, double& minC, double& maxC)
    {
        if (colorMapData.empty() || elementCount <= 0) {
            return defaultScatterColor();
        }
        refreshCDataRange(ptrCData, elementCount, dirty, minC, maxC);
        int colormapSize = static_cast<int>(colorMapData.size() / 3);
        if (colormapSize <= 0) {
            return defaultScatterColor();
        }
        size_t scalarIndex = std::min(pointIndex, static_cast<size_t>(elementCount - 1));
        int colorIndex = 0;
        if (maxC > minC) {
            colorIndex = static_cast<int>(
                std::round((ptrCData[scalarIndex] - minC) / (maxC - minC) * (colormapSize - 1)));
        }
        colorIndex = std::clamp(colorIndex, 0, colormapSize - 1);
        size_t offset = static_cast<size_t>(colorIndex) * 3;
        return { colorMapData[offset], colorMapData[offset + 1], colorMapData[offset + 2] };
    }
    //=============================================================================
    static std::vector<double>
    cDataColorAt(const ArrayOf& cdata, const double* ptrCData, size_t pointCount, size_t pointIndex,
        const std::vector<double>& colorMapData, bool& dirty, double& minC, double& maxC)
    {
        if (cdata.isEmpty() || !ptrCData) {
            return defaultScatterColor();
        }
        if (isTrueColorCData(cdata, pointCount)) {
            return trueColorAt(ptrCData, cdata.rows(), pointIndex);
        }
        if (cdata.elementCount() == 3) {
            return { ptrCData[0], ptrCData[1], ptrCData[2] };
        }
        return mappedCDataColor(
            ptrCData, cdata.elementCount(), pointIndex, colorMapData, dirty, minC, maxC);
    }
    //=============================================================================
    static double
    sizeDataAt(const std::vector<double>& sizeData, size_t pointIndex)
    {
        if (sizeData.size() == 1) {
            return sizeData[0];
        }
        if (pointIndex < sizeData.size()) {
            return sizeData[pointIndex];
        }
        return 36.;
    }
    //=============================================================================
    static void
    appendAlpha(std::vector<double>& color, const std::vector<double>& alphaData, bool isFlat,
        double scalarAlpha, size_t pointIndex)
    {
        if (!isFlat) {
            color.push_back(scalarAlpha);
            return;
        }
        if (alphaData.empty()) {
            color.push_back(1.);
        } else if (alphaData.size() == 1) {
            color.push_back(alphaData[0]);
        } else if (pointIndex < alphaData.size()) {
            color.push_back(alphaData[pointIndex]);
        } else {
            color.push_back(1.);
        }
    }
    //=============================================================================
    static bool
    isDrawableColor(const std::vector<double>& color)
    {
        return color.size() >= 3 && color[0] != -1;
    }
    //=============================================================================
    static bool
    appendRgbaColor(std::vector<double>& rgbaColors, const std::vector<double>& color)
    {
        if (!isDrawableColor(color)) {
            return false;
        }
        rgbaColors.push_back(color[0]);
        rgbaColors.push_back(color[1]);
        rgbaColors.push_back(color[2]);
        rgbaColors.push_back(color.size() > 3 ? color[3] : 1.);
        return true;
    }
    //=============================================================================
    static void
    ensureColorSpec(std::vector<double>& color)
    {
        if (color.size() < 3) {
            color = defaultScatterColor();
        }
    }
    //=============================================================================
    static double
    pseudoRandomValue(size_t index)
    {
        double value = std::sin(static_cast<double>(index + 1) * 12.9898) * 43758.5453;
        return value - std::floor(value);
    }
    //=============================================================================
    static std::vector<double>
    jitterOffsets(
        size_t count, double width, const std::wstring& mode, const std::wstring& direction)
    {
        std::vector<double> offsets(count, 0.);
        if (count <= 1 || width == 0. || mode == GO_PROPERTY_VALUE_NONE_STR) {
            return offsets;
        }
        if (mode == L"rand") {
            for (size_t k = 0; k < count; k++) {
                offsets[k] = width * (pseudoRandomValue(k) - 0.5);
            }
        } else if (mode == L"randn") {
            for (size_t k = 0; k < count; k++) {
                double normal = pseudoRandomValue(k) + pseudoRandomValue(k + 7) - 1.;
                offsets[k] = width * 0.25 * normal;
            }
        } else {
            for (size_t k = 0; k < count; k++) {
                offsets[k]
                    = -width / 2. + width * static_cast<double>(k) / static_cast<double>(count - 1);
            }
        }
        if (direction == L"positive") {
            for (double& offset : offsets) {
                offset = std::abs(offset);
            }
        } else if (direction == L"negative") {
            for (double& offset : offsets) {
                offset = -std::abs(offset);
            }
        }
        return offsets;
    }
    //=============================================================================
    static void
    applyJitter(std::vector<double>& values, const std::vector<double>& sortValues,
        const std::wstring& mode, const std::wstring& direction, double width)
    {
        if (values.empty() || mode == GO_PROPERTY_VALUE_NONE_STR || width == 0.) {
            return;
        }
        std::vector<double> levels(values);
        std::sort(levels.begin(), levels.end());
        levels.erase(std::unique(levels.begin(), levels.end()), levels.end());
        for (double level : levels) {
            std::vector<size_t> indices;
            for (size_t k = 0; k < values.size(); k++) {
                if (values[k] == level) {
                    indices.push_back(k);
                }
            }
            if (indices.size() <= 1) {
                continue;
            }
            std::sort(indices.begin(), indices.end(), [&sortValues](size_t lhs, size_t rhs) {
                if (lhs >= sortValues.size() || rhs >= sortValues.size()) {
                    return lhs < rhs;
                }
                return sortValues[lhs] < sortValues[rhs];
            });
            std::vector<double> offsets = jitterOffsets(indices.size(), width, mode, direction);
            for (size_t k = 0; k < indices.size(); k++) {
                values[indices[k]] += offsets[k];
            }
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
GOScatterSeries::getType()
{
    return GO_PROPERTY_VALUE_SCATTER_STR;
}
//=============================================================================
wstringVector
GOScatterSeries::getVisibleFieldnames()
{
    wstringVector fieldnames = GraphicsObject::getVisibleFieldnames();
    // scatter / scatter3 / swarmchart / polarscatter share this class. RData/ThetaData and
    // the jitter properties are exposed only for a polar scatter. They stay reachable
    // through findProperty, which backs the polar-data machinery. The cartesian-only
    // jitter-width modes are dropped here as well.
    GOAxis* axis = getParentAxis();
    const bool polar = axis && axis->isPolarAxes();
    if (!polar) {
        static const wstringVector cartesianHidden = { GO_R_DATA_PROPERTY_NAME_STR,
            GO_R_DATA_MODE_PROPERTY_NAME_STR, GO_R_DATA_SOURCE_PROPERTY_NAME_STR,
            GO_R_JITTER_PROPERTY_NAME_STR, GO_R_JITTER_DIRECTION_PROPERTY_NAME_STR,
            GO_R_JITTER_WIDTH_PROPERTY_NAME_STR, GO_R_VARIABLE_PROPERTY_NAME_STR,
            GO_THETA_DATA_PROPERTY_NAME_STR, GO_THETA_DATA_MODE_PROPERTY_NAME_STR,
            GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR, GO_THETA_JITTER_PROPERTY_NAME_STR,
            GO_THETA_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_THETA_JITTER_WIDTH_PROPERTY_NAME_STR,
            GO_THETA_VARIABLE_PROPERTY_NAME_STR, GO_X_JITTER_WIDTH_MODE_PROPERTY_NAME_STR,
            GO_Y_JITTER_WIDTH_MODE_PROPERTY_NAME_STR, GO_Z_JITTER_WIDTH_MODE_PROPERTY_NAME_STR };
        fieldnames.erase(std::remove_if(fieldnames.begin(), fieldnames.end(),
                             [](const std::wstring& name) {
                                 return std::find(
                                            cartesianHidden.begin(), cartesianHidden.end(), name)
                                     != cartesianHidden.end();
                             }),
            fieldnames.end());
    }
    return fieldnames;
}
//=============================================================================
GOScatterSeries::GOScatterSeries()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOScatterSeries::~GOScatterSeries() { }
//=============================================================================
void
GOScatterSeries::constructProperties()
{
    registerProperty(new GOVectorProperty, GO_ALPHA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_C_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarAlphaProperty, GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoFlatColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarAlphaProperty, GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_THETA_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_THETA_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_JITTER_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_JITTER_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_THETA_JITTER_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_THETA_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_R_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_R_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_DATA_SOURCE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_JITTER_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_JITTER_DIRECTION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_R_JITTER_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_R_VARIABLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_SIZE_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOScatterSeries::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    std::vector<double> defaultColor = { 0, 0.4470, 0.7410 };
    ArrayOf cdata = ArrayOf::doubleRowVector(defaultColor);
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_C_DATA_PROPERTY_NAME_STR);
    hp->data(cdata);
    setRestrictedStringDefault(GO_C_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, L"o");

    setRestrictedStringColorDefault(
        GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, -1, -1, -1);
    setRestrictedStringColorDefault(
        GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, -1, -1, -1);

    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_THETA_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setRestrictedStringDefault(GO_R_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setStringDefault(GO_THETA_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_R_DATA_SOURCE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_THETA_JITTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setStringDefault(GO_THETA_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
    setScalarDoubleDefault(GO_THETA_JITTER_WIDTH_PROPERTY_NAME_STR, 0.9);
    setStringDefault(GO_R_JITTER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setStringDefault(GO_R_JITTER_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOTH_STR);
    setScalarDoubleDefault(GO_R_JITTER_WIDTH_PROPERTY_NAME_STR, 0.9);
    setStringDefault(GO_THETA_VARIABLE_PROPERTY_NAME_STR, L"");
    setStringDefault(GO_R_VARIABLE_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setTwoVectorDefault(GO_X_DATA_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Y_DATA_PROPERTY_NAME_STR, 0, 1);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);

    std::vector<double> sizeDataDefault = { 36 };
    setVectorDoubleDefault(GO_SIZE_DATA_PROPERTY_NAME_STR, sizeDataDefault);

    GOScalarAlphaProperty* ea
        = static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR));
    ea->data(1.);
    GOScalarAlphaProperty* fa
        = static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR));
    fa->data(1.);
    initializeCommonProperties(this);
}
//=============================================================================
void
GOScatterSeries::updateState()
{
    if (hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
        // Setting CData switches CDataMode to manual. scatter.m restores 'auto' when it
        // uses the color-order color.
        toManual(GO_C_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
        cDataDirty = true;
        limitsDirty = true;
    }

    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR) || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_Z_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
    }
    // X is handled by the auto-index generation below.
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_Y_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_DATA_PROPERTY_NAME_STR) && !hasChanged(GO_Z_DATA_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Z_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_JITTER_WIDTH_PROPERTY_NAME_STR)
        && !hasChanged(GO_X_JITTER_WIDTH_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_X_JITTER_WIDTH_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_JITTER_WIDTH_PROPERTY_NAME_STR)
        && !hasChanged(GO_Y_JITTER_WIDTH_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Y_JITTER_WIDTH_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Z_JITTER_WIDTH_PROPERTY_NAME_STR)
        && !hasChanged(GO_Z_JITTER_WIDTH_MODE_PROPERTY_NAME_STR)) {
        toManual(GO_Z_JITTER_WIDTH_MODE_PROPERTY_NAME_STR);
    }
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        xs.clear();
        xs.resize(ys.size());
        OMP_PARALLEL_FOR_LOOP(ys.size())
        for (ompIndexType i = 0; i < (ompIndexType)ys.size(); i++) {
            xs[i] = (i + 1.0);
        }
    }
    GOVectorProperty* sp
        = static_cast<GOVectorProperty*>(findProperty(GO_X_DATA_PROPERTY_NAME_STR));
    sp->data(xs);
}
//=============================================================================
void
GOScatterSeries::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }

    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.lineWidth(width);

    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    cdata.promoteType(NLS_DOUBLE);
    const double* ptrCData = static_cast<const double*>(cdata.dataPointer());

    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));

    if (zs.empty()) {
        zs.resize(xs.size(), 0.0);
    }
    if (!((xs.size() == ys.size()) && (ys.size() == zs.size()))) {
        return;
    }
    applyJitter(xs, ys, findStringProperty(GO_X_JITTER_PROPERTY_NAME_STR),
        findStringProperty(GO_X_JITTER_DIRECTION_PROPERTY_NAME_STR),
        findScalarDoubleProperty(GO_X_JITTER_WIDTH_PROPERTY_NAME_STR));
    applyJitter(ys, xs, findStringProperty(GO_Y_JITTER_PROPERTY_NAME_STR),
        findStringProperty(GO_Y_JITTER_DIRECTION_PROPERTY_NAME_STR),
        findScalarDoubleProperty(GO_Y_JITTER_WIDTH_PROPERTY_NAME_STR));
    applyJitter(zs, xs, findStringProperty(GO_Z_JITTER_PROPERTY_NAME_STR),
        findStringProperty(GO_Z_JITTER_DIRECTION_PROPERTY_NAME_STR),
        findScalarDoubleProperty(GO_Z_JITTER_WIDTH_PROPERTY_NAME_STR));
    std::vector<double> mxs, mys, mzs;
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (parent) {
        parent->reMap(xs, ys, zs, mxs, mys, mzs);
    }
    if (!parent || mxs.empty()) {
        return;
    }

    GOScalarAlphaProperty* ea
        = static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_EDGE_ALPHA_PROPERTY_NAME_STR));
    GOScalarAlphaProperty* fa
        = static_cast<GOScalarAlphaProperty*>(findProperty(GO_MARKER_FACE_ALPHA_PROPERTY_NAME_STR));
    bool isEdgeAlphaFlat = ea->isFlat();
    bool isFaceAlphaFlat = fa->isFlat();
    GOAutoFlatColorProperty* ec = static_cast<GOAutoFlatColorProperty*>(
        findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR));
    GOAutoFlatColorProperty* fc = static_cast<GOAutoFlatColorProperty*>(
        findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR));
    std::vector<double> alphaData(findVectorDoubleProperty(GO_ALPHA_DATA_PROPERTY_NAME_STR));
    RenderInterface::SymbolType typ
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));

    std::vector<double> sizeData(findVectorDoubleProperty(GO_SIZE_DATA_PROPERTY_NAME_STR));
    if ((typ != RenderInterface::None) || ec->isEqual(GO_PROPERTY_VALUE_NONE_STR)
        || fc->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
        std::vector<double> uc;
        std::vector<double> vc;
        std::vector<double> dc;
        std::vector<size_t> sourceIndices;
        uc.reserve(mxs.size());
        vc.reserve(mxs.size());
        dc.reserve(mxs.size());
        sourceIndices.reserve(mxs.size());
        for (size_t i = 0; i < mxs.size(); i++) {
            double u, v, d;
            bool clipped;
            // The clip-space z lets an opaque surface occlude markers behind it.
            gc.toPixels(mxs[i], mys[i], mzs[i], u, v, d, clipped);
            if (!clipped) {
                uc.push_back(u);
                vc.push_back(v);
                dc.push_back(d);
                sourceIndices.push_back(i);
            }
        }
        gc.setupDirectDraw();

        std::vector<double> colorMapData;
        GOColorVectorProperty* colorMap = static_cast<GOColorVectorProperty*>(
            parent->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
        if (colorMap) {
            colorMapData = colorMap->data();
        }

        std::vector<std::vector<double>> edgeColors;
        std::vector<std::vector<double>> faceColors;
        std::vector<double> markerSizes;
        edgeColors.reserve(uc.size());
        faceColors.reserve(uc.size());
        markerSizes.reserve(uc.size());
        for (size_t k = 0; k < uc.size(); k++) {
            size_t pointIndex = sourceIndices[k];
            double sze = sizeDataAt(sizeData, pointIndex);
            markerSizes.push_back(std::sqrt(std::max(0., sze)));
            std::vector<double> edgeColor = ec->colorSpec();
            std::vector<double> faceColor = fc->colorSpec();
            std::vector<double> CdataVector = cDataColorAt(cdata, ptrCData, mxs.size(), pointIndex,
                colorMapData, cDataDirty, cachedMinC, cachedMaxC);

            if (ec->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
                edgeColor = CdataVector;
            }
            if (fc->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
                faceColor = CdataVector;
            }
            ensureColorSpec(edgeColor);
            ensureColorSpec(faceColor);
            appendAlpha(edgeColor, alphaData, isEdgeAlphaFlat, ea->data(), pointIndex);
            appendAlpha(faceColor, alphaData, isFaceAlphaFlat, fa->data(), pointIndex);
            edgeColors.push_back(edgeColor);
            faceColors.push_back(faceColor);
        }

        // Batched fast path: the marker set becomes one "points" primitive (RenderWeb), so a
        // large scatter animates from a compact display list. A backend that cannot batch
        // (Qt/OpenGL) returns false and the per-point loop below runs. A 'none' edge/face
        // (the -1 sentinel) is passed as an empty array.
        const bool edgeDrawable = !edgeColors.empty() && edgeColors.front()[0] != -1;
        const bool faceDrawable = !faceColors.empty() && faceColors.front()[0] != -1;
        std::vector<double> edgeFlat;
        std::vector<double> faceFlat;
        if (edgeDrawable) {
            edgeFlat.reserve(edgeColors.size() * 4);
            for (const std::vector<double>& c : edgeColors) {
                for (int k = 0; k < 4; k++) {
                    edgeFlat.push_back(k < static_cast<int>(c.size()) ? c[k] : 1.0);
                }
            }
        }
        if (faceDrawable) {
            faceFlat.reserve(faceColors.size() * 4);
            for (const std::vector<double>& c : faceColors) {
                for (int k = 0; k < 4; k++) {
                    faceFlat.push_back(k < static_cast<int>(c.size()) ? c[k] : 1.0);
                }
            }
        }
        if (DrawSymbolBatch(gc, typ, uc, vc, markerSizes, edgeFlat, faceFlat, width, dc)) {
            gc.releaseDirectDraw();
            return;
        }

        for (size_t i = 0; i < uc.size(); i++) {
            if (gc.markerOccluded(uc[i], vc[i], dc[i])) {
                continue;
            }
            DrawSymbol(
                gc, typ, uc[i], vc[i], 0, markerSizes[i], edgeColors[i], faceColors[i], width);
        }
        gc.releaseDirectDraw();
    }
}
//=============================================================================
std::vector<double>
GOScatterSeries::getLimits()
{
    if (!limitsDirty) {
        return limits;
    }
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    limits.resize(0);
    limits.reserve(12);
    limits.push_back(findVectorMin(xs));
    limits.push_back(findVectorMax(xs));
    limits.push_back(findVectorMin(ys));
    limits.push_back(findVectorMax(ys));
    if (zs.empty()) {
        limits.push_back(0);
        limits.push_back(0);
    } else {
        limits.push_back(findVectorMin(zs));
        limits.push_back(findVectorMax(zs));
    }
    // A color-mapped scatter drives CLim like surface/patch CData. True-color CData does not.
    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    double cmin = 0.;
    double cmax = 0.;
    mappedCDataRange(cdata, xs.size(), cmin, cmax);
    limits.push_back(cmin);
    limits.push_back(cmax);
    limits.push_back(0);
    limits.push_back(0);
    limitsDirty = false;
    return limits;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
