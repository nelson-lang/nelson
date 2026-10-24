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
#include "GOContour.hpp"
#include "ContourGenerator.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOColorInterpProperty.hpp"
#include "GOFaceAlphaProperty.hpp"
#include "GOEdgeAlphaProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOZLocationProperty.hpp"
#include "GOAxisHelpers.hpp"
#include "GOList.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "ParallelSort.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOCommonProperties.hpp"
#include "NelsonConfiguration.hpp"
#include "FunctionDef.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
#include <cstddef>
#include <cstdio>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    // EdgeColor/LineColor and FaceColor/Fill are two names for one attribute. Each
    // name writes both, so the last one written wins. Reconciling the pair
    // afterwards cannot work: the changed flags do not record the write order.
    //
    // The twin is looked up by name on every write, never held as a pointer.
    // fcontour re-registers one half of a pair, which destroys the property
    // registered under that name. The twin is written through a qualified call to
    // the base implementation, which does not mirror back.
    //=============================================================================
    class GOContourEdgeColorProperty : public GOColorInterpProperty
    {
    public:
        GOContourEdgeColorProperty(GraphicsObject* owner, const std::wstring& twinName)
            : m_owner(owner), m_twinName(twinName)
        {
        }
        void
        set(ArrayOf value) override
        {
            GOColorInterpProperty::set(value);
            GORestrictedStringColorProperty* twin = dynamic_cast<GORestrictedStringColorProperty*>(
                m_owner->findProperty(m_twinName, false));
            if (twin) {
                twin->GORestrictedStringColorProperty::set(value);
            }
        }

    private:
        GraphicsObject* m_owner;
        std::wstring m_twinName;
    };
    //=============================================================================
    class GOContourFaceColorProperty : public GOColorInterpProperty
    {
    public:
        GOContourFaceColorProperty(GraphicsObject* owner) : m_owner(owner) { }
        void
        set(ArrayOf value) override
        {
            GOColorInterpProperty::set(value);
            GORestrictedStringProperty* fill = dynamic_cast<GORestrictedStringProperty*>(
                m_owner->findProperty(GO_FILL_PROPERTY_NAME_STR, false));
            if (fill) {
                const std::wstring fillValue = isEqual(GO_PROPERTY_VALUE_NONE_STR)
                    ? GO_PROPERTY_VALUE_OFF_STR
                    : GO_PROPERTY_VALUE_ON_STR;
                fill->GORestrictedStringProperty::set(ArrayOf::characterArray(fillValue));
            }
        }

    private:
        GraphicsObject* m_owner;
    };
    //=============================================================================
    class GOContourFillProperty : public GOOnOffProperty
    {
    public:
        GOContourFillProperty(GraphicsObject* owner) : m_owner(owner) { }
        void
        set(ArrayOf value) override
        {
            GOOnOffProperty::set(value);
            GORestrictedStringColorProperty* faceColor
                = dynamic_cast<GORestrictedStringColorProperty*>(
                    m_owner->findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR, false));
            if (!faceColor) {
                return;
            }
            if (isEqual(GO_PROPERTY_VALUE_ON_STR)) {
                // Only synthesize a fill colour when none is set; an existing
                // 'flat'/'interp'/colour-spec value already means filled.
                if (faceColor->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
                    faceColor->GORestrictedStringColorProperty::set(
                        ArrayOf::characterArray(GO_PROPERTY_VALUE_FLAT_STR));
                }
            } else {
                faceColor->GORestrictedStringColorProperty::set(
                    ArrayOf::characterArray(GO_PROPERTY_VALUE_NONE_STR));
            }
        }

    private:
        GraphicsObject* m_owner;
    };
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
GOContour::getType()
{
    return GO_PROPERTY_VALUE_CONTOUR_STR;
}
//=============================================================================
GOContour::GOContour()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOContour::~GOContour() { }
//=============================================================================
void
GOContour::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_CONTOUR_MATRIX_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOFaceAlphaProperty, GO_FACE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOContourFaceColorProperty(this), GO_FACE_COLOR_PROPERTY_NAME_STR);
    // Fill is the user-facing name; FaceColor is the one the fill renderer reads.
    registerProperty(new GOContourFillProperty(this), GO_FILL_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_FILL_BELOW_LEVEL_PROPERTY_NAME_STR, true, false);
    registerProperty(new GOOnOffProperty, GO_FLOATING_PROPERTY_NAME_STR);
    registerProperty(new GOColorInterpProperty, GO_LABEL_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_LABEL_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LABEL_SPACING_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_LEVEL_LIST_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LEVEL_STEP_PROPERTY_NAME_STR);
    registerProperty(new GOContourEdgeColorProperty(this, GO_LINE_COLOR_PROPERTY_NAME_STR),
        GO_EDGE_COLOR_PROPERTY_NAME_STR);
    // LineColor is the user-facing name; EdgeColor is the one the renderer reads.
    registerProperty(new GOContourEdgeColorProperty(this, GO_EDGE_COLOR_PROPERTY_NAME_STR),
        GO_LINE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0, 1), GO_EDGE_ALPHA_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SHOW_TEXT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_TEXT_LIST_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TEXT_LIST_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_TEXT_STEP_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_TEXT_STEP_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOZLocationProperty, GO_Z_LOCATION_PROPERTY_NAME_STR);
    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOContour::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringColorDefault(
        GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    setRestrictedStringColorDefault(
        GO_LINE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR, 0, 0, 0);
    setScalarDoubleDefault(GO_EDGE_ALPHA_PROPERTY_NAME_STR, 1);
    setRestrictedStringColorDefault(
        GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR, 0, 0, 0);
    setRestrictedStringScalarDefault(
        GO_FACE_ALPHA_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 1);
    setRestrictedStringDefault(GO_FILL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_FILL_BELOW_LEVEL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_LEVEL_STEP_PROPERTY_NAME_STR, 0.);
    setRestrictedStringColorDefault(GO_LABEL_COLOR_PROPERTY_NAME_STR,
        GO_PROPERTY_VALUE_COLORSPEC_STR, 0.129411764705882, 0.129411764705882, 0.129411764705882);
    setScalarDoubleDefault(GO_LABEL_SPACING_PROPERTY_NAME_STR, 144.);
    setRestrictedStringDefault(GO_SHOW_TEXT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_TEXT_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_TEXT_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setScalarDoubleDefault(GO_TEXT_STEP_PROPERTY_NAME_STR, 0.);
    setRestrictedStringScalarDefault(
        GO_Z_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_SCALAR_STR, 0.);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_FLOATING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    static_cast<GOArrayOfProperty*>(findProperty(GO_LABEL_FORMAT_PROPERTY_NAME_STR))
        ->data(ArrayOf::characterArray(L"%g"));
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
}
//=============================================================================
void
GOContour::updateState()
{
    // The paired properties are kept in step as they are written, so there is
    // nothing to reconcile here.
    if (hasChanged(GO_LEVEL_LIST_PROPERTY_NAME_STR)) {
        toManual(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_X_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_Y_DATA_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_LEVEL_STEP_PROPERTY_NAME_STR)) {
        toManual(GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_TEXT_LIST_PROPERTY_NAME_STR)) {
        toManual(GO_TEXT_LIST_MODE_PROPERTY_NAME_STR);
    }
    if (hasChanged(GO_TEXT_STEP_PROPERTY_NAME_STR)) {
        toManual(GO_TEXT_STEP_MODE_PROPERTY_NAME_STR);
    }
    ArrayOf zData(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (!zData.isNumeric() || zData.isEmpty()) {
        contourLines.clear();
        zValues.clear();
        rebuildContourMatrix();
        return;
    }
    zData.promoteType(NLS_DOUBLE);

    ArrayOf xInput = stringCheck(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
        ? findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR)
        : ArrayOf::empty();
    ArrayOf yInput = stringCheck(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
        ? findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR)
        : ArrayOf::empty();
    ArrayOf xData(contourCoordinateData(zData, xInput, true));
    ArrayOf yData(contourCoordinateData(zData, yInput, false));

    // Reflect the grid on XData/YData so they can be queried. Auto mode uses the
    // row vectors 1:columns and 1:rows. data() does not flag the property as
    // changed, so auto mode is preserved.
    if (!stringCheck(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        indexType columns = zData.columns();
        double* xp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, columns));
        for (indexType i = 0; i < columns; i++) {
            xp[i] = static_cast<double>(i + 1);
        }
        static_cast<GOArrayOfProperty*>(findProperty(GO_X_DATA_PROPERTY_NAME_STR))
            ->data(ArrayOf(NLS_DOUBLE, Dimensions(1, columns), xp));
    }
    if (!stringCheck(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        indexType rows = zData.rows();
        double* yp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rows));
        for (indexType j = 0; j < rows; j++) {
            yp[j] = static_cast<double>(j + 1);
        }
        static_cast<GOArrayOfProperty*>(findProperty(GO_Y_DATA_PROPERTY_NAME_STR))
            ->data(ArrayOf(NLS_DOUBLE, Dimensions(1, rows), yp));
    }

    // An unset LevelStep reports the round step of the data range, whatever
    // LevelList holds. data() plus clearModified() publishes the value without
    // switching to manual mode.
    if (!stringCheck(GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        GOScalarProperty* stepProperty
            = static_cast<GOScalarProperty*>(findProperty(GO_LEVEL_STEP_PROPERTY_NAME_STR));
        stepProperty->data(defaultContourLevelStep(zData));
        stepProperty->clearModified();
    }

    std::vector<double> levels;
    // Labelled levels for TextList. Same as LevelList, except for the base band
    // that filled contours prepend (the data minimum): it is a fill boundary, not a
    // labelled line.
    std::vector<double> textLevels;

    if (stringCheck(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        levels = findVectorDoubleProperty(GO_LEVEL_LIST_PROPERTY_NAME_STR);
        textLevels = levels;
    } else if (stringCheck(GO_LEVEL_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        const double* pzData = static_cast<const double*>(zData.dataPointer());
        double levelStep = findScalarDoubleProperty(GO_LEVEL_STEP_PROPERTY_NAME_STR);
        double min_val = 0;
        double max_val = 0;
        bool initialized = false;
        for (indexType k = 0; k < zData.elementCount(); k++) {
            if (!std::isfinite(pzData[k])) {
                continue;
            }
            if (!initialized) {
                min_val = pzData[k];
                max_val = pzData[k];
                initialized = true;
            } else {
                min_val = std::min(min_val, pzData[k]);
                max_val = std::max(max_val, pzData[k]);
            }
        }
        if (!initialized || levelStep <= 0) {
            levels.clear();
        } else {
            double lvl1 = std::ceil(min_val / levelStep) * levelStep;
            double lvl2 = std::floor(max_val / levelStep) * levelStep;
            if (lvl1 >= lvl2) {
                indexType rows = zData.rows();
                indexType columns = zData.columns();
                for (int i = 0; i < rows; ++i) {
                    std::vector<double> flattenedZ;

                    for (int j = 0; j < columns; ++j) {
                        double value = pzData[j * rows + i];
                        if (std::isfinite(value)) {
                            flattenedZ.push_back(value);
                        }
                    }
                    if (!flattenedZ.empty()) {
                        parallelSort(flattenedZ);
                        levels.push_back(flattenedZ[flattenedZ.size() / 2]);
                    }
                }
            } else {
                levels.clear();
                for (double level = lvl1; level <= lvl2; level += levelStep) {
                    levels.push_back(level);
                }
            }
        }
        textLevels = levels;
        GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_LEVEL_LIST_PROPERTY_NAME_STR);
        hp->data(levels);
        hp->clearModified();
        toAuto(GO_LEVEL_LIST_MODE_PROPERTY_NAME_STR);
    } else {
        levels = defaultContourLevels(zData);
        textLevels = levels;
        // Filled contours anchor the lowest band at the data minimum, so the auto
        // LevelList is [zMin, interior levels...]. This also sets the colour limits to
        // [zMin, topLevel]. Unfilled contours keep only the interior levels.
        if (!stringCheck(GO_FACE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)
            && !levels.empty()) {
            const double* pzLevels = static_cast<const double*>(zData.dataPointer());
            double zMin = 0.;
            bool found = false;
            for (indexType k = 0; k < zData.elementCount(); ++k) {
                double value = pzLevels[k];
                if (!std::isfinite(value)) {
                    continue;
                }
                if (!found) {
                    zMin = value;
                    found = true;
                } else {
                    zMin = std::min(zMin, value);
                }
            }
            if (found && zMin < levels.front()) {
                levels.insert(levels.begin(), zMin);
            }
        }
        GOVectorProperty* hp = (GOVectorProperty*)findProperty(GO_LEVEL_LIST_PROPERTY_NAME_STR);
        hp->data(levels);
        hp->clearModified();
    }

    // Reflect the labelled levels on TextList when it tracks them automatically.
    // data() does not flag the property as changed, so auto mode is preserved.
    if (!stringCheck(GO_TEXT_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
        GOVectorProperty* tp = (GOVectorProperty*)findProperty(GO_TEXT_LIST_PROPERTY_NAME_STR);
        tp->data(textLevels);
        tp->clearModified();
    }

    // Tracing the level lines is costly and getLimits() calls updateState() on every
    // layout pass. Skip it when data, coordinates and levels are those the current
    // lines were built from.
    std::size_t inputsHash = contourInputsSignature(zData, xData, yData, levels);
    if (contourInputsHashValid && inputsHash == contourInputsHash && !contourLines.empty()) {
        return;
    }

    contourLines.clear();
    zValues.clear();

    for (int i = 0; i < levels.size(); i++) {
        contourLines.push_back(generateContourLines(zData, levels[i], xData, yData));
        zValues.push_back(levels[i]);
    }

    rebuildContourMatrix();
    contourInputsHash = inputsHash;
    contourInputsHashValid = true;
}
//=============================================================================
namespace {
    //=============================================================================
    struct FillPoint
    {
        double x;
        double y;
        double z;
        double value;
    };
    //=============================================================================
    struct LabelPlacement
    {
        double x;
        double y;
        double rotation;
        std::wstring label;
        std::vector<double> color;
    };
    //=============================================================================
    std::vector<FillPoint>
    clipFillPolygon(const std::vector<FillPoint>& polygon, double threshold, bool keepAbove)
    {
        std::vector<FillPoint> output;
        if (polygon.empty()) {
            return output;
        }
        auto inside = [&](const FillPoint& p) {
            return keepAbove ? (p.value >= threshold) : (p.value <= threshold);
        };
        auto intersect = [&](const FillPoint& a, const FillPoint& b) {
            double denom = b.value - a.value;
            double t = (denom == 0.) ? 0. : (threshold - a.value) / denom;
            t = std::min(1., std::max(0., t));
            return FillPoint { a.x + t * (b.x - a.x), a.y + t * (b.y - a.y), a.z + t * (b.z - a.z),
                threshold };
        };

        FillPoint previous = polygon.back();
        bool previousInside = inside(previous);
        for (const FillPoint& current : polygon) {
            bool currentInside = inside(current);
            if (currentInside) {
                if (!previousInside) {
                    output.push_back(intersect(previous, current));
                }
                output.push_back(current);
            } else if (previousInside) {
                output.push_back(intersect(previous, current));
            }
            previous = current;
            previousInside = currentInside;
        }
        return output;
    }
    //=============================================================================
    std::vector<double>
    colormapColor(GraphicsObject* fp, double zValue, double alpha)
    {
        GOFigure* fig = (fp ? fp->getParentFigure() : nullptr);
        if (!fig) {
            return { 0., 0., 0., alpha };
        }
        std::vector<double> colorMap(fig->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
        GOAxis* axis = (fp ? fp->getParentAxis() : nullptr);
        if (!axis) {
            if (colorMap.size() >= 3) {
                return { colorMap[0], colorMap[1], colorMap[2], alpha };
            }
            return { 0., 0., 0., alpha };
        }
        std::vector<double> cLim(axis->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
        double cLimMin = std::min(cLim[0], cLim[1]);
        double cLimMax = std::max(cLim[0], cLim[1]);
        int colorMapLength = static_cast<int>(colorMap.size() / 3);
        if (colorMapLength <= 0) {
            return { 0., 0., 0., alpha };
        }
        int idx = 0;
        if (cLimMax != cLimMin) {
            // Same lookup as GOImage: floor(scaled * colorMapLength) clamped to the last
            // index. Using (colorMapLength - 1) would shift every band.
            double scaled = (zValue - cLimMin) / (cLimMax - cLimMin);
            idx = static_cast<int>(std::floor(scaled * colorMapLength));
        }
        idx = std::min(colorMapLength - 1, std::max(0, idx));
        return { colorMap[3 * idx], colorMap[3 * idx + 1], colorMap[3 * idx + 2], alpha };
    }
    //=============================================================================
    bool
    contourColor(GraphicsObject* fp, const std::wstring& propertyName, double zValue, double alpha,
        std::vector<double>& color)
    {
        GORestrictedStringColorProperty* cp = nullptr;
        if (fp) {
            cp = static_cast<GORestrictedStringColorProperty*>(
                fp->findProperty(propertyName, false));
        }
        if (!cp) {
            return false;
        }
        if (cp->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return false;
        }
        if (cp->isEqual(GO_PROPERTY_VALUE_FLAT_STR) || cp->isEqual(GO_PROPERTY_VALUE_INTERP_STR)) {
            color = colormapColor(fp, zValue, alpha);
        } else {
            color = cp->colorSpec();
            color.push_back(alpha);
        }
        return true;
    }
    //=============================================================================
    double
    contourPlaneZ(GraphicsObject* fp, GOAxis* axis)
    {
        if (!fp) {
            return 0.;
        }
        GORestrictedStringScalarProperty* zp = static_cast<GORestrictedStringScalarProperty*>(
            fp->findProperty(GO_Z_LOCATION_PROPERTY_NAME_STR, false));
        if (!zp) {
            return 0.;
        }
        if (zp->isEqual(GO_PROPERTY_VALUE_ZMIN_STR)) {
            if (!axis) {
                return 0.;
            }
            std::vector<double> zLim = axis->findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR);
            if (zLim.size() < 2) {
                return 0.;
            }
            return zLim[0];
        }
        if (zp->isEqual(GO_PROPERTY_VALUE_ZMAX_STR)) {
            if (!axis) {
                return 0.;
            }
            std::vector<double> zLim = axis->findVectorDoubleProperty(GO_Z_LIM_PROPERTY_NAME_STR);
            if (zLim.size() < 2) {
                return 0.;
            }
            return zLim[1];
        }
        return zp->scalar();
    }
    //=============================================================================
    void
    appendFillFace(FaceList& faces, const std::vector<FillPoint>& polygon,
        const std::vector<double>& color, GOAxis* axis)
    {
        if (polygon.size() < 3) {
            return;
        }
        std::vector<double> xs;
        std::vector<double> ys;
        std::vector<double> zs;
        xs.reserve(polygon.size());
        ys.reserve(polygon.size());
        zs.reserve(polygon.size());
        for (const FillPoint& p : polygon) {
            xs.push_back(p.x);
            ys.push_back(p.y);
            zs.push_back(p.z);
        }
        std::vector<double> mxs;
        std::vector<double> mys;
        std::vector<double> mzs;
        axis->reMap(xs, ys, zs, mxs, mys, mzs);

        Face face;
        face.FaceColorMode = ColorMode::ColorSpec;
        face.EdgeColorMode = ColorMode::None;
        face.FaceColor
            = RGBAColorData(color[0], color[1], color[2], color.size() > 3 ? color[3] : 1.);
        for (size_t k = 0; k < mxs.size(); k++) {
            face.vertices.push_back(point(mxs[k], mys[k], mzs[k]));
        }
        faces.push_back(face);
    }
    //=============================================================================
    void
    paintFilledContours(GraphicsObject* fp, RenderInterface& gc, GOAxis* axis, const ArrayOf& zData,
        const ArrayOf& xData, const ArrayOf& yData, const std::vector<double>& levels)
    {
        if (levels.empty()) {
            return;
        }
        GORestrictedStringColorProperty* fc = static_cast<GORestrictedStringColorProperty*>(
            fp->findProperty(GO_FACE_COLOR_PROPERTY_NAME_STR));
        if (fc->isEqual(GO_PROPERTY_VALUE_NONE_STR)) {
            return;
        }
        GOFaceAlphaProperty* fa
            = static_cast<GOFaceAlphaProperty*>(fp->findProperty(GO_FACE_ALPHA_PROPERTY_NAME_STR));
        double alpha = fa->scalar();
        if (alpha <= 0.) {
            return;
        }

        ArrayOf z(zData);
        ArrayOf x(xData);
        ArrayOf y(yData);
        z.promoteType(NLS_DOUBLE);
        x.promoteType(NLS_DOUBLE);
        y.promoteType(NLS_DOUBLE);
        const double* zp = static_cast<const double*>(z.dataPointer());
        const double* xp = static_cast<const double*>(x.dataPointer());
        const double* yp = static_cast<const double*>(y.dataPointer());
        indexType rows = z.rows();
        indexType cols = z.columns();
        double renderZ = contourPlaneZ(fp, axis);
        FaceList faces;

        auto idx = [&](indexType row, indexType col) { return row + col * rows; };
        auto addBand = [&](const std::vector<FillPoint>& polygon, double low, double high,
                           double colorLevel) {
            std::vector<FillPoint> clipped = polygon;
            if (std::isfinite(low)) {
                clipped = clipFillPolygon(clipped, low, true);
            }
            if (std::isfinite(high)) {
                clipped = clipFillPolygon(clipped, high, false);
            }
            if (clipped.size() >= 3) {
                std::vector<double> color;
                if (contourColor(fp, GO_FACE_COLOR_PROPERTY_NAME_STR, colorLevel, alpha, color)) {
                    appendFillFace(faces, clipped, color, axis);
                }
            }
        };

        auto addTriangle = [&](const FillPoint& a, const FillPoint& b, const FillPoint& c) {
            std::vector<FillPoint> triangle { a, b, c };
            if (fp->stringCheck(GO_FILL_BELOW_LEVEL_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
                addBand(triangle, -std::numeric_limits<double>::infinity(), levels.front(),
                    levels.front());
            }
            for (size_t levelIndex = 0; levelIndex + 1 < levels.size(); levelIndex++) {
                // A band takes the colour of its lower bounding level, not the midpoint.
                addBand(triangle, levels[levelIndex], levels[levelIndex + 1], levels[levelIndex]);
            }
            addBand(
                triangle, levels.back(), std::numeric_limits<double>::infinity(), levels.back());
        };

        for (indexType col = 0; col + 1 < cols; col++) {
            for (indexType row = 0; row + 1 < rows; row++) {
                double z00 = zp[idx(row, col)];
                double z10 = zp[idx(row, col + 1)];
                double z11 = zp[idx(row + 1, col + 1)];
                double z01 = zp[idx(row + 1, col)];
                if (!std::isfinite(z00) || !std::isfinite(z10) || !std::isfinite(z11)
                    || !std::isfinite(z01)) {
                    continue;
                }
                FillPoint p00 { xp[idx(row, col)], yp[idx(row, col)], renderZ, z00 };
                FillPoint p10 { xp[idx(row, col + 1)], yp[idx(row, col + 1)], renderZ, z10 };
                FillPoint p11 { xp[idx(row + 1, col + 1)], yp[idx(row + 1, col + 1)], renderZ,
                    z11 };
                FillPoint p01 { xp[idx(row + 1, col)], yp[idx(row + 1, col)], renderZ, z01 };
                addTriangle(p00, p10, p11);
                addTriangle(p00, p11, p01);
            }
        }

        if (!faces.empty()) {
            PatchRenderOptions options;
            options.lineWidth = 0.;
            options.lineStyle = GO_PROPERTY_VALUE_NONE_STR;
            gc.drawPatch(faces, options);
        }
    }
    //=============================================================================
    std::wstring
    formatContourLabel(GraphicsObject* fp, double level)
    {
        auto defaultLabel = [](double value) {
            char buffer[64];
            std::snprintf(buffer, sizeof(buffer), "%g", value);
            return utf8_to_wstring(buffer);
        };
        ArrayOf labelFormat(fp->findArrayOfProperty(GO_LABEL_FORMAT_PROPERTY_NAME_STR));
        if (labelFormat.isFunctionHandle()) {
            try {
                function_handle fh = labelFormat.asFunctionHandle();
                FunctionDef* funcDef = reinterpret_cast<FunctionDef*>(fh.anonymousHandle);
                auto* eval = static_cast<Evaluator*>(
                    NelsonConfiguration::getInstance()->getMainEvaluator());
                if (funcDef && eval) {
                    ArrayOfVector inputs;
                    inputs << ArrayOf::doubleScalar(level);
                    ArrayOfVector outputs = funcDef->evaluateFunction(eval, inputs, 1);
                    if (!outputs.empty()) {
                        return outputs[0].asWideString();
                    }
                }
            } catch (...) {
                return defaultLabel(level);
            }
        }
        if (labelFormat.isRowVectorCharacterArray()
            || (labelFormat.isStringArray() && labelFormat.isScalar())) {
            std::wstring wideFormat = labelFormat.asWideString();
            if (wideFormat.empty() || wideFormat == L"@string") {
                wideFormat = L"%g";
            }
            if (wideFormat.find(L'%') != std::wstring::npos) {
                std::string fmtString = wstring_to_utf8(wideFormat);
                char buffer[256];
                int count = std::snprintf(buffer, sizeof(buffer), fmtString.c_str(), level);
                if (count > 0) {
                    return utf8_to_wstring(buffer);
                }
            }
            return wideFormat;
        }
        return defaultLabel(level);
    }
    //=============================================================================
    bool
    shouldLabelLevel(GraphicsObject* fp, double level, const std::vector<double>& allLevels)
    {
        auto isClose = [](double a, double b) {
            return std::abs(a - b) < 100 * std::numeric_limits<double>::epsilon();
        };
        if (fp->stringCheck(GO_TEXT_LIST_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
            std::vector<double> textList
                = fp->findVectorDoubleProperty(GO_TEXT_LIST_PROPERTY_NAME_STR);
            for (double value : textList) {
                if (isClose(value, level)) {
                    return true;
                }
            }
            return false;
        }
        if (fp->stringCheck(GO_TEXT_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)) {
            double step = fp->findScalarDoubleProperty(GO_TEXT_STEP_PROPERTY_NAME_STR);
            if (step > 0. && !allLevels.empty()) {
                double scaled = (level - allLevels.front()) / step;
                return isClose(scaled, std::round(scaled));
            }
        }
        return true;
    }
    //=============================================================================
    // Fold the tangent into the half plane a reader can follow: a line walked
    // leftwards has a tangent near 180 degrees and would carry its text upside
    // down. The two quarter turns are kept, a vertical line is never folded.
    double
    uprightLabelRotation(double rotation)
    {
        if (rotation > 90.) {
            return rotation - 180.;
        }
        if (rotation < -90.) {
            return rotation + 180.;
        }
        return rotation;
    }
    //=============================================================================
    void
    paintContourLabels(GraphicsObject* fp, RenderInterface& gc, GOAxis* axis,
        const contourLineCollection& contourLines, const std::vector<double>& levels)
    {
        if (!fp->stringCheck(GO_SHOW_TEXT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
            return;
        }
        if (contourLines.empty() || levels.empty()) {
            return;
        }
        std::vector<double> labelColor;
        double labelSpacing = fp->findScalarDoubleProperty(GO_LABEL_SPACING_PROPERTY_NAME_STR);
        if (labelSpacing <= 0.) {
            labelSpacing = 144.;
        }
        FontSpec font;
        font.family = L"helvetica";
        font.pointSizeF = 11;
        bool floating = fp->stringCheck(GO_FLOATING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
        double planeZ = contourPlaneZ(fp, axis);
        std::vector<LabelPlacement> placements;
        std::vector<double> xLim = axis->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        std::vector<double> yLim = axis->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        double frameMinX = 0.;
        double frameMaxX = 0.;
        double frameMinY = 0.;
        double frameMaxY = 0.;
        bool frameInitialized = false;

        auto contourPointToPixels
            = [&](const contourPoint& point, double z, double& px, double& py) {
                  std::vector<double> xs { point.x };
                  std::vector<double> ys { point.y };
                  std::vector<double> zs { z };
                  std::vector<double> mxs;
                  std::vector<double> mys;
                  std::vector<double> mzs;
                  axis->reMap(xs, ys, zs, mxs, mys, mzs);
                  gc.toPixels(mxs[0], mys[0], mzs[0], px, py);
              };
        auto updateFramePoint = [&](double x, double y) {
            double px = 0.;
            double py = 0.;
            contourPointToPixels(contourPoint(x, y), planeZ, px, py);
            if (!frameInitialized) {
                frameMinX = frameMaxX = px;
                frameMinY = frameMaxY = py;
                frameInitialized = true;
            } else {
                frameMinX = std::min(frameMinX, px);
                frameMaxX = std::max(frameMaxX, px);
                frameMinY = std::min(frameMinY, py);
                frameMaxY = std::max(frameMaxY, py);
            }
        };
        updateFramePoint(xLim[0], yLim[0]);
        updateFramePoint(xLim[0], yLim[1]);
        updateFramePoint(xLim[1], yLim[0]);
        updateFramePoint(xLim[1], yLim[1]);

        auto labelFitsFrame
            = [&](double px, double py, double rotation, const std::wstring& label) -> bool {
            if (!frameInitialized) {
                return true;
            }
            int width = 0;
            int height = 0;
            int xOffset = 0;
            int yOffset = 0;
            gc.measureText(label, font, RenderInterface::Mean, RenderInterface::Mean, width, height,
                xOffset, yOffset);
            double halfWidth = 0.5 * width;
            double halfHeight = 0.5 * height;
            double angle = rotation * M_PI / 180.;
            double xRadius
                = std::abs(std::cos(angle)) * halfWidth + std::abs(std::sin(angle)) * halfHeight;
            double yRadius
                = std::abs(std::sin(angle)) * halfWidth + std::abs(std::cos(angle)) * halfHeight;
            double margin = 4.;
            return (bool)((px - xRadius >= frameMinX + margin)
                && (px + xRadius <= frameMaxX - margin) && (py - yRadius >= frameMinY + margin)
                && (py + yRadius <= frameMaxY - margin));
        };

        for (size_t levelIndex = 0; levelIndex < contourLines.size(); levelIndex++) {
            double level = levels[levelIndex];
            if (!shouldLabelLevel(fp, level, levels)) {
                continue;
            }
            if (!contourColor(fp, GO_LABEL_COLOR_PROPERTY_NAME_STR, level, 1., labelColor)) {
                continue;
            }
            std::wstring label = formatContourLabel(fp, level);
            for (const contourLine& line : contourLines[levelIndex]) {
                if (line.size() < 2) {
                    continue;
                }
                double renderZ = floating ? level : planeZ;
                std::vector<double> pxs(line.size(), 0.);
                std::vector<double> pys(line.size(), 0.);
                std::vector<double> cumulative(line.size(), 0.);
                for (size_t k = 1; k < line.size(); k++) {
                    if (k == 1) {
                        contourPointToPixels(line[0], renderZ, pxs[0], pys[0]);
                    }
                    contourPointToPixels(line[k], renderZ, pxs[k], pys[k]);
                    double dx = pxs[k] - pxs[k - 1];
                    double dy = pys[k] - pys[k - 1];
                    double segmentLength = std::sqrt(dx * dx + dy * dy);
                    cumulative[k] = cumulative[k - 1] + segmentLength;
                }
                double totalLength = cumulative.back();
                if (totalLength < labelSpacing) {
                    continue;
                }
                double nextTarget = std::min(labelSpacing, totalLength / 2.);
                for (size_t k = 1; k < line.size(); k++) {
                    if (cumulative[k] < nextTarget) {
                        continue;
                    }
                    double segmentLength = cumulative[k] - cumulative[k - 1];
                    if (segmentLength <= 0.) {
                        continue;
                    }
                    double t = (nextTarget - cumulative[k - 1]) / segmentLength;
                    t = std::min(1., std::max(0., t));
                    double px = pxs[k - 1] + t * (pxs[k] - pxs[k - 1]);
                    double py = pys[k - 1] + t * (pys[k] - pys[k - 1]);
                    double rotation = uprightLabelRotation(
                        std::atan2(pys[k] - pys[k - 1], pxs[k] - pxs[k - 1]) * 180. / M_PI);
                    if (labelFitsFrame(px, py, rotation, label)) {
                        placements.push_back(
                            LabelPlacement { px, py, rotation, label, labelColor });
                    }
                    nextTarget += labelSpacing;
                }
            }
        }
        if (!placements.empty()) {
            gc.setupDirectDraw();
            for (const LabelPlacement& placement : placements) {
                gc.putText(placement.x, placement.y, placement.label, placement.color,
                    RenderInterface::Mean, RenderInterface::Mean, font, placement.rotation);
            }
            gc.releaseDirectDraw();
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
void
GOContour::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    GOAxis* parent = (GOAxis*)getParentAxis();
    if (!parent) {
        return;
    }
    ArrayOf zData(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (zData.isNumeric() && !zData.isEmpty()) {
        zData.promoteType(NLS_DOUBLE);
        ArrayOf xInput = stringCheck(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
            ? findArrayOfProperty(GO_X_DATA_PROPERTY_NAME_STR)
            : ArrayOf::empty();
        ArrayOf yInput = stringCheck(GO_Y_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR)
            ? findArrayOfProperty(GO_Y_DATA_PROPERTY_NAME_STR)
            : ArrayOf::empty();
        ArrayOf xData(contourCoordinateData(zData, xInput, true));
        ArrayOf yData(contourCoordinateData(zData, yInput, false));
        paintFilledContours(this, gc, parent, zData, xData, yData, zValues);
    }
    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    GORestrictedStringColorProperty* lc
        = (GORestrictedStringColorProperty*)findProperty(GO_EDGE_COLOR_PROPERTY_NAME_STR);
    bool floatflag = stringCheck(GO_FLOATING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    double planeZ = contourPlaneZ(this, parent);
    if (!stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
        gc.lineWidth(width);
        GOAxis* parent = (GOAxis*)getParentAxis();
        if (!parent) {
            return;
        }
        std::vector<double> xLim = parent->findVectorDoubleProperty(GO_X_LIM_PROPERTY_NAME_STR);
        std::vector<double> yLim = parent->findVectorDoubleProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        if (xLim.size() < 2 || yLim.size() < 2) {
            return;
        }

        double edgeAlpha = findScalarDoubleProperty(GO_EDGE_ALPHA_PROPERTY_NAME_STR);
        for (int i = (int)contourLines.size() - 1; i >= 0; i--) {
            contourLineSet cset(contourLines[i]);
            if (stringCheck(GO_EDGE_COLOR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FLAT_STR)) {
                selectColorForRendering(gc, zValues[i], edgeAlpha);
            } else {
                std::vector<double> color = lc->colorSpec();
                color.push_back(edgeAlpha);
                gc.color(color);
            }
            for (int j = 0; j < cset.size(); j++) {
                contourLine aline(cset[j]);
                std::vector<double> xs, ys, zs;
                for (int k = 0; k < aline.size(); k++) {
                    xs.push_back(aline[k].x);
                    ys.push_back(aline[k].y);
                    if (floatflag) {
                        zs.push_back(zValues[i]);
                    } else {
                        zs.push_back(planeZ);
                    }
                }
                std::vector<double> mxs, mys, mzs;
                parent->reMap(xs, ys, zs, mxs, mys, mzs);

                double axisMinX = xLim[0];
                double axisMaxX = xLim[1];
                double axisMinY = yLim[0];
                double axisMaxY = yLim[1];

                for (size_t i = 0; i < mxs.size(); ++i) {
                    mxs[i] = std::min(std::max(mxs[i], axisMinX), axisMaxX);
                    mys[i] = std::min(std::max(mys[i], axisMinY), axisMaxY);
                }

                gc.lineSeries(mxs, mys, mzs);
            }
        }
    }
    paintContourLabels(this, gc, parent, contourLines, zValues);
}
//=============================================================================
std::vector<double>
GOContour::getLimits()
{
    updateState();

    double xMin = 0.;
    double xMax = 0.;
    double yMin = 0.;
    double yMax = 0.;
    double zMin = 0.;
    double zMax = 0.;
    double cMin = 0.;
    double cMax = 0.;

    bool xInitialized = false;
    bool yInitialized = false;
    bool zInitialized = false;

    if (!zValues.empty()) {
        int i = 0;
        double firstZValue = zValues[0];
        for (const auto& contourLineSet : contourLines) {
            double zValue = zValues[i++];
            for (const auto& aline : contourLineSet) {
                for (const auto& point : aline) {
                    if (!xInitialized && std::isfinite(point.x)) {
                        xMin = xMax = point.x;
                        xInitialized = true;
                    }
                    if (!yInitialized && std::isfinite(point.y)) {
                        yMin = yMax = point.y;
                        yInitialized = true;
                    }
                    if (!zInitialized) {
                        zMin = zMax = firstZValue;
                        zInitialized = true;
                    }
                    if (xInitialized && std::isfinite(point.x)) {
                        xMin = std::min(xMin, point.x);
                        xMax = std::max(xMax, point.x);
                    }
                    if (yInitialized && std::isfinite(point.y)) {
                        yMin = std::min(yMin, point.y);
                        yMax = std::max(yMax, point.y);
                    }
                    if (zInitialized) {
                        zMin = std::min(zMin, zValue);
                        zMax = std::max(zMax, zValue);
                    }
                }
            }
        }
    }

    if (!xInitialized) {
        xMin = -1;
        xMax = 1;
    }
    if (!yInitialized) {
        yMin = -1;
        yMax = 1;
    }
    if (!zInitialized) {
        zMin = 0;
        zMax = 0;
    }

    cMin = zMin;
    cMax = zMax;
    // A single contour level collapses the colour range to cMin == cMax. Expanding
    // it to [level - 1, level + 1] maps the level to the middle of the colormap.
    // A degenerate range would be rejected by the axis and push the line to the
    // colormap extreme. Applied to filled and unfilled contours alike.
    if (cMin == cMax) {
        cMin -= 1.;
        cMax += 1.;
    }

    if (stringCheck(GO_FLOATING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        zMin = 0.;
        zMax = 0.;
    }

    return { xMin, xMax, yMin, yMax, zMin, zMax, cMin, cMax, 0, 0 };
}
//=============================================================================
void
GOContour::rebuildContourMatrix()
{
    ArrayOf out = buildContourMatrix(contourLines, zValues);
    GOArrayOfProperty* hp = (GOArrayOfProperty*)findProperty(GO_CONTOUR_MATRIX_PROPERTY_NAME_STR);
    hp->data(out);
}
//=============================================================================
ArrayOf
GOContour::generateDataArray(const std::wstring& name, bool isXcoord)
{
    ArrayOf zData(findArrayOfProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    indexType zRows = zData.rows();
    indexType zCols = zData.columns();

    if (stringCheck(name + L"Mode", GO_PROPERTY_VALUE_MANUAL_STR)) {
        ArrayOf cData(findArrayOfProperty(name));

        if (cData.isVector()
            && ((isXcoord && (cData.elementCount() == zCols))
                || (!isXcoord && (cData.elementCount() == zRows)))) {
            cData.promoteType(NLS_DOUBLE);
            const double* qp = static_cast<const double*>(cData.dataPointer());
            double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, zRows * zCols));
            ArrayOf mat(NLS_DOUBLE, Dimensions(zRows, zCols), dp);

            for (int i = 0; i < zCols; i++) {
                for (int j = 0; j < zRows; j++) {
                    *dp++ = isXcoord ? qp[i] : qp[j];
                }
            }
            return mat;
        } else if (cData.is2D() && (cData.rows() == zRows) && (cData.columns() == zCols)) {
            return cData;
        }
    }

    double* dp = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, zRows * zCols));
    ArrayOf mat(NLS_DOUBLE, Dimensions(zRows, zCols), dp);
    for (int i = 0; i < zCols; i++) {
        for (int j = 0; j < zRows; j++) {
            *dp++ = isXcoord ? i + 1 : j + 1;
        }
    }
    return mat;
}
//=============================================================================
void
GOContour::selectColorForRendering(RenderInterface& gc, double zValue, double edgeAlpha)
{
    GOAxis* axis = getParentAxis();
    GOFigure* figure = getParentFigure();
    if (!axis || !figure) {
        gc.color(std::vector<double> { 0.0, 0.0, 0.0, edgeAlpha });
        return;
    }

    std::vector<double> colorMap(figure->findVectorDoubleProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
    std::vector<double> cLim(axis->findVectorDoubleProperty(GO_C_LIM_PROPERTY_NAME_STR));
    if (cLim.size() < 2 || colorMap.size() < 3) {
        gc.color(std::vector<double> { 0.0, 0.0, 0.0, edgeAlpha });
        return;
    }

    double cLimMin = std::min(cLim[0], cLim[1]);
    double cLimMax = std::max(cLim[0], cLim[1]);
    if (!(cLimMax > cLimMin)) {
        std::vector<double> color = { colorMap[0], colorMap[1], colorMap[2], edgeAlpha };
        gc.color(color);
        return;
    }

    int colorMapLength = static_cast<int>(colorMap.size() / 3);
    if (colorMapLength <= 0) {
        gc.color(std::vector<double> { 0.0, 0.0, 0.0, edgeAlpha });
        return;
    }

    int idx = static_cast<int>((zValue - cLimMin) / (cLimMax - cLimMin) * (colorMapLength - 1));
    idx = std::min(colorMapLength - 1, std::max(0, idx));

    std::vector<double> color
        = { colorMap[3 * idx], colorMap[3 * idx + 1], colorMap[3 * idx + 2], edgeAlpha };

    gc.color(color);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
