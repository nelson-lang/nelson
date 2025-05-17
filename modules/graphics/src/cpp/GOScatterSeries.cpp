//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
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
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOScatterSeries::getType()
{
    return GO_PROPERTY_VALUE_SCATTER_STR;
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
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_SIZE_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOScatterSeries::setupDefaults()
{
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    std::vector<double> defaultColor = { 0, 0.4470, 0.7410 };
    ArrayOf cdata = ArrayOf::doubleVectorConstructor(defaultColor);
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
}
//=============================================================================
void
GOScatterSeries::updateState()
{
    if (hasChanged(GO_C_DATA_PROPERTY_NAME_STR)) {
        toManual(GO_C_DATA_MODE_PROPERTY_NAME_STR);
        clearChanged(GO_C_DATA_PROPERTY_NAME_STR);
        cDataDirty = true;
    }

    if (hasChanged(GO_X_DATA_PROPERTY_NAME_STR) || hasChanged(GO_Y_DATA_PROPERTY_NAME_STR)
        || hasChanged(GO_Z_DATA_PROPERTY_NAME_STR)) {
        limitsDirty = true;
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
    // Check if the scatter series is visible. If not, exit early.
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR))
        return;

    // Set the line width for rendering based on the corresponding property.
    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.lineWidth(width);

    // Retrieve and process the color data array (cdata).
    ArrayOf cdata(findArrayOfProperty(GO_C_DATA_PROPERTY_NAME_STR));
    cdata.promoteType(NLS_DOUBLE); // Ensure the data type is double.
    double* ptrCData = (double*)cdata.getDataPointer();

    // Retrieve the X, Y, and Z coordinate data for the scatter series.
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));

    // If Z data is empty, initialize it to a vector of zeros with the same size as X data.
    if (zs.empty()) {
        std::vector<double> vec(xs.size(), 0.0);
        zs = vec;
    }
    if (!((xs.size() == ys.size()) && (ys.size() == zs.size()))) {
        return;
    }
    std::vector<double> mxs, mys, mzs;
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (parent) {
        parent->reMap(xs, ys, zs, mxs, mys, mzs);
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
        uc.reserve(mxs.size());
        vc.reserve(mxs.size());
        for (int i = 0; i < mxs.size(); i++) {
            double u, v;
            bool clipped;
            gc.toPixels(mxs[i], mys[i], mzs[i], u, v, clipped);
            if (!clipped) {
                uc.push_back(u);
                vc.push_back(v);
            }
        }
        gc.setupDirectDraw();

        for (int i = 0; i < uc.size(); i++) {
            double sze;
            if (sizeData.size() == 1) {
                sze = sizeData[0];
            } else if (sizeData.size() >= uc.size()) {
                sze = sizeData[i];
            } else {
                sze = 36;
            }
            std::vector<double> edgeColor = ec->colorSpec();
            std::vector<double> faceColor = fc->colorSpec();

            std::vector<double> CdataVector;
            if (cdata.isEmpty()) {
                CdataVector = { 0, 0.4470, 0.7410 };
            } else if (cdata.getElementCount() == 3) {
                CdataVector = { ptrCData[0], ptrCData[1], ptrCData[2] };
            } else {
                if (parent) {
                    GOColorVectorProperty* colorMap = static_cast<GOColorVectorProperty*>(
                        parent->findProperty(GO_COLOR_MAP_PROPERTY_NAME_STR));
                    if (colorMap) {
                        if (cDataDirty) {
                            cachedMinC
                                = *std::min_element(ptrCData, ptrCData + cdata.getElementCount());
                            cachedMaxC
                                = *std::max_element(ptrCData, ptrCData + cdata.getElementCount());
                            cDataDirty = false;
                        }
                        double minC = cachedMinC;
                        double maxC = cachedMaxC;

                        int colormapSize = (int)(colorMap->data().size() / 3);

                        int index = static_cast<int>(
                            std::round((ptrCData[i] - minC) / (maxC - minC) * (colormapSize - 1)));
                        index = std::clamp(index, 0, colormapSize - 1);

                        CdataVector = { colorMap->data()[index * 3],
                            colorMap->data()[index * 3 + 1], colorMap->data()[index * 3 + 2] };
                    }
                } else {
                    CdataVector
                        = { ptrCData[(i * 3)], ptrCData[(i * 3) + 1], ptrCData[(i * 3) + 2] };
                }
            }

            if (ec->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
                edgeColor = CdataVector;
            }
            if (fc->isEqual(GO_PROPERTY_VALUE_FLAT_STR)) {
                faceColor = CdataVector;
            }
            if (isEdgeAlphaFlat) {
                if (alphaData.empty()) {
                    edgeColor.push_back(1.0);
                } else if (alphaData.size() == 1) {
                    edgeColor.push_back(alphaData[0]);
                } else if (alphaData.size() == uc.size()) {
                    edgeColor.push_back(alphaData[i]);
                }
            } else {
                edgeColor.push_back(ea->data());
            }
            if (isFaceAlphaFlat) {
                if (alphaData.empty()) {
                    faceColor.push_back(1.0);
                } else if (alphaData.size() == 1) {
                    faceColor.push_back(alphaData[0]);
                } else if (alphaData.size() == uc.size()) {
                    faceColor.push_back(alphaData[i]);
                }
            } else {
                faceColor.push_back(fa->data());
            }
            DrawSymbol(gc, typ, uc[i], vc[i], 0, std::sqrt(sze), edgeColor, faceColor, width);
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
    limits.push_back(0);
    limits.push_back(0);
    limits.push_back(0);
    limits.push_back(0);
    limitsDirty = false;
    return limits;
}
//=============================================================================
}
//=============================================================================
