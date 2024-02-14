//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOLineSeries.hpp"
#include "GOList.hpp"
#include "GraphicsObject.hpp"
#include "GOAxis.hpp"
#include "GOSymbolProperty.hpp"
#include "GOLineStyleProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "MinMaxHelpers.hpp"
#include "RenderHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
GOLineSeries::getType()
{
    return GO_PROPERTY_VALUE_LINE_STR;
}
//=============================================================================
GOLineSeries::GOLineSeries()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOLineSeries::~GOLineSeries() { }
//=============================================================================
void
GOLineSeries::constructProperties()
{
    registerProperty(new GOColorProperty, GO_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOLineStyleProperty, GO_LINE_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LINE_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOSymbolProperty, GO_MARKER_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MARKER_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_X_DATA_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_X_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Y_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_Z_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOLineSeries::setupDefaults()
{
    setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 0, 0.4470, 0.7410);
    setRestrictedStringDefault(GO_LINE_STYLE_PROPERTY_NAME_STR, L"-");
    setScalarDoubleDefault(GO_LINE_WIDTH_PROPERTY_NAME_STR, 0.5);
    setRestrictedStringDefault(GO_MARKER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setThreeVectorDefault(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR, 0, 0, 0);
    setThreeVectorDefault(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR, -1, -1, -1);
    setScalarDoubleDefault(GO_MARKER_SIZE_PROPERTY_NAME_STR, 6);
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_X_DATA_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
    setTwoVectorDefault(GO_X_DATA_PROPERTY_NAME_STR, 0, 1);
    setTwoVectorDefault(GO_Y_DATA_PROPERTY_NAME_STR, 0, 1);
}
//=============================================================================
void
GOLineSeries::updateState()
{
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (isAuto(GO_X_DATA_MODE_PROPERTY_NAME_STR)) {
        xs.clear();
        xs.resize(ys.size());
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)ys.size(); i++) {
            xs[i] = (i + 1.0);
        }
    }
    if (zs.size() == 0) {
        zs.resize(ys.size());
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)ys.size(); i++) {
            zs[i] = 0.0;
        }
    }
    GOVectorProperty* sp
        = static_cast<GOVectorProperty*>(findProperty(GO_X_DATA_PROPERTY_NAME_STR));
    sp->data(xs);
    sp = static_cast<GOVectorProperty*>(findProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    sp->data(zs);
}
//=============================================================================
void
GOLineSeries::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR))
        return;
    double width(findScalarDoubleProperty(GO_LINE_WIDTH_PROPERTY_NAME_STR));
    gc.lineWidth(width);
    GOColorProperty* lc = (GOColorProperty*)findProperty(GO_COLOR_PROPERTY_NAME_STR);
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    if (!((xs.size() == ys.size()) && (ys.size() == zs.size()))) {
        return;
    }
    std::vector<double> mxs, mys, mzs;
    GOAxis* parent = static_cast<GOAxis*>(getParentAxis());
    if (parent) {
        parent->reMap(xs, ys, zs, mxs, mys, mzs);
    }
    if (!lc->isNone()) {
        gc.color(lc->data());
        gc.setLineStyle(findStringProperty(GO_LINE_STYLE_PROPERTY_NAME_STR));
        int n = 0;
        while (n < mxs.size()) {
            std::vector<double> local_mxs;
            std::vector<double> local_mys;
            std::vector<double> local_mzs;
            local_mxs.reserve(mxs.size());
            local_mys.reserve(mxs.size());
            local_mzs.reserve(mxs.size());

            while ((n < mxs.size()) && std::isfinite(mxs[n]) && std::isfinite(mys[n])
                && (std::isfinite(mzs[n]))) {
                local_mxs.push_back(mxs[n]);
                local_mys.push_back(mys[n]);
                local_mzs.push_back(mzs[n]);
                n++;
            }
            gc.lineSeries(local_mxs, local_mys, local_mzs);
            while ((n < mxs.size())
                && !(std::isfinite(mxs[n]) && std::isfinite(mys[n]) && (std::isfinite(mzs[n]))))
                n++;
        }
    }
    GOColorProperty* ec
        = static_cast<GOColorProperty*>(findProperty(GO_MARKER_EDGE_COLOR_PROPERTY_NAME_STR));
    GOColorProperty* fc
        = static_cast<GOColorProperty*>(findProperty(GO_MARKER_FACE_COLOR_PROPERTY_NAME_STR));
    RenderInterface::SymbolType typ
        = StringToSymbol(findStringProperty(GO_MARKER_PROPERTY_NAME_STR));
    double sze = findScalarDoubleProperty(GO_MARKER_SIZE_PROPERTY_NAME_STR) / 2.0;
    if ((typ != RenderInterface::None) || ec->isNone() || fc->isNone()) {
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
            DrawSymbol(gc, typ, uc[i], vc[i], 0, sze, ec->data(), fc->data(), width);
        }
        gc.releaseDirectDraw();
    }
}
//=============================================================================

std::vector<double>
GOLineSeries::getLimits()
{
    std::vector<double> xs(findVectorDoubleProperty(GO_X_DATA_PROPERTY_NAME_STR));
    std::vector<double> ys(findVectorDoubleProperty(GO_Y_DATA_PROPERTY_NAME_STR));
    std::vector<double> zs(findVectorDoubleProperty(GO_Z_DATA_PROPERTY_NAME_STR));
    std::vector<double> limits;
    limits.reserve(12);
    limits.push_back(findVectorMin(xs));
    limits.push_back(findVectorMax(xs));
    limits.push_back(findVectorMin(ys));
    limits.push_back(findVectorMax(ys));
    limits.push_back(findVectorMin(zs));
    limits.push_back(findVectorMax(zs));
    limits.push_back(0);
    limits.push_back(0);
    limits.push_back(0);
    limits.push_back(0);
    return limits;
}
//=============================================================================
}
//=============================================================================
