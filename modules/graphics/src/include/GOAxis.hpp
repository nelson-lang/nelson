//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include "nlsGraphics_exports.h"
#include "ArrayOf.hpp"
#include "GenericTable.hpp"
#include "GraphicsObject.hpp"
#include "GOFigure.hpp"
#include "RenderInterface.hpp"
#include "GOTextInterpreterProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOAxis : public GraphicsObject
{
public:
    //=============================================================================
    GOAxis();
    //=============================================================================
    ~GOAxis() override;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    std::vector<double>
    reMap(const std::vector<double>&, bool forceLinearScale);
    //=============================================================================
    void
    reMap(std::vector<double>, std::vector<double>, std::vector<double>, std::vector<double>&,
        std::vector<double>&, std::vector<double>&);
    //=============================================================================
    bool
    is2D();
    //=============================================================================
    std::vector<double>
    getClientAreaAsPixels()
    {
        return m_box;
    }
    //=============================================================================
    std::vector<double>
    getPropertyVectorAsPixels(RenderInterface& gc, const std::wstring& name);
    //=============================================================================
    std::vector<double>
    reInterpUnits(RenderInterface& gc, std::vector<double> a);
    //=============================================================================
    void
    getView(double& azimuth, double& elevation);
    //=============================================================================
    void
    getNormalizedView(double& azimuth, double& elevation);
    //=============================================================================
    void
    setView(double azimuth, double elevation);
    //=============================================================================
    void
    rotateCamera(
        double previousMouseX, double currentMouseX, double previousMouseY, double currentMouseY);
    //=============================================================================
    void
    zoom(double scaleFactor);
    //=============================================================================
    void
    pan(double stepX, double stepY);
    //=============================================================================
private:
    //=============================================================================
    std::vector<double> m_box;
    //=============================================================================
    void
    loadParulaColorMap();
    //=============================================================================
    double x1pos[3], x2pos[3];
    double y1pos[3], y2pos[3];
    double z1pos[3], z2pos[3];
    bool xvisible, yvisible, zvisible;
    QFont m_font;
    GenericTable<std::wstring, GOGenericProperty*> properties;
    //=============================================================================
    void
    updateAxisFont();
    //=============================================================================
    void
    handlePlotBoxFlags();
    //=============================================================================
    void
    getMaxTickMetric(
        RenderInterface& gc, const std::vector<std::wstring>& labs, double& maxx, double& maxy);
    //=============================================================================
    void
    drawLabel(RenderInterface& gc, double dx, double dy, double x2, double y2,
        const std::vector<double>& color, const std::wstring& txt);
    //=============================================================================
    void
    setupAxis(RenderInterface& gc);
    //=============================================================================
    void
    recalculateTicks();
    //=============================================================================
    int
    getTickCount(
        RenderInterface& gc, double x1, double y1, double z1, double x2, double y2, double z2);
    //=============================================================================
    void
    drawXGridLine(RenderInterface& gc, double t, std::vector<double> limits);
    //=============================================================================
    void
    drawYGridLine(RenderInterface& gc, double t, std::vector<double> limits);
    //=============================================================================
    void
    drawZGridLine(RenderInterface& gc, double t, const std::vector<double>& limits);
    //=============================================================================
    double
    flipX(double t);
    //=============================================================================
    double
    flipY(double t);
    //=============================================================================
    double
    flipZ(double t);
    //=============================================================================
    void
    rePackFigure();
    //=============================================================================
    void
    updateLimits(bool x, bool y, bool z, bool a, bool c);
    //=============================================================================
    void
    computeAxisLimits(const std::wstring& axisLimitPropertyName, indexType indexMin,
        indexType indexMax, double& limMin, double& limMax);
    //=============================================================================
    GOFigure*
    getParentFigure();
    //=============================================================================
    std::vector<double>
    unitsReinterpret(std::vector<double> a);
    //=============================================================================
    std::vector<double>
    getPropertyVectorAsPixels(const std::wstring& name);
    //=============================================================================
    std::vector<double>
    getAxisLimits();
    //=============================================================================
    void setAxisLimits(std::vector<double>);
    //=============================================================================
    double
    mapX(double x, bool forceLinearScale = false);
    //=============================================================================
    double
    mapY(double y, bool forceLinearScale = false);
    //=============================================================================
    double
    mapZ(double z, bool forceLinearScale = false);
    //=============================================================================
    void
    setupProjection(RenderInterface& gc);
    //=============================================================================
    void
    drawBox(RenderInterface& gc);
    //=============================================================================
    void
    drawMinorGridLines(RenderInterface& gc);
    //=============================================================================
    void
    drawGridLines(RenderInterface& gc);
    //=============================================================================
    void
    drawAxisLines(RenderInterface& gc);
    //=============================================================================
    void
    drawTickMarks(RenderInterface& gc);
    //=============================================================================
    void
    drawTickLabels(RenderInterface& gc, const std::vector<double>& color, double px1, double py1,
        double pz1, double px2, double py2, double pz2, double limmin, double limmax, double unitx,
        double unity, double unitz, std::vector<double> maptics, std::vector<double> minortics,
        std::vector<std::wstring> labels, const std::wstring& labelname, int ticlen, double ticdir);
    //=============================================================================
    void
    drawAxisLabels(RenderInterface& gc);
    //=============================================================================
    void
    drawChildren(RenderInterface& gc);
    //=============================================================================
    virtual void
    constructProperties();
    //=============================================================================
    void
    setupDefaults();
    //=============================================================================
    void
    updateCamera();
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
