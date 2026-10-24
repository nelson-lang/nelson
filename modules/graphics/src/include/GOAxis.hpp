//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
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
    // Saved per object, so recycled graphics handles cannot inherit an old view.
    ArrayOf
    navigationReferenceView(bool replace = false);
    //=============================================================================
    ~GOAxis() override;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    wstringVector
    getVisibleFieldnames() override;
    //=============================================================================
    bool
    isPolarAxes();
    //=============================================================================
    static bool
    isPolarAxesOnlyProperty(const std::wstring& name);
    //=============================================================================
    static bool
    isCartesianAxesOnlyProperty(const std::wstring& name);
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    // Attaching a child only flags the axes dirty. A full updateState() rescans every
    // sibling, which made creating N objects O(N^2). The recompute runs at the next
    // draw or computed-property read.
    void
    markStateDirty();
    //=============================================================================
    // No-op unless the axes is dirty. Called from the draw path and from computed-property
    // reads.
    void
    updateStateIfDirty();
    //=============================================================================
    bool
    isStateDirty() const
    {
        return m_stateDirty;
    }
    //=============================================================================
    // Materialises Title/Subtitle/X-Y-ZLabel as real text objects if absent, so
    // ax.Title.String = 'x' works without title() having run first.
    void
    ensureTextSubObjects();
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    void
    setupProjectionOnly(RenderInterface& gc, bool updateLimits = true);
    //=============================================================================
    void
    rePackTitleArea();
    //=============================================================================
    void
    syncTitleTextLayout();
    //=============================================================================
    std::vector<double>
    mapCamera(const std::vector<double>& point);
    //=============================================================================
    std::vector<double>
    reMap(const std::vector<double>&, bool forceLinearScale);
    //=============================================================================
    void
    reMap(const std::vector<double>&, const std::vector<double>&, const std::vector<double>&,
        std::vector<double>&, std::vector<double>&, std::vector<double>&);
    //=============================================================================
    bool
    is2D();
    //=============================================================================
    bool
    renderedAs2D();
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
    updateInteractionState();
    //=============================================================================
    bool
    updateViewState();
    //=============================================================================
    void
    invalidateViewState();
    //=============================================================================
    void
    markViewStateReady();
    //=============================================================================
    void
    zoom(double scaleFactor);
    //=============================================================================
    void
    pan(double stepX, double stepY);
    //=============================================================================
    void
    resetView();
    //=============================================================================
    GOFigure*
    getParentFigure();
    //=============================================================================
    void
    zoomXMode();
    //=============================================================================
    void
    zoomYMode();
    //=============================================================================
    void
    zoomOn();
    //=============================================================================
    void
    zoomOff();
    //=============================================================================
    void
    zoomOut();
    //=============================================================================
    void
    zoomReset();
    //=============================================================================
    void
    panXMode();
    //=============================================================================
    void
    panYMode();
    //=============================================================================
    void
    panOn();
    //=============================================================================
    void
    panOff();
    //=============================================================================
    void
    ensureToolbar(int64 axisHandle);
    //=============================================================================
    void
    restoreToolbar(int64 axisHandle);
    //=============================================================================
    bool
    isDualYAxis();
    //=============================================================================
    // Seeds the inactive side's stashed state and colours both rulers from the first two
    // ColorOrder entries.
    void
    enableDualYAxis();
    //=============================================================================
    // Swaps the canonical Y state with the stashed side when 'side' is not already active.
    void
    setActiveYSide(const std::wstring& side);
    //=============================================================================
    void
    disableDualYAxis();
    //=============================================================================
    void
    applyRulerSideProperty(
        int axisKind, int side, const std::wstring& rulerProperty, const ArrayOf& value);
    //=============================================================================
    // Camera view angle rule shared by the automatic angle, the orthographic zoom and the
    // perspective frustum: the projected data box fills the viewport along its relatively
    // larger extent, and the angle is the one that extent subtends at the camera.
    static bool
    viewAngleSpansWidth(double xSpan, double ySpan, double width, double height);
    //=============================================================================
    static double
    cameraViewAngleForSpan(double span, double distance, double fallback);
    //=============================================================================
private:
    ArrayOf m_navigationReferenceView;
    //=============================================================================
    void
    ensureYAxisRulers();
    //=============================================================================
    void
    syncYAxisRulers();
    //=============================================================================
    void
    ensureSingleAxisRuler(const wchar_t* propName, int axisKind);
    //=============================================================================
    void
    syncSingleAxisRuler(const wchar_t* propName, int axisKind);
    //=============================================================================
    // The value kind ('numeric' | 'datetime') is stored per axis and pushed to the ruler
    // so its Type survives updateState re-runs.
    void
    propagateRulerValueKind(class GORuler* ruler, int axisKind);
    //=============================================================================
    std::wstring
    axesPropertyForRulerProperty(int axisKind, int side, const std::wstring& rulerProperty);
    //=============================================================================
    std::vector<double> m_box;
    int64 m_toolbarHandle = 0;
    //=============================================================================
    void
    loadParulaColorMap();
    //=============================================================================
    double x1pos[3], x2pos[3];
    double y1pos[3], y2pos[3];
    double z1pos[3], z2pos[3];
    bool xvisible = true, yvisible = true, zvisible = true;
    unsigned m_viewStateWarmupCount = 0;
    bool m_stateDirty = false;
    // Height taken from the axes by rePackTitleArea, as a fraction of the parent
    // height, so the next call can give it back before measuring the title again.
    double m_titleRepackShrink = 0.0;
    FontSpec m_font;
    GenericTable<std::wstring, GOGenericProperty*> properties;
    //=============================================================================
    void
    updateAxisFont();
    //=============================================================================
    void
    handlePlotBoxFlags();
    //=============================================================================
    void
    maxTickMetric(
        RenderInterface& gc, const std::vector<std::wstring>& labs, double& maxx, double& maxy);
    //=============================================================================
    void
    drawLabel(RenderInterface& gc, double dx, double dy, double x2, double y2,
        const std::vector<double>& color, const std::wstring& txt, bool manageDirectDraw = true,
        double rotation = 0.0);
    //=============================================================================
    void
    applyCustomTickLabelFormat(const std::wstring& formatPropertyName,
        const std::vector<double>& tickValues, std::vector<std::wstring>& labels);
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
    double
    getAxisPixelLength(
        RenderInterface& gc, double x1, double y1, double z1, double x2, double y2, double z2);
    //=============================================================================
    void
    getAxisPixelDelta(RenderInterface& gc, double x1, double y1, double z1, double x2, double y2,
        double z2, double& length, double& angleDeg);
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
    rePackFigure(bool force = false);
    //=============================================================================
    void
    updateLimits(bool x, bool y, bool z, bool a, bool c);
    //=============================================================================
    void
    computeAxisLimits(const std::wstring& axisLimitPropertyName, indexType indexMin,
        indexType indexMax, double& limMin, double& limMax);
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
    setupProjection(RenderInterface& gc, bool updateLimits = true);
    void
    setupOrthographicProjection(RenderInterface& gc, const std::vector<double>& scale,
        const std::vector<double>& position, std::vector<double> bounds, double defaultViewAngle,
        bool angleSpansWidth);
    //=============================================================================
    // Returns false when the scene is degenerate for perspective, so the caller falls
    // back to the orthographic projection. zMappedMin/zMappedMax are the data-limit
    // depth bounds in view space; angleSpansWidth says which viewport dimension the
    // camera view angle spans.
    bool
    setupPerspectiveProjection(RenderInterface& gc, double zMappedMin, double zMappedMax,
        bool angleSpansWidth, const std::vector<double>& position);
    //=============================================================================
    void
    drawBox(RenderInterface& gc);
    //=============================================================================
    void
    drawPolarBackground(RenderInterface& gc);
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
        double unity, double unitz, const std::vector<double>& maptics,
        const std::vector<double>& minortics, const std::vector<std::wstring>& labels,
        const std::wstring& labelname, int ticlen, double ticdir);
    //=============================================================================
    void
    drawAxisLabels(RenderInterface& gc);
    //=============================================================================
    void
    drawChildren(RenderInterface& gc);
    //=============================================================================
    // Clip cuboid in mapped coordinates, disabled when Clipping is 'off'.
    RenderClipBox
    computeChildrenClipBox();
    //=============================================================================
    // Used to switch the active side, and transiently to draw the inactive ruler through
    // the single-sided path.
    void
    swapSecondaryYState();
    //=============================================================================
    void
    updateSecondaryYLimits();
    //=============================================================================
    void
    drawSecondaryYRuler(RenderInterface& gc);
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
    void
    updateUnits();
    //=============================================================================
    bool zoomXOn = true;
    bool zoomYOn = true;
    //=============================================================================
    std::vector<double> zoomXStore;
    std::vector<double> zoomYStore;
    //=============================================================================
    bool panXOn = true;
    bool panYOn = true;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
