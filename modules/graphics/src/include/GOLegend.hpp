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
#include <set>
#include "GOAxesLayoutManager.hpp"
#include "GraphicsObject.hpp"
#include "Types.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOLegend : public GraphicsObject
{
public:
    // Pixel layout of the box, shared by the size, the drawing and the hit test:
    // paddings, icon column, row pitch and title band, all in pixels.
    struct LegendLayout
    {
        double padLeft = 4.0;
        double padRight = 5.0 / 3.0;
        double iconTextGap = 16.0 / 3.0;
        double padTop = 1.75;
        double padBottom = 1.75;
        double iconWidth = 40.0;
        double rowPitch = 14.0;
        double titleBand = 0.0;
        double titleWidth = 0.0;
        double maxTextWidth = 0.0;
        double columnWidth = 0.0;
        int columns = 1;
        int rows = 0;
        double width = 0.0;
        double height = 0.0;
    };

private:
    int64 m_peerAxesHandle = 0;
    int64 m_titleHandle = 0;
    int64 m_layoutHandle = 0;
    std::vector<int64> m_plotHandles;
    std::vector<int64> m_iconHandles;
    std::vector<int64> m_textHandles;
    std::set<int64> m_seenPeerChildren;
    // Position the automatic layout last wrote: a Position flagged changed with
    // that value (figure resize) is not a user override.
    std::vector<double> m_autoLayoutPosition;
    // The peer axes repaint re-runs every legend: the layout runs once per pass.
    bool m_inAutoLayout = false;
    FontSpec m_font;
    // Metrics measured once per update: size, drawing and hit test reuse them.
    mutable bool m_metricsValid = false;
    mutable LegendLayout m_metrics;

    GraphicsObject*
    peerAxes();
    bool
    isAutoLayoutPosition(const std::vector<double>& position) const;

    void
    constructProperties();
    void
    setupDefaults();
    void
    updateModes();
    void
    updateUnits();
    void
    updateFont();
    void
    updateTitleDefaults();
    void
    updateDynamicEntries();
    void
    updateAutoLayout();
    void
    deleteInternalHandles();
    void
    drawBackground(RenderInterface& gc, const std::vector<double>& pos);
    void
    drawTitle(RenderInterface& gc, const std::vector<double>& pos, const LegendLayout& m);
    void
    drawEntries(RenderInterface& gc, const std::vector<double>& pos, const LegendLayout& m);
    std::vector<double>
    positionAsPixels(RenderInterface& gc);
    std::vector<double>
    positionAsPixels(double width, double height);
    std::vector<std::wstring>
    visibleLabels() const;
    int
    effectiveNumColumns(size_t labelCount) const;
    // True when every entry is a marker without a line: the icon column then
    // shrinks to the marker (markerPixels is the widest marker).
    bool
    markerOnlyEntries(double& markerPixels) const;

public:
    LegendLayout
    layoutMetrics() const;

    GOLegend();
    ~GOLegend() override;

    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_LEGEND_STR;
    }
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;

    int64
    getPeerAxesHandle() const;
    void
    setPeerAxesHandle(int64 handle);
    void
    setPlotHandles(const std::vector<int64>& handles);
    std::vector<int64>
    getPlotHandles() const;
    void
    setLabels(const std::vector<std::wstring>& labels);
    int64
    createTitle(int64 legendHandle);
    int64
    createLayoutOptions(int64 legendHandle, const std::wstring& edgeTile);
    int64
    getTitleHandle() const;
    int64
    getLayoutHandle() const;
    std::vector<int64>
    getIconHandles() const;
    std::vector<int64>
    getTextHandles() const;
    std::pair<double, double>
    preferredSizeInPixels() const;
    void
    restorePeerAxesPosition();
    bool
    hitTest(double x, double y, double width, double height);
    // Resolves a pixel to the legend entry under it (same layout as drawEntries).
    // On a hit, peerHandleOut is the entry's plotted object and regionOut is 0 for
    // the icon column, 1 for the label. (x, y, width, height) use the same pixel
    // frame as hitTest.
    bool
    hitTestEntry(
        double x, double y, double width, double height, int64& peerHandleOut, int& regionOut);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
