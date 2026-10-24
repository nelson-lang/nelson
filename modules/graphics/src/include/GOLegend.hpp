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
#include "GraphicsObject.hpp"
#include "Types.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOLegend : public GraphicsObject
{
private:
    int64 m_peerAxesHandle = 0;
    int64 m_titleHandle = 0;
    int64 m_layoutHandle = 0;
    std::vector<int64> m_plotHandles;
    std::vector<int64> m_iconHandles;
    std::vector<int64> m_textHandles;
    std::set<int64> m_seenPeerChildren;
    std::vector<double> m_peerPositionBeforeResize;
    FontSpec m_font;

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
    drawTitle(RenderInterface& gc, const std::vector<double>& pos, double& y);
    void
    drawEntries(RenderInterface& gc, const std::vector<double>& pos, double y);
    std::vector<double>
    positionAsPixels(RenderInterface& gc);
    std::vector<double>
    positionAsPixels(double width, double height);
    std::vector<std::wstring>
    visibleLabels() const;
    int
    effectiveNumColumns(size_t labelCount) const;
    double
    entryRowHeight() const;

public:
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
    applyLocationToPeerAxes();
    void
    restorePeerAxesPosition();
    bool
    hitTest(double x, double y, double width, double height);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
