//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GraphicsObject.hpp"
#include "Types.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOBubbleLegend : public GraphicsObject
{
private:
    int64 m_peerAxesHandle = 0;
    int64 m_titleHandle = 0;
    int64 m_layoutHandle = 0;
    std::vector<double> m_peerPositionBeforeResize;
    FontSpec m_font;

    void
    constructProperties();
    void
    setupDefaults();
    void
    updateFont();
    std::vector<double>
    positionAsPixels(RenderInterface& gc);
    std::vector<double>
    positionAsPixels(double width, double height);
    void
    drawBackground(RenderInterface& gc, const std::vector<double>& pos);
    void
    drawTitle(RenderInterface& gc, const std::vector<double>& pos, double& y);
    void
    drawBubbles(RenderInterface& gc, const std::vector<double>& pos, double y);
    void
    applyLocationToPeerAxes();
    void
    updateAutoLayout();
    void
    deleteInternalHandles();

public:
    GOBubbleLegend();
    ~GOBubbleLegend() override;

    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_BUBBLE_LEGEND_STR;
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
    restorePeerAxesPosition();
    int64
    createTitle(int64 bubbleLegendHandle, const std::wstring& titleText);
    int64
    createLayoutOptions(int64 bubbleLegendHandle, const std::wstring& edgeTile);
    std::pair<double, double>
    preferredSizeInPixels() const;
    bool
    hitTest(double x, double y, double width, double height);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
