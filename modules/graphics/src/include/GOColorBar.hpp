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
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOColorBar : public GraphicsObject
{
private:
    int64 m_peerAxesHandle = 0;
    int64 m_labelHandle = 0;
    std::vector<double> m_peerPositionBeforeResize;
    std::vector<double> m_autoLayoutPosition;
    std::wstring m_layoutAppliedLocation;
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
    updateAutoAxisLocation();
    void
    updateAutoLimits();
    void
    updateAutoTicks();
    void
    updateLabelDefaults();
    void
    updateFont();
    void
    updateLayoutTile();
    std::vector<double>
    positionAsPixels(RenderInterface& gc);
    std::vector<double>
    positionAsPixels(double width, double height);
    std::vector<double>
    currentColorMap();
    bool
    isVertical() const;
    bool
    isOutside() const;
    void
    drawRamp(RenderInterface& gc, const std::vector<double>& pos);
    void
    drawFrameAndTicks(RenderInterface& gc, const std::vector<double>& pos);
    void
    drawLabel(RenderInterface& gc, const std::vector<double>& pos);
    void
    drawSelectionHighlight(RenderInterface& gc, const std::vector<double>& pos);

public:
    GOColorBar();
    ~GOColorBar() override;

    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_COLORBAR_STR;
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
    setLabelHandle(int64 handle);
    int64
    getLabelHandle() const;
    bool
    hitTest(double x, double y, double width, double height);
    int64
    createLabel(int64 colorBarHandle);
    int64
    createLayoutOptions(int64 colorBarHandle, const std::wstring& edgeTile);
    void
    applyLocationToPeerAxes();
    void
    restorePeerAxesPosition();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
