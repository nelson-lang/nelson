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
//=============================================================================
namespace Nelson {
//=============================================================================
// Lightweight sub-object corresponding to ax.Layout.
// TiledChartLayout mode: exposes Layout.Tile and Layout.TileSpan.
// GridLayout mode (App Designer uigridlayout): exposes Layout.Row and
// Layout.Column (scalar or [start end] span).
enum class LayoutOptionsMode
{
    TiledChartLayout,
    GridLayout
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOLayoutOptions : public GraphicsObject
{
    LayoutOptionsMode m_mode;

public:
    GOLayoutOptions(LayoutOptionsMode mode = LayoutOptionsMode::TiledChartLayout);
    ~GOLayoutOptions() override = default;

    std::wstring
    getType() override;

    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_LAYOUT_OPTIONS_STR;
    }

    LayoutOptionsMode
    mode() const
    {
        return m_mode;
    }

    void
    registerProperties() override;

    void
    updateState() override;

    void
    paintMe(RenderInterface& /*gc*/) override
    {
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
