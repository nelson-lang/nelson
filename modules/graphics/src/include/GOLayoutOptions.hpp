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
// Lightweight sub-object corresponding to ax.Layout in MATLAB.
// Holds the tile placement info for an axes or other object inside a
// TiledChartLayout.  Exposed to Nelson as ax.Layout.Tile and
// ax.Layout.TileSpan.
class NLSGRAPHICS_IMPEXP GOLayoutOptions : public GraphicsObject
{
public:
    GOLayoutOptions();
    ~GOLayoutOptions() override = default;

    std::wstring
    getType() override;

    void
    registerProperties() override;

    void
    updateState() override
    {
    }

    void
    paintMe(RenderInterface& /*gc*/) override
    {
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
