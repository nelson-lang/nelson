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
#include "GOUIContainer.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// uigridlayout: grid layout container.
// RowHeight / ColumnWidth cells are fixed pixels, 'fit' or weighted 'Nx'. Children are placed
// through their Layout.Row / Layout.Column options (auto-placement left to right then top to
// bottom).
class NLSGRAPHICS_IMPEXP GOUIGridLayout : public GOUIContainer
{
public:
    //=============================================================================
    GOUIGridLayout();
    ~GOUIGridLayout() override = default;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    // recompute the whole grid geometry and reposition children for the given
    // grid size in pixels (the widget binding passes the real widget size; the
    // headless path passes the parent's Qt-free size)
    void
    performLayout(double gridW, double gridH);
    //=============================================================================
    // headless (WEB_GUI) layout: resolve the grid size Qt-free from the parent
    // container Position (or the figure render size) and run the layout. Does
    // nothing when a widget backend drives the layout instead.
    void
    performLayout();
    //=============================================================================
    // headless (WEB_GUI): no widget binding is ever created, but the layout
    // must still run to write the children's cell Positions.
    void
    updateState() override;
    //=============================================================================
    bool
    managesOwnGeometry() override
    {
        return true;
    }
    //=============================================================================
    void
    onGeometryChanged() override;
    //=============================================================================
private:
    //=============================================================================
    void
    constructProperties();
    void
    setupDefaults();
    //=============================================================================
    struct TrackSize
    {
        enum class Kind
        {
            Fixed,
            Fit,
            Weight
        } kind
            = Kind::Weight;
        double value = 1.0;
    };
    std::vector<TrackSize>
    parseTrackSizes(const std::wstring& propertyName);
    //=============================================================================
    std::vector<double>
    computeTrackPixels(const std::vector<TrackSize>& tracks, double available, double spacing,
        bool isRow, size_t implicitCount);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
