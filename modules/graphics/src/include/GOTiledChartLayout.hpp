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
#include <vector>
#include <unordered_map>
#include <utility>
#include <string>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOTiledChartLayout : public GraphicsObject
{
public:
    // Tile-span descriptor for each child object.
    struct TileSpanInfo
    {
        int tile; // upper-left tile (1-based, row-major by default)
        int spanRows;
        int spanCols;
    };

    GOTiledChartLayout();
    ~GOTiledChartLayout() override;

    std::wstring
    getType() override;

    void
    registerProperties() override;

    void
    updateState() override;

    void
    paintMe(RenderInterface& gc) override;

    //--- Grid helpers ---------------------------------------------------------
    // Translate 1-based tile index → (row, col), respecting TileIndexing.
    std::pair<int, int>
    tileIndexToRowCol(int tileIndex);

    // Translate (row, col) → 1-based tile index.
    int
    rowColToTileIndex(int row, int col);

    // Return the next empty tile index for a spanRows×spanCols block.
    int
    findNextEmptyTile(int spanRows = 1, int spanCols = 1);

    //--- Position update ------------------------------------------------------
    // Recompute OuterPosition for every child axes from the current grid
    // settings and each child's TileSpanInfo.
    void
    updateChildPositions();

    // For "flow": pick the best grid size given current child count.
    void
    reflowTiles();

    // Update the grid from user code.  This is only valid while the
    // layout is empty and forces TileArrangement to "fixed".
    void
    setGridSizeFromUser(int rows, int cols);

    // Internal grid/arrangement setters used by tiledlayout/nexttile.
    void
    setGridSizeInternal(int rows, int cols, bool fireCallback = true);

    void
    setTileArrangementInternal(const std::wstring& arrangement);

    void
    initializeTextObjects(go_handle layoutHandle);

    bool
    hasTileChildren() const;

    //--- Tile occupancy management --------------------------------------------
    // Register a child at the given tile/span (replaces any previous entry).
    void
    registerChildTile(go_handle childHandle, int tile, int spanRows, int spanCols);

    // Remove a child from the occupancy maps.
    void
    unregisterChild(go_handle childHandle);

    // Retrieve tile info for a child (returns {0,1,1} if not found).
    TileSpanInfo
    getTileSpanInfo(go_handle childHandle) const;

    go_handle
    findChildAtExactRegion(int tile, int spanRows, int spanCols) const;

    go_handle
    findChildAtUpperLeftTile(int tile) const;

    go_handle
    findChildCoveringTile(int tile) const;

    std::vector<go_handle>
    findChildrenOverlappingRegion(int tile, int spanRows, int spanCols) const;

    void
    ensureDynamicGridCanPlace(int spanRows, int spanCols);

    //--- Next-tile cursor -----------------------------------------------------
    int
    getNextTileIndex() const
    {
        return m_nextTileIndex;
    }
    void
    setNextTileIndex(int idx)
    {
        m_nextTileIndex = idx;
    }
    void
    advanceNextTileIndex();

    //--- Edge tiles -----------------------------------------------------------
    void
    registerEdgeTile(const std::wstring& edge, go_handle childHandle);

    go_handle
    getEdgeTile(const std::wstring& edge) const;

    bool
    isEdgeTileHandle(go_handle h) const;

private:
    // Occupancy map: tileIndex (1-based, row-major) → child handle
    // (-1 = empty).  The vector is resized to GridSize(0)*GridSize(1).
    std::vector<go_handle> m_tileOccupancy;

    // Per-child span info.
    std::unordered_map<go_handle, TileSpanInfo> m_tileSpan;

    // Cursor for sequential nexttile calls.
    int m_nextTileIndex = 1;

    // Edge tiles: "north"/"south"/"east"/"west" → handle.
    std::unordered_map<std::wstring, go_handle> m_edgeTiles;

    // --- spacing constants (normalised units) --------------------------------
    static constexpr double SPACING_LOOSE = 0.035;
    static constexpr double SPACING_COMPACT = 0.004;
    static constexpr double SPACING_TIGHT = 0.00;

    static constexpr double PADDING_LOOSE = 0.05;
    static constexpr double PADDING_COMPACT = 0.006;
    static constexpr double PADDING_TIGHT = 0.00;

    double
    getSpacingFraction();

    double
    getPaddingFraction();

    void
    rebuildOccupancy();

    void
    addLayoutChild(go_handle childHandle);

    void
    removeLayoutChild(go_handle childHandle);

    int
    getGridRows() const;

    int
    getGridCols() const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
