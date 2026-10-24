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

    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_TILED_LAYOUT_STR;
    }

    void
    registerProperties() override;

    void
    updateState() override;

    void
    paintMe(RenderInterface& gc) override;

    std::pair<int, int>
    tileIndexToRowCol(int tileIndex);

    int
    rowColToTileIndex(int row, int col);

    int
    findNextEmptyTile(int spanRows = 1, int spanCols = 1);

    void
    updateChildPositions();

    // For "flow": pick the best grid size given current child count.
    void
    reflowTiles();

    // Update the grid from user code.  This is only valid while the
    // layout is empty and forces TileArrangement to "fixed".
    void
    setGridSizeFromUser(int rows, int cols);

    void
    setGridSizeInternal(int rows, int cols, bool fireCallback = true);

    void
    setTileArrangementInternal(const std::wstring& arrangement);

    void
    initializeTextObjects(go_handle layoutHandle);

    bool
    hasTileChildren() const;

    // Replaces any previous entry for the child.
    void
    registerChildTile(go_handle childHandle, int tile, int spanRows, int spanCols);

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

    void
    registerEdgeTile(const std::wstring& edge, go_handle childHandle);

    go_handle
    getEdgeTile(const std::wstring& edge) const;

    bool
    isEdgeTileHandle(go_handle h) const;

private:
    // Occupancy map: tileIndex (1-based, row-major) --> child handle
    // (-1 = empty).  The vector is resized to GridSize(0)*GridSize(1).
    std::vector<go_handle> m_tileOccupancy;

    std::unordered_map<go_handle, TileSpanInfo> m_tileSpan;

    int m_nextTileIndex = 1;

    // Edge tiles: "north"/"south"/"east"/"west" --> handle.
    std::unordered_map<std::wstring, go_handle> m_edgeTiles;

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
