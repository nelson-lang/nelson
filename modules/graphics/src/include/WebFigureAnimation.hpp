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
#include <cstddef>
#include <cstdint>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
// Kept as forward declarations so lightweight consumers (builtin gateways) can
// use the frame-throttle API without the render_web include path.
class GOFigure;
struct WebGlMesh;
//=============================================================================
struct WebFigureAnimationFrameState
{
    bool refreshGeometry = true;
    bool refreshLayout = true;
    bool matrixOnly = false;
};
//=============================================================================
NLSGRAPHICS_IMPEXP bool
tryRenderWebFigureViewOnlyFrame(int64_t handle, GOFigure* fig, int width, int height,
    void (*sink)(void*, const char*, int), void* ctx, bool allowVisibleAxis = false);
//=============================================================================
NLSGRAPHICS_IMPEXP WebFigureAnimationFrameState
beginWebFigureAnimationFrame(int64_t handle, const GOFigure* fig, int width, int height,
    uint64_t displaySignature, uint64_t layoutSignature, const std::vector<WebGlMesh>& meshes,
    bool softwareFallbackEnabled);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
webFigureAnimationMeshChanged(int64_t handle, size_t meshIndex, uint64_t revision);
//=============================================================================
NLSGRAPHICS_IMPEXP void
endWebFigureAnimationFrame(int64_t handle, int width, int height, uint64_t displaySignature,
    uint64_t layoutSignature, const std::vector<WebGlMesh>& meshes);
//=============================================================================
NLSGRAPHICS_IMPEXP void
markWebFigureViewOnlyFrame(int64_t handle);
//=============================================================================
// Drop the retained animation state of a figure. Must be called when the figure
// is closed: figure handles are recycled, so a surviving entry would let a brand
// new figure inherit the mesh revisions of the previous one and receive
// matrix-only frames for geometry the client no longer holds.
NLSGRAPHICS_IMPEXP void
forgetWebFigureAnimation(int64_t handle);
//=============================================================================
// Shared throttle for streamed web frames. Several producers can push a frame
// for the same figure inside one script statement (property-set repaint,
// drawnow, view()); sharing the last-push clock keeps them from rendering the
// same state twice back to back.
NLSGRAPHICS_IMPEXP void
noteWebFigureFramePushed(int64_t handle);
NLSGRAPHICS_IMPEXP bool
webFigureFramePushedWithin(int64_t handle, long long microseconds);
//=============================================================================
} // namespace Nelson
//=============================================================================
