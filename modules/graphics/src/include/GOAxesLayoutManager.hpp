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
#include <string>
#include <vector>
#include "GraphicsObject.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Room outside decorations (legends, colorbars) take from their peer axes. Each side
// stacks its decorations in arrival order; the peer keeps one base geometry, shrunk off
// the outer box (auto layout) or off Position (manual), never updated re-entrantly.
//=============================================================================
enum class AxesLayoutSide
{
    None,
    East,
    West,
    North,
    South
};
//=============================================================================
// Side an outside Location sits on; None for an inside Location.
NLSGRAPHICS_IMPEXP AxesLayoutSide
axesLayoutSideFromLocation(const std::wstring& location);
//=============================================================================
// Gap between the axes box and a decoration: 8 points.
NLSGRAPHICS_IMPEXP double
axesLayoutGapPixels();
//=============================================================================
// Pixels taken by the tick labels, axis label and title on that side of the peer.
NLSGRAPHICS_IMPEXP double
axesTightInsetPixels(GraphicsObject* peer, AxesLayoutSide side);
//=============================================================================
// Peer Position as fractions of its parent, whatever its Units.
NLSGRAPHICS_IMPEXP std::vector<double>
axesPositionNormalized(GraphicsObject* peer);
//=============================================================================
// Box a 2-D peer really draws inside the given box (parent fractions): a locked
// aspect ratio shrinks and centres it; unchanged for an unconstrained peer.
NLSGRAPHICS_IMPEXP std::vector<double>
axesDrawnBox(GraphicsObject* peer, const std::vector<double>& box, double width, double height);
//=============================================================================
// Outer box of the peer before its decorations took room; false when none does.
NLSGRAPHICS_IMPEXP bool
axesLayoutBaseOuterPosition(GraphicsObject* peer, std::vector<double>& outerPosition);
//=============================================================================
// Gives the slot of a decoration back to the peer; no-op when it holds none.
NLSGRAPHICS_IMPEXP void
releaseAxesLayoutSlot(GraphicsObject* peer, GraphicsObject* decoration);
//=============================================================================
// Drops a destroyed decoration from every stack without touching the peers.
NLSGRAPHICS_IMPEXP void
forgetAxesLayoutDecoration(GraphicsObject* decoration);
//=============================================================================
// Places a pixelW x pixelH decoration at a Location: outside stacks it on its side
// (size + 8pt), inside gives the slot back. Returns its box in figW x figH fractions.
NLSGRAPHICS_IMPEXP std::vector<double>
layoutDecorationAroundAxes(GraphicsObject* peer, GraphicsObject* decoration,
    const std::wstring& location, double pixelW, double pixelH, double figW, double figH);
//=============================================================================
// Places an outside colorbar on its side stack. Returns its box in figW x figH
// fractions, empty for an inside Location.
NLSGRAPHICS_IMPEXP std::vector<double>
layoutColorBarAroundAxes(GraphicsObject* peer, GraphicsObject* decoration,
    const std::wstring& location, double figW, double figH);
//=============================================================================
// Updates the peer now, or marks it for its next update when it is already
// updating (its repaint runs the decorations).
NLSGRAPHICS_IMPEXP void
requestPeerAxesUpdate(GraphicsObject* peer);
//=============================================================================
} // namespace Nelson
//=============================================================================
