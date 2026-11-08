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
// Room an outside decoration (legend, colorbar) takes from its peer axes: a slot on one
// side, off the outer box (auto layout) or off Position (manual), never updating the peer
// re-entrantly.
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
// Peer geometry before the slot was taken, in the peer's own Units.
struct AxesLayoutReservation
{
    std::vector<double> basePosition;
    std::vector<double> baseOuterPosition;
    bool outerDriven = false;
    bool
    active() const
    {
        return basePosition.size() == 4;
    }
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
// Keeps slotPixels free on one side of the peer and returns the box (parent fractions)
// the decoration anchors to; idempotent when the peer already has the slot.
NLSGRAPHICS_IMPEXP std::vector<double>
reserveAxesLayoutSlot(GraphicsObject* peer, AxesLayoutSide side, double slotPixels,
    AxesLayoutReservation& reservation);
//=============================================================================
// Gives the slot back to the peer. No-op without an active reservation.
NLSGRAPHICS_IMPEXP void
releaseAxesLayoutSlot(GraphicsObject* peer, AxesLayoutReservation& reservation);
//=============================================================================
// Places a pixelW x pixelH decoration at a Location: outside takes its slot (size + tick
// inset + 8pt), inside gives it back. Returns its box in figW x figH fractions, empty if none.
NLSGRAPHICS_IMPEXP std::vector<double>
layoutDecorationAroundAxes(GraphicsObject* peer, const std::wstring& location, double pixelW,
    double pixelH, double figW, double figH, AxesLayoutReservation& reservation);
//=============================================================================
// Updates the peer now, or marks it for its next update when it is already
// updating (its repaint runs the decorations).
NLSGRAPHICS_IMPEXP void
requestPeerAxesUpdate(GraphicsObject* peer);
//=============================================================================
} // namespace Nelson
//=============================================================================
