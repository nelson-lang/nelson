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
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isDefaultColorBarAxesPosition(const std::vector<double>& pos);
//=============================================================================
bool
isColorBarPeer3D(GraphicsObject* peer);
//=============================================================================
bool
isColorBarPeerContour(GraphicsObject* peer);
//=============================================================================
bool
isColorBarPeerInTiledLayout(GraphicsObject* peer);
//=============================================================================
std::vector<double>
tiledColorBarPeerPosition(GraphicsObject* peer, const std::vector<double>& base);
//=============================================================================
std::vector<double>
tiledColorBarPosition(const std::vector<double>& base, const std::wstring& location, bool peer3D);
//=============================================================================
std::vector<double>
defaultColorBarPosition(const std::wstring& location, bool peer3D, bool peerContour,
    double figureWidth, double figureHeight);
//=============================================================================
std::vector<double>
defaultColorBarPosition(const std::vector<double>& base, const std::wstring& location, bool peer3D,
    bool peerContour, double figureWidth, double figureHeight);
//=============================================================================
std::vector<double>
defaultColorBarPeerPositionForOutside(const std::wstring& location, bool peer3D, bool peerContour,
    double figureWidth, double figureHeight);
//=============================================================================
std::vector<double>
defaultColorBarPeerPositionForOutside(const std::vector<double>& base, const std::wstring& location,
    bool peer3D, bool peerContour, double figureWidth, double figureHeight);
//=============================================================================
// With a locked aspect ratio (axis image / equal / square) the data is drawn in a
// smaller box centered in the axes Position rectangle, and a docked colorbar hugs
// that box. Given the colorbar position computed against peerBox (the rectangle
// it was anchored to), returns the position shifted and resized to hug the drawn
// box. Returned unchanged for an unconstrained 2D peer or a non-axes peer.
std::vector<double>
constrainedColorBarPosition(GraphicsObject* peer, const std::wstring& location,
    const std::vector<double>& colorBarPosition, const std::vector<double>& peerBox,
    double figureWidth, double figureHeight);
//=============================================================================
} // namespace Nelson
//=============================================================================
