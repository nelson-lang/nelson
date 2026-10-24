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
#include "RenderInterface.hpp"
//=============================================================================
NLSGRAPHICS_IMPEXP void
DrawSymbol(RenderInterface& gc, RenderInterface::SymbolType symb, double x, double y, double z,
    double sze, std::vector<double> edgecolor, std::vector<double> fillcolor, double width);
//=============================================================================
// Batched equivalent of a per-point DrawSymbol loop: draws every marker of one
// shape in a single backend call. xs/ys are pre-map pixel centres, sizes the
// per-point marker size (same value DrawSymbol takes). edgeColorsFlat and
// faceColorsFlat hold 4 doubles (r,g,b,a in 0..1) per point, or are empty for a
// 'none' outline / fill. Returns true when the backend drew the batch (or there
// was nothing to draw); false when it cannot batch, so the caller keeps its
// per-point DrawSymbol fallback.
// depths (optional) carries the per-point clip-space z, one per xs/ys entry, so a
// 3D backend can occlude markers that lie behind an opaque surface; pass it empty
// for a 2D plot, where no occlusion is wanted.
NLSGRAPHICS_IMPEXP bool
DrawSymbolBatch(RenderInterface& gc, RenderInterface::SymbolType symb,
    const std::vector<double>& xs, const std::vector<double>& ys, const std::vector<double>& sizes,
    const std::vector<double>& edgeColorsFlat, const std::vector<double>& faceColorsFlat,
    double width, const std::vector<double>& depths = {});
//=============================================================================
// Convenience over DrawSymbolBatch for the common case where every marker shares
// one edge colour, face colour and size (plot/quiver/errorbar/... markers): the
// per-point flat colour arrays are built by repetition. A colour whose first
// component is -1 (or shorter than 3) is treated as 'none'. Returns true when the
// batch was drawn, false when the backend cannot batch (caller falls back).
NLSGRAPHICS_IMPEXP bool
DrawSymbolBatchUniform(RenderInterface& gc, RenderInterface::SymbolType symb,
    const std::vector<double>& xs, const std::vector<double>& ys, double sze,
    const std::vector<double>& edgeColor, const std::vector<double>& faceColor, double width,
    const std::vector<double>& depths = {});
//=============================================================================
NLSGRAPHICS_IMPEXP double
pointsToPixels(double points);
//=============================================================================
NLSGRAPHICS_IMPEXP double
fontPointsToQtPoints(double points);
//=============================================================================
NLSGRAPHICS_IMPEXP RenderInterface::SymbolType
StringToSymbol(const std::wstring& symbolName);
//=============================================================================
