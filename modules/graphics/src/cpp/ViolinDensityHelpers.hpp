//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GraphicsObject.hpp"
#include "RenderInterface.hpp"
#include <string>
#include <vector>
//=============================================================================
// Kernel density and violin drawing shared by the violinplot and
// raincloudplot graphics objects (implemented in GOViolinPlot.cpp).
//=============================================================================
namespace Nelson::ViolinDensityHelpers {
//=============================================================================
// Gaussian kernel density of values sampled on a uniform grid of centers.
void
densityShape(
    const std::vector<double>& values, std::vector<double>& centers, std::vector<double>& widths);
//=============================================================================
// Sorted unique positions of the finite (x, y) pairs.
std::vector<double>
uniquePositions(const std::vector<double>& x, const std::vector<double>& y);
//=============================================================================
// Finite y values whose x equals position.
std::vector<double>
valuesAtPosition(const std::vector<double>& x, const std::vector<double>& y, double position);
//=============================================================================
// Fills and strokes one violin shape; direction is 'both', 'positive' or 'negative'.
void
drawViolin(RenderInterface& gc, GraphicsObject* object, double position,
    const std::vector<double>& values, const std::vector<double>& centers,
    const std::vector<double>& widths, const std::wstring& direction, bool horizontal,
    const std::vector<double>& faceColor, const std::vector<double>& edgeColor);
//=============================================================================
} // namespace Nelson::ViolinDensityHelpers
//=============================================================================
