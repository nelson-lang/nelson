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
#include <string>
#include <vector>
#include <utility>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<double>
convertUnitsVector(const std::wstring& fromUnit, const std::wstring& toUnit, double x, double y,
    double w, double h, double widthCanvas, double heightCanvas);
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<double>
convertFromPixels(const std::wstring& toUnit, double x, double y, double w, double h,
    double widthCanvas, double heightCanvas);
//=============================================================================
NLSGRAPHICS_IMPEXP std::pair<double, double>
convertFromPixels(
    const std::wstring& toUnit, double x, double y, double widthCanvas, double heightCanvas);
//=============================================================================
NLSGRAPHICS_IMPEXP const std::pair<double, double>
convertToPixels(
    const std::wstring& fromUnit, double x, double y, double widthCanvas, double heightCanvas);
//=============================================================================
NLSGRAPHICS_IMPEXP void
convertPosition(const std::wstring& from, const std::wstring& to, std::vector<double>& position,
    double widthCanvas, double heightCanvas);
//=============================================================================
// Same conversions for a [x y w h] position, with the origin of pixels at 1 like MATLAB: the
// first pixel of the parent is at 1, so only x and y are shifted by one pixel.
NLSGRAPHICS_IMPEXP std::vector<double>
convertPositionPixelOrigin(const std::wstring& fromUnit, const std::wstring& toUnit, double x,
    double y, double w, double h, double widthCanvas, double heightCanvas);
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<double>
convertFromPixelsPixelOrigin(const std::wstring& toUnit, double x, double y, double w, double h,
    double widthCanvas, double heightCanvas);
//=============================================================================
NLSGRAPHICS_IMPEXP void
convertPositionPixelOrigin(const std::wstring& from, const std::wstring& to,
    std::vector<double>& position, double widthCanvas, double heightCanvas);
//=============================================================================
// Position in the given units to the zero-based pixels the layout code works with.
NLSGRAPHICS_IMPEXP void
convertPositionToZeroBasedPixels(const std::wstring& from, std::vector<double>& position,
    double widthCanvas, double heightCanvas);
//=============================================================================
} // namespace Nelson
//=============================================================================
