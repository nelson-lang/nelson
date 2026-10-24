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
} // namespace Nelson
//=============================================================================
