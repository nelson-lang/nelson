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
#include "ArrayOf.hpp"
#include "ContourPoint.hpp"
#include "nlsGraphics_exports.h"
#include <cstddef>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
contourCoordinateData(const ArrayOf& zData, const ArrayOf& coordData, bool isXcoord);
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<double>
defaultContourLevels(ArrayOf zData);
//=============================================================================
// The round step the automatic levels are multiples of. Returns 0 when the
// data has no finite range, which is also what a constant surface reports.
NLSGRAPHICS_IMPEXP double
defaultContourLevelStep(ArrayOf zData);
//=============================================================================
NLSGRAPHICS_IMPEXP contourLineSet
generateContourLines(ArrayOf zData, double level, const ArrayOf& xData, const ArrayOf& yData);
//=============================================================================
// Fingerprint of the inputs the contour lines come from: grid size, every z/x/y
// value and the level list. GOContour keeps the last one and skips re-tracing
// when it is unchanged, because getLimits() regenerates on every axis-layout pass.
NLSGRAPHICS_IMPEXP std::size_t
contourInputsSignature(const ArrayOf& zData, const ArrayOf& xData, const ArrayOf& yData,
    const std::vector<double>& levels);
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
buildContourMatrix(const contourLineCollection& contourLines, const std::vector<double>& levels);
//=============================================================================
NLSGRAPHICS_IMPEXP ArrayOf
buildContourMatrix(
    ArrayOf zData, const ArrayOf& xData, const ArrayOf& yData, const std::vector<double>& levels);
//=============================================================================
} // namespace Nelson
//=============================================================================
