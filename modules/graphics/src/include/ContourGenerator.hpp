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
NLSGRAPHICS_IMPEXP contourLineSet
generateContourLines(ArrayOf zData, double level, const ArrayOf& xData, const ArrayOf& yData);
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
