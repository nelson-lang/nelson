//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <limits>
#include "__contourc__Builtin.hpp"
#include "ContourGenerator.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::GraphicsGateway {
//=============================================================================
ArrayOfVector
__contourc__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 4, 4);
    ArrayOf zData(argIn[0]);
    ArrayOf xData(contourCoordinateData(zData, argIn[1], true));
    ArrayOf yData(contourCoordinateData(zData, argIn[2], false));

    std::vector<double> levels;
    if (argIn[3].isEmpty()) {
        levels = defaultContourLevels(zData);
        // Automatic levels report only the contour lines strictly inside the data: a top
        // level that lands on the data maximum would draw just the isolated peak points,
        // so it is dropped here (1:9 -> levels 2..8). The contour object keeps it in
        // LevelList.
        ArrayOf zDouble(zData);
        zDouble.promoteType(NLS_DOUBLE);
        const double* pz = static_cast<const double*>(zDouble.dataPointer());
        double zMax = 0;
        bool found = false;
        for (indexType k = 0; k < zDouble.elementCount(); k++) {
            if (std::isfinite(pz[k]) && (!found || pz[k] > zMax)) {
                zMax = pz[k];
                found = true;
            }
        }
        if (found) {
            const double tolerance
                = 4. * std::numeric_limits<double>::epsilon() * std::max(1., std::fabs(zMax));
            std::vector<double> trimmed;
            trimmed.reserve(levels.size());
            for (double level : levels) {
                if (level < zMax - tolerance) {
                    trimmed.push_back(level);
                }
            }
            levels.swap(trimmed);
        }
    } else {
        ArrayOf levelData(argIn[3]);
        levelData.promoteType(NLS_DOUBLE);
        const double* pLevels = static_cast<const double*>(levelData.dataPointer());
        levels.assign(pLevels, pLevels + levelData.elementCount());
    }

    ArrayOfVector retval;
    retval << buildContourMatrix(zData, xData, yData, levels);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
