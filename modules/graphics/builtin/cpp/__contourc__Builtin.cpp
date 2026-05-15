//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
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
    } else {
        ArrayOf levelData(argIn[3]);
        levelData.promoteType(NLS_DOUBLE);
        const double* pLevels = static_cast<const double*>(levelData.getDataPointer());
        levels.assign(pLevels, pLevels + levelData.getElementCount());
    }

    ArrayOfVector retval;
    retval << buildContourMatrix(zData, xData, yData, levels);
    return retval;
}
//=============================================================================
} // namespace Nelson::GraphicsGateway
//=============================================================================
