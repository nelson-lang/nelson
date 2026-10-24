//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "voronoinBuiltin.hpp"
#include "GeometryBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GeometryGateway::voronoinBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 2);
    Geometry::QhullInput input = arrayToQhullInput(argIn[0]);
    std::string defaults = input.dimension >= 4 ? "v Qbb Qz Qx" : "v Qbb Qz";
    Geometry::VoronoiResult result
        = Geometry::qhullVoronoi(input, qhullOptions(argIn, 1, defaults));
    ArrayOfVector retval;
    retval << verticesToArray(result.vertices, result.vertexCount, result.dimension);
    if (nLhs > 1) {
        retval << cellsToArray(result.cells);
    }
    return retval;
}
//=============================================================================
