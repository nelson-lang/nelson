//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "delaunaynBuiltin.hpp"
#include "GeometryBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GeometryGateway::delaunaynBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    Geometry::QhullInput input = arrayToQhullInput(argIn[0]);
    if (input.pointCount == input.dimension + 1 && !Geometry::qhullInputIsDegenerate(input)) {
        std::vector<std::vector<int>> simplex(1);
        for (size_t k = 0; k < input.pointCount; ++k) {
            simplex[0].push_back(static_cast<int>(k + 1));
        }
        ArrayOfVector retval;
        retval << facetsToArray(simplex);
        return retval;
    }
    std::string defaults = input.dimension >= 4 ? "d Qt Qbb Qc Qz Qx" : "d Qt Qbb Qc Qz";
    Geometry::FacetResult result = Geometry::qhullDelaunay(input, qhullOptions(argIn, 1, defaults));
    ArrayOfVector retval;
    retval << facetsToArray(result.facets);
    return retval;
}
//=============================================================================
