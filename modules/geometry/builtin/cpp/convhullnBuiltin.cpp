//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "convhullnBuiltin.hpp"
#include "GeometryBuiltinHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::GeometryGateway::convhullnBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1, 2);
    Geometry::QhullInput input = arrayToQhullInput(argIn[0]);
    std::string defaults = input.dimension >= 5 ? "Qt Qx" : "Qt";
    Geometry::FacetResult result
        = Geometry::qhullConvexHull(input, qhullOptions(argIn, 1, defaults));
    ArrayOfVector retval;
    retval << facetsToArray(result.facets);
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar(result.volume);
    }
    return retval;
}
//=============================================================================
