//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "blanksBuiltin.hpp"
#include "Error.hpp"
#include "Blanks.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::blanksBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    double nbSpacesAsDouble = param1.getContentAsDoubleScalar();
    if (!std::isfinite(nbSpacesAsDouble)) {
        Error(_W("NaN and Inf not allowed."));
    }
    if (floor(nbSpacesAsDouble) != nbSpacesAsDouble) {
        Error(_W("Expected a integer value."));
    }
    if (nbSpacesAsDouble < 0) {
        nbSpacesAsDouble = 0;
    }
    auto nbSpaces = (indexType)nbSpacesAsDouble;
    retval << Blanks(nbSpaces);
    return retval;
}
//=============================================================================
