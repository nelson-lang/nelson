//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "blanksBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "Blanks.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::blanksBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    double nbSpacesAsDouble = param1.getContentAsDoubleScalar();
    if (!std::isfinite(nbSpacesAsDouble)) {
        raiseError(L"Nelson:string:ERROR_NAN_INF_NOT_ALLOWED", ERROR_NAN_INF_NOT_ALLOWED);
    }
    if (floor(nbSpacesAsDouble) != nbSpacesAsDouble) {
        raiseError(
            L"Nelson:string:ERROR_EXPECTED_AN_INTEGER_VALUE", ERROR_EXPECTED_AN_INTEGER_VALUE);
    }
    if (nbSpacesAsDouble < 0) {
        nbSpacesAsDouble = 0;
    }
    auto nbSpaces = (indexType)nbSpacesAsDouble;
    retval << Blanks(nbSpaces);
    return retval;
}
//=============================================================================
