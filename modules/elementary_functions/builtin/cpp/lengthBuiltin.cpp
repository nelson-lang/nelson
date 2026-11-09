//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lengthBuiltin.hpp"
#include "DimensionQueries.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadRequired.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::lengthBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    retval << ArrayOf::doubleScalar(arrayLength(argIn[0]));
    return retval;
}
//=============================================================================
