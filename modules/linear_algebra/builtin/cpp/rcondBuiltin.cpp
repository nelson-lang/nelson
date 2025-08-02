//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rcondBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "ReciprocalConditionNumber.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::rcondBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isLogical()
        || argIn[0].isCharacterArray() || argIn[0].isIntegerType()) {
        OverloadRequired("rcond");
    }
    retval << ReciprocalConditionNumber(argIn[0], false);
    return retval;
}
//=============================================================================
