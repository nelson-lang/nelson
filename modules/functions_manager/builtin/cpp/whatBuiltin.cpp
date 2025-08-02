//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "whatBuiltin.hpp"
#include "Error.hpp"
#include "What.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::whatBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 0, 0);
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(WhatListOfBuiltin(eval));
    if (nLhs == 2) {
        retval << ArrayOf::toCellArrayOfCharacterColumnVectors(WhatListOfMacro(eval));
    }
    return retval;
}
//=============================================================================
