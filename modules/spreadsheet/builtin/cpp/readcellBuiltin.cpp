//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "readcellBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpreadsheetGateway::readcellBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);

    return retval;
}
//=============================================================================
