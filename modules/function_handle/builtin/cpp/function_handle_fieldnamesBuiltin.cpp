//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "function_handle_fieldnamesBuiltin.hpp"
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::function_handle_fieldnamesBuiltin(
    int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"struct");
    return retval;
}
//=============================================================================
