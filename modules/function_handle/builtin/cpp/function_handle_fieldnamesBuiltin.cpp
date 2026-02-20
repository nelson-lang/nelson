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
#include "PredefinedErrorMessages.hpp"
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
    // we do not allow to display the fields
    raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), NLS_STRUCT_ARRAY_STR, 1);
    return retval;
}
//=============================================================================
