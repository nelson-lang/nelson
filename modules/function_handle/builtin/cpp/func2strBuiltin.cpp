//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "func2strBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::func2strBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf arg1 = argIn[0];
    if (arg1.isFunctionHandle()) {
        function_handle fh = arg1.getContentAsFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        if (cp) {
            retval << ArrayOf::characterArrayConstructor(cp->toString());
        } else {
            Error(_W("Invalid anonymous function."));
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
    }
    return retval;
}
//=============================================================================
