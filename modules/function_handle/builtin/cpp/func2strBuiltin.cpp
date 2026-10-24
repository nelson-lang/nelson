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
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
        function_handle fh = arg1.asFunctionHandle();
        AnonymousMacroFunctionDef* cp
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        if (cp) {
            retval << ArrayOf::characterArray(cp->toString());
        } else {
            Error(L"Nelson:func2str:invalidAnonymousFunction", L"Invalid anonymous function.");
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"function handle");
    }
    return retval;
}
//=============================================================================
