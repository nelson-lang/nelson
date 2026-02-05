//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "COM_methodsBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "methodsComHandleObject.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ComEngineGateway::COM_methodsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    ArrayOf param1 = argIn[0];
    if (!param1.isHandle()) {
        raiseError(L"Nelson:com_engine:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
            ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_HANDLE_STR);
    }
    wstringVector methods;
    methodsComHandleObject(param1, methods);
    retval << ArrayOf::toCellArrayOfCharacterColumnVectors(methods);
    return retval;
}
//=============================================================================
