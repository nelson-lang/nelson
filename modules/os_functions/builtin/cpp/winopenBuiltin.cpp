//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "winopenBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "WinOpen.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::winopenBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
#ifdef _MSC_VER
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    std::wstring cmd;
    if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        cmd = argIn[0].getContentAsWideString();
    } else {
        raiseError(L"Nelson:os:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
            ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_STRING_ARRAY_STR);
    }
    if (!WinOpen(cmd)) {
        raiseError(L"Nelson:os_functions:ERROR_FILENAME_NOT_ASSOCIATED_TO_AN_APPLICATION",
            ERROR_FILENAME_NOT_ASSOCIATED_TO_AN_APPLICATION);
    }
#else
    raiseError(L"Nelson:os_functions:ERROR_NOT_IMPLEMENTED_ON_PLATFORM",
        ERROR_NOT_IMPLEMENTED_ON_PLATFORM);
#endif
    return retval;
}
//=============================================================================
