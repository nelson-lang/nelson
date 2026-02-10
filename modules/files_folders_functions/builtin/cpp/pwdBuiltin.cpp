//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pwdBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GetCurrentDirectory.hpp"
#include "NelsonPrint.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FilesFoldersGateway::pwdBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    std::wstring pwd = GetCurrentDirectory();
    if (pwd.empty()) {
        raiseError(L"Nelson:files_folders_functions:ERROR_IMPOSSIBLE_TO_GET_CURRENT_DIRECTORY",
            ERROR_IMPOSSIBLE_TO_GET_CURRENT_DIRECTORY);
    }
    retval << ArrayOf::characterArrayConstructor(pwd);
    return retval;
}
//=============================================================================
