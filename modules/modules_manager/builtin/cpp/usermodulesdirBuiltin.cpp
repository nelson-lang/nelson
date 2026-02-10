//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "usermodulesdirBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "GetExternalModulesPath.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::usermodulesdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    std::wstring externalModulesPath = GetExternalModulesPath();
    if (!FileSystemWrapper::Path::is_directory(externalModulesPath)) {
        raiseError(L"Nelson:modules_manager:ERROR_IMPOSSIBLE_TO_GET_EXTERNAL_MODULES_DIRECTORY",
            ERROR_IMPOSSIBLE_TO_GET_EXTERNAL_MODULES_DIRECTORY);
    }
    retval << ArrayOf::characterArrayConstructor(externalModulesPath);
    return retval;
}
//=============================================================================
