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
        Error(_W("Impossible to get external modules directory."));
    }
    retval << ArrayOf::characterArrayConstructor(externalModulesPath);
    return retval;
}
//=============================================================================
