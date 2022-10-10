//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include "usermodulesdirBuiltin.hpp"
#include "Error.hpp"
#include "GetExternalModulesPath.hpp"
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
    bool haveError = false;
    try {
        haveError = !std::filesystem::is_directory(externalModulesPath);
    } catch (const std::filesystem::filesystem_error&) {
        haveError = true;
    }
    if (haveError) {
        Error(_W("Impossible to get external modules directory."));
    }
    retval << ArrayOf::characterArrayConstructor(externalModulesPath);
    return retval;
}
//=============================================================================
