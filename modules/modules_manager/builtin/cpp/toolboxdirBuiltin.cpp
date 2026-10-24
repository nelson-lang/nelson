//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "toolboxdirBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ModulePath.hpp"
#include "ModulesHelpers.hpp"
#include "ModulesManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::toolboxdirBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::wstring moduleshortname;
    if (argIn[0].isRowVectorCharacterArray()) {
        moduleshortname = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (IsExistingModuleName(moduleshortname)) {
        retval << ArrayOf::characterArray(GetModulePath(moduleshortname));
    } else {
        Error(L"Nelson:modules_manager:invalidModuleName", L"invalid module name.");
    }
    return retval;
}
//=============================================================================
