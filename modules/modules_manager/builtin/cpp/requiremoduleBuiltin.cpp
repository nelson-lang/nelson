//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "requiremoduleBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ModulesManager.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::requiremoduleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    std::wstring moduleshortname;
    if (argIn[0].isRowVectorCharacterArray()) {
        moduleshortname = argIn[0].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    bool bRes = IsExistingModuleName(moduleshortname);
    if (!bRes) {
        Error(L"Nelson:requiremodule:moduleNotInstalled", L"%s is not installed.",
            std::wstring(L"\'") + moduleshortname + std::wstring(L"\' "));
    }
    return retval;
}
//=============================================================================
