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
#include "i18n.hpp"
#include "ModulesManager.hpp"
#include "characters_encoding.hpp"
#include "PredefinedErrorMessages.hpp"
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
        moduleshortname = argIn[0].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    bool bRes = IsExistingModuleName(moduleshortname);
    if (!bRes) {
        Error(std::wstring(L"\'") + moduleshortname + std::wstring(L"\' ")
            + _W(" is not installed."));
    }
    return retval;
}
//=============================================================================
