//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "semverBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "CompareVersions.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::semverBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    std::string param1 = argIn[0].getContentAsCString();
    std::string param2 = argIn[1].getContentAsCString();
    std::string errorMessage;
    int value = CompareVersions(param1, param2, errorMessage);
    if (!errorMessage.empty()) {
        Error(_W("Cannot compare versions: ") + utf8_to_wstring(errorMessage));
    } else {
        retval << ArrayOf::doubleConstructor(value);
    }
    return retval;
}
//=============================================================================
