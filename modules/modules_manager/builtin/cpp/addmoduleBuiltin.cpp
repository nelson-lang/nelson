//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "addmoduleBuiltin.hpp"
#include "AddModule.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::addmoduleBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);
    std::wstring modulerootpath;
    std::wstring moduleshortname;
    if (argIn[0].isRowVectorCharacterArray()) {
        modulerootpath = argIn[0].asWideString();
        if (!StringHelpers::ends_with(modulerootpath, L"\\")
            && (!StringHelpers::ends_with(modulerootpath, L"/"))) {
            modulerootpath.append(L"/");
        }
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (argIn[1].isRowVectorCharacterArray()) {
        moduleshortname = argIn[1].asWideString();
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    AddModule(eval, modulerootpath, moduleshortname);
    return retval;
}
//=============================================================================
