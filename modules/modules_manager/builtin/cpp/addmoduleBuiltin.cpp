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
#include "StringHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
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
        modulerootpath = argIn[0].getContentAsWideString();
        if (!StringHelpers::ends_with(modulerootpath, L"\\")
            && (!StringHelpers::ends_with(modulerootpath, L"/"))) {
            modulerootpath.append(L"/");
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    if (argIn[1].isRowVectorCharacterArray()) {
        moduleshortname = argIn[1].getContentAsWideString();
    } else {
        Error(ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED);
    }
    AddModule(eval, modulerootpath, moduleshortname);
    return retval;
}
//=============================================================================
