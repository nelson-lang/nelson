//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "removegatewayBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "RemoveGateway.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ModulesManagerGateway::removegatewayBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    if (argIn[0].isRowVectorCharacterArray()) {
        std::wstring dynlibName = argIn[0].asWideString();
        RemoveGateway(eval, dynlibName);
    } else {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    return retval;
}
//=============================================================================
