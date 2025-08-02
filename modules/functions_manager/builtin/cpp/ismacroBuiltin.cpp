//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismacroBuiltin.hpp"
#include "Error.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::ismacroBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string name = argIn[0].getContentAsCString();
    wstringVector filenames;
    bool res = PathFunctionIndexerManager::getInstance()->find(name, filenames);
    retval << ArrayOf::logicalConstructor(res);
    return retval;
}
//=============================================================================
