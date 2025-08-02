//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isbuiltinBuiltin.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::isbuiltinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    std::wstring name = param1.getContentAsWideString();
    std::string uname = wstring_to_utf8(name);
    bool res = false;
    std::wstring filename;
    res = BuiltInFunctionDefManager::getInstance()->find(uname, filename);
    retval << ArrayOf::logicalConstructor(res);
    return retval;
}
//=============================================================================
