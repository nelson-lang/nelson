//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isvarBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IsVariable.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::MemoryGateway::isvarBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf param1 = argIn[0];
        std::wstring varName = param1.asWideString();
        bool res = IsVariable(eval, SCOPE_LEVEL::LOCAL_SCOPE, varName);
        retval << ArrayOf::logicalScalar(res);
    } else {
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        std::wstring scopeName = param1.asWideString();
        std::wstring varName = param2.asWideString();
        bool res = false;
        if (scopeName == L"global") {
            res = IsVariable(eval, SCOPE_LEVEL::GLOBAL_SCOPE, varName);
        } else if (scopeName == L"base") {
            res = IsVariable(eval, SCOPE_LEVEL::BASE_SCOPE, varName);
        } else if (scopeName == L"local") {
            res = IsVariable(eval, SCOPE_LEVEL::LOCAL_SCOPE, varName);
        } else if (scopeName == L"caller") {
            res = IsVariable(eval, SCOPE_LEVEL::CALLER_SCOPE, varName);
        } else {
            Error(L"Nelson:isvar:invalidScope", L"%s expected.",
                L"Argument #1 : 'global', 'base', 'local' or 'caller'");
        }
        retval << ArrayOf::logicalScalar(res);
    }
    return retval;
}
//=============================================================================
