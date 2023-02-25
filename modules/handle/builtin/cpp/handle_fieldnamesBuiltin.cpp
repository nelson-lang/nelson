//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "handle_fieldnamesBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "HandleGenericObject.hpp"
#include "HandleManager.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HandleGateway::handle_fieldnamesBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    std::wstring handleTypeName = param1.getHandleCategory();
    if (handleTypeName == utf8_to_wstring(NLS_HANDLE_STR) || handleTypeName == L"") {
        Error(_W("Invalid handle."));
    }
    std::wstring ufunctionNameGetHandle = handleTypeName + L"_fieldnames";
    std::string functionNameGetHandle = wstring_to_utf8(ufunctionNameGetHandle);
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    if (!context->lookupFunction(functionNameGetHandle, funcDef)) {
        std::wstring msg = ufunctionNameGetHandle + L" " + _W("not defined.");
        Error(msg);
    }
    if ((funcDef->type() == NLS_BUILT_IN_FUNCTION) || (funcDef->type() == NLS_MACRO_FUNCTION)) {
        ArrayOfVector argInCopy;
        argInCopy.push_back(param1);
        if (argIn.size() == 2) {
            argInCopy.push_back(argIn[1]);
        }
        retval = funcDef->evaluateFunction(eval, argInCopy, nLhs);
    } else {
        std::wstring msg = ufunctionNameGetHandle + L" " + _W("not defined.");
        Error(msg);
    }
    return retval;
}
//=============================================================================
