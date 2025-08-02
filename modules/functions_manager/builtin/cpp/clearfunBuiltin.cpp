//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "clearfunBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClearFunction.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::clearfunBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 1);
    ArrayOf param1 = argIn[0];
    std::wstring functionname;
    if (param1.isRowVectorCharacterArray()) {
        functionname = argIn[0].getContentAsWideString();
    } else if (param1.isFunctionHandle()) {
        function_handle fh = param1.getContentAsFunctionHandle();
        if (fh.anonymousHandle) {
            AnonymousMacroFunctionDef* anonymousFunction
                = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
            if (anonymousFunction->isFunctionHandle()) {
                functionname = utf8_to_wstring(anonymousFunction->getContent());
            }
        }
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    }
    retval << ArrayOf::logicalConstructor(ClearBuiltin(functionname));
    return retval;
}
//=============================================================================
