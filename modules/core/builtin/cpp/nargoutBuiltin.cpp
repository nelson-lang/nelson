//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nargoutBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "NargOut.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::nargoutBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 1);
    if (argIn.empty()) {
        Context* context = eval->getContext();
        if (context->getCurrentScope()->getName() == "base") {
            Error(_W("not allowed in base scope."));
        } else {
            int nargout = context->getCurrentScope()->getNargOut();
            retval << ArrayOf::doubleConstructor(nargout);
        }
    } else {
        // argIn.size() == 1
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring name = param1.getContentAsWideString();
            retval << ArrayOf::doubleConstructor(NargOut(eval, name));
        } else if (param1.isFunctionHandle()) {
            function_handle fh = param1.getContentAsFunctionHandle();
            if (fh.anonymousHandle == nullptr) {
                raiseError(L"Nelson:core:ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED",
                    ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED, 1, NLS_FUNCTION_HANDLE_STR);
            }
            AnonymousMacroFunctionDef* anonymousFunction
                = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
            if (anonymousFunction) {
                if (anonymousFunction->isFunctionHandle()) {
                    retval << ArrayOf::doubleConstructor(
                        NargOut(eval, utf8_to_wstring(anonymousFunction->getName())));
                } else {
                    retval << ArrayOf::doubleConstructor(anonymousFunction->nargout());
                }
            }
        } else {
            raiseError(
                L"Nelson:core:ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_FUNCTION_HANDLE_EXPECTED",
                ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_FUNCTION_HANDLE_EXPECTED, 1);
        }
    }
    return retval;
}
//=============================================================================
