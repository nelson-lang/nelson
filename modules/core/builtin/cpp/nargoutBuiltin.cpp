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
#include "ErrorID.hpp"
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
            Error(L"Nelson:core:notAllowedInBaseScope", L"not allowed in base scope.");
        } else {
            int nargout = context->getCurrentScope()->getNargOut();
            retval << ArrayOf::doubleScalar(nargout);
        }
    } else {
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring name = param1.asWideString();
            retval << ArrayOf::doubleScalar(NargOut(eval, name));
        } else if (param1.isFunctionHandle()) {
            function_handle fh = param1.asFunctionHandle();
            if (fh.anonymousHandle == nullptr) {
                Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                    L"function handle");
            }
            AnonymousMacroFunctionDef* anonymousFunction
                = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
            if (anonymousFunction) {
                if (anonymousFunction->isBoundNestedFunctionHandle()) {
                    // A handle to a nested function is not resolvable by name from
                    // this scope; introspect the bound function directly.
                    retval << ArrayOf::doubleScalar(anonymousFunction->boundFunctionNargout());
                } else if (anonymousFunction->isFunctionHandle()) {
                    retval << ArrayOf::doubleScalar(
                        NargOut(eval, utf8_to_wstring(anonymousFunction->getName())));
                } else {
                    retval << ArrayOf::doubleScalar(anonymousFunction->nargout());
                }
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string or function handle");
        }
    }
    return retval;
}
//=============================================================================
